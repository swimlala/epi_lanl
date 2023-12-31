CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:30Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170930  20220204114426  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�\���1   @�]hK��@5�7KƧ��b��l�C�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B33B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�#�D�T{D���D���D�D�X�D��3D��D�{D�P�D��=D���D�!�D�VDڇ�D��\D��D�S�D�\D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~{@�=q@�=qA�A9�AY�Ay�A��\A��\A��\A��\Ȁ\A܏\A�\A��\BG�BG�B{Bz�B&G�B.G�B6G�B>G�BFG�BNG�BVG�B^G�BfG�BnG�BvG�B~G�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D d{D �{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�Dd{D�{D	d{D	�{D
d{D
�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�D^D�{Dd{D�{Dj�D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{D d{D �{D!d{D!�{D"d{D"�{D#d{D#�{D$d{D$�{D%d{D%�{D&d{D&�{D'd{D'�{D(d{D(�{D)d{D)�{D*d{D*�{D+d{D+�{D,d{D,�{D-d{D-�{D.d{D.�{D/d{D/�{D0d{D0�{D1d{D1�{D2d{D2�{D3d{D3�{D4d{D4�{D5d{D5�{D6d{D6�{D7d{D7�{D8d{D8�{D9d{D9�{D:d{D:�{D;d{D;�{D<d{D<�{D=d{D=�{D>d{D>�{D?d{D?�{D@d{D@�{DAd{DA�{DBd{DB�{DCd{DC�{DDd{DD�{DEd{DE�{DFd{DF�{DGd{DG�{DHd{DH�{DId{DI�{DJ^DJ�{DKd{DK�{DLd{DL�{DMd{DM�{DNd{DN�{DOd{DO�{DPd{DP�{DQd{DQ�{DRd{DR�{DSd{DS�{DTd{DT�{DUd{DU�{DVd{DV�{DWd{DW�{DXd{DX�{DYd{DY�{DZd{DZ�{D[d{D[�{D\d{D\�{D]d{D]�{D^d{D^�{D_d{D_�{D`d{D`�{Dad{Da�{Dbd{Db�{Dcd{Dc�{Ddd{Dd�{Ded{De�{Dfd{Df�{Dgd{Dg�{Dh^Dh�{Did{Di�{Djd{Dj�{Dkd{Dk�{Dld{Dl�{Dmd{Dm�{Dnd{Dn�{Dod{Do�{Dpd{Dp�{Dqd{Dq�{Drd{Dr�{Dsd{Ds�{Dtd{Dt�Dy�
D�D�F�D���D��D�\D�J�D�}pD��QD��D�C3D�|zD��3D�)D�HQD�y�D�љD�3D�E�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��
A��#A��TA��HA��mA��mA��`A��`A��yA��TA��HA��A�ĜAź^Aź^AŅA�9XA�r�A��A�?}A��^A�?}A�A���A�%A���A�JA��/A��A�I�A���A���A��A�=qA�ȴA�S�A�
=A��-A���A���A���A�bNA�1'A�&�A���A�=qA��^A�E�A�VA���A�K�A���A�bNA�  A��A�7LA���A��wA�z�A�dZA���A���A�-A���A���A�-A�oA���A���A�&�A��A��A��#A�+A���A�A��DA�9XA��A�&�A�oA�/A��A�ĜA���A���A�|�A�bNA��/A���A�9XA�oA��uA�ȴA�^5A���A���A�oA�7LA�;dA�{A��hA�O�A���A�bA�z�A�|�A���A�jA��A{��AyoAu�7Ar^5Ap��Ao7LAk��Aj�!AhAeXAb��Aa�-A^I�A[|�AZA�AYƨAXE�AV�+AU��AUoAS|�AQ��AP��AO��ANjALĜAK��AKAH��AFAE�ADn�AC"�AA��A@bNA?l�A>VA<��A;��A:bNA7�A6~�A5��A4�A37LA29XA1��A0�A.��A-�wA,^5A+|�A*ȴA)l�A(��A(1A'�A&bA#�#A"�HA �jA 1A��A�AG�A�A��A��A^5A��A33AĜA�A��A�wAK�A��A{A�-AȴAI�A�#A��A`BA1'AG�A
ZA	hsA9XA��A�7A��A�PAȴAbNAl�A�yA-A�#A�A �!@���@�^5@��/@�|�@�7L@��m@��+@��@�(�@�;d@�E�@�b@�\)@��y@�+@�$�@��-@�%@��;@��@�D@�E�@�/@�bN@�\)@�%@�ƨ@�33@�J@�r�@� �@��
@�t�@��y@�x�@��;@�@�^5@��@��y@��@��@���@·+@�E�@���@�r�@�l�@�M�@��T@�hs@�Ĝ@��
@�ȴ@Ų-@���@Ý�@�C�@�x�@�A�@��
@�ff@��@�I�@�1'@��F@�v�@���@���@� �@�|�@��y@���@�E�@��@�%@��D@�A�@���@���@�K�@�5?@���@�&�@���@�z�@��
@�o@�~�@��@�V@��@���@���@�  @��;@��@���@�M�@�@���@�hs@��@���@�x�@�G�@�%@���@�9X@��;@��@�S�@�@��H@��@���@���@���@�n�@��T@�O�@��`@���@��m@���@��P@�S�@��@��+@���@��^@�x�@���@�1@��w@�;d@�
=@��H@��\@�V@���@��@�G�@�V@��/@��j@��u@�bN@�A�@� �@��@���@��P@�\)@���@���@��+@��@���@���@�`B@��@���@�Ĝ@���@�Z@�1'@��@�ƨ@�\)@�"�@��@�o@���@��+@��+@�~�@�n�@�V@�E�@�{@��^@��h@��@�V@���@��9@���@�z�@�Z@�1@�  @��m@��w@�t�@�
=@��R@�E�@���@�`B@��@��/@���@�z�@�j@�j@��@�r�@�(�@��
@���@���@��@�t�@�S�@���@��@�@�x�@�?}@��@���@��`@���@�bN@�1'@��@�1'@�bN@�bN@�Q�@�(�@��@�\)@�o@�ȴ@���@�ff@�-@��@���@��h@�O�@�V@�Ĝ@�j@�9X@��@��@��m@�\)@�+@��@��H@��\@�~�@�~�@�ff@�$�@��#@���@��-@���@�p�@�&�@���@��/@��@��@�Z@�9X@��m@��@��F@�|�@�C�@�j�@yF@qe,@h_@`֡@W��@P��@I��@FM�@?˒@:p;@4��@0V�@*��@$�4@�h@�@$t@�D@b�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A��
A��#A��TA��HA��mA��mA��`A��`A��yA��TA��HA��A�ĜAź^Aź^AŅA�9XA�r�A��A�?}A��^A�?}A�A���A�%A���A�JA��/A��A�I�A���A���A��A�=qA�ȴA�S�A�
=A��-A���A���A���A�bNA�1'A�&�A���A�=qA��^A�E�A�VA���A�K�A���A�bNA�  A��A�7LA���A��wA�z�A�dZA���A���A�-A���A���A�-A�oA���A���A�&�A��A��A��#A�+A���A�A��DA�9XA��A�&�A�oA�/A��A�ĜA���A���A�|�A�bNA��/A���A�9XA�oA��uA�ȴA�^5A���A���A�oA�7LA�;dA�{A��hA�O�A���A�bA�z�A�|�A���A�jA��A{��AyoAu�7Ar^5Ap��Ao7LAk��Aj�!AhAeXAb��Aa�-A^I�A[|�AZA�AYƨAXE�AV�+AU��AUoAS|�AQ��AP��AO��ANjALĜAK��AKAH��AFAE�ADn�AC"�AA��A@bNA?l�A>VA<��A;��A:bNA7�A6~�A5��A4�A37LA29XA1��A0�A.��A-�wA,^5A+|�A*ȴA)l�A(��A(1A'�A&bA#�#A"�HA �jA 1A��A�AG�A�A��A��A^5A��A33AĜA�A��A�wAK�A��A{A�-AȴAI�A�#A��A`BA1'AG�A
ZA	hsA9XA��A�7A��A�PAȴAbNAl�A�yA-A�#A�A �!@���@�^5@��/@�|�@�7L@��m@��+@��@�(�@�;d@�E�@�b@�\)@��y@�+@�$�@��-@�%@��;@��@�D@�E�@�/@�bN@�\)@�%@�ƨ@�33@�J@�r�@� �@��
@�t�@��y@�x�@��;@�@�^5@��@��y@��@��@���@·+@�E�@���@�r�@�l�@�M�@��T@�hs@�Ĝ@��
@�ȴ@Ų-@���@Ý�@�C�@�x�@�A�@��
@�ff@��@�I�@�1'@��F@�v�@���@���@� �@�|�@��y@���@�E�@��@�%@��D@�A�@���@���@�K�@�5?@���@�&�@���@�z�@��
@�o@�~�@��@�V@��@���@���@�  @��;@��@���@�M�@�@���@�hs@��@���@�x�@�G�@�%@���@�9X@��;@��@�S�@�@��H@��@���@���@���@�n�@��T@�O�@��`@���@��m@���@��P@�S�@��@��+@���@��^@�x�@���@�1@��w@�;d@�
=@��H@��\@�V@���@��@�G�@�V@��/@��j@��u@�bN@�A�@� �@��@���@��P@�\)@���@���@��+@��@���@���@�`B@��@���@�Ĝ@���@�Z@�1'@��@�ƨ@�\)@�"�@��@�o@���@��+@��+@�~�@�n�@�V@�E�@�{@��^@��h@��@�V@���@��9@���@�z�@�Z@�1@�  @��m@��w@�t�@�
=@��R@�E�@���@�`B@��@��/@���@�z�@�j@�j@��@�r�@�(�@��
@���@���@��@�t�@�S�@���@��@�@�x�@�?}@��@���@��`@���@�bN@�1'@��@�1'@�bN@�bN@�Q�@�(�@��@�\)@�o@�ȴ@���@�ff@�-@��@���@��h@�O�@�V@�Ĝ@�j@�9X@��@��@��m@�\)@�+@��@��H@��\@�~�@�~�@�ff@�$�@��#@���@��-@���@�p�@�&�@���@��/@��@��@�Z@�9X@��m@��@��F@�|�G�O�@�j�@yF@qe,@h_@`֡@W��@P��@I��@FM�@?˒@:p;@4��@0V�@*��@$�4@�h@�@$t@�D@b�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
,B
,B
-B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
+B
+B
)�B
(�B
&�B
8RB
>wB
?}B
C�B
G�B
J�B
N�B
T�B
R�B
XB
[#B
`BB
l�B
s�B
�B
��B
�?B
��B
��B
�B
�NB
�ZB
��BB	7BbB!�B%�B8RBA�BP�BbNB|�B�1B�VB��B��B�!B�dB��B�B�HB�TB�sB�B��B��B��B1B	7B
=BVB{B�B!�B&�B/B0!B8RB8RB49B2-B+B&�B�B�BuBbB+B��B�B�/B��B��B�dB�?B�B�B��B��B�PB~�Bn�BZB>wB(�BJB
�B
��B
�'B
�DB
VB
VB	�ZB	��B	�LB	��B	�bB	�B	m�B	dZB	ZB	D�B	9XB	2-B	 �B	�B	JB		7B	+B��B��B��B��B�sB�ZB�B��BŢBÖB�wB�FB��B��B��B��B��B��B��B��B�bB�hB�hB�JB�+B�B�B|�Bx�Bw�Bu�Bt�Br�Bn�BjBiyBiyBn�Bn�BjBcTB]/BXBXBS�BQ�BN�BL�BI�BH�BG�BF�BE�BD�BB�B@�B<jB7LB6FB5?B49B49B33B1'B0!B-B,B)�B-B0!B+B(�B+B-B.B,B'�B&�B(�B'�B(�B'�B'�B&�B%�B#�B"�B!�B#�B#�B#�B%�B$�B%�B%�B&�B&�B&�B&�B&�B&�B%�B&�B&�B&�B(�B'�B'�B(�B,B-B-B.B/B/B/B0!B0!B1'B2-B33B2-B5?B8RB:^B<jB>wB>wB>wB>wBA�BB�BE�BE�BF�BG�BJ�BK�BL�BR�BR�BS�BZB]/B^5BbNBbNBdZBdZBgmBm�Bp�Bp�Br�Bs�Bt�Bu�Bv�Bx�B{�Bz�Bz�B{�B{�B|�B�B�B�B�%B�+B�JB�oB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�XB�wBBŢB��B��B��B��B��B��B��B��B��B��B��B�#B�;B�fB�B�B��B��B��B��B��B��B	B	B	%B	DB	hB	uB	�B	�B	�B	�B	 �B	#�B	&�B	(�B	-B	/B	0!B	2-B	49B	6FB	7LB	:^B	<jB	?}B	B�B	F�B	G�B	K�B	O�B	P�B	T�B	[#B	_;B	bNB	cTB	e`B	hsB	jB	m�B	o�B	s�B	t�B	t�B	t�B	x�B	y�B	y�B	z�B	{�B	|�B	}�B	� B	�B	�%B	�1B	�DB	�VB	�\B	�bB	�bB	�hB	�hB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�3B	�9B	�?B	�FB	�LB	�LB	�LB	�RB	�XB	�^B	�jB	�}B	B	B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�mB	�sB	�yB	�yB	�MB
�B
B
!�B
(sB
2�B
?cB
BuB
IlB
O�B
TaB
X�B
Z�B
`vB
f�B
k�B
o�B
s�B
v+B
z�B
}V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
(�B
/B
0B
40B
8HB
;ZB
?rB
E�B
C�B
H�B
K�B
P�B
]!B
dLB
t�B
�1B
��B
�B
�eB
țB
��B
��B
�QB
�B
��B �BNBfB(�B2	BAcBR�BmgBx�B~�B�B�XB��B��B�bBȀBѷB��B��B��B�AB�ZB�fB��B��B��B��B�BB4BQB�B �B(�B(�B$�B"�BmBUB+B�B�B �B��B�mB�BͧB�MB�HB��B��B��B��B�cB�B}�Bo�B_ BJ�B/B�B
��B
�B
�`B
��B
{�B
F�B	�B	�B	��B	�B	��B	�-B	u�B	^aB	U+B	J�B	5rB	*0B	#B	�B	_B�)B�B�B��B�B�B�B�YB�@B��B��B��B��B�cB�4B��B��B��B��B��B��B��B�uB�WB�]B�]B}ABx#BuBsBm�Bi�Bh�Bf�Be�Bc�B_�B[}BZwBZwB_�B_�B[~BTTBN0BIBIBD�BB�B?�B=�B:�B9�B8�B7�B6�B5�B3�B1�B-sB(VB'PB&IB%DB%DB$>B"2B!-BBB	BB!.BBBBB"BB B�BB BBBB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BBBBBB#B#B)B 0B 0B 0B!6B!6B"<B#CB$IB#CB&UB)hB+tB-�B/�B/�B/�B/�B2�B3�B6�B6�B7�B8�B;�B<�B=�BDBDBEBK1BNCBOIBSbBSbBUnBUnBX�B^�Ba�Ba�Bc�Bd�Be�Bf�Bg�Bi�Bl�Bk�Bk�Bl�Bl�Bn BrBsBt$Bw7Bx=B}[B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�)B�;B�eB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�,B�DB�nBޘB�B��B��B��B��B��B� B�B�B�*B�IB	lB	yB	
�B	�B	�B	�B	�B	�B	�B	�B	B	 B	!"B	#.B	%:B	'FB	(LB	+^B	-jB	0|B	3�B	7�B	8�B	<�B	@�B	A�B	E�B	LB	P7B	SJB	TPB	V[B	YnB	[zB	^�B	`�B	d�B	e�B	e�B	e�B	i�B	j�B	j�B	k�B	l�B	m�B	n�B	p�B	uB	wB	y)B	|;B	MB	�SB	�YB	�YB	�_B	�_B	�_B	�_B	�fB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�-B	�3B	�:B	�?B	�?B	�@B	�EB	�KB	�QB	�]B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�2B	�2B	�8B	�8B	�8B	�>B	�DB	�IB	�OB	�\B	�bB	�hG�O�B	�;B	��B

jB
�B
]B
#�B
0KB
3]B
:TB
@�B
EHB
I�B
K�B
Q\B
W�B
\kB
`�B
dgB
gB
k�B
n:111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.43 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.006) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144262022020411442620220204114426  AO  ARCAADJP                                                                    20200619170930    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170930  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170930  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114426  IP                  G�O�G�O�G�O�                