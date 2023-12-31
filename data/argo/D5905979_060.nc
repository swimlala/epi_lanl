CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:08Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170908  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               <A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ة|�T�1   @ة+��@6��G�{�c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    <A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�	�D�`�D��=D��{D��D�S�D��fD��fD�,�D�S3D��D��fD��D�O�Dڒ=D��
D�"=D�W\D��D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�\)@�\)A�A;�A[�A{�A��
A��
A�
=A��
A��
A��
A��
A��
B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�B�B�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD n�D �Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D	n�D	�D
n�D
�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�DuD�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D n�D �D!n�D!�D"n�D"�D#n�D#�D$n�D$�D%n�D%�D&n�D&�D'n�D'�D(n�D(�D)n�D)�D*uD*�D+n�D+�D,n�D,�D-n�D-�D.n�D.�D/n�D/�D0n�D0�D1n�D1�D2n�D2�RD3n�D3�D4n�D4�D5n�D5�D6n�D6�D7n�D7�D8n�D8�D9n�D9�D:n�D:�D;n�D;�D<n�D<�D=n�D=�D>n�D>�D?n�D?�D@n�D@�DAn�DA�DBn�DB�DCn�DC�DDn�DD�DEn�DE�DFn�DF�DGn�DG�DHn�DH�DIn�DI�DJn�DJ�DKhRDK�DLn�DL�DMn�DM�DNn�DN�DOn�DO�DPn�DP�DQn�DQ�DRn�DR�DSn�DS�DTn�DT�DUn�DU�DVn�DV�DWn�DW�DXn�DX�DYn�DY�DZn�DZ�D[n�D[�D\n�D\�D]n�D]�D^n�D^�D_n�D_�D`n�D`�Dan�Da�Dbn�Db�Dcn�Dc�Ddn�Dd�Den�De�Dfn�Df�Dgn�Dg�Dhn�Dh�Din�Di�Djn�Dj�Dkn�Dk�Dln�Dl�Dmn�Dm�Dnn�Dn�Don�Do�Dpn�Dp�Dqn�Dq�Drn�Dr�Dsn�Ds�Dtn�Dt��Dy��D�HD�XRD���D���D��D�J�D���D���D�$)D�J�D��pD���D�
D�G
Dډ�D��fD��D�N�D�RD��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��hA�ZA�9XA�&�A� �A��A��mA��jA���A��A�r�A�`BA�M�A�?}A�1'A�&�A��A�{A�oA�JA���A��A��TA��;A��HA��HA��;A��/A��/A��/A��/A��#A��#A��#A��#A��A��
A���A���A���A���A��\A�v�A�{A��mA�r�A��A��7A��/A��A��;A�K�A�ƨA��
A��A���A��A�^5A�bNA�M�A�oA��+A���A��A��!A���A�+A�7LA��PA��!A�~�A��9A�
=A��A���A���A�l�A��
A��A�ffA���A���A�$�A�~�A���A�?}A�&�A�?}A�A��A�G�A���A�;dA� �A�A�A���A�A��/A��
A�x�A���A�v�A�
=A~��A}�7A}
=A|=qAyXAx~�Aw��At�!ArVAq��Ap-Anr�Amt�Ak�;Aj1'Ah��AeƨAb5?Aa��A`��A_;dA^r�A]�A\$�AZbNAX��AXM�AWhsAVz�AV�AUS�AS\)AQ?}AOƨAO��ANȴAL�AJ�\AI+AHjAH�AGAGS�AF�uAF=qAE�#AC%ABE�AA�^A@Q�A?`BA=7LA;dZA:A9�^A9|�A933A8�/A7\)A6 �A4�A3t�A2�A1�A1��A0��A0�+A01A.�`A.  A-O�A,�+A+�;A+��A+p�A+A*^5A)�PA(�uA'�wA'S�A&��A%�^A#�TA"$�A �yA|�AĜA^5A  AE�A�9AO�A��A5?AE�A"�A��A��A~�A&�A|�A�A�A$�A"�A
1A	hsA	/Av�A��Ar�A�
A��AK�AȴA��A9XA;dA I�@��T@�Z@���@�x�@�j@��F@�?}@�o@�\@�5?@�h@�C�@���@��/@�1'@�`B@�K�@��H@�\@�^5@���@��@�9X@�n�@�Z@߅@��y@�E�@ݙ�@���@���@�C�@��@ٙ�@�Ĝ@���@׾w@�dZ@��@�G�@Ӆ@�S�@�K�@�v�@�V@��@�=q@�p�@�Q�@ʸR@ɉ7@�?}@���@�l�@�ȴ@�n�@�J@��`@���@�S�@�v�@���@���@���@���@�ff@���@�I�@��
@�dZ@���@�ȴ@���@�E�@��#@�`B@�9X@��@��R@��@���@��m@���@���@�ff@�@�O�@�z�@��m@���@�t�@�K�@��y@���@�A�@��@�n�@���@���@�bN@�Z@�  @��;@���@�\)@��@�v�@�{@�@�O�@���@��u@��@���@��y@��+@�~�@�M�@�$�@�{@�{@��@���@�O�@�X@��h@�X@�7L@�&�@���@�z�@� �@��m@��@���@��P@�K�@��H@�M�@��-@��@�%@�V@��u@�I�@�I�@�1@�ƨ@���@�dZ@�S�@�K�@�l�@�S�@�;d@��@��R@�^5@�=q@�@��#@�@�p�@���@�bN@�9X@��;@���@�S�@���@�ff@���@�7L@��j@��D@�b@��
@��@���@�S�@��@���@��@���@�v�@�E�@��@���@�7L@�%@�&�@���@�I�@��;@��
@��F@�+@�ȴ@��R@��R@���@�n�@�M�@�=q@���@��#@���@�@��@�z�@� �@��
@��w@��@���@�S�@�"�@�ȴ@��+@�ff@�-@�@��h@�p�@�X@�G�@�7L@��@���@���@���@�bN@�1'@�  @��w@��@�C�@��y@�v�@�E�@�{@��@���@��^@���@���@��@�X@�7L@�?}@�?}@�G�@��@�Ĝ@��@���@���@��u@��u@�r�@�A�@�9X@�  @���@���@��[@x�@n�,@f��@_��@Yk�@Q�^@J��@C��@=A @5hs@/�@,7�@(��@#S�@�)@S&@�@�4@_@�Q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A��hA�ZA�9XA�&�A� �A��A��mA��jA���A��A�r�A�`BA�M�A�?}A�1'A�&�A��A�{A�oA�JA���A��A��TA��;A��HA��HA��;A��/A��/A��/A��/A��#A��#A��#A��#A��A��
A���A���A���A���A��\A�v�A�{A��mA�r�A��A��7A��/A��A��;A�K�A�ƨA��
A��A���A��A�^5A�bNA�M�A�oA��+A���A��A��!A���A�+A�7LA��PA��!A�~�A��9A�
=A��A���A���A�l�A��
A��A�ffA���A���A�$�A�~�A���A�?}A�&�A�?}A�A��A�G�A���A�;dA� �A�A�A���A�A��/A��
A�x�A���A�v�A�
=A~��A}�7A}
=A|=qAyXAx~�Aw��At�!ArVAq��Ap-Anr�Amt�Ak�;Aj1'Ah��AeƨAb5?Aa��A`��A_;dA^r�A]�A\$�AZbNAX��AXM�AWhsAVz�AV�AUS�AS\)AQ?}AOƨAO��ANȴAL�AJ�\AI+AHjAH�AGAGS�AF�uAF=qAE�#AC%ABE�AA�^A@Q�A?`BA=7LA;dZA:A9�^A9|�A933A8�/A7\)A6 �A4�A3t�A2�A1�A1��A0��A0�+A01A.�`A.  A-O�A,�+A+�;A+��A+p�A+A*^5A)�PA(�uA'�wA'S�A&��A%�^A#�TA"$�A �yA|�AĜA^5A  AE�A�9AO�A��A5?AE�A"�A��A��A~�A&�A|�A�A�A$�A"�A
1A	hsA	/Av�A��Ar�A�
A��AK�AȴA��A9XA;dA I�@��T@�Z@���@�x�@�j@��F@�?}@�o@�\@�5?@�h@�C�@���@��/@�1'@�`B@�K�@��H@�\@�^5@���@��@�9X@�n�@�Z@߅@��y@�E�@ݙ�@���@���@�C�@��@ٙ�@�Ĝ@���@׾w@�dZ@��@�G�@Ӆ@�S�@�K�@�v�@�V@��@�=q@�p�@�Q�@ʸR@ɉ7@�?}@���@�l�@�ȴ@�n�@�J@��`@���@�S�@�v�@���@���@���@���@�ff@���@�I�@��
@�dZ@���@�ȴ@���@�E�@��#@�`B@�9X@��@��R@��@���@��m@���@���@�ff@�@�O�@�z�@��m@���@�t�@�K�@��y@���@�A�@��@�n�@���@���@�bN@�Z@�  @��;@���@�\)@��@�v�@�{@�@�O�@���@��u@��@���@��y@��+@�~�@�M�@�$�@�{@�{@��@���@�O�@�X@��h@�X@�7L@�&�@���@�z�@� �@��m@��@���@��P@�K�@��H@�M�@��-@��@�%@�V@��u@�I�@�I�@�1@�ƨ@���@�dZ@�S�@�K�@�l�@�S�@�;d@��@��R@�^5@�=q@�@��#@�@�p�@���@�bN@�9X@��;@���@�S�@���@�ff@���@�7L@��j@��D@�b@��
@��@���@�S�@��@���@��@���@�v�@�E�@��@���@�7L@�%@�&�@���@�I�@��;@��
@��F@�+@�ȴ@��R@��R@���@�n�@�M�@�=q@���@��#@���@�@��@�z�@� �@��
@��w@��@���@�S�@�"�@�ȴ@��+@�ff@�-@�@��h@�p�@�X@�G�@�7L@��@���@���@���@�bN@�1'@�  @��w@��@�C�@��y@�v�@�E�@�{@��@���@��^@���@���@��@�X@�7L@�?}@�?}@�G�@��@�Ĝ@��@���@���@��u@��u@�r�@�A�@�9X@�  @���G�O�@��[@x�@n�,@f��@_��@Yk�@Q�^@J��@C��@=A @5hs@/�@,7�@(��@#S�@�)@S&@�@�4@_@�Q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB+B)�B,B.B.B-B-B.B.B/B0!B1'B2-B2-B33B49B5?B6FB7LB7LB8RB9XB8RB8RB9XB9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB<jB>wB?}B@�B?}BC�BH�BK�BK�BW
B\)BZBcTBs�Bm�BhsBffBe`BdZBaHB^5BQ�BH�BB�B<jB6FB'�B�B\BB��B�ZB�B��B�?B�'B�B��B��B��B�PB�Bs�BaHBXBP�BD�B>wB<jB33B#�B�B�B\B1B
��B
�B
�ZB
�B
�
B
��B
��B
�dB
��B
�VB
w�B
iyB
e`B
aHB
P�B
F�B
@�B
0!B
�B
hB
1B	��B	�B	�B	�TB	�)B	��B	�?B	�3B	��B	��B	��B	��B	�PB	�B	z�B	u�B	q�B	k�B	hsB	bNB	XB	K�B	A�B	@�B	?}B	-B	"�B	�B	oB	bB	\B	PB	
=B	B	B��B�B�B�fB�BB�B��BȴBƨBŢBÖBB�}B�^B�'B�B�B�B�3B�'B�!B�B��B��B��B��B��B��B��B��B��B��B�uB�PB�DB�=B�%B|�Bo�BgmB^5BZBW
BT�BP�BK�BG�BC�BB�B@�B=qB<jB;dB7LB6FB5?B5?B33B2-B2-B0!B/B-B/B,B.B)�B)�B+B)�B(�B(�B)�B&�B(�B&�B&�B&�B&�B%�B'�B%�B%�B%�B$�B'�B%�B%�B$�B(�B'�B'�B'�B&�B&�B'�B'�B)�B+B+B,B,B,B,B+B.B.B.B/B/B/B/B1'B2-B5?B49B49B6FB8RB=qB=qB?}BA�BE�BF�BG�BH�BM�BP�BQ�BQ�BT�BW
BXB[#B^5BdZBjBm�Bs�B~�B�B�B�1B�7B�=B�JB�\B�bB�oB��B��B��B��B�B�-B�LB�RB�XB�jB�wB��BĜBŢBƨBƨBǮB��B��B�B�#B�HB�fB�yB�B�B�B�B�B�B��B��B��B	B	%B	1B	DB	PB	bB	oB	uB	�B	�B	�B	�B	�B	�B	&�B	+B	2-B	9XB	;dB	B�B	I�B	K�B	M�B	N�B	O�B	Q�B	T�B	T�B	W
B	VB	XB	^5B	bNB	cTB	ffB	jB	m�B	q�B	t�B	u�B	x�B	z�B	{�B	�B	�B	�B	�%B	�7B	�JB	�PB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�?B	�?B	�LB	�RB	�XB	�^B	�jB	�jB	�jB	�qB	�wB	��B	��B	��B	ĜB	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
gB
B
 B
!B
%�B
1AB
7�B
?B
E�B
HB
N"B
R�B
W$B
[WB
`BB
e�B
h�B
ncB
r�B
x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B#B!�B$
B&B&B%B%B&B&B'B(#B))B*/B*/B+5B,;B-AB.HB/MB/MB0SB1YB0SB0SB1YB1YB2_B2_B2_B2_B2_B2_B2_B2_B2_B2_B3eB3eB3eB4kB6xB7~B8�B7~B;�B@�BC�BC�BOBT*BRB[UBk�Be�B`tB^hB]bB\\BYJBV7BI�B@�B:�B4oB.LB�B�BeB�B��B�fB�B��B�OB�7B�+B��B��B��B�cB{&Bk�BY^BP'BH�B<�B6�B4�B+MB�B�B�ByB NB
��B
�B
�zB
�>B
�+B
�B
�B
��B
��B
�|B
o�B
a�B
]�B
YrB
IB
>�B
8�B
(OB
�B
	�B
 bB	�B	��B	��B	ۈB	�]B	�-B	�vB	�jB	�.B	�
B	��B	��B	��B	|TB	sB	m�B	i�B	c�B	`�B	Z�B	POB	DB	9�B	8�B	7�B	%PB	B	�B	
�B	�B	�B	�B	�B�dB�XB�B��B��BޮB؊B�YB�)B��B��B��B��B��B��B��B�tB�hB�[B�[B��B�tB�nB�bB�DB�+B�B�B�B��B��B��B��B��B��B��B��B��B~vBu@Bg�B_�BV�BRrBO_BMSBI;BDB@B;�B:�B8�B5�B4�B3�B/�B.�B-�B-�B+�B*�B*�B(|B'vB%iB'vB$cB&oB"XB"XB#^B"XB!RB!RB"XBFB!SBFBFBFBFB@B MBABABAB;B NBABAB;B!TB NB NB NBHBHB OB OB"[B#aB#aB$gB$gB$gB$gB#aB&sB&sB&sB'zB'zB'zB'zB)�B*�B-�B,�B,�B.�B0�B5�B5�B7�B9�B>B?B@BABF2BIDBJKBJKBM\BOhBPnBS�BV�B\�Bb�Be�BlBwWB{nB}{B��B��B��B��B��B��B��B��B�B�2B�DB�iB��B��B��B��B��B��B��B��B��B�B�B�B� B�QB�]B�|B٠B޾B��B��B��B��B�B�B�B�2B�EB�QB�iB�|B	 �B	�B	�B	�B	
�B	�B	�B	�B	B	B	B	B	>B	#WB	*�B	1�B	3�B	:�B	BB	DB	F&B	G,B	H2B	J?B	MPB	MPB	O\B	NVB	PbB	V�B	Z�B	[�B	^�B	b�B	e�B	i�B	mB	nB	q&B	s1B	t7B	z\B	|iB	}oB	~uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�1B	�PB	�bB	�iB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�8B	�PB	�VB	�cB	�cB	�cB	�cB	�iB	�iB	�uB	�{B	�{B	ցB	؎B	؎B	ٔB	ٔB	ٔB	ښB	ښB	ښB	ښB	۠B	ݫB	ݬB	ޱB	߸B	߸B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�G�O�B	�\B	��B
XB
	JB
kB
GB
)�B
/�B
7]B
>B
@_B
FjB
J�B
OlB
S�B
X�B
^EB
`�B
f�B
kB
pI111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.27 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170908    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170908  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170908  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                