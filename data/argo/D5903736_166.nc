CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041154  20190604094024  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�ڛ�4�=1   @�ڜ�3�@3Tz�G��d��hr�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�?�D�uqD�ɚD��D�@RD�l{D��HD�fD�O
D��=D���D��\D�0RD�
D�D�{D�G
D�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\A�\)Aޏ\A�\A��\BG�B�HBG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B��
B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�B��
B��B�p�B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC�RCE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnz�Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt��Dy�RD� D�9�D�o�D���D��D�:�D�f�D�ӅD��D�IGD��zD��
D���D�*�D�yGD�QD��D�AGD�QD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�/A�/A�1'A�/A�1'A�1'A�1'A�33A�5?A�5?A�1'A�-A�(�A�+AٮA�I�A�+A�VA��yA���A�ȴA�ƨA�AؼjAظRAضFAذ!AؑhA�x�A�z�A�O�A���A��
Aח�A�K�A֥�A�%A�dZA��/A��A�I�Aϣ�A�~�A���A�ZAŋDAİ!A��mA�ƨA�A��A��DA��!A�
=A���A�G�A���A�ĜA���A��A��TA���A��hA�K�A�7LA���A�z�A��A��!A�1A�VA��A��^A�Q�A���A��A��!A��A�oA�A�A�A�A��A��A�bA�/A�\)A�7LA�oA�~�A���A�I�A��A��/A��!A�ĜA��mA��#A��7A�JA�^5A�;dA�t�A�ƨA~ZA}��A{�A{\)Ay|�Awx�Au;dAs
=Ar�!Aq�^Ao/Am;dAk��AhVAgx�Ag�AfZAf  Adv�Aa�;A_�A_;dA]��A\ȴA[p�AYdZAWx�AU�PAS\)AQdZAO��AN�AM��AL  AJ�AH��AF�AE��AC�AA�TA?�wA>{A=�A=33A;��A7�#A5��A3&�A0�uA/�TA/�PA.��A.JA-�
A,jA*��A)33A(M�A'�7A&�RA%dZA$ �A#`BA!��A �A�PA�A1'AdZA{A`BA��A9XAt�A1'A��AA  A7LA��A��A��A��AZA�#A?}A+A7LA
1A	��A��Av�A�-A�RA�AJA��Av�A�A �@�;d@�G�@�S�@�%@��@�K�@���@�(�@�33@��y@�$�@��@�F@웦@�z�@���@���@�n�@��@��@��@�C�@�+@��@��#@��@��@�@ާ�@��@ܬ@�t�@�^5@٩�@�&�@�b@ץ�@֗�@�X@�bN@�t�@���@�{@�z�@�\)@�v�@͙�@���@̬@�ȴ@�7L@ǶF@��@�^5@őh@Ý�@¸R@¸R@¸R@�~�@��-@�`B@�z�@�b@��w@�ȴ@��-@�V@�A�@���@�33@��+@�~�@�E�@�-@���@���@�G�@�9X@��w@�C�@��@��R@�@�7L@�I�@�b@�;d@��H@��R@�v�@�@�J@��@��#@�G�@��9@���@�l�@��@�n�@�M�@��@��-@�7L@���@�1@�\)@���@�~�@�M�@��@�@���@��`@��@��P@��@�ƨ@��@�dZ@�{@���@��h@��@���@��j@��`@�%@��@�(�@��;@��
@�ƨ@��P@�l�@���@��R@�J@���@�O�@���@���@�I�@��@�
=@���@��@��H@�ȴ@���@�n�@��@��h@�G�@�V@���@�bN@�9X@� �@�1@��w@�\)@�;d@��@���@�$�@��@��#@���@���@���@���@���@��@�p�@���@�bN@�Z@�I�@�1'@���@��@���@��T@�O�@�?}@�7L@�x�@���@�`B@�V@��`@�Ĝ@���@��u@��D@�bN@�bN@� �@�1@��w@�dZ@�C�@�o@���@�n�@�V@��@�O�@��u@�  @���@���@��
@�  @���@���@�|�@�\)@�C�@�+@��@�v�@�=q@��@��@���@�?}@��@���@��`@��9@�z�@�Z@�I�@���@��m@��
@��P@�S�@���@��@�
=@�+@�
=@���@��H@��!@�v�@��@���@���@��@�hs@�G�@�&�@��9@�bN@�Z@�9X@��@�  @��;@��@�dZ@�;d@�o@�@��y@��@���@�v�@�E�@�=q@�E�@�-@��@���@�G�@�G�@�/@���@�Ĝ@���@��8@}%F@u�@n#:@f�@_�P@W�@O]�@G)_@A@:� @3l�@/�@)x�@$�@ l"@W?@E�@�@	l@	ϫ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�1'A�/A�/A�1'A�/A�1'A�1'A�1'A�33A�5?A�5?A�1'A�-A�(�A�+AٮA�I�A�+A�VA��yA���A�ȴA�ƨA�AؼjAظRAضFAذ!AؑhA�x�A�z�A�O�A���A��
Aח�A�K�A֥�A�%A�dZA��/A��A�I�Aϣ�A�~�A���A�ZAŋDAİ!A��mA�ƨA�A��A��DA��!A�
=A���A�G�A���A�ĜA���A��A��TA���A��hA�K�A�7LA���A�z�A��A��!A�1A�VA��A��^A�Q�A���A��A��!A��A�oA�A�A�A�A��A��A�bA�/A�\)A�7LA�oA�~�A���A�I�A��A��/A��!A�ĜA��mA��#A��7A�JA�^5A�;dA�t�A�ƨA~ZA}��A{�A{\)Ay|�Awx�Au;dAs
=Ar�!Aq�^Ao/Am;dAk��AhVAgx�Ag�AfZAf  Adv�Aa�;A_�A_;dA]��A\ȴA[p�AYdZAWx�AU�PAS\)AQdZAO��AN�AM��AL  AJ�AH��AF�AE��AC�AA�TA?�wA>{A=�A=33A;��A7�#A5��A3&�A0�uA/�TA/�PA.��A.JA-�
A,jA*��A)33A(M�A'�7A&�RA%dZA$ �A#`BA!��A �A�PA�A1'AdZA{A`BA��A9XAt�A1'A��AA  A7LA��A��A��A��AZA�#A?}A+A7LA
1A	��A��Av�A�-A�RA�AJA��Av�A�A �@�;d@�G�@�S�@�%@��@�K�@���@�(�@�33@��y@�$�@��@�F@웦@�z�@���@���@�n�@��@��@��@�C�@�+@��@��#@��@��@�@ާ�@��@ܬ@�t�@�^5@٩�@�&�@�b@ץ�@֗�@�X@�bN@�t�@���@�{@�z�@�\)@�v�@͙�@���@̬@�ȴ@�7L@ǶF@��@�^5@őh@Ý�@¸R@¸R@¸R@�~�@��-@�`B@�z�@�b@��w@�ȴ@��-@�V@�A�@���@�33@��+@�~�@�E�@�-@���@���@�G�@�9X@��w@�C�@��@��R@�@�7L@�I�@�b@�;d@��H@��R@�v�@�@�J@��@��#@�G�@��9@���@�l�@��@�n�@�M�@��@��-@�7L@���@�1@�\)@���@�~�@�M�@��@�@���@��`@��@��P@��@�ƨ@��@�dZ@�{@���@��h@��@���@��j@��`@�%@��@�(�@��;@��
@�ƨ@��P@�l�@���@��R@�J@���@�O�@���@���@�I�@��@�
=@���@��@��H@�ȴ@���@�n�@��@��h@�G�@�V@���@�bN@�9X@� �@�1@��w@�\)@�;d@��@���@�$�@��@��#@���@���@���@���@���@��@�p�@���@�bN@�Z@�I�@�1'@���@��@���@��T@�O�@�?}@�7L@�x�@���@�`B@�V@��`@�Ĝ@���@��u@��D@�bN@�bN@� �@�1@��w@�dZ@�C�@�o@���@�n�@�V@��@�O�@��u@�  @���@���@��
@�  @���@���@�|�@�\)@�C�@�+@��@�v�@�=q@��@��@���@�?}@��@���@��`@��9@�z�@�Z@�I�@���@��m@��
@��P@�S�@���@��@�
=@�+@�
=@���@��H@��!@�v�@��@���@���@��@�hs@�G�@�&�@��9@�bN@�Z@�9X@��@�  @��;@��@�dZ@�;d@�o@�@��y@��@���@�v�@�E�@�=q@�E�@�-@��@���@�G�@�G�@�/@���@�ĜG�O�@��8@}%F@u�@n#:@f�@_�P@W�@O]�@G)_@A@:� @3l�@/�@)x�@$�@ l"@W?@E�@�@	l@	ϫ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
[#B
B)�B�+B�JB�VB�uB��B��B��B��B��B��B�FB��BhB�B5?BD�BZBy�Bw�Bn�Be`BjBo�Bt�BbNBQ�B9XB�BDB
=BDBbB�B�B+B49B>wBP�BcTBl�Bv�B�B�VB�VB�JB�1B~�Bz�Bz�B}�B�{B��B��B��B��B��B�Bt�Bs�B]/BJ�B@�B)�B	7B��B�fB�
BŢB��B�oBk�BS�B<jB'�B�BPB  B
�B
�`B
�B
��B
��B
�}B
�-B
��B
�oB
�7B
�+B
}�B
{�B
r�B
cTB
S�B
E�B
A�B
8RB
"�B
oB
B	�fB	�BB	�5B	�#B	�B	��B	�}B	�'B	�B	��B	��B	�PB	�B	y�B	n�B	dZB	[#B	S�B	N�B	G�B	?}B	8RB	1'B	(�B	!�B	�B	uB	JB	+B	B	B��B�`B�B��B��BɺBƨBÖB��B��B�dB�?B�!B�B��B��B��B��B��B��B�oB�\B�=B�B�B�B� B~�B�B�B�B~�B}�B~�B}�B� B~�B}�B}�B|�B{�By�Bx�Bt�Bp�Bo�Bm�Bl�BjBgmBdZBe`BffBgmBhsBm�Bm�BiyBe`BhsBq�Bq�Bo�Bm�Bu�By�B�B�+B�DB�oB�{B�{B�uB�oB�uB�uB��B��B��B�B�'B�'B�9B�9B�LB�FB�FB�XB�qB�wB�qB�wB�wB�}B�}B�}B��B��BBĜBȴBȴBƨBŢB��B��B��B��B�B�B�B�/B�HB�HB�HB�TB�sB�yB�B�B�B�B��B��B��B��B��B	B	B	B	B	%B	%B	1B	JB	PB	VB	\B	bB	bB	bB	oB	�B	�B	�B	$�B	$�B	$�B	'�B	(�B	+B	-B	.B	1'B	2-B	49B	8RB	:^B	;dB	=qB	?}B	B�B	C�B	I�B	I�B	J�B	L�B	N�B	P�B	P�B	P�B	Q�B	S�B	\)B	]/B	aHB	aHB	aHB	aHB	cTB	e`B	iyB	k�B	n�B	p�B	r�B	t�B	x�B	z�B	{�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�+B	�=B	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�9B	�?B	�FB	�LB	�LB	�RB	�XB	�^B	�wB	ÖB	ÖB	B	ÖB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�yB	�mB	�mB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
hB
@B
�B
"B
)�B
3�B
9rB
@ B
HfB
OB
S�B
X�B
]�B
aHB
e�B
jeB
n�B
s�B
x�B
~]B
�;B
�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�B	�B	�B	�|B	�wB	�tB	�}B	�}B	�|B	�{B	�B	�B	�B	��B	��B
V�B
�9B%�B��B��B��B�B�AB�WB�iB�qB�xB��B��B�]BBLB0�B@<BU�ButBslBj7BaBfBk>Bp\B]�BM�B4�B,B�B�B�BB8BVB&�B/�B:BL�B^�Bh.BrgB~�B��B��B��B��Bz�Bv�Bv�By�B�B�_B��B��B��B�NB~�Bp]Bo[BX�BFdB<*B%�B�B�mB�BҵB�MB��B�Bg4BO�B8B#�BMB	B
��B
�MB
�B
��B
ΫB
�vB
�1B
��B
��B
�(B
��B
��B
y�B
w�B
nnB
_B
O�B
A]B
=HB
4B
�B
-B	��B	�&B	�B	��B	��B	��B	ͬB	�=B	��B	��B	��B	�QB	�B	|�B	u�B	jYB	`B	V�B	O�B	J�B	CtB	;GB	4B	,�B	$�B	�B	kB	?B	B	�B	 �B��B��B�/B��B̲BǖBŊB�yB�dB�RB�SB�4B�B��B��B��B��B��B�yB�kB�OB�BB�.B�B��B~�B|�B{�Bz�B|�B�B}�Bz�By�Bz�By�B{�Bz�By�By�Bx�Bw�Bu�Bt�Bp�BlwBkrBicBh_BfQBcAB`,Ba4Bb<Bc=BdGBieBihBeNBa5BdHBm�Bm~BktBigBq�Bu�B|�B��B�B�DB�QB�NB�JB�@B�KB�KB�nB��B��B��B��B��B�B�B� B�B�B�-B�GB�KB�DB�KB�MB�PB�PB�QB�WB�YB�eB�qBĉBĈB�|B�wBǘBɩB̹B��B��B��B��B�B�B�B�B�*B�EB�JB�dB�jB�nB�vB�B�B��B��B��B��B��B	 �B	 �B	�B	�B	B	B		"B	
'B	/B	5B	7B	6B	BB	_B	zB	�B	 �B	 �B	 �B	#�B	$�B	&�B	(�B	)�B	,�B	-�B	0B	4&B	6+B	78B	9DB	;PB	>`B	?kB	E�B	E�B	F�B	H�B	J�B	L�B	L�B	L�B	M�B	O�B	W�B	YB	]B	]B	]B	]B	_$B	a2B	eIB	gWB	jfB	ltB	n�B	p�B	t�B	v�B	w�B	{�B	|�B	}�B	~�B	~�B	�B	�B	�B	��B	��B	��B	�
B	�-B	�2B	�:B	�;B	�JB	�QB	�[B	�]B	�jB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�!B	�&B	�*B	�EB	�cB	�gB	�_B	�cB	ŋB	ǓB	ƍB	ǘB	ɠB	ʨB	̱B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�)B	�1B	�0B	�+B	�3B	�;B	�GB	�9B	�:B	�4B	�?B	�?B	�YB	�dB	�dB	�pB	�pB	�vB	�~B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
	B
	B

&B

"B

$B
(B
(G�O�B

B
�B
�B
%^B
/iB
5AB
;�B
D2B
J�B
O�B
T�B
YeB
]B
a�B
f1B
jbB
o�B
tPB
z$B
}B
�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.004(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940242019060409402420190604094024  AO  ARCAADJP                                                                    20181121041154    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041154  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041154  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094024  IP                  G�O�G�O�G�O�                