CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-03-18T17:01:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Ad   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cd   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    KX   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  UL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230318170112  20230318170112  5906231 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  8375                            2B  A   APEX                            8809                            080319                          846 @��n��`1   @��Q���D;dZ�@D��^5?}1   GPS     Primary sampling: mixed [deep: discrete, shallow: averaged]                                                                                                                                                                                                        �A   A   A   @�  @�  A   A   AA��A`  A~ffA�  A�  A�33A�  A�  A�  A���B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<�fD=  D=� D>  D>�fD?  D?� D@  D@y�DA  DA� DA��DB� DC  DC�fDD  DD� DEfDE�fDFfDF� DGfDG�fDH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DM��DNy�DN��DO� DP  DPy�DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDtY�Dy��D�D�X�D��{D��qD� RD�b�D��=D�ʏD��D�PRD��D��HD�*�D�X Dڋ3D���D��D�S3D�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�=q@�=qA�A>�RA]�A{�A��\A��\A�A��\AΏ\Aޏ\A�\)A��\BG�BG�BG�B�B'�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B��
B���B���B���B���B���B���B���B���B���B���B���B���B�p�B�p�Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C�C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7�RC9��C;��C=��C?��CA�RCC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C��)C��)C��)C��)C���C���C��)C��)C���C���C���C���C���C���C���C���C���C���C���C��)C��)C���C���C���C���C��)C���C���C���C���C���C���C��)C��)C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#nD#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*nD*�{D+t{D+�{D,t{D,�{D-t{D-�{D.z�D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2��D3t{D3�{D4t{D4�{D5z�D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9��D:t{D:�{D;t{D;�{D<z�D<�{D=t{D=�{D>z�D>�{D?t{D?�{D@nD@�{DAt{DA�DBt{DB�{DCz�DC�{DDt{DD��DEz�DE��DFt{DF��DGz�DG�{DHt{DH�{DIt{DI�{DJt{DJ��DKt{DK�{DLt{DL�{DMt{DM�DNnDN�DOt{DO�{DPnDP�DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZz�DZ��D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{DbnDb�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�DhnDh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{DmnDm�{Dnt{Dn�{DonDo�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds��DtNDy�>D�\D�S3D���D�׮D��D�]D��zD���D��D�J�D��QD�ÅD�$�D�R=DڅpD�� D��D�MpD�{�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�  A���A���A���A���A�  A���A���A���A���A��A��A���A��A��A��A��mA��yA��mA��`A��HA��;A��#A��A���A���A���A���A��jA���A���A��A�bA��`A���A��A��A�|�A�p�A��uA��^A�\)A��/A��jA��A��uA���A�ZA�M�A�E�A�?}A�-A�A��
A��7A�-A�dZA���A�M�A�;dA��A��`A�A��
A��A�{A��\AVA}x�A}�A|�9A|�DAz�Ay�wAy`BAy&�Ax�RAw��Aw"�Av�Au��At��AtI�At�As�As��AshsAr�HArr�Aq�#ApA�An�An�uAnbNAn(�Am��Am+Ak�-Ai;dAh�yAhM�Ag��Ag�Ae�AeVAdĜAdVAc�Ac��AcC�Ab�`Ab$�Aa��Aat�Aap�AahsAa`BAaK�A`��A`bNA_�
A^��A]�#A]7LA\bNA[��A[+AZ^5AY��AYVAX�AXĜAX~�AX1'AW�wAWC�AW+AV��AV��AV�!AV�uAV�uAV~�AVE�AU��AU�FAU\)AT�ATȴAT�jATVAS�ASdZAR�\AQ|�AQXAQO�AQdZAQO�AQAPv�AP��AOƨANZAM�-AL�9AK��AJ�yAJ�DAJJAI;dAH  AF�!AFVAFM�AFM�AE��AEG�AC�PABĜABVAA�A@z�A?�
A?&�A>��A>�A>v�A>Q�A>^5A>jA>z�A>^5A>E�A>5?A>1A=ƨA=XA<�!A;�;A;A:^5A9�;A9;dA8VA6��A6�A5&�A4r�A4-A3��A3�mA4�A4jA4�9A4�RA4�+A4I�A41A3�FA3p�A3VA2�jA2jA25?A2{A1�A1+A0��A0ZA/�^A-��A+��A+�A*�yA*�A*��A*�RA*ZA*1A)��A(��A(A�A'�A'��A'�A'x�A'p�A'p�A'hsA'XA'G�A'oA&�A&�`A&��A&�RA&��A&�\A&�DA&�!A&�RA&�RA&�A&��A&�`A&��A&��A&jA%�A%t�A$��A$Q�A#�A"n�A!�-A!"�A 5?A�mA�AM�A�FA�AVA��A33AȴAz�A5?A�TAC�A`BA��A��AhsA?}A;dA�A�AVAoAA��A��AȴAjA^5A(�A��A��AE�A �A�;A+A�!AZA�
A
=A��A$�A�;A�FA�hA;dAĜA(�A��A�-A��Al�A`BA\)A+AA
ĜA
5?A	�
A	p�A�!A�PA&�AĜA��A�\AC�A�HA�\An�AE�A1'A�A��A�A�
A��AdZA �`A M�@��m@�;d@��!@�@�A�@�@�Ĝ@�ȴ@��@�A�@�w@�t�@�@�+@�-@�I�@�ȴ@��@�@�(�@���@�1@��@��@��@�p�@�G�@�l�@�J@ݑh@�7L@�7L@�/@�?}@�ƨ@�ff@��T@���@���@��@���@���@��@���@�r�@�S�@�M�@�J@�Q�@�@�x�@���@��;@�@�X@̓u@˕�@�t�@�S�@�+@��H@ʧ�@�v�@�$�@�&�@ȣ�@��;@Ǿw@ǥ�@�|�@Ə\@��`@�@�`B@�{@�=q@�5?@�{@�@��@�K�@�ȴ@���@�V@�33@�@�j@�l�@�ȴ@�V@��T@��T@�@���@���@���@���@��@���@���@�v�@�5?@�J@��@�%@���@�Q�@���@�ȴ@��@�O�@�z�@�(�@���@�+@��+@�@��h@�%@�Ĝ@��@��@���@�t�@�ȴ@��T@�-@�$�@���@�IR@� �@���@�y>@t*�@f;�@_O@X|�@Q�t@L  @E�t@A��@=��@:p;@6($@5T�@3!-@0��@.��@-�@,j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�  A���A���A���A���A�  A���A���A���A���A��A��A���A��A��A��A��mA��yA��mA��`A��HA��;A��#A��A���A���A���A���A��jA���A���A��A�bA��`A���A��A��A�|�A�p�A��uA��^A�\)A��/A��jA��A��uA���A�ZA�M�A�E�A�?}A�-A�A��
A��7A�-A�dZA���A�M�A�;dA��A��`A�A��
A��A�{A��\AVA}x�A}�A|�9A|�DAz�Ay�wAy`BAy&�Ax�RAw��Aw"�Av�Au��At��AtI�At�As�As��AshsAr�HArr�Aq�#ApA�An�An�uAnbNAn(�Am��Am+Ak�-Ai;dAh�yAhM�Ag��Ag�Ae�AeVAdĜAdVAc�Ac��AcC�Ab�`Ab$�Aa��Aat�Aap�AahsAa`BAaK�A`��A`bNA_�
A^��A]�#A]7LA\bNA[��A[+AZ^5AY��AYVAX�AXĜAX~�AX1'AW�wAWC�AW+AV��AV��AV�!AV�uAV�uAV~�AVE�AU��AU�FAU\)AT�ATȴAT�jATVAS�ASdZAR�\AQ|�AQXAQO�AQdZAQO�AQAPv�AP��AOƨANZAM�-AL�9AK��AJ�yAJ�DAJJAI;dAH  AF�!AFVAFM�AFM�AE��AEG�AC�PABĜABVAA�A@z�A?�
A?&�A>��A>�A>v�A>Q�A>^5A>jA>z�A>^5A>E�A>5?A>1A=ƨA=XA<�!A;�;A;A:^5A9�;A9;dA8VA6��A6�A5&�A4r�A4-A3��A3�mA4�A4jA4�9A4�RA4�+A4I�A41A3�FA3p�A3VA2�jA2jA25?A2{A1�A1+A0��A0ZA/�^A-��A+��A+�A*�yA*�A*��A*�RA*ZA*1A)��A(��A(A�A'�A'��A'�A'x�A'p�A'p�A'hsA'XA'G�A'oA&�A&�`A&��A&�RA&��A&�\A&�DA&�!A&�RA&�RA&�A&��A&�`A&��A&��A&jA%�A%t�A$��A$Q�A#�A"n�A!�-A!"�A 5?A�mA�AM�A�FA�AVA��A33AȴAz�A5?A�TAC�A`BA��A��AhsA?}A;dA�A�AVAoAA��A��AȴAjA^5A(�A��A��AE�A �A�;A+A�!AZA�
A
=A��A$�A�;A�FA�hA;dAĜA(�A��A�-A��Al�A`BA\)A+AA
ĜA
5?A	�
A	p�A�!A�PA&�AĜA��A�\AC�A�HA�\An�AE�A1'A�A��A�A�
A��AdZA �`A M�@��m@�;d@��!@�@�A�@�@�Ĝ@�ȴ@��@�A�@�w@�t�@�@�+@�-@�I�@�ȴ@��@�@�(�@���@�1@��@��@��@�p�@�G�@�l�@�J@ݑh@�7L@�7L@�/@�?}@�ƨ@�ff@��T@���@���@��@���@���@��@���@�r�@�S�@�M�@�J@�Q�@�@�x�@���@��;@�@�X@̓u@˕�@�t�@�S�@�+@��H@ʧ�@�v�@�$�@�&�@ȣ�@��;@Ǿw@ǥ�@�|�@Ə\@��`@�@�`B@�{@�=q@�5?@�{@�@��@�K�@�ȴ@���@�V@�33@�@�j@�l�@�ȴ@�V@��T@��T@�@���@���@���@���@��@���@���@�v�@�5?@�J@��@�%@���@�Q�@���@�ȴ@��@�O�@�z�@�(�@���@�+@��+@�@��h@�%@�Ĝ@��@��@���@�t�@�ȴ@��T@�-@�$�@���@�IR@� �@���@�y>@t*�@f;�@_O@X|�@Q�t@L  @E�t@A��@=��@:p;@6($@5T�@3!-@0��@.��@-�@,j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BC�BC�BC�BC�BD�BC�BD�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BD�BC�BD�BD�BD�BD�BD�BD�BD�BD�BD�BC�BA�B:^B7LB>wBH�BN�BM�BL�BK�BH�BD�BC�BC�BB�BB�BA�BB�B>wB>wB>wB?}B?}B?}B>wB=qB7LB/B,B$�B#�B!�B�B�BuB\BDBB��B�ZB�BB�HB�yB�fB�B�B�;B�;B�)B�)B�B��B��B��B��B��BǮBŢBÖB�wB�dB�-B�B��B��B��B��B��B��B�%B�B~�By�Bt�Bn�BgmBffBbNB`BB^5B[#BYBS�BP�BN�BN�BN�BN�BM�BJ�BG�BC�B<jB6FB33B,B(�B"�B�B�BhBbB\BJB	7B%BBBBBB+BDBPBJBDB
=B	7B+BBBB��B��B�B�fB�NB�HB�NB�NB�#B��B�B��B�^B�'B��B��B�\B�=B�Bz�Bn�BbNB`BBbNBe`BdZB]/BK�B>wB8RB/B#�B�B�BVBVBPB\BoBuB{B{BuB{B�B�B�BVB+B��B��B�B�fB�B��B�qB�3B��B��B��B��B��B�!B�LB��BB��B�wB�dB�XB�?B�-B�!B�B�B��B��B��B��B�bB� Bk�BcTB^5B^5B^5B\)BYBVBQ�BL�BH�BF�BD�BE�BD�BC�BD�BD�BD�BC�BB�BA�BC�BC�BA�BA�BA�BA�BA�BB�BB�BA�BA�BA�BA�B@�B>wB<jB9XB5?B1'B$�B�B�BuBPBDB%BB
��B
��B
��B
�B
�B
�sB
�fB
�ZB
�NB
�/B
�B
ȴB
ǮB
ƨB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ŢB
ŢB
ƨB
ÖB
ŢB
ȴB
ƨB
ÖB
��B
�}B
�wB
�jB
�XB
�LB
�?B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�hB
�VB
�DB
�+B
~�B
{�B
u�B
r�B
hsB
_;B
\)B
[#B
\)B
]/B
\)B
\)B
ZB
YB
YB
YB
VB
T�B
R�B
Q�B
P�B
M�B
L�B
H�B
E�B
?}B
9XB
6FB
0!B
.B
,B
)�B
(�B
#�B
!�B
�B
�B
�B
VB
B	��B	��B	�B	�B	�fB	�mB	�TB	�)B	�#B	�B	�B	�B	�B	��B	��B	��B	��B	ǮB	ǮB	ǮB	ǮB	ƨB	ǮB	ƨB	ĜB	ĜB	ƨB	ƨB	�wB	�qB	�qB	�^B	�RB	�3B	�'B	�'B	�-B	�-B	�-B	�-B	�!B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	�{B	�{B	�{B	�{B	�oB	�oB	�hB	�hB	�hB	�bB	�hB	�\B	�\B	�VB	�VB	�VB	�VB	�PB	�PB	�PB	�PB	�JB	�PB	�JB	�JB	�JB	�JB	�DB	�DB	�DB	�DB	�DB	�DB	�DB	�DB	�JB	�JB	�XB	��B	��B	�|B	�JB	�;B	�^B
#TB
:^B
IRB
e�B
w�B
��B
��B
��B
��B
ܬB
�iB
��BgB
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BC�BC�BC�BC�BD�BC�BD�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BD�BC�BD�BD�BD�BD�BD�BD�BD�BD�BD�BC�BA�B:^B7LB>wBH�BN�BM�BL�BK�BH�BD�BC�BC�BB�BB�BA�BB�B>wB>wB>wB?}B?}B?}B>wB=qB7LB/B,B$�B#�B!�B�B�BuB\BDBB��B�ZB�BB�HB�yB�fB�B�B�;B�;B�)B�)B�B��B��B��B��B��BǮBŢBÖB�wB�dB�-B�B��B��B��B��B��B��B�%B�B~�By�Bt�Bn�BgmBffBbNB`BB^5B[#BYBS�BP�BN�BN�BN�BN�BM�BJ�BG�BC�B<jB6FB33B,B(�B"�B�B�BhBbB\BJB	7B%BBBBBB+BDBPBJBDB
=B	7B+BBBB��B��B�B�fB�NB�HB�NB�NB�#B��B�B��B�^B�'B��B��B�\B�=B�Bz�Bn�BbNB`BBbNBe`BdZB]/BK�B>wB8RB/B#�B�B�BVBVBPB\BoBuB{B{BuB{B�B�B�BVB+B��B��B�B�fB�B��B�qB�3B��B��B��B��B��B�!B�LB��BB��B�wB�dB�XB�?B�-B�!B�B�B��B��B��B��B�bB� Bk�BcTB^5B^5B^5B\)BYBVBQ�BL�BH�BF�BD�BE�BD�BC�BD�BD�BD�BC�BB�BA�BC�BC�BA�BA�BA�BA�BA�BB�BB�BA�BA�BA�BA�B@�B>wB<jB9XB5?B1'B$�B�B�BuBPBDB%BB
��B
��B
��B
�B
�B
�sB
�fB
�ZB
�NB
�/B
�B
ȴB
ǮB
ƨB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ŢB
ŢB
ƨB
ÖB
ŢB
ȴB
ƨB
ÖB
��B
�}B
�wB
�jB
�XB
�LB
�?B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�hB
�VB
�DB
�+B
~�B
{�B
u�B
r�B
hsB
_;B
\)B
[#B
\)B
]/B
\)B
\)B
ZB
YB
YB
YB
VB
T�B
R�B
Q�B
P�B
M�B
L�B
H�B
E�B
?}B
9XB
6FB
0!B
.B
,B
)�B
(�B
#�B
!�B
�B
�B
�B
VB
B	��B	��B	�B	�B	�fB	�mB	�TB	�)B	�#B	�B	�B	�B	�B	��B	��B	��B	��B	ǮB	ǮB	ǮB	ǮB	ƨB	ǮB	ƨB	ĜB	ĜB	ƨB	ƨB	�wB	�qB	�qB	�^B	�RB	�3B	�'B	�'B	�-B	�-B	�-B	�-B	�!B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	�{B	�{B	�{B	�{B	�oB	�oB	�hB	�hB	�hB	�bB	�hB	�\B	�\B	�VB	�VB	�VB	�VB	�PB	�PB	�PB	�PB	�JB	�PB	�JB	�JB	�JB	�JB	�DB	�DB	�DB	�DB	�DB	�DB	�DB	�DB	�JB	�JB	�XB	��B	��B	�|B	�JB	�;B	�^B
#TB
:^B
IRB
e�B
w�B
��B
��B
��B
��B
ܬB
�iB
��BgB
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230318170112                              AO  ARCAADJP                                                                    20230318170112    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230318170112  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230318170112  QCF$                G�O�G�O�G�O�0               