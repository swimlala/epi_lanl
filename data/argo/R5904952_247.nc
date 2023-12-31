CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:01Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190601  20181005190601  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)��~�1   @��+���@1+I�^�c��/��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�33A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BPffBX  B`  Bh  Bo��Bw��B��B�  B�  B�33B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C�C�C  C�fC  C  C  C  C  C�fC   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  Dy�D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
�fDfD� D��Dy�D  D�fD  D� D��Dy�D  D� D  D� D  D� DfD� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D�fDfD�fDfD�fDfD� D��Dy�D  D�fD   D � D!  D!� D"  D"� D#fD#� D$  D$� D$��D%� D&  D&� D'fD'�fD(  D(� D)fD)�fD*fD*� D+  D+� D,  D,� D-  D-�fD.fD.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6�fD7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<fD<� D<��D=�fD>  D>y�D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE�fDFfDF�fDG  DGy�DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDPfDP� DQ  DQ� DR  DR� DSfDS�fDTfDT� DU  DU� DV  DV� DW  DW� DW��DX� DYfDY� DY��DZ� D[fD[�fD\fD\� D]  D]� D^  D^� D_fD_�fD`fD`� D`��Da� DbfDb�fDc  Dc� DdfDd� De  De� Df  Df�fDg  Dg� Dh  Dh�fDifDi�fDjfDj� Dk  Dk�fDl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr�fDs  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dw�3Dy��D�L{D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�{Ap�A%p�AEp�Aep�A��RA��RA��A��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B(��B1\)B9\)BA\)BI\)BQBY\)Ba\)Bi\)Bp��Bx��B�z�B��B��B��GB��GB��B��B��B��B��GB��B��B��B��B��B��B��BĮBȮB̮BЮB��GBخBܮB�B�B�B�B�B��B�z�B��C W
CW
CW
CW
CW
C
W
Cp�Cp�CW
C=pCW
CW
CW
CW
CW
C=pC W
C"p�C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<=pC>W
C@W
CBW
CDW
CFW
CHW
CJW
CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~p�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�8RC�+�C�+�C�8RC��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�8RC�8RC�8RC�8RC�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C��C��C�+�C�+�C�+�C�8RC�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�D �D ��D�D�]D�D��D�D��D�D��D]D��D�D��D�D��D�D��D	�D	��D
�D
�)D)D��D]D�]D�D�)D�D��D]D�]D�D��D�D��D�D��D)D��D]D�]D�D��D�D��D�D��D�D��D�D��D�D�)D)D�)D)D�)D)D��D]D�]D�D�)D �D ��D!�D!��D"�D"��D#)D#��D$�D$��D%]D%��D&�D&��D')D'�)D(�D(��D))D)�)D*)D*��D+�D+��D,�D,��D-�D-�)D.)D.��D/�D/��D0�D0��D1]D1��D2�D2��D3�D3��D4�D4��D5�D5�]D6�D6�)D7�D7��D8�D8��D9�D9�)D:�D:��D;�D;��D<)D<��D=]D=�)D>�D>�]D?�D?��D@�D@��DA�DA��DB�DB�)DC�DC��DD�DD��DE�DE�)DF)DF�)DG�DG�]DH]DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�)DP)DP��DQ�DQ��DR�DR��DS)DS�)DT)DT��DU�DU��DV�DV��DW�DW��DX]DX��DY)DY��DZ]DZ��D[)D[�)D\)D\��D]�D]��D^�D^��D_)D_�)D`)D`��Da]Da��Db)Db�)Dc�Dc��Dd)Dd��De�De��Df�Df�)Dg�Dg��Dh�Dh�)Di)Di�)Dj)Dj��Dk�Dk�)Dl�Dl��Dm�Dm��Dn)Dn��Do�Do��Dp�Dp�)Dq�Dq��Dr�Dr�)Ds�Ds��Dt�Dt�)Du�Du��Dv�Dv��Dw�Dw��Dw��Dy�{D�W\D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�C�A�C�A�C�A�E�A�1'A�A�A� �A�bA�  A���AƼjAƥ�Aƕ�A�~�A��AŃA�z�A�^5A�9XA�33A�1'Aŉ7A�v�A�VA�7LA�+A�(�A�"�A�{A��/AĬAħ�A���A���A��A���A�A���A�ƨAċDAđhAăAģ�A��A�O�A�I�A�^5A�&�A��;A��AĶFAčPA�S�AÁA��A�"�A�ĜA���A��^A���A��hA�1'A���A�Q�A�"�A� �A�1A��-A�VA�=qA�ȴA���A�1A�1A��HA���A��\A�G�A��A��PA��TA�v�A��DA���A�O�A���A��A���A���A��9A�XA��7A��+A�"�A���A�ȴA��#A���A�;dA�
=A�$�A�/A�x�A�I�A�t�Ax~�Ak��Ai33Af5?AdjAaC�A_��A]��A[��AY�mAW�wAT�uAS��ARv�AP$�AJ�yAG�AEADA�AC\)ABM�AAdZA@bA??}A>~�A="�A;�A5�TA1��A/��A.  A*JA)t�A)+A'�A&��A&ȴA&A�A$ĜA"VA!XA�Ax�A�\AZA�mA��AZAVAVA9XAƨA(�AVAdZA�A"�A�;A��A;dA��AA�A �A-AJA1A�A��A�hAI�AG�A�AbA?}AZA?}A$�A+A��A
E�A	7LAbA(�A��A�TA�AVA�jAVAA�A
=A�uAȴA �/@��H@��^@�X@�?}@���@�hs@��@�@�-@�1@� �@�j@��@�`B@�j@�1@�!@�{@��@�$�@�&�@���@�J@�^5@��@��@�o@�@�  @�!@�j@ާ�@��#@�@܃@���@ݑh@�/@��`@�Z@�
=@թ�@ӥ�@�\)@�"�@�o@�l�@���@��/@�&�@Ցh@ԣ�@��m@ӕ�@ҧ�@�5?@���@��@�bN@ύP@�{@���@�|�@�G�@��@�7L@�O�@�hs@�1'@�t�@��@�`B@��@��
@�A�@�?}@�@ļj@�bN@ļj@�9X@���@�dZ@���@�ff@�@���@��^@��7@���@��j@��9@��@�A�@���@�@��!@�v�@�V@�n�@�E�@�=q@�-@���@���@���@��D@��m@��@��@�K�@�C�@��H@��@�&�@��@�1@��w@�S�@��H@���@��\@�n�@�$�@���@�`B@�7L@��@���@���@���@���@���@���@�~�@�{@��@�G�@���@��@��@��j@�Ĝ@��u@�9X@��;@�K�@�@��!@��+@�{@���@��@���@���@�bN@�I�@�9X@�  @�ƨ@���@�t�@�\)@�;d@��@�@��y@���@��+@�^5@�-@��@��^@���@���@��7@�%@�bN@���@�|�@�;d@�@��@���@�E�@���@�p�@�V@��@�1'@��@��
@�dZ@�33@��@��\@��@��@��-@�G�@���@��@��@�I�@�1'@�(�@�1@���@�;d@��\@�5?@��@��#@��T@���@�/@��D@�A�@��F@�C�@�"�@�
=@��@��\@�M�@��#@�x�@�?}@��j@�A�@��@��w@�t�@�;d@��H@�n�@��@��@��#@��@�X@�/@�O�@���@���@��@��u@�A�@�1@�ƨ@���@�l�@�S�@�;d@�33@���@��@��R@��!@��+@�E�@�5?@�{@��@���@���@�x�@��@��9@�bN@�1'@��m@���@�dZ@�;d@��y@�E�@�-@���@���@���@�p�@�7L@��@�V@��D@� �@��
@���@���@��@���@���@�|�@���@�=q@m^�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�C�A�C�A�C�A�E�A�1'A�A�A� �A�bA�  A���AƼjAƥ�Aƕ�A�~�A��AŃA�z�A�^5A�9XA�33A�1'Aŉ7A�v�A�VA�7LA�+A�(�A�"�A�{A��/AĬAħ�A���A���A��A���A�A���A�ƨAċDAđhAăAģ�A��A�O�A�I�A�^5A�&�A��;A��AĶFAčPA�S�AÁA��A�"�A�ĜA���A��^A���A��hA�1'A���A�Q�A�"�A� �A�1A��-A�VA�=qA�ȴA���A�1A�1A��HA���A��\A�G�A��A��PA��TA�v�A��DA���A�O�A���A��A���A���A��9A�XA��7A��+A�"�A���A�ȴA��#A���A�;dA�
=A�$�A�/A�x�A�I�A�t�Ax~�Ak��Ai33Af5?AdjAaC�A_��A]��A[��AY�mAW�wAT�uAS��ARv�AP$�AJ�yAG�AEADA�AC\)ABM�AAdZA@bA??}A>~�A="�A;�A5�TA1��A/��A.  A*JA)t�A)+A'�A&��A&ȴA&A�A$ĜA"VA!XA�Ax�A�\AZA�mA��AZAVAVA9XAƨA(�AVAdZA�A"�A�;A��A;dA��AA�A �A-AJA1A�A��A�hAI�AG�A�AbA?}AZA?}A$�A+A��A
E�A	7LAbA(�A��A�TA�AVA�jAVAA�A
=A�uAȴA �/@��H@��^@�X@�?}@���@�hs@��@�@�-@�1@� �@�j@��@�`B@�j@�1@�!@�{@��@�$�@�&�@���@�J@�^5@��@��@�o@�@�  @�!@�j@ާ�@��#@�@܃@���@ݑh@�/@��`@�Z@�
=@թ�@ӥ�@�\)@�"�@�o@�l�@���@��/@�&�@Ցh@ԣ�@��m@ӕ�@ҧ�@�5?@���@��@�bN@ύP@�{@���@�|�@�G�@��@�7L@�O�@�hs@�1'@�t�@��@�`B@��@��
@�A�@�?}@�@ļj@�bN@ļj@�9X@���@�dZ@���@�ff@�@���@��^@��7@���@��j@��9@��@�A�@���@�@��!@�v�@�V@�n�@�E�@�=q@�-@���@���@���@��D@��m@��@��@�K�@�C�@��H@��@�&�@��@�1@��w@�S�@��H@���@��\@�n�@�$�@���@�`B@�7L@��@���@���@���@���@���@���@�~�@�{@��@�G�@���@��@��@��j@�Ĝ@��u@�9X@��;@�K�@�@��!@��+@�{@���@��@���@���@�bN@�I�@�9X@�  @�ƨ@���@�t�@�\)@�;d@��@�@��y@���@��+@�^5@�-@��@��^@���@���@��7@�%@�bN@���@�|�@�;d@�@��@���@�E�@���@�p�@�V@��@�1'@��@��
@�dZ@�33@��@��\@��@��@��-@�G�@���@��@��@�I�@�1'@�(�@�1@���@�;d@��\@�5?@��@��#@��T@���@�/@��D@�A�@��F@�C�@�"�@�
=@��@��\@�M�@��#@�x�@�?}@��j@�A�@��@��w@�t�@�;d@��H@�n�@��@��@��#@��@�X@�/@�O�@���@���@��@��u@�A�@�1@�ƨ@���@�l�@�S�@�;d@�33@���@��@��R@��!@��+@�E�@�5?@�{@��@���@���@�x�@��@��9@�bN@�1'@��m@���@�dZ@�;d@��y@�E�@�-@���@���@���@�p�@�7L@��@�V@��D@� �@��
@���@���@��@���@���@�|�@���@�=q@m^�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYBYBYBXBW
B��B��B	B	B	B	B	B	+B	
=B	PB	 �B	:^B	E�B	A�B	=qB	?}B	A�B	XB	ZB	W
B	R�B	Q�B	Q�B	P�B	O�B	K�B	J�B	P�B	ZB	ffB	e`B	iyB	jB	jB	cTB	YB	]/B	aHB	m�B	�B	�dB	�5B	�B
"�B
;dB
o�B
{�B
�B
�=BB=qB7LBT�B�7B��B��B�RB�wB��B��BÖBĜBȴB��B��B��B��BǮB�}B�XB�3B�'B�'B�-B�LB�qBǮBƨBÖB��B��B�5BȴB��B�JBw�B]/BL�B!�B�B�B{B  B
��B
��B
w�B
K�B
/B
%B	�`B	��B	bNB	B�B�TB�#B��BȴB��B�dB�3B�B�qB�B	B��B��B�}B�jB�XB�^B�wB��B�B�B�B�B�`B��B�LB�B�B�'B�'B�!B�3B�FB�LB�XB�qB�jB�^B�FB�^B�qB�qB�qB��B��B��B��B��B��B�qB�qB��BBǮB	+B	8RB	6FB	&�B	+B	/B	8RB	>wB	C�B	J�B	K�B	L�B	N�B	O�B	S�B	O�B	M�B	L�B	L�B	M�B	H�B	>wB	1'B	(�B	$�B	33B	8RB	D�B	K�B	M�B	N�B	P�B	P�B	O�B	Q�B	W
B	K�B	@�B	9XB	5?B	5?B	7LB	6FB	-B	%�B	#�B	#�B	�B	,B	8RB	?}B	E�B	F�B	D�B	C�B	H�B	L�B	N�B	K�B	G�B	A�B	N�B	ZB	aHB	l�B	l�B	ffB	iyB	dZB	]/B	]/B	cTB	cTB	jB	p�B	r�B	r�B	p�B	iyB	[#B	S�B	S�B	VB	YB	_;B	e`B	m�B	t�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�B	� B	{�B	u�B	v�B	p�B	o�B	s�B	w�B	{�B	}�B	{�B	x�B	w�B	v�B	t�B	x�B	� B	�B	�B	� B	�+B	�%B	�B	�B	�B	�B	�%B	�1B	�=B	�VB	�VB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�^B	�qB	�}B	��B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	ȴB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�/B	�5B	�/B	�5B	�BB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
.�B
3�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BYBYBYBXBW
B��B��B	B	B	B	B	B	+B	
=B	PB	 �B	:^B	E�B	A�B	=qB	?}B	A�B	XB	ZB	W
B	R�B	Q�B	Q�B	P�B	O�B	K�B	J�B	P�B	ZB	ffB	e`B	iyB	jB	jB	cTB	YB	]/B	aHB	m�B	�B	�dB	�5B	�B
"�B
;dB
o�B
{�B
�B
�=BB=qB7LBT�B�7B��B��B�RB�wB��B��BÖBĜBȴB��B��B��B��BǮB�}B�XB�3B�'B�'B�-B�LB�qBǮBƨBÖB��B��B�5BȴB��B�JBw�B]/BL�B!�B�B�B{B  B
��B
��B
w�B
K�B
/B
%B	�`B	��B	bNB	B�B�TB�#B��BȴB��B�dB�3B�B�qB�B	B��B��B�}B�jB�XB�^B�wB��B�B�B�B�B�`B��B�LB�B�B�'B�'B�!B�3B�FB�LB�XB�qB�jB�^B�FB�^B�qB�qB�qB��B��B��B��B��B��B�qB�qB��BBǮB	+B	8RB	6FB	&�B	+B	/B	8RB	>wB	C�B	J�B	K�B	L�B	N�B	O�B	S�B	O�B	M�B	L�B	L�B	M�B	H�B	>wB	1'B	(�B	$�B	33B	8RB	D�B	K�B	M�B	N�B	P�B	P�B	O�B	Q�B	W
B	K�B	@�B	9XB	5?B	5?B	7LB	6FB	-B	%�B	#�B	#�B	�B	,B	8RB	?}B	E�B	F�B	D�B	C�B	H�B	L�B	N�B	K�B	G�B	A�B	N�B	ZB	aHB	l�B	l�B	ffB	iyB	dZB	]/B	]/B	cTB	cTB	jB	p�B	r�B	r�B	p�B	iyB	[#B	S�B	S�B	VB	YB	_;B	e`B	m�B	t�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�B	� B	{�B	u�B	v�B	p�B	o�B	s�B	w�B	{�B	}�B	{�B	x�B	w�B	v�B	t�B	x�B	� B	�B	�B	� B	�+B	�%B	�B	�B	�B	�B	�%B	�1B	�=B	�VB	�VB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�^B	�qB	�}B	��B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	ȴB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�/B	�5B	�/B	�5B	�BB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
.�B
3�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190601                              AO  ARCAADJP                                                                    20181005190601    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190601  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190601  QCF$                G�O�G�O�G�O�8000            