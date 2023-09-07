CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:18Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191718  20181005191718  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����a�1   @���)�,@4��1&��dol�C��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ��B'33B0  B8  B@ffBH��BO��BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
�C�C  C�fC�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCS�fCV�CX�CZ  C\  C^�C`�Cb  Cd  Cf�Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv�Cx33Cz�C|  C~  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C��C��C��C�  C��C��3C��3C��C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C��C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C��3C�  C�  C�  C��3C��3C��3C�  C��C��C�  C�  C��C�  C��C��C��3C�  C�  C�  C��C�  C��C�  C��3C�  C��3C��3C��C�  C��3C��3C�  C��3C�  C�  C��3C��C��C�  C�  C�  C��C�  C��C�  C��3C�  C�  C�  C��C��C��C�  C�  C��C�  C��C��C�  C�  C�  C��3C�  C��C�  C��C�  D   D �fDfD� DfD�fD��D� DfD�fD  Dy�D  D� D  D� DfD� D	  D	y�D	��D
y�D  Dy�D  D�fD  Dy�D  D�fDfD� D��D� DfD� D  Dy�D  D� D��D� D  D� D�D�fDfD� D��D� DfD�fDfD� D��D� DfD�fDfD�fDfD� D  D�fD fD �fD!fD!�fD"fD"� D#  D#�fD$fD$� D$��D%y�D%��D&� D'  D'� D(fD(� D)  D)�fD*  D*y�D*��D+y�D+��D,� D,��D-� D-��D.� D/fD/�fD0  D0� D1  D1� D2  D2�fD3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9�fD:fD:�fD;fD;� D;��D<� D=  D=� D=��D>� D?fD?�fD@fD@� DA  DA� DB  DB� DCfDC�fDDfDD�fDE  DEy�DE��DFy�DF��DG� DG��DHy�DH��DI�fDJ  DJ� DKfDK��DL  DL� DMfDM� DN  DN� DOfDO�fDPfDP� DQ  DQ�fDR  DRs3DR��DS� DT  DTy�DU  DU� DVfDV� DW  DWy�DW��DX� DY  DY�fDZ�DZ��D[�D[� D[��D\� D]fD]� D^  D^� D^��D_� D`  D`� DafDa�fDbfDb�fDcfDc� Dd  Dd�fDe  Dey�Df  Df� Dg  Dg�fDh  Dh� DifDi�fDjfDj� Dk  Dk� Dk��Dl� Dl��Dm� Dn  Dn� Do  Do� Do��Dpy�Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDwٚDy�3D�*�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��HA�
A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B"(�B(�\B1\)B9\)BABJ(�BP��BY\)Ba\)Bh��Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��GB��B��BĮBȮB��GBЮBԮBخBܮB�B�B�B�B�B��B��B��GC W
CW
CW
CW
CW
C
p�Cp�CW
C=pC=pCW
CW
CW
CW
CW
CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8=pC:W
C<W
C>W
C@W
CBW
CDW
CFW
CHW
CJW
CLW
CNW
CPW
CR=pCT=pCVp�CXp�CZW
C\W
C^p�C`p�CbW
CdW
Cfp�ChW
CjW
Clp�CnW
CpW
CrW
CtW
Cvp�Cx�=Czp�C|W
C~W
C�+�C�+�C�+�C�8RC�8RC�+�C��C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C��C�+�C�8RC�8RC�8RC�EC�8RC�+�C�8RC��C��C�8RC��C�+�C�8RC�+�C�+�C�+�C��C�+�C�+�C�+�C�8RC�+�C�8RC�+�C�+�C�+�C��C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�8RC�+�C�+�C�8RC��C�+�C�+�C�+�C��C��C��C�+�C�8RC�8RC�+�C�+�C�8RC�+�C�8RC�8RC��C�+�C�+�C�+�C�8RC�+�C�EC�+�C��C�+�C��C��C�8RC�+�C��C��C�+�C��C�+�C�+�C��C�8RC�8RC�+�C�+�C�+�C�8RC�+�C�8RC�+�C��C�+�C�+�C�+�C�8RC�8RC�8RC�+�C�+�C�8RC�+�C�8RC�8RC�+�C�+�C�+�C��C�+�C�8RC�+�C�8RC�+�D �D �)D)D��D)D�)D]D��D)D�)D�D�]D�D��D�D��D)D��D	�D	�]D
]D
�]D�D�]D�D�)D�D�]D�D�)D)D��D]D��D)D��D�D�]D�D��D]D��D�D��D"�D�)D)D��D]D��D)D�)D)D��D]D��D)D�)D)D�)D)D��D�D�)D )D �)D!)D!�)D")D"��D#�D#�)D$)D$��D%]D%�]D&]D&��D'�D'��D()D(��D)�D)�)D*�D*�]D+]D+�]D,]D,��D-]D-��D.]D.��D/)D/�)D0�D0��D1�D1��D2�D2�)D3)D3��D4�D4��D5�D5��D6�D6��D7�D7��D8)D8��D9�D9�)D:)D:�)D;)D;��D<]D<��D=�D=��D>]D>��D?)D?�)D@)D@��DA�DA��DB�DB��DC)DC�)DD)DD�)DE�DE�]DF]DF�]DG]DG��DH]DH�]DI]DI�)DJ�DJ��DK)DK��DL�DL��DM)DM��DN�DN��DO)DO�)DP)DP��DQ�DQ�)DR�DR��DS]DS��DT�DT�]DU�DU��DV)DV��DW�DW�]DX]DX��DY�DY�)DZ"�DZ��D["�D[��D\]D\��D])D]��D^�D^��D_]D_��D`�D`��Da)Da�)Db)Db�)Dc)Dc��Dd�Dd�)De�De�]Df�Df��Dg�Dg�)Dh�Dh��Di)Di�)Dj)Dj��Dk�Dk��Dl]Dl��Dm]Dm��Dn�Dn��Do�Do��Dp]Dp�]Dq�Dq��Dr)Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw�)Dw�]Dy��D�5p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A��A�$�A�+A�+A�-A�/A�/A�-A�-A� �A���A���A۶FAۛ�Aە�Aۗ�AۍPAہA���A֑hA� �A�ZA�ĜA��
A�/A�oA�n�A�~�A���A�=qA��A�;dA��HA�|�A�r�A���A�1'A���A�;dA�|�A�-A��A���A�=qA��A�A�ƨA�33A�%A��DA�
=A���A��jA��A��FA�ĜA��;A�(�A�?}A���A��A���A���A�{A�=qA���A��^A�1A�~�A�%A���A��!A�JA�p�A�bNA��hA���A�ZA��jA��PA���A���A�E�A�
=A��A�M�A��jA���A��\A�=qA�
=A�G�Azv�Aw��Au33ArVAoVAjM�Ah�AedZAb�A_`BA]%A[
=AYK�AY
=AW��AV5?AT��ATv�AS�7AQ�AP��AO�hAMp�AJ��AIt�AG�AGAF�DAE��AD��AC�
ABȴAAoA?�wA>�A=�;A<(�A:�A7K�A5��A4$�A0��A.��A-��A-hsA,�yA+�A*�A)��A'��A&A%hsA%&�A$�A$=qA#/A"�!A"9XA!�A!��A n�A|�A5?AXAE�A�hAVA��AbNAO�A�!AI�AK�A �A�;A��A7LA�Az�AXAƨAK�A�hA%An�A�hAĜA��A��AM�A�A
1AZA��A�AjA��A�A{A �`A -@���@��H@�/@�;d@��^@�7L@�l�@�-@�z�@���@�@�O�@��`@��@띲@�33@��H@��@�Ĝ@�C�@�v�@�^@�@��/@݁@��@��/@��
@�  @�33@�J@�\)@Ϯ@�Q�@�o@�O�@�A�@��@�A�@��@�@ʏ\@ʗ�@�n�@�{@�hs@ȓu@�33@�ȴ@�J@š�@ř�@Ł@�`B@�?}@���@��;@§�@��/@�S�@�E�@�X@�X@�?}@��h@���@�@�x�@��`@��j@��@��@�|�@��P@���@��^@��9@�\)@�{@�r�@�ȴ@���@��w@��H@�x�@�V@�%@���@���@���@�O�@���@��u@�Z@�(�@�dZ@�V@��@���@� �@��;@��@�t�@�\)@�33@���@��@��y@��H@���@��!@�v�@�n�@�-@�@��@��@��@���@�J@��T@�@�ff@���@��+@�{@�X@�/@�7L@�V@�Ĝ@���@�9X@�1@�t�@���@�n�@�@�x�@��@��/@��D@�  @��F@��@�
=@���@��@���@�5?@��@��@���@�@��^@���@��@�p�@��@��9@�Z@�  @�C�@�$�@���@�X@�G�@�/@�V@���@��@�9X@��@��w@��P@��@��\@�^5@�ff@���@���@��R@���@��@�33@���@�hs@�V@���@���@�9X@��@��@�ƨ@���@�Z@���@��D@��`@���@���@�E�@��H@��y@��@���@���@��R@��!@���@���@��\@�ff@�E�@�E�@�5?@��@���@��T@�@���@�/@��9@���@�(�@�dZ@�o@�"�@�+@�K�@��@��!@�=q@�$�@�@�p�@�&�@��`@�%@�?}@��@���@���@��j@���@�r�@��@��F@���@�l�@�C�@�"�@��H@���@��@��@��H@���@��!@�M�@��#@��7@��`@�K�@��@�5?@��@��@�@�@���@���@��7@��@�O�@��@���@��@��@�1@�@�  @
=@~ȴ@~��@+@l�@��@+@~v�@~@}O�@|�/@|Z@|z�@|��@}O�@~ȴ@+@
=@;d@��@jxl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� �A��A�$�A�+A�+A�-A�/A�/A�-A�-A� �A���A���A۶FAۛ�Aە�Aۗ�AۍPAہA���A֑hA� �A�ZA�ĜA��
A�/A�oA�n�A�~�A���A�=qA��A�;dA��HA�|�A�r�A���A�1'A���A�;dA�|�A�-A��A���A�=qA��A�A�ƨA�33A�%A��DA�
=A���A��jA��A��FA�ĜA��;A�(�A�?}A���A��A���A���A�{A�=qA���A��^A�1A�~�A�%A���A��!A�JA�p�A�bNA��hA���A�ZA��jA��PA���A���A�E�A�
=A��A�M�A��jA���A��\A�=qA�
=A�G�Azv�Aw��Au33ArVAoVAjM�Ah�AedZAb�A_`BA]%A[
=AYK�AY
=AW��AV5?AT��ATv�AS�7AQ�AP��AO�hAMp�AJ��AIt�AG�AGAF�DAE��AD��AC�
ABȴAAoA?�wA>�A=�;A<(�A:�A7K�A5��A4$�A0��A.��A-��A-hsA,�yA+�A*�A)��A'��A&A%hsA%&�A$�A$=qA#/A"�!A"9XA!�A!��A n�A|�A5?AXAE�A�hAVA��AbNAO�A�!AI�AK�A �A�;A��A7LA�Az�AXAƨAK�A�hA%An�A�hAĜA��A��AM�A�A
1AZA��A�AjA��A�A{A �`A -@���@��H@�/@�;d@��^@�7L@�l�@�-@�z�@���@�@�O�@��`@��@띲@�33@��H@��@�Ĝ@�C�@�v�@�^@�@��/@݁@��@��/@��
@�  @�33@�J@�\)@Ϯ@�Q�@�o@�O�@�A�@��@�A�@��@�@ʏ\@ʗ�@�n�@�{@�hs@ȓu@�33@�ȴ@�J@š�@ř�@Ł@�`B@�?}@���@��;@§�@��/@�S�@�E�@�X@�X@�?}@��h@���@�@�x�@��`@��j@��@��@�|�@��P@���@��^@��9@�\)@�{@�r�@�ȴ@���@��w@��H@�x�@�V@�%@���@���@���@�O�@���@��u@�Z@�(�@�dZ@�V@��@���@� �@��;@��@�t�@�\)@�33@���@��@��y@��H@���@��!@�v�@�n�@�-@�@��@��@��@���@�J@��T@�@�ff@���@��+@�{@�X@�/@�7L@�V@�Ĝ@���@�9X@�1@�t�@���@�n�@�@�x�@��@��/@��D@�  @��F@��@�
=@���@��@���@�5?@��@��@���@�@��^@���@��@�p�@��@��9@�Z@�  @�C�@�$�@���@�X@�G�@�/@�V@���@��@�9X@��@��w@��P@��@��\@�^5@�ff@���@���@��R@���@��@�33@���@�hs@�V@���@���@�9X@��@��@�ƨ@���@�Z@���@��D@��`@���@���@�E�@��H@��y@��@���@���@��R@��!@���@���@��\@�ff@�E�@�E�@�5?@��@���@��T@�@���@�/@��9@���@�(�@�dZ@�o@�"�@�+@�K�@��@��!@�=q@�$�@�@�p�@�&�@��`@�%@�?}@��@���@���@��j@���@�r�@��@��F@���@�l�@�C�@�"�@��H@���@��@��@��H@���@��!@�M�@��#@��7@��`@�K�@��@�5?@��@��@�@�@���@���@��7@��@�O�@��@���@��@��@�1@�@�  @
=@~ȴ@~��@+@l�@��@+@~v�@~@}O�@|�/@|Z@|z�@|��@}O�@~ȴ@+@
=@;d@��@jxl11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B+B1'B9XB;dBL�B_;Be`BjBs�B�B�DB�bB��B�B��B��B��B��B�hB�bB�hB��B��B�oB�uB�oB�bB�bB��B��B��B�\B�Bl�BXBF�B:^B.B�BhBPB1B��B�B�yB�/B��B�wB�9B��B�JB{�BjBbNBXBI�B>wB0!B)�B�BB
��B
��B
�B
�B
�`B
��B
��B
�FB
�B
��B
�%B
H�B
�B
1B	��B	�BB	��B	�!B	��B	�hB	�B	o�B	cTB	XB	M�B	J�B	D�B	=qB	6FB	49B	/B	(�B	 �B	�B	hB	%B��B��B�B�B�B�B�`B�BB�B��B��B��BƨB��B�RB�3B�B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�=B�+B�%B�B�B�B�B�B�B�1B�B}�B{�Bz�Bx�Bw�Bz�Bx�Bq�Bu�B|�B~�B}�By�Bw�Bt�Bw�B|�B�%B�JB�VB�\B�uB�uB�uB�{B��B��B�{B�oB�hB�bB�bB�\B�hB�\B�JB�7B�B�B�B�B�B�B�B�B�B�B�B� B~�B~�B|�Bv�Bs�Bq�Bo�BiyBn�Bl�BdZBXBP�BM�BM�BK�BL�BQ�BYBcTBhsBjBk�Bl�Bl�Bm�Bn�Bv�Bw�Bw�Bx�Bx�Bx�Bw�Bz�B{�By�Bw�By�Bz�By�Bz�B|�B� B�B�bB�hB�oB�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�XBBĜBƨB��B��B�B�NB�yB�B�B�B�B��B��B��B	+B	bB	uB	�B	�B	!�B	&�B	'�B	)�B	-B	/B	33B	6FB	9XB	=qB	A�B	F�B	G�B	K�B	M�B	O�B	S�B	VB	W
B	ZB	[#B	\)B	]/B	^5B	`BB	aHB	cTB	e`B	jB	jB	k�B	o�B	p�B	o�B	o�B	p�B	q�B	q�B	r�B	s�B	s�B	s�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	w�B	{�B	~�B	�B	�=B	�DB	�VB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�3B	�?B	�jB	�wB	�wB	�}B	��B	B	B	B	B	ÖB	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�;B	�NB	�ZB	�fB	�mB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
  B
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B+B1'B9XB;dBL�B_;Be`BjBs�B�B�DB�bB��B�B��B��B��B��B�hB�bB�hB��B��B�oB�uB�oB�bB�bB��B��B��B�\B�Bl�BXBF�B:^B.B�BhBPB1B��B�B�yB�/B��B�wB�9B��B�JB{�BjBbNBXBI�B>wB0!B)�B�BB
��B
��B
�B
�B
�`B
��B
��B
�FB
�B
��B
�%B
H�B
�B
1B	��B	�BB	��B	�!B	��B	�hB	�B	o�B	cTB	XB	M�B	J�B	D�B	=qB	6FB	49B	/B	(�B	 �B	�B	hB	%B��B��B�B�B�B�B�`B�BB�B��B��B��BƨB��B�RB�3B�B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�=B�+B�%B�B�B�B�B�B�B�1B�B}�B{�Bz�Bx�Bw�Bz�Bx�Bq�Bu�B|�B~�B}�By�Bw�Bt�Bw�B|�B�%B�JB�VB�\B�uB�uB�uB�{B��B��B�{B�oB�hB�bB�bB�\B�hB�\B�JB�7B�B�B�B�B�B�B�B�B�B�B�B� B~�B~�B|�Bv�Bs�Bq�Bo�BiyBn�Bl�BdZBXBP�BM�BM�BK�BL�BQ�BYBcTBhsBjBk�Bl�Bl�Bm�Bn�Bv�Bw�Bw�Bx�Bx�Bx�Bw�Bz�B{�By�Bw�By�Bz�By�Bz�B|�B� B�B�bB�hB�oB�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�XBBĜBƨB��B��B�B�NB�yB�B�B�B�B��B��B��B	+B	bB	uB	�B	�B	!�B	&�B	'�B	)�B	-B	/B	33B	6FB	9XB	=qB	A�B	F�B	G�B	K�B	M�B	O�B	S�B	VB	W
B	ZB	[#B	\)B	]/B	^5B	`BB	aHB	cTB	e`B	jB	jB	k�B	o�B	p�B	o�B	o�B	p�B	q�B	q�B	r�B	s�B	s�B	s�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	w�B	{�B	~�B	�B	�=B	�DB	�VB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�3B	�?B	�jB	�wB	�wB	�}B	��B	B	B	B	B	ÖB	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�;B	�NB	�ZB	�fB	�mB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
  B
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191718                              AO  ARCAADJP                                                                    20181005191718    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191718  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191718  QCF$                G�O�G�O�G�O�8000            