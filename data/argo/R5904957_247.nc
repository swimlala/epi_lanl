CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:51Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140851  20181024140851  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d���v1   @��e��QV@5�=p��
�dA�7K�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D	  D	y�D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)y�D)��D*y�D+  D+� D,  D,� D-  D-� D.  D.y�D.��D/y�D0  D0� D1  D1� D2  D2� D3fD3�fD4  D4� D5  D5�fD6  D6y�D6��D7� D8  D8� D9  D9� D9��D:y�D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DBy�DB��DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO�fDP  DP� DP��DQ� DRfDR�fDSfDS� DT  DTy�DU  DU� DU��DV� DW  DW� DX  DXy�DY  DY� DZ  DZ�fD[fD[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� DtfDt�fDu  Duy�Dv  Dv� Dw  Dw� Dw�3Dy�HD�9HD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A���B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B��3B�� B�� B�L�B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C&fC &fC"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4Y�C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ&fC\&fC^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ CrY�Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�3C�  C�,�C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�,�C�  C�,�C�  C�3C�  C�  C�  C�  C�  C�,�C�  C�  C�,�C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�,�C�  C�3C�3C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�  C�  C�  C�  D  D � DfD� D D� D D� D D� D D��D D� D D� D D� D	 D	��D
 D
� D D��D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D	�D� D D� DfD� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D&fD&� D' D'� D( D(� D) D)��D*	�D*��D+ D+� D, D,� D- D-� D. D.��D/	�D/��D0 D0� D1 D1� D2 D2� D3fD3�fD4 D4� D5 D5�fD6 D6��D7	�D7� D8 D8� D9 D9� D:	�D:��D; D;��D< D<� D= D=� D> D>� D? D?� D@	�D@� DA DA� DB DB��DC	�DC��DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DOfDO�fDP DP� DQ	�DQ� DRfDR�fDSfDS� DT DT��DU DU� DV	�DV� DW DW� DX DX��DY DY� DZ DZ�fD[fD[�fD\fD\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp��Dq Dq� Dr Dr� Ds Ds� DtfDt�fDu Du��Dv Dv� Dw Dw� Dw�3Dy�HD�AHD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A��mA��A��yA��mA��TA��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��A���A�1A��A��A��A� �A�"�A��A��A�{A�A��AǴ9Aǝ�Aǉ7A�v�A�VAƑhA�-Aş�A� �Aė�A�M�A�A��/A�ȴAá�AÇ+A�hsA�A�&�A�E�A��TA�A�A�r�A���A�+A�9XA�dZA�;dA��9A�JA���A�VA��
A�O�A�^5A���A���A�t�A�x�A��A�^5A�
=A��;A���A��-A�hsA�  A���A��A�  A�ĜA�$�A�XA�C�A�bA�jA�p�A�"�A���A�ffA�  A�n�A�A�v�A�jA���A��
A���A�v�A�hsA�S�A��A��`A��TA�VA�VA�5?A�1'A}t�Ay�mAv�Atn�As�-Ar(�An�/Ak��Aj��Aj��Aj��Ah�jAgO�Af  Ad��Aa��A_VA]hsA[?}AYS�AWG�AT��AS�AS�AQ�TAPZANVAKt�AH��AFĜAEC�AC`BAA��A@��A=�wA;p�A7\)A6��A6(�A5��A5�A5�hA3�wA1|�A0�uA/��A.$�A,n�A+�A*�RA)�A(^5A'hsA&  A$�uA$1A$  A#�wA"�A!�A!�^A!�A 1'A��A1'A�PA�jAG�AbNA��A��A��AAffAbA�A��A �A��A�AbNA"�AffA�mA�A33A�TA
JA	��A	33A�!A�AƨAdZA�A�^A��AM�A��AA�#AS�A Z@�o@��-@��9@�z�@�9X@�+@�v�@�{@�O�@���@�Q�@���@��H@��;@���@��@��@��@�5?@띲@�@��@�$�@��D@��@ߥ�@�~�@��@ݲ-@��@�C�@�@ڸR@��T@ٺ^@�`B@���@�Ĝ@�j@�ƨ@�o@�V@�@Չ7@��`@�ƨ@��@�`B@У�@�r�@��m@�t�@ύP@ύP@�33@�ff@ͩ�@˅@�"�@�o@�ȴ@�n�@�$�@���@ɑh@���@�9X@ǥ�@��y@Ƨ�@�5?@�z�@�J@��7@�p�@�G�@�V@���@�r�@��w@�S�@���@��u@���@���@�ff@�X@��D@���@�n�@�J@��#@��9@��@���@���@�&�@��j@�r�@��m@���@�;d@��+@�J@���@�/@��D@�z�@���@��u@� �@��
@�l�@�;d@�"�@�+@�;d@�;d@�C�@�K�@��@��@��R@���@�v�@�n�@�n�@�^5@�5?@��-@�x�@�&�@�r�@�  @��y@�ȴ@���@�hs@��u@�1@���@��@��R@�ff@�E�@���@��T@��-@�p�@�Ĝ@��D@�1'@��@�dZ@�;d@��R@�n�@��@��@�j@��@���@��H@��@���@��!@��^@��u@��@���@�X@���@��^@���@��@��@�  @�9X@�1'@��@��P@�"�@��y@���@��@�x�@��`@���@�1@�(�@�A�@�Z@�r�@�z�@�j@� �@���@�|�@�S�@��@�^5@�5?@�5?@�E�@�n�@�ff@�-@�`B@��u@���@��@��@��P@��y@�dZ@��@�+@��@�ȴ@��+@�v�@�5?@��T@��@�%@��@��
@���@�\)@�"�@�33@�;d@�+@�
=@���@���@�S�@��P@��@�ȴ@��@�`B@��`@���@�z�@�1'@�(�@��@�b@��@��@�;d@�\)@�K�@���@���@��R@�ff@�{@�J@�J@�J@�@��-@�&�@��@��@��`@��u@�j@�r�@��@��m@��F@���@��P@�t�@�\)@�C�@��I@r�]@`�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��mA��A��yA��mA��TA��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��A���A�1A��A��A��A� �A�"�A��A��A�{A�A��AǴ9Aǝ�Aǉ7A�v�A�VAƑhA�-Aş�A� �Aė�A�M�A�A��/A�ȴAá�AÇ+A�hsA�A�&�A�E�A��TA�A�A�r�A���A�+A�9XA�dZA�;dA��9A�JA���A�VA��
A�O�A�^5A���A���A�t�A�x�A��A�^5A�
=A��;A���A��-A�hsA�  A���A��A�  A�ĜA�$�A�XA�C�A�bA�jA�p�A�"�A���A�ffA�  A�n�A�A�v�A�jA���A��
A���A�v�A�hsA�S�A��A��`A��TA�VA�VA�5?A�1'A}t�Ay�mAv�Atn�As�-Ar(�An�/Ak��Aj��Aj��Aj��Ah�jAgO�Af  Ad��Aa��A_VA]hsA[?}AYS�AWG�AT��AS�AS�AQ�TAPZANVAKt�AH��AFĜAEC�AC`BAA��A@��A=�wA;p�A7\)A6��A6(�A5��A5�A5�hA3�wA1|�A0�uA/��A.$�A,n�A+�A*�RA)�A(^5A'hsA&  A$�uA$1A$  A#�wA"�A!�A!�^A!�A 1'A��A1'A�PA�jAG�AbNA��A��A��AAffAbA�A��A �A��A�AbNA"�AffA�mA�A33A�TA
JA	��A	33A�!A�AƨAdZA�A�^A��AM�A��AA�#AS�A Z@�o@��-@��9@�z�@�9X@�+@�v�@�{@�O�@���@�Q�@���@��H@��;@���@��@��@��@�5?@띲@�@��@�$�@��D@��@ߥ�@�~�@��@ݲ-@��@�C�@�@ڸR@��T@ٺ^@�`B@���@�Ĝ@�j@�ƨ@�o@�V@�@Չ7@��`@�ƨ@��@�`B@У�@�r�@��m@�t�@ύP@ύP@�33@�ff@ͩ�@˅@�"�@�o@�ȴ@�n�@�$�@���@ɑh@���@�9X@ǥ�@��y@Ƨ�@�5?@�z�@�J@��7@�p�@�G�@�V@���@�r�@��w@�S�@���@��u@���@���@�ff@�X@��D@���@�n�@�J@��#@��9@��@���@���@�&�@��j@�r�@��m@���@�;d@��+@�J@���@�/@��D@�z�@���@��u@� �@��
@�l�@�;d@�"�@�+@�;d@�;d@�C�@�K�@��@��@��R@���@�v�@�n�@�n�@�^5@�5?@��-@�x�@�&�@�r�@�  @��y@�ȴ@���@�hs@��u@�1@���@��@��R@�ff@�E�@���@��T@��-@�p�@�Ĝ@��D@�1'@��@�dZ@�;d@��R@�n�@��@��@�j@��@���@��H@��@���@��!@��^@��u@��@���@�X@���@��^@���@��@��@�  @�9X@�1'@��@��P@�"�@��y@���@��@�x�@��`@���@�1@�(�@�A�@�Z@�r�@�z�@�j@� �@���@�|�@�S�@��@�^5@�5?@�5?@�E�@�n�@�ff@�-@�`B@��u@���@��@��@��P@��y@�dZ@��@�+@��@�ȴ@��+@�v�@�5?@��T@��@�%@��@��
@���@�\)@�"�@�33@�;d@�+@�
=@���@���@�S�@��P@��@�ȴ@��@�`B@��`@���@�z�@�1'@�(�@��@�b@��@��@�;d@�\)@�K�@���@���@��R@�ff@�{@�J@�J@�J@�@��-@�&�@��@��@��`@��u@�j@�r�@��@��m@��F@���@��P@�t�@�\)@�C�@��I@r�]@`�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBBBB+B1B	7B1BPBDBhB�B#�B'�B)�B-B:^B?}B@�BD�BP�BgmBz�B�+B�hB��BŢB�BuB.B<jBO�B[#Be`BjBn�Bu�Bz�B� B�Bp�Bn�Bx�B�B�%B�7B�+BhsBD�BhsB�B�hB��B��B��B��B��B�hB�PB�{B�1B~�Bp�BffB=qB�B��B�`B��B1B�B��BǮB��B��B�7BcTBVB=qB"�B{BJB
��B
�B
�B
�NB
��B
�B
��B
�hB
�PB
�JB
�=B
�%B
�B
t�B
l�B
jB
ffB
VB
?}B
$�B
VB	��B	��B	�yB	��B	�jB	�LB	�?B	�-B	��B	��B	�oB	�7B	w�B	gmB	^5B	Q�B	G�B	=qB	2-B	.B	+B	$�B	�B	uB	DB	B��B��B�B�B�NB�
B��BŢBB��B�}B�wB�dB�9B�!B�B�B��B��B��B��B��B��B��B��B�uB�oB�hB�bB�VB�PB�JB�DB�1B�+B�B�B�B}�B}�B|�B|�B{�B{�By�Bx�Bw�Bu�Br�Bn�BjBgmBffBe`Be`BdZBcTBbNBbNBbNBaHBaHBaHBaHBaHBaHBbNBbNBbNBaHB`BBbNBe`BffBgmBgmBiyBjBjBk�Bl�Bm�Bp�Bq�Br�Br�Bs�Bw�Bv�B|�B� B� B|�Bx�Bv�Bu�Bv�Bw�Bw�Bv�Bx�Bz�Bz�B|�B}�B}�B~�B�B�B�%B�+B�+B�1B�=B�DB�PB�VB�VB�\B�hB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�?B�XB�qB�}B��BB��BŢB��B��B��B��B�B�
B�B�#B�)B�ZB�sB�yB�sB�fB�fB�fB�B��B��B��B		7B	PB	VB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	+B	0!B	5?B	5?B	:^B	?}B	D�B	F�B	H�B	J�B	J�B	J�B	K�B	L�B	Q�B	T�B	W
B	YB	[#B	\)B	]/B	^5B	^5B	bNB	cTB	cTB	bNB	`BB	^5B	^5B	^5B	\)B	ZB	ZB	[#B	\)B	^5B	_;B	`BB	bNB	cTB	dZB	e`B	jB	k�B	m�B	o�B	o�B	p�B	r�B	t�B	v�B	z�B	y�B	x�B	w�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�VB	�VB	�VB	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�9B	�9B	�9B	�RB	�LB	�LB	�XB	�XB	�^B	�dB	�qB	�}B	��B	�}B	�}B	��B	��B	ŢB	ǮB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�/B	�)B	�)B	�#B	�)B	�5B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�mB	�fB	�fB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B
B
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BBBBBBBBBBBBBBBBBB+B1B	7B1BPBDBhB�B#�B'�B)�B-B:^B?}B@�BD�BP�BgmBz�B�+B�hB��BŢB�BuB.B<jBO�B[#Be`BjBn�Bu�Bz�B� B�Bp�Bn�Bx�B�B�%B�7B�+BhsBD�BhsB�B�hB��B��B��B��B��B�hB�PB�{B�1B~�Bp�BffB=qB�B��B�`B��B1B�B��BǮB��B��B�7BcTBVB=qB"�B{BJB
��B
�B
�B
�NB
��B
�B
��B
�hB
�PB
�JB
�=B
�%B
�B
t�B
l�B
jB
ffB
VB
?}B
$�B
VB	��B	��B	�yB	��B	�jB	�LB	�?B	�-B	��B	��B	�oB	�7B	w�B	gmB	^5B	Q�B	G�B	=qB	2-B	.B	+B	$�B	�B	uB	DB	B��B��B�B�B�NB�
B��BŢBB��B�}B�wB�dB�9B�!B�B�B��B��B��B��B��B��B��B��B�uB�oB�hB�bB�VB�PB�JB�DB�1B�+B�B�B�B}�B}�B|�B|�B{�B{�By�Bx�Bw�Bu�Br�Bn�BjBgmBffBe`Be`BdZBcTBbNBbNBbNBaHBaHBaHBaHBaHBaHBbNBbNBbNBaHB`BBbNBe`BffBgmBgmBiyBjBjBk�Bl�Bm�Bp�Bq�Br�Br�Bs�Bw�Bv�B|�B� B� B|�Bx�Bv�Bu�Bv�Bw�Bw�Bv�Bx�Bz�Bz�B|�B}�B}�B~�B�B�B�%B�+B�+B�1B�=B�DB�PB�VB�VB�\B�hB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�?B�XB�qB�}B��BB��BŢB��B��B��B��B�B�
B�B�#B�)B�ZB�sB�yB�sB�fB�fB�fB�B��B��B��B		7B	PB	VB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	+B	0!B	5?B	5?B	:^B	?}B	D�B	F�B	H�B	J�B	J�B	J�B	K�B	L�B	Q�B	T�B	W
B	YB	[#B	\)B	]/B	^5B	^5B	bNB	cTB	cTB	bNB	`BB	^5B	^5B	^5B	\)B	ZB	ZB	[#B	\)B	^5B	_;B	`BB	bNB	cTB	dZB	e`B	jB	k�B	m�B	o�B	o�B	p�B	r�B	t�B	v�B	z�B	y�B	x�B	w�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�VB	�VB	�VB	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�9B	�9B	�9B	�RB	�LB	�LB	�XB	�XB	�^B	�dB	�qB	�}B	��B	�}B	�}B	��B	��B	ŢB	ǮB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�/B	�)B	�)B	�#B	�)B	�5B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�mB	�fB	�fB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B
B
%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140851                              AO  ARCAADJP                                                                    20181024140851    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140851  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140851  QCF$                G�O�G�O�G�O�0               