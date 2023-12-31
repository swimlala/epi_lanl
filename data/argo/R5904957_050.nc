CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:14Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140814  20181024140814  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               2A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׼$�G�L1   @׼%ffy@3N��+�c�����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      2A   A   A   @�ff@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD�fDfD� D  D� D  D� D	fD	� D	��D
y�D  D�fDfD� D��D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D�fD  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D �fD!fD!�fD"  D"y�D"��D#y�D#��D$y�D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+fD+�fD,fD,�fD-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DGy�DH  DH�fDIfDI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN�fDOfDO� DP  DP� DP��DQ� DR  DR� DS  DS�fDT  DTy�DU  DU� DVfDV� DV��DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D^��D_� D`  D`� Da  Da� Db  Db� Db��Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq�fDrfDr�fDs  Ds� Dt  Dt� DufDu�fDv  Dv� Dw  Dw� DwٚDy��D�=�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A  A$  ABffAbffA�  A�  A�  A�  A�  A�  A���A�  B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  BaffBiffBq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�L�B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ CY�C@ C@ C@ C@ C@ C@ C@ C&fC@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@Y�CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\&fC^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ CxY�Cz@ C|@ C~@ C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D D� D D� DfD�fDfD� D D� D D� D	fD	� D
	�D
��D D�fDfD� D	�D� D D��D D� D D�fD D� D D� D D� D D� D D�fD D��D	�D� D D� D D� D D� D D� D D� D D��D D� D D� D  D �fD!fD!�fD" D"��D#	�D#��D$	�D$��D%	�D%� D& D&� D' D'� D( D(� D) D)� D*fD*� D+fD+�fD,fD,�fD-fD-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7	�D7� D8 D8� D9 D9� D: D:� D;fD;� D< D<� D= D=� D> D>�fD? D?� D@ D@� DA DA� DB	�DB� DC DC� DD DD� DE DE� DF DF� DG	�DG��DH DH�fDIfDI� DJ DJ� DKfDK� DL DL� DM DM� DN DN�fDOfDO� DP DP� DQ	�DQ� DR DR� DS DS�fDT DT��DU DU� DVfDV� DW	�DW� DXfDX�fDY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^��D_	�D_� D` D`� Da Da� Db Db� Dc	�Dc��Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� DqfDq�fDrfDr�fDs Ds� Dt Dt� DufDu�fDv Dv� Dw Dw� Dw�Dy��D�E�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�5?A�33A�7LA�33A�7LA�7LA�-A�  A��A��mA���Aף�A�t�A�I�A�VA���A��/A԰!AӬA��TA�;dA���A���AЇ+A���AΉ7A�1A�(�A��A���A�jA�  A�ĜAȴ9A�A�ZA��A���A�?}A��A�p�A�A�A� �A��yAÉ7A�^5A�(�A��
A���A�1'A��A�A�A��;A�ZA���A��mA��9A��A��A���A��A��`A�A�hsA��HA��\A�VA�33A���A��A�Q�A��A���A��yA�A�M�A�(�A�5?A�A�O�A�(�A�JA���A�;dA��mA��A��FA�l�A���A�A�A���A��7A��A��hA�p�A���A�(�A�v�A��A�A�A�%A�"�A�O�A�K�A��-A�  A� �A���A�A|z�AyO�Au\)ArffAn�Al��Aj�AhM�Ag��Ae�Ab��A`{A^��A]t�A[�AZ�AWhsAU�;AT�AT9XAR�/ARJAQ+APJAN��ALbNAI33AH-AG
=AD^5AA��A@A?VA=\)A;��A9��A8��A8=qA6�A5?}A4��A3G�A/��A,�A*�RA(I�A&z�A&�A%C�A$n�A#�A!?}A ��A 1A%A �A�-A�hA\)A�jA�AbNA�\A5?A9XAJA�
A
=A33A��A��AG�A�A�A��AXAt�A	�A	`BA	7LA��A�A�A��Az�A$�A��A�`A  A �HA -@���@�n�@��@��@�@�hs@��j@�ȴ@�&�@��@�9X@��;@�;d@�@�7@�bN@�\@�+@��T@��@�F@�t�@�33@�ȴ@�ff@�`B@�A�@�l�@⟾@�G�@��
@�"�@�V@�hs@�V@��/@�Ĝ@܋D@��@��;@�ƨ@�=q@�K�@֟�@�-@�@Ձ@��@ӕ�@���@�%@�J@ͩ�@���@�(�@���@�&�@�1'@���@�33@�ȴ@�O�@��
@�33@���@���@��#@��9@���@�(�@��@�=q@��@�`B@���@��9@���@��u@�A�@��@�ff@�X@��j@���@�dZ@�;d@��@���@�7L@�I�@��F@�=q@��j@��9@���@�bN@�9X@��w@�"�@�^5@�x�@��`@�Z@�I�@�1'@��@�1@���@��;@�K�@��@��@��!@��\@�~�@�5?@��@�{@�J@�{@���@��#@���@���@��-@���@��-@���@���@���@��@�V@�j@��@���@��@�"�@�^5@�`B@���@�Ĝ@���@�Ĝ@�Ĝ@��j@��@�r�@�Q�@��@��F@�dZ@�C�@�o@�ff@�@��@��j@���@�1'@���@�t�@�K�@���@�E�@��#@���@���@���@��7@�hs@�?}@���@��D@�Q�@�1@�ƨ@�S�@���@��@���@�G�@���@�Ĝ@�bN@���@���@�\)@�
=@�v�@�V@�E�@�=q@���@���@���@��@���@���@��9@��@���@���@���@��u@��@�r�@�bN@�Z@�Z@�Z@��@�l�@�l�@�K�@�33@��R@�v�@�=q@�5?@�-@�{@�J@��T@��^@��@��9@���@���@���@���@���@���@���@��D@�I�@��@�S�@�
=@��@��H@�E�@��#@�hs@�7L@��@���@�bN@��@�  @�ƨ@��@�33@��@��!@��+@�M�@�{@���@�p�@�`B@�?}@�&�@�%@��@��/@��;@�S�@�C�@�;d@�"�@��R@���@��+@�5?@���@���@�x�@�X@�&�@�V@��@�j@�A�@�1'@�  @���@��@�|�@�dZ@�S�@�"�@�o@���@���@�0U@p��@_C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�5?A�33A�7LA�33A�7LA�7LA�-A�  A��A��mA���Aף�A�t�A�I�A�VA���A��/A԰!AӬA��TA�;dA���A���AЇ+A���AΉ7A�1A�(�A��A���A�jA�  A�ĜAȴ9A�A�ZA��A���A�?}A��A�p�A�A�A� �A��yAÉ7A�^5A�(�A��
A���A�1'A��A�A�A��;A�ZA���A��mA��9A��A��A���A��A��`A�A�hsA��HA��\A�VA�33A���A��A�Q�A��A���A��yA�A�M�A�(�A�5?A�A�O�A�(�A�JA���A�;dA��mA��A��FA�l�A���A�A�A���A��7A��A��hA�p�A���A�(�A�v�A��A�A�A�%A�"�A�O�A�K�A��-A�  A� �A���A�A|z�AyO�Au\)ArffAn�Al��Aj�AhM�Ag��Ae�Ab��A`{A^��A]t�A[�AZ�AWhsAU�;AT�AT9XAR�/ARJAQ+APJAN��ALbNAI33AH-AG
=AD^5AA��A@A?VA=\)A;��A9��A8��A8=qA6�A5?}A4��A3G�A/��A,�A*�RA(I�A&z�A&�A%C�A$n�A#�A!?}A ��A 1A%A �A�-A�hA\)A�jA�AbNA�\A5?A9XAJA�
A
=A33A��A��AG�A�A�A��AXAt�A	�A	`BA	7LA��A�A�A��Az�A$�A��A�`A  A �HA -@���@�n�@��@��@�@�hs@��j@�ȴ@�&�@��@�9X@��;@�;d@�@�7@�bN@�\@�+@��T@��@�F@�t�@�33@�ȴ@�ff@�`B@�A�@�l�@⟾@�G�@��
@�"�@�V@�hs@�V@��/@�Ĝ@܋D@��@��;@�ƨ@�=q@�K�@֟�@�-@�@Ձ@��@ӕ�@���@�%@�J@ͩ�@���@�(�@���@�&�@�1'@���@�33@�ȴ@�O�@��
@�33@���@���@��#@��9@���@�(�@��@�=q@��@�`B@���@��9@���@��u@�A�@��@�ff@�X@��j@���@�dZ@�;d@��@���@�7L@�I�@��F@�=q@��j@��9@���@�bN@�9X@��w@�"�@�^5@�x�@��`@�Z@�I�@�1'@��@�1@���@��;@�K�@��@��@��!@��\@�~�@�5?@��@�{@�J@�{@���@��#@���@���@��-@���@��-@���@���@���@��@�V@�j@��@���@��@�"�@�^5@�`B@���@�Ĝ@���@�Ĝ@�Ĝ@��j@��@�r�@�Q�@��@��F@�dZ@�C�@�o@�ff@�@��@��j@���@�1'@���@�t�@�K�@���@�E�@��#@���@���@���@��7@�hs@�?}@���@��D@�Q�@�1@�ƨ@�S�@���@��@���@�G�@���@�Ĝ@�bN@���@���@�\)@�
=@�v�@�V@�E�@�=q@���@���@���@��@���@���@��9@��@���@���@���@��u@��@�r�@�bN@�Z@�Z@�Z@��@�l�@�l�@�K�@�33@��R@�v�@�=q@�5?@�-@�{@�J@��T@��^@��@��9@���@���@���@���@���@���@���@��D@�I�@��@�S�@�
=@��@��H@�E�@��#@�hs@�7L@��@���@�bN@��@�  @�ƨ@��@�33@��@��!@��+@�M�@�{@���@�p�@�`B@�?}@�&�@�%@��@��/@��;@�S�@�C�@�;d@�"�@��R@���@��+@�5?@���@���@�x�@�X@�&�@�V@��@�j@�A�@�1'@�  @���@��@�|�@�dZ@�S�@�"�@�o@���@���@�0U@p��@_C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
ɺB
ƨB
ǮB
ɺB
��B
�5B
�B
��BB1BbBbBhB{B+B49BA�BVBk�Bw�B{�B|�B��B��B�B�HB�B��B{B#�B'�B)�B-B2-BH�B^5BcTBffBk�B~�B�B�DB��B��B��B��B��B��B��B�B�3B�^BĜB�B�BB�5B�#B�B�mB�B�B�B�sB�BB��B��B��B��B��B�uB�hB�=B�B�Bk�BXBF�B#�B��B�B��B��B��B��B�B�;B��B�XB��B�\BdZB	7B
�B
��B
�-B
|�B
W
B
'�B
\B	��B	�fB	��B	�qB	�B	��B	�\B	�+B	s�B	e`B	YB	Q�B	K�B	D�B	@�B	8RB	0!B	)�B	$�B	�B	�B	uB	JB	B��B�B�sB�TB�#B��B��BǮB��B�jB�LB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B�B��BĜBɺB��B��B��B��BɺBĜB�wB�3B�'B�-B�3B�^BȴB��B�wB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B�B��B�B�B�B�B�B�B�!B�!B�!B�!B�B�B�B�9B�FB�RB�XB�XB�^B�wB�wBB��B��B��B��B�
B�BB�ZB�ZB�fB�`B�B�B�B�B�B��B	  B	B	B	1B	DB	JB	\B	oB	oB	oB	oB	uB	�B	�B	"�B	$�B	(�B	)�B	+B	+B	0!B	33B	9XB	;dB	=qB	A�B	B�B	C�B	E�B	H�B	K�B	N�B	O�B	P�B	S�B	ZB	_;B	e`B	jB	n�B	q�B	u�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�+B	�+B	�+B	�1B	�1B	�1B	�1B	�7B	�7B	�DB	�PB	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�3B	�9B	�RB	�dB	�}B	��B	��B	ÖB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�/B	�;B	�HB	�NB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B

=B
	7B
JB
VB
VB
VB
VB
\B
\B
\B
bB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)yB
8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
ɺB
ƨB
ǮB
ɺB
��B
�5B
�B
��BB1BbBbBhB{B+B49BA�BVBk�Bw�B{�B|�B��B��B�B�HB�B��B{B#�B'�B)�B-B2-BH�B^5BcTBffBk�B~�B�B�DB��B��B��B��B��B��B��B�B�3B�^BĜB�B�BB�5B�#B�B�mB�B�B�B�sB�BB��B��B��B��B��B�uB�hB�=B�B�Bk�BXBF�B#�B��B�B��B��B��B��B�B�;B��B�XB��B�\BdZB	7B
�B
��B
�-B
|�B
W
B
'�B
\B	��B	�fB	��B	�qB	�B	��B	�\B	�+B	s�B	e`B	YB	Q�B	K�B	D�B	@�B	8RB	0!B	)�B	$�B	�B	�B	uB	JB	B��B�B�sB�TB�#B��B��BǮB��B�jB�LB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B�B��BĜBɺB��B��B��B��BɺBĜB�wB�3B�'B�-B�3B�^BȴB��B�wB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B�B��B�B�B�B�B�B�B�!B�!B�!B�!B�B�B�B�9B�FB�RB�XB�XB�^B�wB�wBB��B��B��B��B�
B�BB�ZB�ZB�fB�`B�B�B�B�B�B��B	  B	B	B	1B	DB	JB	\B	oB	oB	oB	oB	uB	�B	�B	"�B	$�B	(�B	)�B	+B	+B	0!B	33B	9XB	;dB	=qB	A�B	B�B	C�B	E�B	H�B	K�B	N�B	O�B	P�B	S�B	ZB	_;B	e`B	jB	n�B	q�B	u�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�+B	�+B	�+B	�1B	�1B	�1B	�1B	�7B	�7B	�DB	�PB	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�3B	�9B	�RB	�dB	�}B	��B	��B	ÖB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�/B	�;B	�HB	�NB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B

=B
	7B
JB
VB
VB
VB
VB
\B
\B
\B
bB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)yB
8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140814                              AO  ARCAADJP                                                                    20181024140814    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140814  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140814  QCF$                G�O�G�O�G�O�0               