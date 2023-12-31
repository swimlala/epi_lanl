CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:16Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190516  20181005190516  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               0A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׻eO՘G1   @׻e�b�\@1�M����c�S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      0A   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0ffB8ffB@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��3C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D��Dy�D  D� D  D� D��D� D��Dy�D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D�fD  D� D  D� D��D� D  D�fDfD� D  D� D  D� D  Dy�D��Dy�D  D� DfD� D  D� D  Dy�D��Dy�D  D�fDfD�fD  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D%��D&y�D'  D'� D(  D(� D)  D)� D*  D*�fD+fD+� D,  D,�fD-  D-� D.  D.� D/fD/�fD0  D0y�D0��D1y�D1��D2y�D2��D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:�fD;fD;y�D;��D<�fD=  D=� D=��D>y�D?  D?�fD@  D@� DAfDA� DB  DB� DCfDC� DC��DDy�DD��DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DK��DL� DM  DM� DM��DN� DOfDO�fDP  DPy�DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DVfDV�fDW  DW� DX  DX� DX��DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�De��Dfy�Dg  Dg� Dh  Dh� Di  Di�fDjfDj�fDkfDk� DlfDl�fDm  Dm� DnfDn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Dsy�Dt  Dt� Du  Du� Dv  Dv� DwfDwy�Dw� Dy��D�H D�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @<��@�  @�  A  A$  AD  Ad  A�  A�  A�33A�  A�  A�  A�  A�  B  B	  B  B  B!ffB)ffB1ffB9ffBA  BI  BQ  BY  Ba  Bi  BqffByffB�� B�� B�� B�� B�� B��3B�� B�L�B�� B�� B�� B�� B�� B�� B�� B��3B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C&fC@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6&fC8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ CfY�Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�,�C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�3C�  C�  C�  C�  C�3C�  C�,�C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�3C�  C�3C�  C�,�C�  C�  C�  C�  C�,�C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�,�C�  C�  C�  C�  C�  D  D � D D� D	�D��D D� D D� D	�D� D	�D��D D� D D� D	 D	� D
 D
� D	�D� D D� D D�fD D� D D� D	�D� D D�fDfD� D D� D D� D D��D	�D��D D� DfD� D D� D D��D	�D��D D�fDfD�fD D� D D� D  D � D! D!� D" D"�fD# D#� D$ D$� D% D%� D&	�D&��D' D'� D( D(� D) D)� D* D*�fD+fD+� D, D,�fD- D-� D. D.� D/fD/�fD0 D0��D1	�D1��D2	�D2��D3	�D3� D4 D4� D5	�D5� D6 D6� D7 D7� D8 D8� D9 D9� D:	�D:�fD;fD;��D<	�D<�fD= D=� D>	�D>��D? D?�fD@ D@� DAfDA� DB DB� DCfDC� DD	�DD��DE	�DE��DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK��DL	�DL� DM DM� DN	�DN� DOfDO�fDP DP��DQ DQ� DR DR� DS DS�fDT DT� DU DU� DVfDV�fDW DW� DX DX� DY	�DY� DZfDZ� D[ D[� D\ D\� D] D]� D^ D^� D_fD_�fD` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De��Df	�Df��Dg Dg� Dh Dh� Di Di�fDjfDj�fDkfDk� DlfDl�fDm Dm� DnfDn�fDo Do� Dp Dp� Dq Dq� Dr Dr�fDs Ds��Dt Dt� Du Du� Dv Dv� DwfDw��Dw� Dy��D�P D�{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�dZA�hsA�ffA�ffA�dZA�`BA�^5A�^5A�^5A�XA�G�A�(�A�{A�oA�VA���A���A־wAֶFA֥�A֝�A֓uAև+A�VA���AլA�t�A�ffA�ZA�5?A�1A�|�A��HA��#A��A���Aϕ�A�A��A͙�A�;dA�AˁA���A�&�AȍPA�x�A�=qA��HA�;dAƁA��TA�$�A�`BA���A� �A���A�~�A���A�M�A�v�A��9A�E�A���A��TA�33A�A�bA�x�A��A��
A��A��A��wA�G�A�
=A��A�VA�ffA�~�A���A�JA���A�ĜA���A�O�A���A� �A�+A�A�(�A���A��RA���A���A���A��A���A�"�A��-A�A���A�ffA�n�A�{A�(�A�x�A�%A�|�A�/A��#A���A���A}�A}��A|��A{%AxZAo��AlȴAi��AhffAg|�A_�A]\)A\AZjAW��ATjAQG�AMt�ALE�AKO�AI�^AHv�AGp�AF�uAE��AE&�AC�wAA�A@A�A>n�A<bA:�A7O�A5A3"�A2~�A1�^A/�A.��A,{A*�`A)?}A(^5A'p�A&n�A%t�A$�+A#�TA"�`A!�AC�A�AȴA�-AAn�AA9XA��A�^Av�A�PA��A%A�AK�A��A�A%A	��A	%A�!A-A��AoAffA�#A�/A=qA��A`BAC�A��A�+A�uA��AE�A ��A =q@�+@�7L@��@���@��@�ƨ@��@�X@�(�@�ȴ@�x�@�u@�{@���@�@�@�7L@�b@�C�@�-@㝲@��@�E�@�=q@�E�@�7@�j@�Q�@݉7@۾w@�+@ڏ\@�M�@��@�Ĝ@�dZ@�?}@�j@�1@ӥ�@�9X@ԃ@��#@�j@�M�@У�@�dZ@�@Ͳ-@͡�@�p�@��T@ͺ^@�j@�hs@�|�@��@���@Ƨ�@�o@�\)@���@�ff@��#@�O�@�G�@��@ģ�@§�@��@�=q@��9@��;@�t�@�S�@�=q@�`B@�?}@�7L@�x�@��^@��-@�t�@�;d@�1@�Z@�ƨ@�l�@�1@���@�1@��u@�ff@�"�@�|�@��w@���@�ƨ@�|�@��y@��!@���@�x�@��#@���@���@��j@���@��@��u@�r�@��@�ȴ@�v�@�{@�J@��@���@��7@��@�j@��;@��F@�"�@���@�@���@�&�@��u@�|�@�o@�v�@��#@���@��7@�X@��@��@���@�j@��@��!@�V@��@�O�@�Z@��@�=q@�p�@���@�r�@���@��/@�%@�7L@���@��@�V@��^@�1'@���@�K�@�+@�n�@�5?@�{@�-@�hs@��u@��@��D@���@���@�{@�=q@�M�@�=q@���@���@�/@�Ĝ@���@��u@��@��u@�A�@�"�@�^5@�5?@�-@�$�@���@��@���@�x�@�`B@�O�@���@�@�j@�9X@�Z@�A�@�  @��@��@�"�@�@��@���@�^5@�E�@��@�@��^@���@���@��7@�x�@�`B@�G�@�7L@��@���@�1'@��@���@���@�C�@��H@��!@��\@�n�@�M�@�J@��^@��7@�G�@���@��9@��D@��@�r�@�I�@��@��;@��F@�S�@�+@�@��y@���@�v�@���@�p�@��@���@��@� �@��w@�dZ@�;d@��H@�v�@���@��j@��@�9X@���@���@���@�@���@�M�@��@��#@�`B@��@���@�b@���@�t�@�+@�"�@�ȴ@�5?@��@��-@��h@��@�`B@�X@�G�@��<@~�@k;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�dZA�hsA�ffA�ffA�dZA�`BA�^5A�^5A�^5A�XA�G�A�(�A�{A�oA�VA���A���A־wAֶFA֥�A֝�A֓uAև+A�VA���AլA�t�A�ffA�ZA�5?A�1A�|�A��HA��#A��A���Aϕ�A�A��A͙�A�;dA�AˁA���A�&�AȍPA�x�A�=qA��HA�;dAƁA��TA�$�A�`BA���A� �A���A�~�A���A�M�A�v�A��9A�E�A���A��TA�33A�A�bA�x�A��A��
A��A��A��wA�G�A�
=A��A�VA�ffA�~�A���A�JA���A�ĜA���A�O�A���A� �A�+A�A�(�A���A��RA���A���A���A��A���A�"�A��-A�A���A�ffA�n�A�{A�(�A�x�A�%A�|�A�/A��#A���A���A}�A}��A|��A{%AxZAo��AlȴAi��AhffAg|�A_�A]\)A\AZjAW��ATjAQG�AMt�ALE�AKO�AI�^AHv�AGp�AF�uAE��AE&�AC�wAA�A@A�A>n�A<bA:�A7O�A5A3"�A2~�A1�^A/�A.��A,{A*�`A)?}A(^5A'p�A&n�A%t�A$�+A#�TA"�`A!�AC�A�AȴA�-AAn�AA9XA��A�^Av�A�PA��A%A�AK�A��A�A%A	��A	%A�!A-A��AoAffA�#A�/A=qA��A`BAC�A��A�+A�uA��AE�A ��A =q@�+@�7L@��@���@��@�ƨ@��@�X@�(�@�ȴ@�x�@�u@�{@���@�@�@�7L@�b@�C�@�-@㝲@��@�E�@�=q@�E�@�7@�j@�Q�@݉7@۾w@�+@ڏ\@�M�@��@�Ĝ@�dZ@�?}@�j@�1@ӥ�@�9X@ԃ@��#@�j@�M�@У�@�dZ@�@Ͳ-@͡�@�p�@��T@ͺ^@�j@�hs@�|�@��@���@Ƨ�@�o@�\)@���@�ff@��#@�O�@�G�@��@ģ�@§�@��@�=q@��9@��;@�t�@�S�@�=q@�`B@�?}@�7L@�x�@��^@��-@�t�@�;d@�1@�Z@�ƨ@�l�@�1@���@�1@��u@�ff@�"�@�|�@��w@���@�ƨ@�|�@��y@��!@���@�x�@��#@���@���@��j@���@��@��u@�r�@��@�ȴ@�v�@�{@�J@��@���@��7@��@�j@��;@��F@�"�@���@�@���@�&�@��u@�|�@�o@�v�@��#@���@��7@�X@��@��@���@�j@��@��!@�V@��@�O�@�Z@��@�=q@�p�@���@�r�@���@��/@�%@�7L@���@��@�V@��^@�1'@���@�K�@�+@�n�@�5?@�{@�-@�hs@��u@��@��D@���@���@�{@�=q@�M�@�=q@���@���@�/@�Ĝ@���@��u@��@��u@�A�@�"�@�^5@�5?@�-@�$�@���@��@���@�x�@�`B@�O�@���@�@�j@�9X@�Z@�A�@�  @��@��@�"�@�@��@���@�^5@�E�@��@�@��^@���@���@��7@�x�@�`B@�G�@�7L@��@���@�1'@��@���@���@�C�@��H@��!@��\@�n�@�M�@�J@��^@��7@�G�@���@��9@��D@��@�r�@�I�@��@��;@��F@�S�@�+@�@��y@���@�v�@���@�p�@��@���@��@� �@��w@�dZ@�;d@��H@�v�@���@��j@��@�9X@���@���@���@�@���@�M�@��@��#@�`B@��@���@�b@���@�t�@�+@�"�@�ȴ@�5?@��@��-@��h@��@�`B@�X@�G�@��<@~�@k;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
<jB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
B�B
K�B
S�B
iyB
r�B
s�B
}�B
|�B
� B
�B
�DB
�uB
ȴB
��B
��B
��B
�B%B#�B)�B,B5?BjB}�B~�B�B�hB�B�dB�BB%B�B8RBC�BO�B^5Bu�BhsBW
BH�BH�BL�BO�BW
B_;Bu�B�PB��B��B�dB�jB�^B�LB�-B�B��B��B�VB�%B~�By�Bp�BgmBP�BD�B>wB49B�B�B	7B��B�#B��BǮB��Br�B49B-B1B
�B
ɺB
�B
��B
��B
|�B
y�B
o�B
k�B
t�B
S�B
\B	��B
  B	��B	�BB	ƨB	�uB	�B	q�B	iyB	_;B	A�B	6FB	0!B	)�B	$�B	�B	1B��B�B�B�sB�NB�;B�)B�B�B��B��BȴBƨB�wB�qB�dB�LB�RB�LB�?B�9B�9B�?B�3B�?B�^B�FB�-B�'B�B�B�B��B��B��B��B��B�B�-B�-B��B��B��B��B��B��B��B��B�B�-B�'B�RB�qB�dB�dB�dB�wBÖBȴBƨB�}B�dB�dB�dB�dB�wBŢBǮB��B��B��B��B��B��B��BȴBÖBBBǮBȴBɺBȴBȴB��BɺBȴBǮBǮBȴBǮBŢBŢBŢBŢBǮBȴB��B��B��B��B��B��B��B��B��B�B�B�B�
B�B�#B�NB�yB��B��B��B��B��B��B	  B	B		7B	hB	�B	�B	�B	uB	uB	{B	�B	�B	&�B	)�B	-B	1'B	2-B	5?B	<jB	;dB	7LB	8RB	9XB	5?B	33B	1'B	1'B	1'B	33B	33B	7LB	;dB	?}B	A�B	A�B	C�B	I�B	N�B	M�B	M�B	S�B	W
B	YB	_;B	o�B	x�B	~�B	�B	�B	�B	�%B	�%B	�B	�B	�B	�+B	�7B	�1B	�7B	�JB	�JB	�JB	�JB	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�FB	�LB	�LB	�LB	�LB	�LB	�FB	�?B	�9B	�3B	�3B	�3B	�9B	�FB	�dB	�wB	B	ȴB	��B	��B	��B	��B	ɺB	ȴB	��B	ɺB	ȴB	ȴB	��B	��B	ȴB	ȴB	��B	�B	�B	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
1B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
+B
+B
+B
+B
+B
%B
%B
B
+B
+B
	7B
	7B
	7B
	7B
	7B

=B
	�B
�B
*2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
<jB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
B�B
K�B
S�B
iyB
r�B
s�B
}�B
|�B
� B
�B
�DB
�uB
ȴB
��B
��B
��B
�B%B#�B)�B,B5?BjB}�B~�B�B�hB�B�dB�BB%B�B8RBC�BO�B^5Bu�BhsBW
BH�BH�BL�BO�BW
B_;Bu�B�PB��B��B�dB�jB�^B�LB�-B�B��B��B�VB�%B~�By�Bp�BgmBP�BD�B>wB49B�B�B	7B��B�#B��BǮB��Br�B49B-B1B
�B
ɺB
�B
��B
��B
|�B
y�B
o�B
k�B
t�B
S�B
\B	��B
  B	��B	�BB	ƨB	�uB	�B	q�B	iyB	_;B	A�B	6FB	0!B	)�B	$�B	�B	1B��B�B�B�sB�NB�;B�)B�B�B��B��BȴBƨB�wB�qB�dB�LB�RB�LB�?B�9B�9B�?B�3B�?B�^B�FB�-B�'B�B�B�B��B��B��B��B��B�B�-B�-B��B��B��B��B��B��B��B��B�B�-B�'B�RB�qB�dB�dB�dB�wBÖBȴBƨB�}B�dB�dB�dB�dB�wBŢBǮB��B��B��B��B��B��B��BȴBÖBBBǮBȴBɺBȴBȴB��BɺBȴBǮBǮBȴBǮBŢBŢBŢBŢBǮBȴB��B��B��B��B��B��B��B��B��B�B�B�B�
B�B�#B�NB�yB��B��B��B��B��B��B	  B	B		7B	hB	�B	�B	�B	uB	uB	{B	�B	�B	&�B	)�B	-B	1'B	2-B	5?B	<jB	;dB	7LB	8RB	9XB	5?B	33B	1'B	1'B	1'B	33B	33B	7LB	;dB	?}B	A�B	A�B	C�B	I�B	N�B	M�B	M�B	S�B	W
B	YB	_;B	o�B	x�B	~�B	�B	�B	�B	�%B	�%B	�B	�B	�B	�+B	�7B	�1B	�7B	�JB	�JB	�JB	�JB	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�FB	�LB	�LB	�LB	�LB	�LB	�FB	�?B	�9B	�3B	�3B	�3B	�9B	�FB	�dB	�wB	B	ȴB	��B	��B	��B	��B	ɺB	ȴB	��B	ɺB	ȴB	ȴB	��B	��B	ȴB	ȴB	��B	�B	�B	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
1B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
+B
+B
+B
+B
+B
%B
%B
B
+B
+B
	7B
	7B
	7B
	7B
	7B

=B
	�B
�B
*2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190516                              AO  ARCAADJP                                                                    20181005190516    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190516  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190516  QCF$                G�O�G�O�G�O�8000            