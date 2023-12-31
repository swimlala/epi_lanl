CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:00Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190600  20181005190600  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��idP{�1   @��i���@0���S���c��1&�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C?�fCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � DfD�fD  D� D  D�fDfD�fD  D� D  D�fD  D� D  Dy�D	  D	� D
  D
� D
��Dy�D��D� D  D� D  D� D  Dy�D��D� D  D� D��D� D  D� D  D� D��D� D  Dy�D  D� D  D� D  D� D  D� DfD�fDfD� D��Dy�D  D�fDfD�fD   D � D!  D!� D"  D"� D#  D#�fD$fD$�fD%fD%�fD&fD&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2y�D2��D3� D4fD4�fD5  D5� D6  D6�fD7fD7� D7��D8� D9  D9�fD:  D:� D:��D;y�D;��D<� D=  D=� D>fD>�fD?  D?� D@fD@� D@��DA� DB  DB� DC  DC� DC��DDy�DE  DE�fDF  DF� DG  DG� DH  DHy�DI  DI�fDJ  DJy�DK  DK� DLfDL�fDMfDM� DNfDN�fDO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DSy�DT  DT� DU  DU�fDV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ�fD[  D[y�D\  D\�fD]  D]� D]��D^� D_  D_�fD`  D`� Da  Day�Da��Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� DgfDg�fDh  Dh� Di  Di� DjfDj� Dk  Dk� Dk��Dl� Dm  Dm� DnfDn�fDo  Do� Dp  Dp� Dp��Dq� Dr  Dr� Dr��Ds�fDtfDt� Du  Du� Dv  Dv�fDw  Dw� DwٚDy��D�8�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @)��@�  @�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A�33B  B	  B  B  B!  B)  B1  B9  BAffBI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B��3B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B��3B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C&fC@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8Y�C:@ C<@ C>@ C@&fCB&fCD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�3C�3C�3C�  C�,�C�  C�  C�,�C�  C�  C�  C�  C�  C�3C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�3C�3C�3C�  C�  C�  C�  C�3C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�3C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�,�C�  C�  C�  C�  C�3C�3C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  D  D � DfD�fD D� D D�fDfD�fD D� D D�fD D� D D��D	 D	� D
 D
� D	�D��D	�D� D D� D D� D D��D	�D� D D� D	�D� D D� D D� D	�D� D D��D D� D D� D D� D D� DfD�fDfD� D	�D��D D�fDfD�fD  D � D! D!� D" D"� D# D#�fD$fD$�fD%fD%�fD&fD&� D' D'��D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2	�D2��D3	�D3� D4fD4�fD5 D5� D6 D6�fD7fD7� D8	�D8� D9 D9�fD: D:� D;	�D;��D<	�D<� D= D=� D>fD>�fD? D?� D@fD@� DA	�DA� DB DB� DC DC� DD	�DD��DE DE�fDF DF� DG DG� DH DH��DI DI�fDJ DJ��DK DK� DLfDL�fDMfDM� DNfDN�fDO DO� DP DP� DQ DQ� DR DR�fDS DS��DT DT� DU DU�fDV DV� DW DW� DX	�DX� DY DY� DZ DZ�fD[ D[��D\ D\�fD] D]� D^	�D^� D_ D_�fD` D`� Da Da��Db	�Db� Dc Dc� Dd Dd� De	�De� Df Df� DgfDg�fDh Dh� Di Di� DjfDj� Dk Dk� Dl	�Dl� Dm Dm� DnfDn�fDo Do� Dp Dp� Dq	�Dq� Dr Dr� Ds	�Ds�fDtfDt� Du Du� Dv Dv�fDw Dw� Dw�Dy��D�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȓuAȍPAȕ�Aș�Aȗ�Aș�Aș�Aȗ�Aȗ�Aț�Aȝ�A�n�A�7LA�r�AƩ�Ař�A�G�A�-A��A�
=A���A���AľwAļjAĮAď\Ać+A�l�A�E�A��A�%A���A��A��TA��/A���A���A�ĜA�oA�Q�A�t�AăAĮAĮAĬAć+A�jA�XA�/A��A�A���A�Q�A��AA���A�r�A�\)A�K�A���A�&�A��;A��jA�`BA�-A�bA�r�A�ȴA��A�5?A��HA�/A���A�E�A���A���A�|�A���A��^A���A�ĜA�`BA��A�=qA�VA���A�p�A���A�ƨA�;dA��TA�|�A�%A���A��A�$�A�"�A�$�A�bNA�1A�E�A�dZA�jA���A�ĜA�  A��\A�oAzA�Ay7LAu"�An�+AlA�Ag�^Ac�TAa7LAY�^AUoASARȴAQdZAQ
=AP�+AN��AK��AJ�+AHȴAE&�AD~�ACXAB�A=�
A5x�A3�A2�A.�9A-dZA+��A*v�A)l�A'�7A&�A%S�A$A#��A#�-A"~�A!��A!|�A ��A��Az�A��A�#A�uA��A��A�A|�A�A^5A(�A�A�A��AbNAJAz�Ax�A��A�FAA�A��A�-Al�A
A�A	C�A�jA1A
=A�jAC�AXA��A~�AM�AJA�#A?}A ȴA I�@���@���@�$�@�@�p�@�hs@�X@�b@�ȴ@���@�
=@�K�@�dZ@�l�@��@��#@��@���@� �@��@�@�@�9@�C�@�\@��@��@�l�@�n�@�O�@�S�@旍@噚@�(�@�R@���@�-@�@�`B@�Q�@�C�@��@ޗ�@�ff@�=q@���@ݡ�@���@�  @�\)@�o@�@�v�@�z�@�9X@��@��m@�"�@��T@պ^@�x�@�S�@�~�@�{@�5?@�{@Ѓ@�;d@�ff@Ͳ-@˕�@�+@�o@˝�@˥�@˝�@�
=@�hs@�"�@��@�dZ@�9X@���@Ǖ�@�ff@ř�@�O�@�7L@Ĭ@��@�ƨ@�t�@���@��@�1'@�(�@�o@\@�5?@�$�@�@��h@�z�@���@���@�/@�@���@���@�ƨ@��
@�S�@�{@��@��T@��`@���@��@���@�dZ@�C�@�o@�E�@�{@�~�@��@��7@�p�@�hs@�x�@�5?@��@��@���@�Q�@�S�@�E�@��^@�J@�=q@�V@�@�O�@���@�Q�@�9X@�1'@�b@��m@��+@��@���@��@�O�@���@�j@�(�@�ƨ@�C�@��@���@�5?@��@���@���@�j@�9X@�b@��
@��F@�dZ@�l�@�|�@���@��#@��^@���@��@�7L@�V@��j@���@�z�@�j@�A�@� �@��@��w@��w@���@�t�@��@�l�@�K�@�
=@���@�;d@�
=@��R@��+@��!@�M�@�p�@���@��@��m@���@�|�@�dZ@��@���@�@��T@���@�%@�/@�%@��u@�1'@��@��@�|�@�33@��@���@�~�@�-@�`B@�/@��j@�z�@�I�@�1'@�ƨ@��P@�t�@�C�@�"�@��H@��\@�M�@�-@���@��h@�G�@�V@�(�@�1@�  @�dZ@���@�V@�$�@��@��-@�?}@��@��@��u@�bN@�(�@���@�S�@�+@�;d@�33@�o@��@�n�@�=q@��7@�O�@�hs@�7L@���@���@��u@�Z@�b@�ƨ@���@���@��F@�t�@�"�@�@��@��y@��y@��@��@���@��R@�~�@�E�@��@�@�p�@�&�@��`@��j@�bN@��@��@��P@�&@w��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AȓuAȍPAȕ�Aș�Aȗ�Aș�Aș�Aȗ�Aȗ�Aț�Aȝ�A�n�A�7LA�r�AƩ�Ař�A�G�A�-A��A�
=A���A���AľwAļjAĮAď\Ać+A�l�A�E�A��A�%A���A��A��TA��/A���A���A�ĜA�oA�Q�A�t�AăAĮAĮAĬAć+A�jA�XA�/A��A�A���A�Q�A��AA���A�r�A�\)A�K�A���A�&�A��;A��jA�`BA�-A�bA�r�A�ȴA��A�5?A��HA�/A���A�E�A���A���A�|�A���A��^A���A�ĜA�`BA��A�=qA�VA���A�p�A���A�ƨA�;dA��TA�|�A�%A���A��A�$�A�"�A�$�A�bNA�1A�E�A�dZA�jA���A�ĜA�  A��\A�oAzA�Ay7LAu"�An�+AlA�Ag�^Ac�TAa7LAY�^AUoASARȴAQdZAQ
=AP�+AN��AK��AJ�+AHȴAE&�AD~�ACXAB�A=�
A5x�A3�A2�A.�9A-dZA+��A*v�A)l�A'�7A&�A%S�A$A#��A#�-A"~�A!��A!|�A ��A��Az�A��A�#A�uA��A��A�A|�A�A^5A(�A�A�A��AbNAJAz�Ax�A��A�FAA�A��A�-Al�A
A�A	C�A�jA1A
=A�jAC�AXA��A~�AM�AJA�#A?}A ȴA I�@���@���@�$�@�@�p�@�hs@�X@�b@�ȴ@���@�
=@�K�@�dZ@�l�@��@��#@��@���@� �@��@�@�@�9@�C�@�\@��@��@�l�@�n�@�O�@�S�@旍@噚@�(�@�R@���@�-@�@�`B@�Q�@�C�@��@ޗ�@�ff@�=q@���@ݡ�@���@�  @�\)@�o@�@�v�@�z�@�9X@��@��m@�"�@��T@պ^@�x�@�S�@�~�@�{@�5?@�{@Ѓ@�;d@�ff@Ͳ-@˕�@�+@�o@˝�@˥�@˝�@�
=@�hs@�"�@��@�dZ@�9X@���@Ǖ�@�ff@ř�@�O�@�7L@Ĭ@��@�ƨ@�t�@���@��@�1'@�(�@�o@\@�5?@�$�@�@��h@�z�@���@���@�/@�@���@���@�ƨ@��
@�S�@�{@��@��T@��`@���@��@���@�dZ@�C�@�o@�E�@�{@�~�@��@��7@�p�@�hs@�x�@�5?@��@��@���@�Q�@�S�@�E�@��^@�J@�=q@�V@�@�O�@���@�Q�@�9X@�1'@�b@��m@��+@��@���@��@�O�@���@�j@�(�@�ƨ@�C�@��@���@�5?@��@���@���@�j@�9X@�b@��
@��F@�dZ@�l�@�|�@���@��#@��^@���@��@�7L@�V@��j@���@�z�@�j@�A�@� �@��@��w@��w@���@�t�@��@�l�@�K�@�
=@���@�;d@�
=@��R@��+@��!@�M�@�p�@���@��@��m@���@�|�@�dZ@��@���@�@��T@���@�%@�/@�%@��u@�1'@��@��@�|�@�33@��@���@�~�@�-@�`B@�/@��j@�z�@�I�@�1'@�ƨ@��P@�t�@�C�@�"�@��H@��\@�M�@�-@���@��h@�G�@�V@�(�@�1@�  @�dZ@���@�V@�$�@��@��-@�?}@��@��@��u@�bN@�(�@���@�S�@�+@�;d@�33@�o@��@�n�@�=q@��7@�O�@�hs@�7L@���@���@��u@�Z@�b@�ƨ@���@���@��F@�t�@�"�@�@��@��y@��y@��@��@���@��R@�~�@�E�@��@�@�p�@�&�@��`@��j@�bN@��@��@��P@�&@w��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�}B�}B�}B��BÖBÖBÖB��B�B�/B�mB�mB��B	  B	B	+B	JB	hB	hB	bB	\B	PB	DB	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	33B	N�B	e`B	w�B	ǮB	��B	�sB
VB
2-B
B�B
e`B
��B
��B
�ZB'�B;dBXBq�B~�B�B�B�{B��B��B�JBr�Bx�B��BǮB�jBB��BƨBB�
B�`B�B�B�B��B�B�B�B��B��B�B�)B��B��B��B��B�VBu�Bm�BcTB=qB+B#�B{BB
�/B
��B
dZB
�B	�yB	�RB	�{B	�=B	�JB	r�B	R�B	I�B	.B	DB��B�B�B��BBɺB�B�B	B	JB	#�B	/B	@�B	D�B	F�B	Q�B	Q�B	YB	Q�B	!�B��B��B��B�FB�B�B�B�B�!B�3B�LB�?B�FB�^BB��BBǮBĜBB�wB�qBB��B��BƨB��B��B��B��B��B��B��B�B�B�B�B��B��BǮBƨBɺBǮB��B�qB�jB�jB�XB�FB�B��B��B��B��B��B��B�-B�!B�B�B�FB�^B�jB�}BÖB��B��B�B�/B�HB�ZB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	%B	+B	1B	1B	1B	1B	
=B	VB	hB	{B	�B	�B	�B	�B	 �B	#�B	(�B	+B	+B	,B	+B	33B	7LB	9XB	=qB	<jB	>wB	<jB	:^B	8RB	<jB	A�B	F�B	M�B	N�B	M�B	E�B	>wB	?}B	G�B	R�B	S�B	R�B	XB	XB	XB	ZB	[#B	ZB	`BB	e`B	k�B	m�B	p�B	q�B	v�B	v�B	w�B	z�B	{�B	{�B	{�B	z�B	v�B	x�B	� B	�1B	�bB	�hB	�oB	�bB	�\B	�JB	�+B	�%B	�+B	�7B	�1B	�1B	�1B	�+B	�7B	�=B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�FB	�^B	��B	B	B	ĜB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�)B	�5B	�;B	�;B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B
	7B

=B

=B
	7B
	7B

=B
	7B
1B
+B
+B
	7B

=B
DB
DB
PB
PB
VB
\B
\B
\B
bB
bB
hB
{B
{B
uB
uB
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!B
-)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B�}B�}B�}B��BÖBÖBÖB��B�B�/B�mB�mB��B	  B	B	+B	JB	hB	hB	bB	\B	PB	DB	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	33B	N�B	e`B	w�B	ǮB	��B	�sB
VB
2-B
B�B
e`B
��B
��B
�ZB'�B;dBXBq�B~�B�B�B�{B��B��B�JBr�Bx�B��BǮB�jBB��BƨBB�
B�`B�B�B�B��B�B�B�B��B��B�B�)B��B��B��B��B�VBu�Bm�BcTB=qB+B#�B{BB
�/B
��B
dZB
�B	�yB	�RB	�{B	�=B	�JB	r�B	R�B	I�B	.B	DB��B�B�B��BBɺB�B�B	B	JB	#�B	/B	@�B	D�B	F�B	Q�B	Q�B	YB	Q�B	!�B��B��B��B�FB�B�B�B�B�!B�3B�LB�?B�FB�^BB��BBǮBĜBB�wB�qBB��B��BƨB��B��B��B��B��B��B��B�B�B�B�B��B��BǮBƨBɺBǮB��B�qB�jB�jB�XB�FB�B��B��B��B��B��B��B�-B�!B�B�B�FB�^B�jB�}BÖB��B��B�B�/B�HB�ZB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	%B	+B	1B	1B	1B	1B	
=B	VB	hB	{B	�B	�B	�B	�B	 �B	#�B	(�B	+B	+B	,B	+B	33B	7LB	9XB	=qB	<jB	>wB	<jB	:^B	8RB	<jB	A�B	F�B	M�B	N�B	M�B	E�B	>wB	?}B	G�B	R�B	S�B	R�B	XB	XB	XB	ZB	[#B	ZB	`BB	e`B	k�B	m�B	p�B	q�B	v�B	v�B	w�B	z�B	{�B	{�B	{�B	z�B	v�B	x�B	� B	�1B	�bB	�hB	�oB	�bB	�\B	�JB	�+B	�%B	�+B	�7B	�1B	�1B	�1B	�+B	�7B	�=B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�FB	�^B	��B	B	B	ĜB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�)B	�5B	�;B	�;B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B
	7B

=B

=B
	7B
	7B

=B
	7B
1B
+B
+B
	7B

=B
DB
DB
PB
PB
VB
\B
\B
\B
bB
bB
hB
{B
{B
uB
uB
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!B
-)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190600                              AO  ARCAADJP                                                                    20181005190600    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190600  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190600  QCF$                G�O�G�O�G�O�8000            