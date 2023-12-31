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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190601  20181005190601  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���b:|z1   @�����wb@1W���+�c��`A�71   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @33@�  @�  A��A   A@  A`  A�  A�33A�33A�33A�  A�  A�  A���B   B  B  B  B   B'��B/��B7��B@  BH  BP  BW��B_��Bg��Bo��Bw��B��B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C�C�C  C  C�C  C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3D   D � D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D  D� D	  D	� D
fD
� D  D�fD  Dy�D  D�fDfD� D  Dy�D  D� D  D� D  D� D��Dy�D  D� DfD�fD  D� D��D� D  D� D  D�fDfD�fD  D� D  Dy�D  D� D  D�fDfD� D   D y�D!  D!�fD"  D"�fD#  D#y�D#��D$� D%fD%� D&  D&�fD'fD'�fD(fD(� D)  D)� D*fD*�fD+fD+� D,  D,� D-  D-� D.  D.� D/  D/�fD0fD0� D0��D1� D2  D2� D3  D3�fD4fD4�fD5  D5� D6fD6�fD7fD7�fD8fD8� D9  D9� D:  D:�fD;  D;� D<  D<�fD=fD=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DCfDC�fDDfDD�fDEfDE�fDFfDF� DF��DG� DH  DH� DH��DI� DJ  DJy�DJ��DK� DL  DL� DM  DMy�DM��DN� DO  DO� DO��DP� DQfDQ�fDR  DR� DS  DS� DT  DT�fDUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[�fD\  D\� D]fD]� D]��D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� De��Df� Dg  Dg� Dh  Dh� Di  Diy�Di��Dj� DkfDk�fDl  Dl�fDmfDm�fDnfDn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dty�Du  Du�fDv  Dv� Dw  Dw� Dw�3Dy��D�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @#33@�  @�  A��A$  AD  Ad  A�  A�33A�33A�33A�  A�  A�  A���B  B	  B  B  B!  B(��B0��B8��BA  BI  BQ  BX��B`��Bh��Bp��Bx��B�L�B�� B��3B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̳3BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C&fC@ C@ C@ C
@ C@ CY�CY�C@ C@ CY�C@ C&fC&fC@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>&fC@&fCB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ CfY�ChY�Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�3C�  C�  C�3C�  C�,�C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�3C�3C�3C�  C�,�C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3D  D � D D� D D� D D� D	�D��D	�D� D D� D D� D D� D	 D	� D
fD
� D D�fD D��D D�fDfD� D D��D D� D D� D D� D	�D��D D� DfD�fD D� D	�D� D D� D D�fDfD�fD D� D D��D D� D D�fDfD� D  D ��D! D!�fD" D"�fD# D#��D$	�D$� D%fD%� D& D&�fD'fD'�fD(fD(� D) D)� D*fD*�fD+fD+� D, D,� D- D-� D. D.� D/ D/�fD0fD0� D1	�D1� D2 D2� D3 D3�fD4fD4�fD5 D5� D6fD6�fD7fD7�fD8fD8� D9 D9� D: D:�fD; D;� D< D<�fD=fD=� D> D>� D? D?� D@ D@� DAfDA� DB DB� DCfDC�fDDfDD�fDEfDE�fDFfDF� DG	�DG� DH DH� DI	�DI� DJ DJ��DK	�DK� DL DL� DM DM��DN	�DN� DO DO� DP	�DP� DQfDQ�fDR DR� DS DS� DT DT�fDUfDU� DV DV� DW DW� DX DX� DY DY� DZ DZ��D[ D[�fD\ D\� D]fD]� D^	�D^� D_ D_� D`fD`� Da Da� Db Db� Dc Dc� Dd Dd� DefDe� Df	�Df� Dg Dg� Dh Dh� Di Di��Dj	�Dj� DkfDk�fDl Dl�fDmfDm�fDnfDn� Do Do� Dp Dp��Dq Dq� Dr Dr� Ds	�Ds� Dt Dt��Du Du�fDv Dv� Dw Dw� Dw�3Dy��D�${111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�bNA�^5A�\)A�XA�\)A�^5A�M�A�oA�^5A��A�A�E�A���A��A�?}A�A�A�l�AƋDA�K�A��A�1'A�AƩ�A�n�A�~�A�7LAŏ\A�XA��A�oA��A���A�
=A���A�l�A�O�A�9XA�-A�(�A�$�A�JA�A���A���A��^A���A��\A��\A�ffA�-A�ƨA��RA��^A���A�jA�A�r�A��A�  A��A��A��DA�x�A�v�A�n�A�Q�A�$�A��A���A�v�A�XA��A��9A�%A���A�+A�A��mA��A�Q�A��A��hA�t�A���A���A�t�A��^A�A�$�A��A�`BA��/A��jA�(�A�C�A�A��A��A�r�A��A���A�t�A�JA��jA�{A��hA~�9A{�At��Aq�-Ap �Aj�DA`bNA\�jAYhsAUl�AR��AQ�-AQ�AP��AO��AK��AE+AA33A>Q�A=?}A<  A9��A7��A5�-A3�#A2I�A0�uA/��A/��A/��A/�A-��A*��A*-A)�A)O�A(��A(M�A'��A'�7A'\)A'A&E�A%p�A$�A!�A��A��A?}AAE�A��A�mA��Az�A�;A��A-A�mAt�A�`A%A�`A��A�A^5A�AC�AVA�/A��A�!A��A
��A	XA��A�+AffAI�A-AA�-A�A�9A�DAbNA �AdZA�AA��A�^A�mA�#A�#A�AƨA+A�;A�#A�AA�AĜA��A��A��A+A��AbN@��!@�r�@�V@��y@�J@�-@�{@��@�r�@���@�A�@띲@��@�Q�@�z�@�Z@�"�@��@�v�@��@�j@�P@�S�@��@��@��T@�7@�hs@�`B@�X@�9@�@�  @���@�5?@�X@���@��@�&�@���@߮@�J@�{@݁@�V@�1@���@�$�@��@�9X@��;@�v�@Ԭ@���@ӶF@��@ӕ�@ӕ�@�dZ@�"�@���@д9@�r�@�1'@��;@Ͼw@υ@�\)@��@Ώ\@�@�E�@�=q@�G�@�x�@�1@�\)@���@˝�@˅@�33@���@��@�9X@��y@Ƨ�@�ff@ř�@Ĵ9@�9X@��@þw@�"�@�ff@��-@�/@��`@��@���@��@�9X@���@�+@��!@�ff@�n�@��@���@�Q�@�1@�\)@�@�@��H@�ff@�-@�{@��@���@�z�@�  @��
@���@�K�@�@���@��@���@�p�@�O�@�7L@�%@���@���@�dZ@�ȴ@�V@�@�&�@���@�Z@��@��w@�l�@��H@���@��@�`B@�G�@�%@��@�bN@��@���@�t�@�;d@�@��y@�ȴ@��+@�$�@���@�O�@��`@���@�Ĝ@���@�Q�@�b@�K�@��H@���@�M�@�$�@��@���@���@�O�@��@���@���@�z�@�9X@� �@�;d@���@�v�@�ff@�J@��@��@��-@�x�@���@�z�@�9X@�ƨ@�dZ@�"�@���@��+@��@��@��D@�Q�@� �@��F@�l�@�o@�n�@��@�p�@���@��j@�Z@�  @��@���@�C�@���@�ȴ@���@�-@��T@���@���@��@�?}@��@��@���@��9@���@��@�(�@���@��@�|�@�t�@�l�@�K�@�"�@�ȴ@�v�@��@��^@��h@�p�@�/@���@��9@�Z@��@��P@��@�\)@�o@���@�ȴ@��@��@��7@��/@�Ĝ@�I�@��w@���@�l�@�K�@�+@�+@�dZ@�33@���@��\@�E�@�@�G�@��e@,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bNA�bNA�^5A�\)A�XA�\)A�^5A�M�A�oA�^5A��A�A�E�A���A��A�?}A�A�A�l�AƋDA�K�A��A�1'A�AƩ�A�n�A�~�A�7LAŏ\A�XA��A�oA��A���A�
=A���A�l�A�O�A�9XA�-A�(�A�$�A�JA�A���A���A��^A���A��\A��\A�ffA�-A�ƨA��RA��^A���A�jA�A�r�A��A�  A��A��A��DA�x�A�v�A�n�A�Q�A�$�A��A���A�v�A�XA��A��9A�%A���A�+A�A��mA��A�Q�A��A��hA�t�A���A���A�t�A��^A�A�$�A��A�`BA��/A��jA�(�A�C�A�A��A��A�r�A��A���A�t�A�JA��jA�{A��hA~�9A{�At��Aq�-Ap �Aj�DA`bNA\�jAYhsAUl�AR��AQ�-AQ�AP��AO��AK��AE+AA33A>Q�A=?}A<  A9��A7��A5�-A3�#A2I�A0�uA/��A/��A/��A/�A-��A*��A*-A)�A)O�A(��A(M�A'��A'�7A'\)A'A&E�A%p�A$�A!�A��A��A?}AAE�A��A�mA��Az�A�;A��A-A�mAt�A�`A%A�`A��A�A^5A�AC�AVA�/A��A�!A��A
��A	XA��A�+AffAI�A-AA�-A�A�9A�DAbNA �AdZA�AA��A�^A�mA�#A�#A�AƨA+A�;A�#A�AA�AĜA��A��A��A+A��AbN@��!@�r�@�V@��y@�J@�-@�{@��@�r�@���@�A�@띲@��@�Q�@�z�@�Z@�"�@��@�v�@��@�j@�P@�S�@��@��@��T@�7@�hs@�`B@�X@�9@�@�  @���@�5?@�X@���@��@�&�@���@߮@�J@�{@݁@�V@�1@���@�$�@��@�9X@��;@�v�@Ԭ@���@ӶF@��@ӕ�@ӕ�@�dZ@�"�@���@д9@�r�@�1'@��;@Ͼw@υ@�\)@��@Ώ\@�@�E�@�=q@�G�@�x�@�1@�\)@���@˝�@˅@�33@���@��@�9X@��y@Ƨ�@�ff@ř�@Ĵ9@�9X@��@þw@�"�@�ff@��-@�/@��`@��@���@��@�9X@���@�+@��!@�ff@�n�@��@���@�Q�@�1@�\)@�@�@��H@�ff@�-@�{@��@���@�z�@�  @��
@���@�K�@�@���@��@���@�p�@�O�@�7L@�%@���@���@�dZ@�ȴ@�V@�@�&�@���@�Z@��@��w@�l�@��H@���@��@�`B@�G�@�%@��@�bN@��@���@�t�@�;d@�@��y@�ȴ@��+@�$�@���@�O�@��`@���@�Ĝ@���@�Q�@�b@�K�@��H@���@�M�@�$�@��@���@���@�O�@��@���@���@�z�@�9X@� �@�;d@���@�v�@�ff@�J@��@��@��-@�x�@���@�z�@�9X@�ƨ@�dZ@�"�@���@��+@��@��@��D@�Q�@� �@��F@�l�@�o@�n�@��@�p�@���@��j@�Z@�  @��@���@�C�@���@�ȴ@���@�-@��T@���@���@��@�?}@��@��@���@��9@���@��@�(�@���@��@�|�@�t�@�l�@�K�@�"�@�ȴ@�v�@��@��^@��h@�p�@�/@���@��9@�Z@��@��P@��@�\)@�o@���@�ȴ@��@��@��7@��/@�Ĝ@�I�@��w@���@�l�@�K�@�+@�+@�dZ@�33@���@��\@�E�@�@�G�@��e@,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�`B�BB	B	'�B	=qB	e`B	�B
B
}�B
��B
��B
�=B
�oB
�B
�dBPB\)BgmB�B��B��B��B�B�7Bn�BhsBYBL�BM�BN�BO�BO�BP�BR�B[#B_;BcTBl�Bp�Bu�Bv�By�B�%B�Bu�Bt�B�B�=B�uB��B��BÖBȴB�/BB{B�B$�B,B6FBA�BP�BjB|�B�uB�bB�+B�B�+B?}B�B�sB�;B��B��B�VB�DB�7B�Bv�Bo�B�\B�'B�?B�-B�B�\B`BBM�B7LBPB
�B
��B
�B
l�B
dZB
N�B
F�B
-B	��B	B	��B	|�B	gmB	XB	1'B��B�;B��BƨB�B�TB�sB�B��B	\B��B�TB��BƨB�}B�LB�-B�FB�9B�3B�3B�3B�-B�'B�!B�!B�XB�jB��BȴB��B��B��B�
B�B�B�;B�5B�/B��B�RB�3B�?B�wB�XB�FB�LB�qB�wB��B��B�
B�B�B�B�5B�HB�HB�;B�)B�)B�BB�NB�ZB�`B�fB�`B�fB�B�B�B�B�B�B�B�B��B��B��B�B��B��B��B��B��B	1B	�B	�B	!�B	!�B	$�B	(�B	8RB	e`B	gmB	l�B	~�B	�B	�JB	�oB	�bB	�JB	�PB	�VB	u�B	l�B	_;B	YB	P�B	P�B	YB	[#B	ZB	VB	O�B	W
B	p�B	~�B	�B	�%B	�JB	�JB	�JB	�=B	�1B	�+B	�7B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�^B	�^B	�dB	�jB	�qB	�jB	�wB	��B	��B	��B	��B	��B	��B	��B	�}B	ŢB	ȴB	ǮB	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�5B	�HB	�;B	�;B	�;B	�;B	�NB	�TB	�ZB	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B
DB
JB
JB
JB
VB
VB
VB
VB
\B
hB
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
pB
+�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B�B�`B�BB	B	'�B	=qB	e`B	�B
B
}�B
��B
��B
�=B
�oB
�B
�dBPB\)BgmB�B��B��B��B�B�7Bn�BhsBYBL�BM�BN�BO�BO�BP�BR�B[#B_;BcTBl�Bp�Bu�Bv�By�B�%B�Bu�Bt�B�B�=B�uB��B��BÖBȴB�/BB{B�B$�B,B6FBA�BP�BjB|�B�uB�bB�+B�B�+B?}B�B�sB�;B��B��B�VB�DB�7B�Bv�Bo�B�\B�'B�?B�-B�B�\B`BBM�B7LBPB
�B
��B
�B
l�B
dZB
N�B
F�B
-B	��B	B	��B	|�B	gmB	XB	1'B��B�;B��BƨB�B�TB�sB�B��B	\B��B�TB��BƨB�}B�LB�-B�FB�9B�3B�3B�3B�-B�'B�!B�!B�XB�jB��BȴB��B��B��B�
B�B�B�;B�5B�/B��B�RB�3B�?B�wB�XB�FB�LB�qB�wB��B��B�
B�B�B�B�5B�HB�HB�;B�)B�)B�BB�NB�ZB�`B�fB�`B�fB�B�B�B�B�B�B�B�B��B��B��B�B��B��B��B��B��B	1B	�B	�B	!�B	!�B	$�B	(�B	8RB	e`B	gmB	l�B	~�B	�B	�JB	�oB	�bB	�JB	�PB	�VB	u�B	l�B	_;B	YB	P�B	P�B	YB	[#B	ZB	VB	O�B	W
B	p�B	~�B	�B	�%B	�JB	�JB	�JB	�=B	�1B	�+B	�7B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�^B	�^B	�dB	�jB	�qB	�jB	�wB	��B	��B	��B	��B	��B	��B	��B	�}B	ŢB	ȴB	ǮB	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�5B	�HB	�;B	�;B	�;B	�;B	�NB	�TB	�ZB	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B
DB
JB
JB
JB
VB
VB
VB
VB
\B
hB
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
pB
+�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190601                              AO  ARCAADJP                                                                    20181005190601    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190601  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190601  QCF$                G�O�G�O�G�O�8000            