CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:41Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190541  20181005190541  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��黋D�1   @���B^�~@0�I�^5�c�?|�h1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@���@���A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C�  C��C��C�  C��3C��3C��3C��3D   D � DfD� D��Dy�D  D� D  D� D  D� D  D� D  D�fD  Dy�D��D	� D
  D
� D  D� D  D�fDfD�fD  D� D  Dy�D  D� D��Dy�D  D�fD  D� D  Dy�D  D�fD  Dy�D��D� DfD�fD  D� D  D� D  D� D  D� DfD� D  D� D  Dy�D   D � D!  D!� D!��D"� D#  D#y�D$  D$�fD%  D%� D&  D&y�D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+y�D+��D,y�D-  D-� D.  D.y�D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5�fD6fD6� D6��D7y�D7��D8� D9  D9� D:  D:�fD;  D;y�D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB�fDCfDC� DC��DDy�DD��DEy�DF  DF� DG  DG� DH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DL��DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DT��DU� DV  DV� DW  DW� DXfDX� DY  DY�fDZ  DZ� D[  D[y�D\  D\�fD]  D]� D^  D^� D^��D_� D`  D`� D`��Da� DbfDb� Dc  Dc�fDdfDd�fDe  De� De��Df� DgfDg� Dh  Dh� DifDi�fDj  Dj� Dk  Dk� DlfDl�fDmfDm� Dm��Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dw��Dyw�D�?�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@���AffA$  AD  Ad  A�  A�  A�  A�  A�  A�33A�  A�  B  B	  B  B  B!  B)  B1  B9  BA  BIffBQ  BY  BaffBiffBq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�L�B�L�B�� B�� B�� BĀ Bȳ3B̳3BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ CY�C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb&fCd&fCf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ CzY�C|Y�C~Y�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�  C�,�C�,�C�  C�3C�3C�3C�3D  D � DfD� D	�D��D D� D D� D D� D D� D D�fD D��D		�D	� D
 D
� D D� D D�fDfD�fD D� D D��D D� D	�D��D D�fD D� D D��D D�fD D��D	�D� DfD�fD D� D D� D D� D D� DfD� D D� D D��D  D � D! D!� D"	�D"� D# D#��D$ D$�fD% D%� D& D&��D' D'�fD( D(� D) D)� D* D*� D+ D+��D,	�D,��D- D-� D. D.��D/ D/�fD0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5fD5�fD6fD6� D7	�D7��D8	�D8� D9 D9� D: D:�fD; D;��D<	�D<� D= D=� D> D>� D? D?� D@ D@� DA DA��DB DB�fDCfDC� DD	�DD��DE	�DE��DF DF� DG DG� DH DH� DI	�DI� DJ DJ� DK DK� DL DL� DM	�DM�fDN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT��DU	�DU� DV DV� DW DW� DXfDX� DY DY�fDZ DZ� D[ D[��D\ D\�fD] D]� D^ D^� D_	�D_� D` D`� Da	�Da� DbfDb� Dc Dc�fDdfDd�fDe De� Df	�Df� DgfDg� Dh Dh� DifDi�fDj Dj� Dk Dk� DlfDl�fDmfDm� Dn	�Dn��Do Do� Dp Dp� Dq Dq� Dr Dr��Ds Ds� Dt Dt� Du Du� Dv Dv� DwfDw� Dw��Dy��D�G�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�?}A�E�A�G�A�M�A�VA�XA�VA�O�A�XA�ZA�VA�Q�A�O�A�S�A�Q�A�I�A�5?A�"�A�{A�  A���A���A���A���A���A��A��yA��TA��#A�ȴAѴ9A�~�A�\)A�/A�A��;AЋDA�I�A�5?A��A��A���A�VA�  AͅA�`BA�VA��/A��A��A�Q�A� �A���A���A�z�A�JA�VA��A�jA�&�A�A��PA�-A��7A�O�A�C�A���A��+A�"�A�p�A��uA��HA�(�A�"�A��jA���A��A�JA�ƨA�1A�x�A���A���A�&�A�A��A�A�A�ȴA��9A��`A���A�5?A���A�G�A�{A�%A�-A}�Az��AyO�AyS�Ax��As�An�HAk`BAiƨAhM�AgdZAb5?A^9XA[��AY+AUO�AS��AR�jAQt�AP�DANr�AK�AK`BAI��AE�ADn�AC�mAB��AA��A?��A>VA<�9A6�!A4��A3;dA1`BA/K�A-hsA,5?A+K�A)��A(E�A&��A%XA#�7A!p�A�hAjA��A�A�;AffA��A��Av�A�mA�A�
A&�Az�A�A�jAƨA�A��At�A�HA��A�A�yA�wA	G�A�A�A��A�hA�AffAC�A�`A�AQ�AM�A9XAA�^AI�A ��A 1'@���@�ȴ@��@�j@�I�@��!@��-@���@��u@�33@�n�@�?}@�j@�@�x�@�ƨ@��@�x�@���@�1'@�|�@�V@�h@�Z@�ff@�x�@�Z@��;@�dZ@��@��#@��@��u@��@�S�@�o@ޗ�@�-@���@�p�@ܴ9@� �@ۮ@�S�@���@ج@�ƨ@�dZ@�@�E�@��@թ�@�hs@���@�  @Ӆ@��@���@�^5@���@�p�@��/@��@ϥ�@���@�^5@��@���@��#@Ͳ-@�p�@�7L@̛�@�b@˾w@�;d@�o@��H@�ȴ@ʟ�@�~�@��T@�?}@ȼj@�Ĝ@ȓu@�b@���@��;@�S�@�o@Ƈ+@�-@��@ă@�r�@�Z@��@î@ÍP@�t�@��@���@�7L@�X@�G�@�%@���@�Q�@���@���@�@��R@���@�~�@�n�@�E�@���@��@��w@�t�@��\@�-@�@���@���@��@�X@��@���@�Ĝ@��@�33@���@�ff@�$�@��@��7@�/@��j@��@���@�;d@���@��!@���@�@�&�@���@�Q�@�(�@��u@��@�9X@�  @�ƨ@�S�@��H@�ff@��@�V@��@��9@�bN@��@�  @�ƨ@�l�@�;d@�"�@��H@�E�@���@�X@�V@�z�@�I�@�1@�+@�@�=q@���@���@�?}@��`@��D@�I�@���@�@�^5@�@���@��@�7L@�V@��`@�z�@�I�@�A�@�9X@��;@�C�@�;d@���@���@�
=@�o@��@�o@��y@��R@���@��+@�V@���@��T@�hs@�V@�Ĝ@��9@�z�@��F@�t�@�o@�
=@���@���@�=q@���@��@��/@���@���@��D@�A�@�1@�ƨ@���@�K�@�@���@�v�@�V@�-@��T@�`B@�%@�Ĝ@�r�@�A�@��m@���@�K�@��@�@�"�@�o@��H@��y@�ȴ@�E�@�-@���@�@�x�@�X@���@��@�bN@�1@��
@�|�@�K�@�o@��@���@��\@�ff@��T@���@�?}@���@�z�@�Q�@�9X@���@��@�\)@�;d@�+@�33@�;d@��@���@�v�@�ff@��@���@�hs@�7L@��@���@���@�r�@�A�@�  @��w@���@�O@|�@i�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�?}A�E�A�G�A�M�A�VA�XA�VA�O�A�XA�ZA�VA�Q�A�O�A�S�A�Q�A�I�A�5?A�"�A�{A�  A���A���A���A���A���A��A��yA��TA��#A�ȴAѴ9A�~�A�\)A�/A�A��;AЋDA�I�A�5?A��A��A���A�VA�  AͅA�`BA�VA��/A��A��A�Q�A� �A���A���A�z�A�JA�VA��A�jA�&�A�A��PA�-A��7A�O�A�C�A���A��+A�"�A�p�A��uA��HA�(�A�"�A��jA���A��A�JA�ƨA�1A�x�A���A���A�&�A�A��A�A�A�ȴA��9A��`A���A�5?A���A�G�A�{A�%A�-A}�Az��AyO�AyS�Ax��As�An�HAk`BAiƨAhM�AgdZAb5?A^9XA[��AY+AUO�AS��AR�jAQt�AP�DANr�AK�AK`BAI��AE�ADn�AC�mAB��AA��A?��A>VA<�9A6�!A4��A3;dA1`BA/K�A-hsA,5?A+K�A)��A(E�A&��A%XA#�7A!p�A�hAjA��A�A�;AffA��A��Av�A�mA�A�
A&�Az�A�A�jAƨA�A��At�A�HA��A�A�yA�wA	G�A�A�A��A�hA�AffAC�A�`A�AQ�AM�A9XAA�^AI�A ��A 1'@���@�ȴ@��@�j@�I�@��!@��-@���@��u@�33@�n�@�?}@�j@�@�x�@�ƨ@��@�x�@���@�1'@�|�@�V@�h@�Z@�ff@�x�@�Z@��;@�dZ@��@��#@��@��u@��@�S�@�o@ޗ�@�-@���@�p�@ܴ9@� �@ۮ@�S�@���@ج@�ƨ@�dZ@�@�E�@��@թ�@�hs@���@�  @Ӆ@��@���@�^5@���@�p�@��/@��@ϥ�@���@�^5@��@���@��#@Ͳ-@�p�@�7L@̛�@�b@˾w@�;d@�o@��H@�ȴ@ʟ�@�~�@��T@�?}@ȼj@�Ĝ@ȓu@�b@���@��;@�S�@�o@Ƈ+@�-@��@ă@�r�@�Z@��@î@ÍP@�t�@��@���@�7L@�X@�G�@�%@���@�Q�@���@���@�@��R@���@�~�@�n�@�E�@���@��@��w@�t�@��\@�-@�@���@���@��@�X@��@���@�Ĝ@��@�33@���@�ff@�$�@��@��7@�/@��j@��@���@�;d@���@��!@���@�@�&�@���@�Q�@�(�@��u@��@�9X@�  @�ƨ@�S�@��H@�ff@��@�V@��@��9@�bN@��@�  @�ƨ@�l�@�;d@�"�@��H@�E�@���@�X@�V@�z�@�I�@�1@�+@�@�=q@���@���@�?}@��`@��D@�I�@���@�@�^5@�@���@��@�7L@�V@��`@�z�@�I�@�A�@�9X@��;@�C�@�;d@���@���@�
=@�o@��@�o@��y@��R@���@��+@�V@���@��T@�hs@�V@�Ĝ@��9@�z�@��F@�t�@�o@�
=@���@���@�=q@���@��@��/@���@���@��D@�A�@�1@�ƨ@���@�K�@�@���@�v�@�V@�-@��T@�`B@�%@�Ĝ@�r�@�A�@��m@���@�K�@��@�@�"�@�o@��H@��y@�ȴ@�E�@�-@���@�@�x�@�X@���@��@�bN@�1@��
@�|�@�K�@�o@��@���@��\@�ff@��T@���@�?}@���@�z�@�Q�@�9X@���@��@�\)@�;d@�+@�33@�;d@��@���@�v�@�ff@��@���@�hs@�7L@��@���@���@�r�@�A�@�  @��w@���@�O@|�@i�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BC�BC�BC�BC�BC�BC�BB�BB�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BD�BF�BG�BK�BL�BQ�BVBYBbNBk�Br�Bw�By�B��B��B��BŢB�B�BJB�B�B�BoB+BE�BL�BS�BaHBgmBhsBiyBgmBdZBk�Bt�Bu�Bw�Bx�B|�By�Bp�Bl�BS�B33B�BoBBB  B��B�B�ZB�B�-B�B_;B!�B
��B
�mB
p�B
B�B
<jB
33B
�B	��B	�B	�5B	��B	�wB	�'B	�B	��B	��B	�\B	~�B	z�B	q�B	hsB	`BB	J�B	2-B	�B	bB	B��B��B��B�B�yB�NB�;B�B��B��B��B��BǮBB�}B�RB�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B�B�B�3B�FB�XB�^B�}BŢB��B��B��B��BȴBB��B�wB�qB��BB��B�B��B��B�B�5B�/B�BB�`B�fB�fB�yB�B�yB�sB�sB�sB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	  B��B��B��B��B��B��B��B	B	B	1B		7B	PB	VB	bB	oB	oB	oB	hB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	)�B	+B	+B	-B	-B	0!B	1'B	33B	33B	6FB	7LB	9XB	A�B	H�B	L�B	M�B	M�B	P�B	T�B	W
B	W
B	XB	XB	ZB	[#B	[#B	\)B	]/B	^5B	aHB	e`B	ffB	hsB	iyB	k�B	l�B	o�B	p�B	q�B	q�B	q�B	q�B	s�B	v�B	y�B	y�B	}�B	}�B	~�B	~�B	~�B	~�B	}�B	}�B	~�B	� B	�B	�B	�B	�%B	�+B	�+B	�+B	�7B	�=B	�VB	�bB	�bB	�hB	�oB	�uB	�uB	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�RB	�jB	�qB	�wB	�}B	��B	��B	��B	B	��B	��B	��B	��B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�fB	�`B	�fB	�fB	�mB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
PB
PB
bB
<B
'B
3�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BC�BC�BC�BC�BC�BC�BB�BB�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BD�BF�BG�BK�BL�BQ�BVBYBbNBk�Br�Bw�By�B��B��B��BŢB�B�BJB�B�B�BoB+BE�BL�BS�BaHBgmBhsBiyBgmBdZBk�Bt�Bu�Bw�Bx�B|�By�Bp�Bl�BS�B33B�BoBBB  B��B�B�ZB�B�-B�B_;B!�B
��B
�mB
p�B
B�B
<jB
33B
�B	��B	�B	�5B	��B	�wB	�'B	�B	��B	��B	�\B	~�B	z�B	q�B	hsB	`BB	J�B	2-B	�B	bB	B��B��B��B�B�yB�NB�;B�B��B��B��B��BǮBB�}B�RB�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B�B�B�3B�FB�XB�^B�}BŢB��B��B��B��BȴBB��B�wB�qB��BB��B�B��B��B�B�5B�/B�BB�`B�fB�fB�yB�B�yB�sB�sB�sB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	  B��B��B��B��B��B��B��B	B	B	1B		7B	PB	VB	bB	oB	oB	oB	hB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	)�B	+B	+B	-B	-B	0!B	1'B	33B	33B	6FB	7LB	9XB	A�B	H�B	L�B	M�B	M�B	P�B	T�B	W
B	W
B	XB	XB	ZB	[#B	[#B	\)B	]/B	^5B	aHB	e`B	ffB	hsB	iyB	k�B	l�B	o�B	p�B	q�B	q�B	q�B	q�B	s�B	v�B	y�B	y�B	}�B	}�B	~�B	~�B	~�B	~�B	}�B	}�B	~�B	� B	�B	�B	�B	�%B	�+B	�+B	�+B	�7B	�=B	�VB	�bB	�bB	�hB	�oB	�uB	�uB	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�RB	�jB	�qB	�wB	�}B	��B	��B	��B	B	��B	��B	��B	��B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�fB	�`B	�fB	�fB	�mB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
PB
PB
bB
<B
'B
3�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190541                              AO  ARCAADJP                                                                    20181005190541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190541  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190541  QCF$                G�O�G�O�G�O�8000            