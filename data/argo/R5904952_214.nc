CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:54Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190554  20181005190554  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��鼮�F1   @���I��6@1WKƧ��c��;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A��A!��A@  A`  A�  A�  A�  A�  A���A�  A�  A���B   B  B  B  B��B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� DfD� D  D� D  D� D  D� D  D� D  D�fD	fD	� D	��D
� DfD�fD  D� D  D� D  D� DfD� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  Dy�D  D� D   D y�D!  D!� D"  D"�fD#  D#� D$  D$� D$��D%� D&  D&� D'fD'�fD(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D0��D1� D2  D2� D3  D3y�D4  D4�fD5  D5� D5��D6� D7  D7� D8  D8y�D8��D9� D9��D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DEfDE� DF  DF� DG  DG� DHfDH� DI  DI�fDJ  DJ� DK  DK� DK��DL� DM  DMy�DN  DN� DO  DO� DP  DP� DP��DQ� DRfDR� DR��DS� DTfDT� DU  DU� DV  DV�fDW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\fD\�fD]  D]� D^  D^� D_fD_� D`fD`� D`��Da� DbfDb�fDc  Dc� DdfDd�fDefDe� De��Dfy�Df��Dg� Dg��Dhy�Dh��Di� DjfDj�fDkfDk� Dl  Dl� DmfDm� Dm��Dn� Do  Do� Dp  Dp�fDqfDq� Dr  Dr� Dr��Ds� Dt  Dt� Du  Du�fDvfDv� Dw  Dw�fDwٚDy~D�&�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @C33@�  @�  A��A%��AD  Ad  A�  A�  A�  A�  A���A�  A�  A���B  B	  B  B  B ��B(��B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B��3B�� BĀ BȀ B̀ B�L�B�L�B؀ B܀ B�� B� B� B� B�� B� B�� B�L�C @ C@ C@ C@ C&fC
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CBY�CD@ CF@ CH@ CJ@ CL@ CNY�CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ CfY�ChY�Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�3C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�,�C�  C�  C�  C�3C�  C�  C�  C�  C�3C�3C�3C�  C�  C�,�C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�3C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D�fD D� DfD� D D� D D� D D� D D� D D�fD	fD	� D
	�D
� DfD�fD D� D D� D D� DfD� D D��D D�fD D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D�fD D� D D��D D� D  D ��D! D!� D" D"�fD# D#� D$ D$� D%	�D%� D& D&� D'fD'�fD( D(� D) D)� D*	�D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0��D1	�D1� D2 D2� D3 D3��D4 D4�fD5 D5� D6	�D6� D7 D7� D8 D8��D9	�D9� D:	�D:� D; D;� D< D<� D= D=�fD> D>� D? D?� D@ D@� DA DA� DB DB� DCfDC� DD DD� DEfDE� DF DF� DG DG� DHfDH� DI DI�fDJ DJ� DK DK� DL	�DL� DM DM��DN DN� DO DO� DP DP� DQ	�DQ� DRfDR� DS	�DS� DTfDT� DU DU� DV DV�fDW DW� DXfDX� DY DY� DZ DZ� D[ D[� D\fD\�fD] D]� D^ D^� D_fD_� D`fD`� Da	�Da� DbfDb�fDc Dc� DdfDd�fDefDe� Df	�Df��Dg	�Dg� Dh	�Dh��Di	�Di� DjfDj�fDkfDk� Dl Dl� DmfDm� Dn	�Dn� Do Do� Dp Dp�fDqfDq� Dr Dr� Ds	�Ds� Dt Dt� Du Du�fDvfDv� Dw Dw�fDw�Dy�D�.�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȑhAȓuAȏ\AȑhAȑhAȏ\AȑhAȑhAȏ\Aȏ\Aȏ\Aȏ\AȋDAȇ+A�v�A�ffA�S�A�=qA�(�A�oA�{A��A��A��A��A��A��A��A��A�VA�A�bA���A���Aǟ�A�n�A�9XA��yA���Aƴ9A�`BA�5?A�?}A�7LA�VA��yA��A���Aź^AŲ-Aũ�Aş�Aţ�Aũ�AžwA���A�(�A�C�A���Aŗ�A�5?Aď\A��A��A���A���A��
A��A�G�A���A�dZA��#A�{A���A��jA��HA�I�A���A��A���A�&�A�1'A�XA��\A��A���A���A�`BA��^A�&�A�`BA��A��#A�C�A� �A�~�A�`BA��^A�ffA�5?A�"�A�x�A��\A�VA��#A�M�A�l�A���A���A�x�A|r�Az��At��AmG�AgƨAc7LAbI�Aa\)A^JAU��AQ�TAO��AOdZANffAKXAF�AE7LAD~�AC��AA�FA@��A?�-A>�yA=��A<�yA;G�A:9XA9"�A8��A8JA6M�A4{A3A1�hA0��A.�jA,��A+�7A*��A)C�A'�-A&ĜA&A$�\A#�TA#dZA"~�A!��A!��A!`BA ��A (�A/A�DA  A�A��A�yA�Ax�A�jA��A~�A��A�
A=qAƨA;dA�jA�RA��AVA��A?}AȴAS�Ar�A�7AC�A	�mA1'AI�A/A{A\)A ��A Z@�K�@��@�$�@�G�@��H@�V@��@��@��H@��y@�bN@@��@���@���@�/@웦@�\)@�
=@��@��y@�@�7L@���@�u@�Q�@��;@�@���@�A�@��m@⟾@��`@��u@�Q�@�C�@��@ݺ^@�O�@ܓu@�l�@�o@ڗ�@��@ٙ�@���@�A�@�K�@֏\@֏\@և+@��T@���@���@�X@ӥ�@�J@Դ9@�bN@��/@�bN@�1'@�5?@�@���@Ь@϶F@��H@�~�@�hs@˾w@ɩ�@��@�r�@�9X@ȃ@ȓu@ȓu@�bN@ȋD@�A�@�dZ@�
=@�M�@��T@ũ�@��y@�t�@ǍP@�;d@�@ŉ7@��`@�t�@�V@�V@���@���@�$�@�~�@��
@� �@�@���@��@��@��#@�O�@��9@���@�X@�b@�l�@��!@�M�@���@�ƨ@�9X@���@�?}@��D@���@�v�@�hs@��D@��
@�;d@��@��+@�v�@�M�@��@��#@��h@��^@��-@��@��7@��@��@���@�b@�ƨ@��@���@��P@�"�@�C�@��m@���@��@��^@�p�@�r�@�  @���@��@�t�@�\)@�S�@�C�@��H@�@���@��7@��@�hs@��@��@� �@�t�@�S�@��@���@���@�n�@�@�G�@��D@� �@��@��;@��w@��@���@��@�;d@�~�@���@���@���@��@��@�|�@�l�@�t�@��F@�ƨ@��w@��@�\)@�S�@�33@��@���@�-@���@���@�x�@�hs@�`B@�O�@�?}@�V@�Ĝ@�bN@��@�b@���@���@�C�@�~�@��@���@�p�@�`B@�hs@�x�@��@�x�@�V@�&�@��@���@�1'@���@�|�@�dZ@�K�@�+@�n�@�@��@��#@�@�hs@�?}@�/@�%@��j@��9@��9@��j@�Ĝ@���@��u@��D@�Q�@�9X@�1'@�ƨ@�dZ@�K�@�;d@�@��+@�~�@�^5@�E�@�{@���@���@��^@���@���@��7@�p�@�G�@���@�z�@�A�@��F@�K�@�
=@��y@��R@���@�n�@�{@�@���@���@�hs@�&�@��@���@�Z@��
@���@�xl@���@m��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AȑhAȓuAȏ\AȑhAȑhAȏ\AȑhAȑhAȏ\Aȏ\Aȏ\Aȏ\AȋDAȇ+A�v�A�ffA�S�A�=qA�(�A�oA�{A��A��A��A��A��A��A��A��A�VA�A�bA���A���Aǟ�A�n�A�9XA��yA���Aƴ9A�`BA�5?A�?}A�7LA�VA��yA��A���Aź^AŲ-Aũ�Aş�Aţ�Aũ�AžwA���A�(�A�C�A���Aŗ�A�5?Aď\A��A��A���A���A��
A��A�G�A���A�dZA��#A�{A���A��jA��HA�I�A���A��A���A�&�A�1'A�XA��\A��A���A���A�`BA��^A�&�A�`BA��A��#A�C�A� �A�~�A�`BA��^A�ffA�5?A�"�A�x�A��\A�VA��#A�M�A�l�A���A���A�x�A|r�Az��At��AmG�AgƨAc7LAbI�Aa\)A^JAU��AQ�TAO��AOdZANffAKXAF�AE7LAD~�AC��AA�FA@��A?�-A>�yA=��A<�yA;G�A:9XA9"�A8��A8JA6M�A4{A3A1�hA0��A.�jA,��A+�7A*��A)C�A'�-A&ĜA&A$�\A#�TA#dZA"~�A!��A!��A!`BA ��A (�A/A�DA  A�A��A�yA�Ax�A�jA��A~�A��A�
A=qAƨA;dA�jA�RA��AVA��A?}AȴAS�Ar�A�7AC�A	�mA1'AI�A/A{A\)A ��A Z@�K�@��@�$�@�G�@��H@�V@��@��@��H@��y@�bN@@��@���@���@�/@웦@�\)@�
=@��@��y@�@�7L@���@�u@�Q�@��;@�@���@�A�@��m@⟾@��`@��u@�Q�@�C�@��@ݺ^@�O�@ܓu@�l�@�o@ڗ�@��@ٙ�@���@�A�@�K�@֏\@֏\@և+@��T@���@���@�X@ӥ�@�J@Դ9@�bN@��/@�bN@�1'@�5?@�@���@Ь@϶F@��H@�~�@�hs@˾w@ɩ�@��@�r�@�9X@ȃ@ȓu@ȓu@�bN@ȋD@�A�@�dZ@�
=@�M�@��T@ũ�@��y@�t�@ǍP@�;d@�@ŉ7@��`@�t�@�V@�V@���@���@�$�@�~�@��
@� �@�@���@��@��@��#@�O�@��9@���@�X@�b@�l�@��!@�M�@���@�ƨ@�9X@���@�?}@��D@���@�v�@�hs@��D@��
@�;d@��@��+@�v�@�M�@��@��#@��h@��^@��-@��@��7@��@��@���@�b@�ƨ@��@���@��P@�"�@�C�@��m@���@��@��^@�p�@�r�@�  @���@��@�t�@�\)@�S�@�C�@��H@�@���@��7@��@�hs@��@��@� �@�t�@�S�@��@���@���@�n�@�@�G�@��D@� �@��@��;@��w@��@���@��@�;d@�~�@���@���@���@��@��@�|�@�l�@�t�@��F@�ƨ@��w@��@�\)@�S�@�33@��@���@�-@���@���@�x�@�hs@�`B@�O�@�?}@�V@�Ĝ@�bN@��@�b@���@���@�C�@�~�@��@���@�p�@�`B@�hs@�x�@��@�x�@�V@�&�@��@���@�1'@���@�|�@�dZ@�K�@�+@�n�@�@��@��#@�@�hs@�?}@�/@�%@��j@��9@��9@��j@�Ĝ@���@��u@��D@�Q�@�9X@�1'@�ƨ@�dZ@�K�@�;d@�@��+@�~�@�^5@�E�@�{@���@���@��^@���@���@��7@�p�@�G�@���@�z�@�A�@��F@�K�@�
=@��y@��R@���@�n�@�{@�@���@���@�hs@�&�@��@���@�Z@��
@���@�xl@���@m��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�yB
�sB
�mB
�mB
�fB
�mB
�sB
�yB
�B
�B
�B
�B
�B
��B
��B
��B
��BB�B@�BN�BW
B\)BYBP�BQ�BT�BZB^5Be`BhsBgmBm�Bs�Bt�Bt�Bu�Bt�Bt�By�B�B�uB�?B�;BB	7B+B0!BaHBl�Bz�Bv�Bo�B]/BF�B8RB:^B<jB>wB>wB>wB6FB/B-B,B(�B'�B)�B%�B!�B�B�B�B�BDBB  B��B��B��B�B�NBƨB�B�oB[#BH�B:^B1'B"�BJB
�B
�RB
�uB
~�B
ffB
5?B	�B	�B	��B	s�B	[#B	F�B	@�B	9XB	%�B	B�B�TB�BB�B��B��B�wB�qB�dB�XB�RB�FB�9B�-B�!B�B�B�B�B��B��B��B��B��B��B��B�B�B��B��B��B�B�B��B�B�B�B�B�!B�!B�!B�!B�!B�B�3B�LB�LB�dB�qB�}B��BBĜB��B��B��B��BɺBȴBȴBȴBȴBǮBǮBƨBǮBƨBƨBĜB�wB�'B��B�Bz�Bt�Bq�Bk�B\)BW
BVB[#Bs�B��B��B�Bx�B�B�\B�bB�B�VB��B��B��B��B�B�!B�-B�dB�}B��B��B��B��BǮB��B��B��B��B��B�B�`B�mB�B�B�B�B��B��B��B��B��B��B��B��B��B	B	1B	PB	{B	�B	)�B	)�B	+B	A�B	ZB	aHB	gmB	k�B	gmB	hsB	\)B	T�B	T�B	T�B	S�B	Q�B	O�B	L�B	J�B	L�B	N�B	VB	XB	XB	ZB	^5B	cTB	e`B	gmB	jB	l�B	o�B	z�B	�B	�B	�+B	�+B	�+B	�%B	�B	|�B	z�B	z�B	w�B	w�B	|�B	�+B	�DB	�1B	�B	�%B	�hB	�hB	�hB	�\B	�PB	�7B	�%B	�B	�7B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�LB	�jB	�XB	�LB	�LB	�FB	�dB	�jB	�qB	�qB	�wB	�wB	�wB	�qB	�wB	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�
B	�B	�B	�#B	�/B	�/B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�`B	�fB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
+B

=B

=B

=B

=B

=B

=B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
bB
bB
hB
hB
�B
B
�B
-�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�yB
�sB
�mB
�mB
�fB
�mB
�sB
�yB
�B
�B
�B
�B
�B
��B
��B
��B
��BB�B@�BN�BW
B\)BYBP�BQ�BT�BZB^5Be`BhsBgmBm�Bs�Bt�Bt�Bu�Bt�Bt�By�B�B�uB�?B�;BB	7B+B0!BaHBl�Bz�Bv�Bo�B]/BF�B8RB:^B<jB>wB>wB>wB6FB/B-B,B(�B'�B)�B%�B!�B�B�B�B�BDBB  B��B��B��B�B�NBƨB�B�oB[#BH�B:^B1'B"�BJB
�B
�RB
�uB
~�B
ffB
5?B	�B	�B	��B	s�B	[#B	F�B	@�B	9XB	%�B	B�B�TB�BB�B��B��B�wB�qB�dB�XB�RB�FB�9B�-B�!B�B�B�B�B��B��B��B��B��B��B��B�B�B��B��B��B�B�B��B�B�B�B�B�!B�!B�!B�!B�!B�B�3B�LB�LB�dB�qB�}B��BBĜB��B��B��B��BɺBȴBȴBȴBȴBǮBǮBƨBǮBƨBƨBĜB�wB�'B��B�Bz�Bt�Bq�Bk�B\)BW
BVB[#Bs�B��B��B�Bx�B�B�\B�bB�B�VB��B��B��B��B�B�!B�-B�dB�}B��B��B��B��BǮB��B��B��B��B��B�B�`B�mB�B�B�B�B��B��B��B��B��B��B��B��B��B	B	1B	PB	{B	�B	)�B	)�B	+B	A�B	ZB	aHB	gmB	k�B	gmB	hsB	\)B	T�B	T�B	T�B	S�B	Q�B	O�B	L�B	J�B	L�B	N�B	VB	XB	XB	ZB	^5B	cTB	e`B	gmB	jB	l�B	o�B	z�B	�B	�B	�+B	�+B	�+B	�%B	�B	|�B	z�B	z�B	w�B	w�B	|�B	�+B	�DB	�1B	�B	�%B	�hB	�hB	�hB	�\B	�PB	�7B	�%B	�B	�7B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�LB	�jB	�XB	�LB	�LB	�FB	�dB	�jB	�qB	�qB	�wB	�wB	�wB	�qB	�wB	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�
B	�B	�B	�#B	�/B	�/B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�`B	�fB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
+B

=B

=B

=B

=B

=B

=B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
bB
bB
hB
hB
�B
B
�B
-�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190554                              AO  ARCAADJP                                                                    20181005190554    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190554  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190554  QCF$                G�O�G�O�G�O�8000            