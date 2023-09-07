CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:37Z creation      
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190537  20181005190537  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)mj��1   @��*��@0��x����c�M���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A~ffA�33A�  A�  A�  A�  A���A�  B   BffBffB  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B���B���B�  B�  B���B���C  C�C�C  C
  C  C�fC�fC  C  C  C  C�C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C��C��C��C�  C��3C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��C�  C�  C�  C��3C��3C��3D y�D  D�fD  D� D  D� D  Dy�D  D� D  D� D  Dy�D��Dy�D��D	y�D
  D
� D  D� D  D� DfD� D��D� D  D� D  D� DfD� D  D� DfD�fD  D�fD  D�fDfD� D  D� D��D� D  D� D  D�fD  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D%��D&� D'  D'y�D'��D(� D)fD)� D*  D*� D*��D+� D,  D,y�D-  D-�fD.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D:  D:� D;  D;y�D<  D<� D=  D=y�D>  D>� D?  D?� D?��D@y�DA  DA� DB  DB� DC  DCy�DD  DD� DEfDE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDKfDK� DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQfDQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV�fDWfDW�fDXfDX� DX��DYy�DY��DZ� D[  D[�fD\fD\�fD]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� DcfDc�fDdfDd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dm��Dny�Do  Do� DpfDp� Dp��Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw` Dyi�D�=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�  A  A$  AD  Ad  A�33A�33A�  A�  A�  A�  A���A�  B  B	ffBffB  B!  B)ffB1ffB9  BA  BI  BQ  BY  Ba  Bi  BqffBy  B�� B�� B�L�B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B��3BĀ BȀ B�L�BЀ BԳ3B؀ B܀ B�� B� B� B�L�B�L�B� B�� B�L�C &fC@ CY�CY�C@ C
@ C@ C&fC&fC@ C@ C@ C@ CY�C@ C@ C @ C"&fC$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF&fCH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ CpY�Cr@ Ct@ Cv@ Cx@ CzY�C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�,�C�,�C�,�C�,�C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�3C�  C�  C�  C�  C�3C�3C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�,�C�  C�3C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�3C�3C�  C�,�C�,�C�,�C�,�C�  C�3C�  C�  C�  C�,�C�,�C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�,�C�  C�,�C�  C�  C�  C�3C�3D 	�D ��D D�fD D� D D� D D��D D� D D� D D��D	�D��D		�D	��D
 D
� D D� D D� DfD� D	�D� D D� D D� DfD� D D� DfD�fD D�fD D�fDfD� D D� D	�D� D D� D D�fD D� D D� D D� D	�D� D D� D  D � D! D!� D" D"� D# D#� D$ D$��D% D%� D&	�D&� D' D'��D(	�D(� D)fD)� D* D*� D+	�D+� D, D,��D- D-�fD. D.� D/ D/��D0 D0� D1 D1� D2 D2� D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D: D:� D; D;��D< D<� D= D=��D> D>� D? D?� D@	�D@��DA DA� DB DB� DC DC��DD DD� DEfDE�fDF DF� DG DG� DH DH� DI DI� DJ DJ�fDKfDK� DL DL� DM DM� DN DN� DO DO�fDP DP� DQfDQ� DR DR� DS DS� DT DT��DU DU� DV DV�fDWfDW�fDXfDX� DY	�DY��DZ	�DZ� D[ D[�fD\fD\�fD] D]� D^ D^�fD_ D_� D` D`� Da Da� Db Db� DcfDc�fDdfDd� De	�De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm	�Dm��Dn	�Dn��Do Do� DpfDp� Dq	�Dq� DrfDr� Ds Ds� Dt Dt� Du Du� Dv Dv� Dw Dwp Dyy�D�Eq1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�z�A҅A�p�A�~�AҍPAҍPAҍPAҏ\Aҏ\AґhAґhAғuAғuAҕ�Aҕ�Aҕ�Aҗ�Aҙ�Aқ�Aқ�Aқ�Aқ�Aқ�Aҝ�Aҝ�Aҝ�Aҟ�Aҡ�Aҡ�Aң�Aң�Aң�Aң�Aң�Aң�Aң�Aҥ�Aҧ�Aҧ�Aҧ�Aҩ�Aҩ�Aҥ�Aҧ�Aҧ�Aҥ�Aҧ�Aҥ�Aқ�Aҕ�Aҗ�AҍPA�hsA�+A�oAѴ9A�bNA���Aͥ�A��A̗�A�\)A��A��
A�VA�r�A�33AöFA���A���A�JA��PA�?}A��^A�
=A��9A�E�A�%A�VA���A��A�p�A��HA�ƨA���A�I�A���A��A���A��FA��yA�ȴA��#A�-A��A��DA���A��`A��A�jA���A�XA��hA���A���A�r�A�JA�9XA�5?A���A���A�{A�`BA�A�bNA�{A�z�A��A}��A{&�Au�An~�Aj��Ac��AbffA^ �AV�ATĜAP~�AM��AM�AJ�9AIG�AF�DAE�AC|�A@��A=��A:��A:1A9�A81'A7�PA6�`A5�mA4��A2�+A1�A0��A/x�A-��A+S�A)��A(=qA'�A'/A&�jA&ZA%��A%"�A#+A!�A 1A$�A��AdZAjA"�A9XAC�A�HA5?Az�AG�A�uA�A��AZA�AoAz�A"�A��AjA(�A�A��A(�A�;A�A��AVA	��A	p�A	"�AVA1Ap�A�RAffA-A�AC�AĜA��A�#A�A ��@��m@�K�@�^5@���@�bN@�ƨ@���@�@�j@�ff@�j@���@���@��/@@�Ĝ@�@��@�ƨ@�7@�7L@�G�@�9@��m@�dZ@�33@�+@�@ᙚ@�j@߮@��@�5?@��@݉7@ܼj@�b@��@�7L@�%@ؼj@�bN@׍P@�5?@�%@�9X@�l�@҇+@�J@��T@�x�@���@Ѓ@�bN@�9X@�1'@� �@϶F@��@�ȴ@�5?@�&�@̴9@�j@��;@�\)@���@ʇ+@�/@�Q�@�"�@�+@�ȴ@�v�@��T@�/@��`@��/@�%@��@���@ģ�@���@�C�@¸R@�-@�@��@�G�@�X@�+@� �@�C�@�l�@��T@��D@��P@�+@��@�v�@���@�7L@�&�@��@���@�I�@�A�@�A�@�I�@��@��
@���@���@�(�@� �@�+@��+@�{@��^@�hs@���@��@��@���@��@�I�@��@���@���@���@�@�G�@��/@��9@�Z@��@���@��@��@�E�@�-@��@�@�p�@���@�A�@���@�33@���@�v�@���@�?}@��@�Ĝ@��@�bN@�1'@�  @��
@��@��@�l�@�C�@�"�@�
=@��y@�v�@�@�hs@�?}@�/@�&�@�/@�&�@��@���@��@�r�@�z�@�Q�@�  @�l�@�\)@��@�~�@�M�@��@��h@��@�(�@���@���@�l�@���@�~�@�E�@�@��@���@�O�@��@�%@��/@���@�j@�(�@���@���@���@��@�dZ@���@���@�M�@�J@���@�hs@�?}@�7L@�&�@��@���@��@���@�j@��@��@�;d@�
=@��@���@�=q@�$�@��^@���@�O�@�/@��`@���@��@�I�@���@��
@���@���@���@�V@�5?@�-@��T@���@�p�@�G�@�/@���@�Q�@��m@���@�l�@�"�@�
=@���@��y@��@��R@���@��+@�^5@�5?@�J@���@�hs@�O�@�7L@�%@��`@��u@�Z@�Q�@�A�@�1'@�(�@��@��@��@��@}0�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�z�A҅A�p�A�~�AҍPAҍPAҍPAҏ\Aҏ\AґhAґhAғuAғuAҕ�Aҕ�Aҕ�Aҗ�Aҙ�Aқ�Aқ�Aқ�Aқ�Aқ�Aҝ�Aҝ�Aҝ�Aҟ�Aҡ�Aҡ�Aң�Aң�Aң�Aң�Aң�Aң�Aң�Aҥ�Aҧ�Aҧ�Aҧ�Aҩ�Aҩ�Aҥ�Aҧ�Aҧ�Aҥ�Aҧ�Aҥ�Aқ�Aҕ�Aҗ�AҍPA�hsA�+A�oAѴ9A�bNA���Aͥ�A��A̗�A�\)A��A��
A�VA�r�A�33AöFA���A���A�JA��PA�?}A��^A�
=A��9A�E�A�%A�VA���A��A�p�A��HA�ƨA���A�I�A���A��A���A��FA��yA�ȴA��#A�-A��A��DA���A��`A��A�jA���A�XA��hA���A���A�r�A�JA�9XA�5?A���A���A�{A�`BA�A�bNA�{A�z�A��A}��A{&�Au�An~�Aj��Ac��AbffA^ �AV�ATĜAP~�AM��AM�AJ�9AIG�AF�DAE�AC|�A@��A=��A:��A:1A9�A81'A7�PA6�`A5�mA4��A2�+A1�A0��A/x�A-��A+S�A)��A(=qA'�A'/A&�jA&ZA%��A%"�A#+A!�A 1A$�A��AdZAjA"�A9XAC�A�HA5?Az�AG�A�uA�A��AZA�AoAz�A"�A��AjA(�A�A��A(�A�;A�A��AVA	��A	p�A	"�AVA1Ap�A�RAffA-A�AC�AĜA��A�#A�A ��@��m@�K�@�^5@���@�bN@�ƨ@���@�@�j@�ff@�j@���@���@��/@@�Ĝ@�@��@�ƨ@�7@�7L@�G�@�9@��m@�dZ@�33@�+@�@ᙚ@�j@߮@��@�5?@��@݉7@ܼj@�b@��@�7L@�%@ؼj@�bN@׍P@�5?@�%@�9X@�l�@҇+@�J@��T@�x�@���@Ѓ@�bN@�9X@�1'@� �@϶F@��@�ȴ@�5?@�&�@̴9@�j@��;@�\)@���@ʇ+@�/@�Q�@�"�@�+@�ȴ@�v�@��T@�/@��`@��/@�%@��@���@ģ�@���@�C�@¸R@�-@�@��@�G�@�X@�+@� �@�C�@�l�@��T@��D@��P@�+@��@�v�@���@�7L@�&�@��@���@�I�@�A�@�A�@�I�@��@��
@���@���@�(�@� �@�+@��+@�{@��^@�hs@���@��@��@���@��@�I�@��@���@���@���@�@�G�@��/@��9@�Z@��@���@��@��@�E�@�-@��@�@�p�@���@�A�@���@�33@���@�v�@���@�?}@��@�Ĝ@��@�bN@�1'@�  @��
@��@��@�l�@�C�@�"�@�
=@��y@�v�@�@�hs@�?}@�/@�&�@�/@�&�@��@���@��@�r�@�z�@�Q�@�  @�l�@�\)@��@�~�@�M�@��@��h@��@�(�@���@���@�l�@���@�~�@�E�@�@��@���@�O�@��@�%@��/@���@�j@�(�@���@���@���@��@�dZ@���@���@�M�@�J@���@�hs@�?}@�7L@�&�@��@���@��@���@�j@��@��@�;d@�
=@��@���@�=q@�$�@��^@���@�O�@�/@��`@���@��@�I�@���@��
@���@���@���@�V@�5?@�-@��T@���@�p�@�G�@�/@���@�Q�@��m@���@�l�@�"�@�
=@���@��y@��@��R@���@��+@�^5@�5?@�J@���@�hs@�O�@�7L@�%@��`@��u@�Z@�Q�@�A�@�1'@�(�@��@��@��@��@}0�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bm�Bm�Bl�Bm�Bm�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bp�Bp�Bp�Bx�B~�B�B�+B�B�B�B�%B�%B�%B}�B~�B�B�uB�XBƨB��B�B+B]/BZBJ�BI�BL�B}�B��B�bB�DB� Bp�BiyB\)BI�B7LB%�B�BDB��B�B�BB�B�qB��B��B��B�1Bk�B$�B1B
��B
�;B
�dB
��B
�{B
l�B
P�B
9XB
+B
�B	��B	�B	��B	�B	��B	w�B	@�B	 �B�B�`BȴB�B��B�DB�B�B� Bz�Bx�Bv�Bu�Bu�Bz�B� B�B�B�+B�1B�7B�PB�uB��B��B��B��B�'B�-B�!B�B�!B�!B�B�B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�XBĜBƨBĜBŢB��B��B�B�B�BB�5B�5B�;B�;B�HB�NB�;B�B�)B�B��B��B��B�B��B��B�B�)B�B�B�fB�TB�ZB�#B��B��B��B��B��B��B�B�)B�)B�HB�fB�fB�B�B�B�B��B��B��B	B	+B	DB	VB	PB	bB	oB	{B	�B	�B	�B	!�B	&�B	'�B	%�B	%�B	$�B	&�B	'�B	(�B	)�B	)�B	)�B	)�B	,B	.B	0!B	2-B	6FB	7LB	7LB	8RB	:^B	<jB	>wB	>wB	@�B	?}B	E�B	G�B	F�B	G�B	J�B	J�B	K�B	N�B	R�B	S�B	S�B	T�B	VB	VB	T�B	S�B	R�B	T�B	XB	[#B	VB	VB	\)B	^5B	]/B	]/B	]/B	^5B	^5B	_;B	bNB	dZB	dZB	dZB	gmB	k�B	k�B	n�B	r�B	u�B	v�B	x�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�DB	�bB	�bB	�oB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�XB	�jB	�jB	�qB	�wB	�}B	��B	��B	��B	B	B	B	ĜB	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�5B	�5B	�BB	�BB	�BB	�;B	�/B	�)B	�)B	�)B	�)B	�5B	�BB	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
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
	7B
	7B
DB
VB
\B
bB
hB
hB
bB
bB
bB
bB
bB
hB
oB
uB
{B
�B
$�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222Bm�Bm�Bl�Bm�Bm�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bp�Bp�Bp�Bx�B~�B�B�+B�B�B�B�%B�%B�%B}�B~�B�B�uB�XBƨB��B�B+B]/BZBJ�BI�BL�B}�B��B�bB�DB� Bp�BiyB\)BI�B7LB%�B�BDB��B�B�BB�B�qB��B��B��B�1Bk�B$�B1B
��B
�;B
�dB
��B
�{B
l�B
P�B
9XB
+B
�B	��B	�B	��B	�B	��B	w�B	@�B	 �B�B�`BȴB�B��B�DB�B�B� Bz�Bx�Bv�Bu�Bu�Bz�B� B�B�B�+B�1B�7B�PB�uB��B��B��B��B�'B�-B�!B�B�!B�!B�B�B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�XBĜBƨBĜBŢB��B��B�B�B�BB�5B�5B�;B�;B�HB�NB�;B�B�)B�B��B��B��B�B��B��B�B�)B�B�B�fB�TB�ZB�#B��B��B��B��B��B��B�B�)B�)B�HB�fB�fB�B�B�B�B��B��B��B	B	+B	DB	VB	PB	bB	oB	{B	�B	�B	�B	!�B	&�B	'�B	%�B	%�B	$�B	&�B	'�B	(�B	)�B	)�B	)�B	)�B	,B	.B	0!B	2-B	6FB	7LB	7LB	8RB	:^B	<jB	>wB	>wB	@�B	?}B	E�B	G�B	F�B	G�B	J�B	J�B	K�B	N�B	R�B	S�B	S�B	T�B	VB	VB	T�B	S�B	R�B	T�B	XB	[#B	VB	VB	\)B	^5B	]/B	]/B	]/B	^5B	^5B	_;B	bNB	dZB	dZB	dZB	gmB	k�B	k�B	n�B	r�B	u�B	v�B	x�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�DB	�bB	�bB	�oB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�XB	�jB	�jB	�qB	�wB	�}B	��B	��B	��B	B	B	B	ĜB	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�5B	�5B	�BB	�BB	�BB	�;B	�/B	�)B	�)B	�)B	�)B	�5B	�BB	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
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
	7B
	7B
DB
VB
\B
bB
hB
hB
bB
bB
bB
bB
bB
hB
oB
uB
{B
�B
$�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190537                              AO  ARCAADJP                                                                    20181005190537    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190537  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190537  QCF$                G�O�G�O�G�O�8000            