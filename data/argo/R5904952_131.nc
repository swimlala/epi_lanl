CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:34Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190534  20181005190534  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�JjX1   @��*$��@0��Q��c�p��
=1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�33A�  A�  A���A�  A�  B   B��B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fDfD� D��D� D  D� D  D� D��Dy�D  D� D  D� D  Dy�D	  D	� D
fD
� D  D� D  D� D��Dy�D  D� D��D� D  D� DfD� D  D� D  D�fDfD� D��D� D  D� D  D� D  D� D  Dy�D  D� D  Dy�D  Dy�D  D� D  D� D  D� D   D � D!  D!y�D!��D"� D#fD#� D#��D$� D$��D%y�D%��D&y�D&��D'� D(fD(� D(��D)� D*  D*�fD+  D+� D,  D,� D-  D-� D-��D.y�D/  D/� D0fD0�fD1  D1y�D1��D2y�D2��D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D9��D:y�D;  D;�fD<  D<y�D=  D=� D>  D>y�D>��D?� D?��D@� DA  DAy�DB  DB� DC  DC� DD  DD�fDE  DEy�DF  DF� DG  DG� DH  DH� DH��DI� DJfDJ� DJ��DK� DLfDL� DM  DMy�DM��DN� DO  DO� DPfDP� DP��DQy�DQ��DR� DSfDS�fDTfDT� DU  DU� DV  DV� DWfDW�fDXfDX� DY  DY� DY��DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D_��D`� Da  Da� Db  Db�fDc  Dc� Dc��Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dj��Dk� Dl  Dly�Dl��Dm� DnfDn�fDo  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv�fDw  Dw� Dw�fDy�=D�B�D�}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���AffA"ffABffAbffA�33A�33A�ffA�33A�33A�  A�33A�33B ��B34B34B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�BԀ B�L�B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC�C&fC
@ C@ C&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&@ C(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�fC�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�fC�fC�3C�3C�3C�3C�3C�3C�  C�  C�  C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�  C�  C�  C�3C�3C�3C�3C�fC�3C�3C�fC�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D � D D��D4D��D	�D��D	�D��D4D�4D	�D��D	�D��D	�D�4D		�D	��D
 D
��D	�D��D	�D��D4D�4D	�D��D4D��D	�D��D D��D	�D��D	�D� D D��D4D��D	�D��D	�D��D	�D��D	�D�4D	�D��D	�D�4D	�D�4D	�D��D	�D��D	�D��D 	�D ��D!	�D!�4D"4D"��D# D#��D$4D$��D%4D%�4D&4D&�4D'4D'��D( D(��D)4D)��D*	�D*� D+	�D+��D,	�D,��D-	�D-��D.4D.�4D/	�D/��D0 D0� D1	�D1�4D24D2�4D34D3�4D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D94D9�4D:4D:�4D;	�D;� D<	�D<�4D=	�D=��D>	�D>�4D?4D?��D@4D@��DA	�DA�4DB	�DB��DC	�DC��DD	�DD� DE	�DE�4DF	�DF��DG	�DG��DH	�DH��DI4DI��DJ DJ��DK4DK��DL DL��DM	�DM�4DN4DN��DO	�DO��DP DP��DQ4DQ�4DR4DR��DS DS� DT DT��DU	�DU��DV	�DV��DW DW� DX DX��DY	�DY��DZ4DZ�4D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_ D_��D`4D`��Da	�Da��Db	�Db� Dc	�Dc��Dd4Dd�4De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj�4Dk4Dk��Dl	�Dl�4Dm4Dm��Dn Dn� Do	�Do��Dp	�Dp��Dq	�Dq�4Dr	�Dr��Ds	�Ds��Dt	�Dt��Du	�Du��Dv Dv� Dw	�Dw��Dw� Dy��D�G\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�
=A��A�$�A�"�A�&�A�$�A�"�A�$�A� �A� �A�"�A�"�A�-A�-A�1'A�/A�/A�5?A�1'A�7LA�;dA�I�A�bNA�|�A�x�A�t�A�p�A�jA�bNA�ZA�S�A�S�A�S�A�S�A�K�A�O�A�\)A�t�Aԏ\AԮAԺ^AԼjAԣ�Aԝ�Aԟ�Aԡ�Aԣ�Aԟ�Aԕ�Aԇ+A�x�A�n�A�33A� �A��A�%AӮA�hsA�{Aң�A�bA�1A�+A�A���A���A���A��/A�
=A�dZA�XA�oA�$�A��PA�?}A��HA��A�VA��RA�+A��hA�  A�S�A�ZA�A�
=A�A�A�VA�  A��jA��!A��A��mA���A��A���A�Q�A���A��yA��
A�S�A�n�A��!A�=qA���A��A��A�S�A���A�p�A�=qA���A���A���A���A|�!Az��Aw��Aq��AmC�AjȴAi�Ae�Aa��A_��AZffAV�AT��APĜAOS�AOO�AN9XAK�mAJE�AG��AB9XA@E�A>=qA<I�A:�RA9?}A7�PA6��A5�A4�uA3�-A3A2A�A1
=A.ĜA.ZA-�A-"�A,�DA+�-A+�A*{A)"�A(�A(I�A'��A'+A&��A%�mA%��A$�\A#��A#|�A"�9A!�A!oA ��A��A$�A��A�jAM�AAhsA33A��AZA��A"�A(�A�mA�hA{A�DAG�A��A~�A��A	�A�AE�AJA�A��A��A�AbNAE�AK�A ȴA ff@��w@��H@��!@��T@�p�@�G�@��@�dZ@�@��@�I�@���@��-@���@�V@��+@��@���@�v�@��T@��/@�bN@�o@�ƨ@�;d@�Ĝ@�1'@陚@�I�@�dZ@�7@�(�@�V@��@�1'@�-@��@ڗ�@��T@��@�@�j@ְ!@�$�@�p�@�/@�%@�z�@Ӆ@ҏ\@��@Ѳ-@љ�@љ�@��@�Z@��
@ϕ�@�o@�n�@�{@��@�hs@�?}@��@�j@��m@���@ˮ@�S�@ʏ\@�{@�@ț�@�9X@�ƨ@�+@�o@��y@Ƨ�@�5?@ź^@�&�@��;@�;d@�ff@�n�@�V@���@���@��D@�Z@��u@��9@��9@���@�9X@��;@�^5@�I�@���@�n�@��@�;d@�S�@�t�@���@�@��@��@��@�-@�$�@��h@��@���@�Z@�1'@���@���@�M�@���@�@��@��\@�7L@���@��9@��u@�Z@��j@��P@��w@�ƨ@�S�@�K�@�@��!@�n�@�{@��@���@�V@�Ĝ@��u@��@�A�@�  @���@�t�@���@��R@�V@��-@��@��j@��-@��w@���@�+@�@�`B@�p�@���@�5?@�{@�{@�O�@�r�@��/@�
=@��F@�l�@�dZ@���@��-@�G�@�/@��`@�Ĝ@��/@��/@��9@��u@�1@�-@���@���@���@���@�5?@�V@�$�@�p�@�9X@�b@��F@���@�33@�
=@���@�v�@�^5@�5?@�{@�@�X@�7L@��@��j@�Q�@�Q�@�b@���@���@��@�
=@�ȴ@��\@�E�@���@���@��D@�z�@�r�@�I�@��@�\)@�t�@�t�@�33@�-@�%@�Ĝ@��@�z�@�1@�\)@�S�@�S�@�C�@�C�@��@�r�@�9X@��F@�ff@��-@�x�@���@��h@���@�ȴ@�33@�;d@�ȴ@��^@��7@�V@�I�@���@��@���@��m@��;@��w@��F@�dZ@��R@�`B@�b@��
@��@���@�t�@�S�@�K�@�;d@�;d@�+@��y@��\@�$�@�$�@�-@�$�@�{@���@��@�,�@nTa111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�
=A��A�$�A�"�A�&�A�$�A�"�A�$�A� �A� �A�"�A�"�A�-A�-A�1'A�/A�/A�5?A�1'A�7LA�;dA�I�A�bNA�|�A�x�A�t�A�p�A�jA�bNA�ZA�S�A�S�A�S�A�S�A�K�A�O�A�\)A�t�Aԏ\AԮAԺ^AԼjAԣ�Aԝ�Aԟ�Aԡ�Aԣ�Aԟ�Aԕ�Aԇ+A�x�A�n�A�33A� �A��A�%AӮA�hsA�{Aң�A�bA�1A�+A�A���A���A���A��/A�
=A�dZA�XA�oA�$�A��PA�?}A��HA��A�VA��RA�+A��hA�  A�S�A�ZA�A�
=A�A�A�VA�  A��jA��!A��A��mA���A��A���A�Q�A���A��yA��
A�S�A�n�A��!A�=qA���A��A��A�S�A���A�p�A�=qA���A���A���A���A|�!Az��Aw��Aq��AmC�AjȴAi�Ae�Aa��A_��AZffAV�AT��APĜAOS�AOO�AN9XAK�mAJE�AG��AB9XA@E�A>=qA<I�A:�RA9?}A7�PA6��A5�A4�uA3�-A3A2A�A1
=A.ĜA.ZA-�A-"�A,�DA+�-A+�A*{A)"�A(�A(I�A'��A'+A&��A%�mA%��A$�\A#��A#|�A"�9A!�A!oA ��A��A$�A��A�jAM�AAhsA33A��AZA��A"�A(�A�mA�hA{A�DAG�A��A~�A��A	�A�AE�AJA�A��A��A�AbNAE�AK�A ȴA ff@��w@��H@��!@��T@�p�@�G�@��@�dZ@�@��@�I�@���@��-@���@�V@��+@��@���@�v�@��T@��/@�bN@�o@�ƨ@�;d@�Ĝ@�1'@陚@�I�@�dZ@�7@�(�@�V@��@�1'@�-@��@ڗ�@��T@��@�@�j@ְ!@�$�@�p�@�/@�%@�z�@Ӆ@ҏ\@��@Ѳ-@љ�@љ�@��@�Z@��
@ϕ�@�o@�n�@�{@��@�hs@�?}@��@�j@��m@���@ˮ@�S�@ʏ\@�{@�@ț�@�9X@�ƨ@�+@�o@��y@Ƨ�@�5?@ź^@�&�@��;@�;d@�ff@�n�@�V@���@���@��D@�Z@��u@��9@��9@���@�9X@��;@�^5@�I�@���@�n�@��@�;d@�S�@�t�@���@�@��@��@��@�-@�$�@��h@��@���@�Z@�1'@���@���@�M�@���@�@��@��\@�7L@���@��9@��u@�Z@��j@��P@��w@�ƨ@�S�@�K�@�@��!@�n�@�{@��@���@�V@�Ĝ@��u@��@�A�@�  @���@�t�@���@��R@�V@��-@��@��j@��-@��w@���@�+@�@�`B@�p�@���@�5?@�{@�{@�O�@�r�@��/@�
=@��F@�l�@�dZ@���@��-@�G�@�/@��`@�Ĝ@��/@��/@��9@��u@�1@�-@���@���@���@���@�5?@�V@�$�@�p�@�9X@�b@��F@���@�33@�
=@���@�v�@�^5@�5?@�{@�@�X@�7L@��@��j@�Q�@�Q�@�b@���@���@��@�
=@�ȴ@��\@�E�@���@���@��D@�z�@�r�@�I�@��@�\)@�t�@�t�@�33@�-@�%@�Ĝ@��@�z�@�1@�\)@�S�@�S�@�C�@�C�@��@�r�@�9X@��F@�ff@��-@�x�@���@��h@���@�ȴ@�33@�;d@�ȴ@��^@��7@�V@�I�@���@��@���@��m@��;@��w@��F@�dZ@��R@�`B@�b@��
@��@���@�t�@�S�@�K�@�;d@�;d@�+@��y@��\@�$�@�$�@�-@�$�@�{@���@��@�,�@nTa111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�HB
�HB
�BB
�HB
�HB
�HB
�HB
�BB
�HB
�;B
�;B
�BB
�;B
�NB
�NB
�TB
�TB
�NB
�ZB
�NB
�`B
�mB
�B+B(�BB�BD�BD�BD�BD�BC�BD�BE�BG�BI�BH�BL�BXB`BBjBs�Bx�Bz�B|�B~�B� B� B�B�B�B�B�%B�%B�%B�+B�+B�%B�B~�By�Bs�Bl�BO�B5?BB�B��B��BɺB�TBB'�BL�B~�B�DB�uB�uB�bBt�BdZB]/BYBJ�BF�BI�BR�B[#BVBD�B<jB:^B.B�B�BoBB�B�NB�BƨB��B�Bs�B[#BG�B/B�B  B
��B
�{B
r�B
[#B
M�B
K�B
<jB
uB	�B	�!B	��B	z�B	K�B	&�B	�B	\B	B�`B�B�qB�B��B��B�{B�oB�PB�1B�B|�Bz�Bz�B{�B}�B~�B�B�B�+B�JB�\B�oB�{B��B��B��B��B��B�B�B�3B�?B�^B�}BÖBĜBĜBĜBƨBɺB��B��B��B��B��B��B�
B�BB�HB�NB�TB�ZB�`B�ZB�`B�TB�NB�/B�BɺB�}B�3B�B�B�B�B�B�B�B�B�B�'B�!B�'B�3B�?B�?B�FB�jB�qB�qB�qB�wB�}B�wB�}B�}B�wB��BB��B��BȴB��B�BB�mB�B�B��B��B��B��B��B	  B��B�B�B	  B��B��B�B��B�B�sB�fB�fB�`B�;B�)B�;B�TB�yB��B��B��B��B	  B	B	B	%B	%B	B	B	B	B	%B	B	%B	%B	%B	+B	1B		7B		7B	JB	uB	�B	�B	�B	#�B	$�B	$�B	%�B	-B	/B	-B	,B	,B	/B	0!B	0!B	1'B	33B	6FB	8RB	9XB	9XB	=qB	C�B	H�B	N�B	O�B	P�B	Q�B	VB	YB	ZB	[#B	\)B	ZB	T�B	L�B	L�B	Q�B	\)B	ffB	jB	q�B	s�B	s�B	t�B	w�B	y�B	{�B	|�B	|�B	|�B	}�B	~�B	� B	�B	�B	�B	~�B	}�B	y�B	q�B	p�B	t�B	x�B	{�B	~�B	�B	�B	�1B	�JB	�hB	�oB	�uB	�oB	�oB	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�XB	�LB	�B	�B	�B	�B	�-B	�jB	�}B	�}B	�qB	�^B	�}B	��B	��B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�B	�
B	�B	�)B	�BB	�HB	�TB	�fB	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
1B
DB
JB
DB
	7B
DB
JB
DB

=B

=B
DB
VB
\B
\B
\B
bB
\B
JB
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
+B
+B
1B
1B
1B
	7B
	7B
�B
	7B
�B
-)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
�HB
�HB
�BB
�HB
�HB
�HB
�HB
�BB
�HB
�;B
�;B
�BB
�;B
�NB
�NB
�TB
�TB
�NB
�ZB
�NB
�`B
�mB
�B+B(�BB�BD�BD�BD�BD�BC�BD�BE�BG�BI�BH�BL�BXB`BBjBs�Bx�Bz�B|�B~�B� B� B�B�B�B�B�%B�%B�%B�+B�+B�%B�B~�By�Bs�Bl�BO�B5?BB�B��B��BɺB�TBB'�BL�B~�B�DB�uB�uB�bBt�BdZB]/BYBJ�BF�BI�BR�B[#BVBD�B<jB:^B.B�B�BoBB�B�NB�BƨB��B�Bs�B[#BG�B/B�B  B
��B
�{B
r�B
[#B
M�B
K�B
<jB
uB	�B	�!B	��B	z�B	K�B	&�B	�B	\B	B�`B�B�qB�B��B��B�{B�oB�PB�1B�B|�Bz�Bz�B{�B}�B~�B�B�B�+B�JB�\B�oB�{B��B��B��B��B��B�B�B�3B�?B�^B�}BÖBĜBĜBĜBƨBɺB��B��B��B��B��B��B�
B�BB�HB�NB�TB�ZB�`B�ZB�`B�TB�NB�/B�BɺB�}B�3B�B�B�B�B�B�B�B�B�B�'B�!B�'B�3B�?B�?B�FB�jB�qB�qB�qB�wB�}B�wB�}B�}B�wB��BB��B��BȴB��B�BB�mB�B�B��B��B��B��B��B	  B��B�B�B	  B��B��B�B��B�B�sB�fB�fB�`B�;B�)B�;B�TB�yB��B��B��B��B	  B	B	B	%B	%B	B	B	B	B	%B	B	%B	%B	%B	+B	1B		7B		7B	JB	uB	�B	�B	�B	#�B	$�B	$�B	%�B	-B	/B	-B	,B	,B	/B	0!B	0!B	1'B	33B	6FB	8RB	9XB	9XB	=qB	C�B	H�B	N�B	O�B	P�B	Q�B	VB	YB	ZB	[#B	\)B	ZB	T�B	L�B	L�B	Q�B	\)B	ffB	jB	q�B	s�B	s�B	t�B	w�B	y�B	{�B	|�B	|�B	|�B	}�B	~�B	� B	�B	�B	�B	~�B	}�B	y�B	q�B	p�B	t�B	x�B	{�B	~�B	�B	�B	�1B	�JB	�hB	�oB	�uB	�oB	�oB	�bB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�XB	�LB	�B	�B	�B	�B	�-B	�jB	�}B	�}B	�qB	�^B	�}B	��B	��B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�B	�
B	�B	�)B	�BB	�HB	�TB	�fB	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
1B
DB
JB
DB
	7B
DB
JB
DB

=B

=B
DB
VB
\B
\B
\B
bB
\B
JB
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
+B
+B
1B
1B
1B
	7B
	7B
�B
	7B
�B
-)222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190534                              AO  ARCAADJP                                                                    20181005190534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190534  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190534  QCF$                G�O�G�O�G�O�8000            