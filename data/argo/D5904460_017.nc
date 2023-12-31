CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:55Z AOML 3.0 creation; 2016-08-07T21:17:31Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221255  20160807141731  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_017                   2C  D   APEX                            6487                            072314                          846 @�%�/��1   @�%���@+�-�c��/��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh��Bo��Bx  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�fD�fD�<�D�� D��fD��D�S3D���D��fD�3D�L�D��3Dǖf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @`  @�33@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���B  B
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBk33Br  BzffB�33B�33B�33B�ffB�  B�  B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$� C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB�4CD�4CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%� D&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK��DL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDt�fDy��D�)�D�P D��3D��D�0 D�ffD�� D�ٙD�fD�` D��fDǩ�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�O�A�O�A�I�A�1A�ĜA���A���A�+A��/A�VA��yA��jA�?}A�I�A�K�A�
=A�/A��^A�VA���A���A�r�A��`A�\)A��/A��A���A�ƨA��A��A�/A�ffA�=qA��A�XA��7A��A�/A���A�oA�ZA��FA�I�A���A��TA�{A��PA�v�A�n�A�1'A��RA�z�A�
=A��wA�JA��A|�RA{%Aw�7As�^Ap�An�+AkdZAiG�Ag�;AbĜA]ƨA[�^AXz�AS��AP�DAN1AKAG��AEG�ABZA@�A;�A7+A4�\A3C�A2�!A2{A1"�A0Q�A/�TA0 �A05?A0{A/�
A/�A/7LA.�A.��A.�\A.z�A.jA.M�A.bA-`BA+��A+"�A(��A&{A#G�A" �A!t�A ��AS�AĜAA�A�A  A�A1'A&�A%A��A��AC�AA�A�7AVA��AZAVA��A��At�A;dA�7A|�AO�A7LA�/A5?A��A�`AQ�A
^5A/Av�A��A1A�AƨA�A��A�9A�+A(�A9XA�HA��A��A��AS�A��AAp�A��A �@��FA @�|�@��+@�S�@��A 9XA=qAv�A5?AA �A ��A jA A�@�@�dZ@� �@�?}@�G�@�@��@�@���@�J@���@���@�|�@��/@�@�?}@�  @�^@�1'@@홚@홚@��@�F@�"�@�Q�@�n�@��@��/@���@�?}@�p�@�5?@�@�X@��@��`@���@�u@��@㕁@�v�@��@�%@��@�z�@�bN@� �@�  @�ƨ@ޏ\@�-@���@ݲ-@�x�@��@ܴ9@��m@�o@�@�o@ۮ@��
@�l�@�o@�
=@��y@�ff@���@�@�`B@�1'@�|�@��@ָR@�v�@�J@�O�@��/@Դ9@�9X@���@ӶF@ӕ�@�@ѩ�@Ѓ@��;@ϝ�@�t�@�+@θR@͑h@���@�j@�1@˥�@�K�@���@ʇ+@�-@��@��#@���@�G�@�r�@Ǯ@�+@�o@�
=@��@�ff@�-@��T@őh@�&�@��@ģ�@�1@Õ�@�o@°!@�-@��-@�G�@��/@���@�Z@��@���@���@���@�O�@�&�@��j@���@��@�1@���@��w@��@�"�@��y@���@��+@�$�@�@��@��u@�I�@��w@�K�@�+@�33@��H@��\@��T@�X@��`@��j@�z�@�bN@�  @���@���@�5?@��^@�`B@��@��`@�Ĝ@���@�j@�S�@�
=@�M�@��#@��^@��7@���@�A�@��m@��
@���@�\)@��@��@��H@���@�n�@��@�`B@���@��`@���@���@�bN@�Q�@�ƨ@�+@��+@�M�@�p�@��@���@��u@�Z@� �@��w@�33@���@�n�@��#@�X@�?}@�V@���@�r�@�Z@�1'@�ƨ@�t�@�C�@�ȴ@��+@�^5@�@���@�x�@�/@�%@��/@��j@���@�z�@�b@��
@���@���@�S�@�o@��y@�ȴ@���@�J@���@��@���@���@��D@��@�r�@�j@�Z@�9X@� �@�1@��m@�ƨ@��@���@�\)@�+@�@��@�v�@�M�@�=q@�@�&�@�%@��@���@��@�Z@��F@�K�@�o@���@��\@�M�@��-@�V@���@��@��P@�K�@�ȴ@�Ĝ@�=q@�%@v�y@n�R@g��@\(�@QX@I��@@��@;�@4j@/�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111 A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�O�A�O�A�I�A�1A�ĜA���A���A�+A��/A�VA��yA��jA�?}A�I�A�K�A�
=A�/A��^A�VA���A���A�r�A��`A�\)A��/A��A���A�ƨA��A��A�/A�ffA�=qA��A�XA��7A��A�/A���A�oA�ZA��FA�I�A���A��TA�{A��PA�v�A�n�A�1'A��RA�z�A�
=A��wA�JA��A|�RA{%Aw�7As�^Ap�An�+AkdZAiG�Ag�;AbĜA]ƨA[�^AXz�AS��AP�DAN1AKAG��AEG�ABZA@�A;�A7+A4�\A3C�A2�!A2{A1"�A0Q�A/�TA0 �A05?A0{A/�
A/�A/7LA.�A.��A.�\A.z�A.jA.M�A.bA-`BA+��A+"�A(��A&{A#G�A" �A!t�A ��AS�AĜAA�A�A  A�A1'A&�A%A��A��AC�AA�A�7AVA��AZAVA��A��At�A;dA�7A|�AO�A7LA�/A5?A��A�`AQ�A
^5A/Av�A��A1A�AƨA�A��A�9A�+A(�A9XA�HA��A��A��AS�A��AAp�A��A �@��FA @�|�@��+@�S�@��A 9XA=qAv�A5?AA �A ��A jA A�@�@�dZ@� �@�?}@�G�@�@��@�@���@�J@���@���@�|�@��/@�@�?}@�  @�^@�1'@@홚@홚@��@�F@�"�@�Q�@�n�@��@��/@���@�?}@�p�@�5?@�@�X@��@��`@���@�u@��@㕁@�v�@��@�%@��@�z�@�bN@� �@�  @�ƨ@ޏ\@�-@���@ݲ-@�x�@��@ܴ9@��m@�o@�@�o@ۮ@��
@�l�@�o@�
=@��y@�ff@���@�@�`B@�1'@�|�@��@ָR@�v�@�J@�O�@��/@Դ9@�9X@���@ӶF@ӕ�@�@ѩ�@Ѓ@��;@ϝ�@�t�@�+@θR@͑h@���@�j@�1@˥�@�K�@���@ʇ+@�-@��@��#@���@�G�@�r�@Ǯ@�+@�o@�
=@��@�ff@�-@��T@őh@�&�@��@ģ�@�1@Õ�@�o@°!@�-@��-@�G�@��/@���@�Z@��@���@���@���@�O�@�&�@��j@���@��@�1@���@��w@��@�"�@��y@���@��+@�$�@�@��@��u@�I�@��w@�K�@�+@�33@��H@��\@��T@�X@��`@��j@�z�@�bN@�  @���@���@�5?@��^@�`B@��@��`@�Ĝ@���@�j@�S�@�
=@�M�@��#@��^@��7@���@�A�@��m@��
@���@�\)@��@��@��H@���@�n�@��@�`B@���@��`@���@���@�bN@�Q�@�ƨ@�+@��+@�M�@�p�@��@���@��u@�Z@� �@��w@�33@���@�n�@��#@�X@�?}@�V@���@�r�@�Z@�1'@�ƨ@�t�@�C�@�ȴ@��+@�^5@�@���@�x�@�/@�%@��/@��j@���@�z�@�b@��
@���@���@�S�@�o@��y@�ȴ@���@�J@���@��@���@���@��D@��@�r�@�j@�Z@�9X@� �@�1@��m@�ƨ@��@���@�\)@�+@�@��@�v�@�M�@�=q@�@�&�@�%@��@���@��@�Z@��F@�K�@�o@���@��\@�M�@��-@�V@���@��@��P@�K�G�O�@�Ĝ@�=q@�%@v�y@n�R@g��@\(�@QX@I��@@��@;�@4j@/�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B>wB;dB:^B:^B49B2-B0!B&�B'�B0!B7LBT�B\)BO�BT�BVBYB]/B`BBbNB�VB	7B$�B'�B)�B�BPBB�B�B��BPB�B�LB��B�\Bn�BP�B5?B!�BhB
��B
�B
�5B
ǮB
�!B
��B
��B
�uB
�B
v�B
cTB
W
B
E�B
.B
�B
B	�B	�B	ƨB	�-B	��B	�{B	p�B	Q�B	E�B	5?B	!�B	hB	B��B�B�;B��BɺB��B�jB�^B�XB�^B��B��BŢB��B�NB��B��B��B��B	+B	bB	�B	�B	$�B	-B	2-B	8RB	M�B	dZB	gmB	T�B	D�B	-B	0!B	/B	0!B	/B	0!B	33B	<jB	G�B	_;B	aHB	N�B	@�B	>wB	;dB	/B	�B	
=B	+B	B	B		7B	1B	�B	 �B	 �B	!�B	+B	5?B	7LB	7LB	>wB	A�B	?}B	;dB	:^B	6FB	$�B	!�B	)�B	+B	1'B	/B	-B	-B	8RB	>wB	F�B	S�B	cTB	jB	gmB	cTB	l�B	|�B	z�B	v�B	o�B	gmB	aHB	k�B	jB	iyB	cTB	gmB	��B	�jB	��B	B	�}B	B	ǮB	ŢB	ĜB	�dB	��B	�!B	�XB	�B	��B	��B	��B	�9B	�jB	��B	��B	��B	�TB	�B	�sB	�TB	�)B	�
B	��B	��B	��B	�;B	�sB	�fB	�#B	��B	��B	��B	�
B	�5B	�BB	�sB	�yB	�B	�B	�B	�B	�B	�NB	�)B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�HB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�fB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�sB	�sB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
1B
+B
+B
%B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
%B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
\B
bB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
 �B
0!B
 �B
)�B
1'B
9XB
C�B
G�B
M�B
S�B
XB
^5B
aHB
e`B
hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111 B?PB?OB?OB?PB?PB?OB?PB?PB?PB?PB?QB?OB?MB?OB?OB?PB?QB?PB?PB?PB?QB?PB?PB?QB?MB?OB>IB;6B:1B:/B4B1�B/�B&�B'�B/�B7BT�B[�BO�BT�BU�BX�B\�B`BbB�"B	B$�B'�B)�B[BB�B��B�KB��BB�aB�B��B�#Bn`BP�B5B!�B.B
��B
�GB
��B
�vB
��B
�dB
�OB
�=B
��B
v�B
cB
V�B
EmB
-�B
rB
�B	�UB	��B	�tB	��B	��B	�HB	psB	Q�B	EoB	5B	!�B	9B	�B��B�TB�BѻBɈB�ZB�:B�/B�&B�/B�PB�XB�oB��B�B�B��B��B��B	�B	-B	RB	}B	$�B	,�B	1�B	8B	M�B	d"B	g3B	T�B	DdB	,�B	/�B	.�B	/�B	.�B	/�B	2�B	<2B	GwB	^�B	aB	N�B	@IB	>;B	;,B	.�B	`B	
B	�B	�B	�B	�B	�B	fB	 �B	 �B	!�B	*�B	5B	7B	7B	>;B	AOB	?CB	;)B	:%B	6B	$�B	!�B	)�B	*�B	0�B	.�B	,�B	,�B	8B	>;B	FkB	S�B	cB	j?B	g/B	cB	lLB	|�B	z�B	v�B	oaB	g1B	a	B	kGB	jBB	i:B	cB	g.B	�YB	�*B	�CB	�OB	�;B	�MB	�mB	�aB	�ZB	�%B	��B	��B	�B	��B	�pB	�qB	�XB	��B	�(B	�AB	ΓB	ԻB	�B	�BB	�1B	�B	��B	��B	ѫB	ΘB	УB	��B	�0B	�%B	��B	ӴB	ӲB	ԺB	��B	��B	�B	�/B	�5B	�HB	�AB	�MB	�TB	�NB	�	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�0B	�BB	�dB	�~B	�~B	�}B	�}B	�}B	�wB	�rB	�rB	�gB	�ZB	�MB	�HB	�GB	�EB	�@B	�GB	�FB	�GB	�FB	�AB	�BB	�@B	�:B	�7B	�4B	�5B	�6B	�0B	�0B	�(B	�*B	�.B	�;B	�FB	�EB	�NB	�YB	�XB	�_B	�`B	�]B	�aB	�^B	�gB	�eB	�cB	�cB	�eB	�iB	�qB	�pB	�oB	�rB	�rB	�vB	�}B	�}B	�~B	�|B	�{B	�|B	�}B	�}B	�{B	�|B	�uB	�wB	�qB	�qB	�jB	�gB	�hB	�pB	�qB	�nB	�xB	�~B	�|B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B
B
B
	B

B
B
	B
B
B

B
B
B
B
B
&B
'B
%B
&B
'B
&B
$B
.B
-B
,B
2B
4B
2B
;B
<B
:B
=B
8B
8B
?B
@B
@B
>B
AB
BB
@B
CB
>B
CB
IB
EB
JB
SB
RB
OB
QB
OB
OB
RB
SB
TB
QB
RB
PB
TB
YB
WB
UB
XB
]B
`B
^B
_B
bB
cB
aB
dB
bB
cB
jB
oB
oB
oB
nB
oB
wB
wB
uB
 |B
 {B
 |G�O�B
 }B
)�B
0�B
9B
CKB
GfB
M�B
S�B
W�B
]�B
`�B
eB
h(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417312016080714173120160807141731  AO  ARCAADJP                                                                    20150226221255    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221255  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221255  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141731  IP                  G�O�G�O�G�O�                