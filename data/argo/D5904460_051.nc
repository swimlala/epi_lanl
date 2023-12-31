CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-22T02:16:04Z AOML 3.0 creation; 2016-08-07T21:17:37Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150522021604  20160807141737  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               3A   AO  5285_8895_051                   2C  D   APEX                            6487                            072314                          846 @�R|�!�1   @�R}K�@@.� ě���c��9Xb1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    3A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDys3D��D�S3D�p D�c3D�  D�<�D�|�D���D���D�<�D��fDǬ�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBl  BrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF�4CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD,�D�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDu  Dy��D�  D�ffD��3D�vfD�3D�P D�� D�� D� D�P D���D�� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӅA�7LA���A��`A��
A���A���A���A�ȴA�ĜA���A���A���AҾwAҼjAҸRAҴ9AҲ-Aҗ�A�5?A�33A�JA���A��A��A��A��yAδ9A�ZA͑hA�&�A�v�A�n�A�`BA�33A�XA�{A���AœuA�Q�A��mA�;dA�+A�ĜA��A��TA��A§�A��-A�^5A�ƨA��RA��/A�
=A��DA�hsA��
A�{A�  A��;A��PA��A��/A�33A� �A�1A�t�A��A�ƨA��A��A�^5A�t�A�XA��TA�ȴA�?}A���A��`A���A���A�5?A�~�A�M�A���A��mA�x�A�S�A�=qA��A��wA}��AyO�Ar�Ao;dAjA�Ah��AdffA]��A[�AZbAX~�AVjAR�+AN��AL�AI��AFI�AC��A?��A<�A:��A9p�A7�TA6�RA4�+A3�PA2��A1��A1l�A1C�A0��A0��A0n�A0M�A0  A/x�A.�A-dZA-dZA-\)A-?}A,�uA+hsA*�DA*{A)�mA)|�A(��A(v�A(bNA(A�A'�-A&��A&A�A%��A%t�A$�A#�A#&�A"��A"��A"�uA"^5A"1A!�^A!�A ĜA �A��Ax�A\)AoA�A�A�A��A1'AA/A��AZA{A�FA7LAE�AJA�A�wA`BA/AA�/A��AM�A��A�A��AI�A�A��A�FA`BA�jAffA�A�-A7LAn�AA�hA`BA�A�DA�AK�A
=A�A%Av�AM�A=qA �A��Ax�AS�AC�A
�`A
v�A
I�A	�mA	��A	�A	S�A	%A�9AE�A�
A"�A�jAv�AM�A�;Ap�A7LA�!A5?A��A��A/A��Az�A�AhsA;dAoA ��A ��A 9X@�t�@��R@��@�X@�bN@�1@��
@�S�@�o@��@���@���@�&�@��@��m@�t�@�"�@�{@�z�@���@��y@�@�@�O�@�@��@�r�@�1'@�l�@�{@�G�@��@�r�@�  @�|�@�"�@�+@�7@蛦@�  @�\)@�ȴ@�hs@��@���@��@���@���@�j@�1'@�C�@�5?@���@�hs@�A�@ۥ�@ڟ�@�J@��@ف@��@ج@�A�@���@ו�@֏\@Ձ@�V@ԋD@�bN@�A�@��@���@�|�@�V@�@�X@��@���@�z�@�t�@���@�ȴ@Ο�@�ff@�-@ͺ^@�x�@���@�Q�@ˮ@�l�@�o@ʗ�@�@�?}@�Ĝ@�Ĝ@ȣ�@�Z@��
@���@Ƨ�@�^5@�$�@ũ�@ř�@�G�@ă@�1@î@�dZ@��H@�~�@�5?@�J@��T@���@��@���@�1'@��F@�o@��!@��#@��@�&�@��@�Q�@��@���@�=q@���@��7@��`@��D@� �@���@�"�@���@�v�@�E�@�J@��7@��`@�j@��@��m@��
@���@�@��H@��+@���@���@��+@�$�@��@��^@�X@�&�@��/@�z�@��@�ƨ@���@�"�@���@���@�^5@�@��-@��@���@��9@�j@� �@��@��@��@���@�n�@�-@���@�&�@��`@��9@�Q�@��@���@��F@�|�@�C�@��@��R@�ff@�$�@��@���@�p�@�?}@��j@�1'@���@�S�@�C�@�o@��@��R@�v�@�^5@�=q@��@��-@��@�p�@�G�@�9X@�(�@���@��F@�\)@��@�5?@��@�x�@��@�Ĝ@��D@�bN@�Z@�A�@��;@��@�t�@�K�@�;d@�S�@�C�@�o@���@�V@��T@���@��D@y�7@o|�@h��@`Ĝ@Y�7@R~�@L�j@E�@<��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 AӅA�7LA���A��`A��
A���A���A���A�ȴA�ĜA���A���A���AҾwAҼjAҸRAҴ9AҲ-Aҗ�A�5?A�33A�JA���A��A��A��A��yAδ9A�ZA͑hA�&�A�v�A�n�A�`BA�33A�XA�{A���AœuA�Q�A��mA�;dA�+A�ĜA��A��TA��A§�A��-A�^5A�ƨA��RA��/A�
=A��DA�hsA��
A�{A�  A��;A��PA��A��/A�33A� �A�1A�t�A��A�ƨA��A��A�^5A�t�A�XA��TA�ȴA�?}A���A��`A���A���A�5?A�~�A�M�A���A��mA�x�A�S�A�=qA��A��wA}��AyO�Ar�Ao;dAjA�Ah��AdffA]��A[�AZbAX~�AVjAR�+AN��AL�AI��AFI�AC��A?��A<�A:��A9p�A7�TA6�RA4�+A3�PA2��A1��A1l�A1C�A0��A0��A0n�A0M�A0  A/x�A.�A-dZA-dZA-\)A-?}A,�uA+hsA*�DA*{A)�mA)|�A(��A(v�A(bNA(A�A'�-A&��A&A�A%��A%t�A$�A#�A#&�A"��A"��A"�uA"^5A"1A!�^A!�A ĜA �A��Ax�A\)AoA�A�A�A��A1'AA/A��AZA{A�FA7LAE�AJA�A�wA`BA/AA�/A��AM�A��A�A��AI�A�A��A�FA`BA�jAffA�A�-A7LAn�AA�hA`BA�A�DA�AK�A
=A�A%Av�AM�A=qA �A��Ax�AS�AC�A
�`A
v�A
I�A	�mA	��A	�A	S�A	%A�9AE�A�
A"�A�jAv�AM�A�;Ap�A7LA�!A5?A��A��A/A��Az�A�AhsA;dAoA ��A ��A 9X@�t�@��R@��@�X@�bN@�1@��
@�S�@�o@��@���@���@�&�@��@��m@�t�@�"�@�{@�z�@���@��y@�@�@�O�@�@��@�r�@�1'@�l�@�{@�G�@��@�r�@�  @�|�@�"�@�+@�7@蛦@�  @�\)@�ȴ@�hs@��@���@��@���@���@�j@�1'@�C�@�5?@���@�hs@�A�@ۥ�@ڟ�@�J@��@ف@��@ج@�A�@���@ו�@֏\@Ձ@�V@ԋD@�bN@�A�@��@���@�|�@�V@�@�X@��@���@�z�@�t�@���@�ȴ@Ο�@�ff@�-@ͺ^@�x�@���@�Q�@ˮ@�l�@�o@ʗ�@�@�?}@�Ĝ@�Ĝ@ȣ�@�Z@��
@���@Ƨ�@�^5@�$�@ũ�@ř�@�G�@ă@�1@î@�dZ@��H@�~�@�5?@�J@��T@���@��@���@�1'@��F@�o@��!@��#@��@�&�@��@�Q�@��@���@�=q@���@��7@��`@��D@� �@���@�"�@���@�v�@�E�@�J@��7@��`@�j@��@��m@��
@���@�@��H@��+@���@���@��+@�$�@��@��^@�X@�&�@��/@�z�@��@�ƨ@���@�"�@���@���@�^5@�@��-@��@���@��9@�j@� �@��@��@��@���@�n�@�-@���@�&�@��`@��9@�Q�@��@���@��F@�|�@�C�@��@��R@�ff@�$�@��@���@�p�@�?}@��j@�1'@���@�S�@�C�@�o@��@��R@�v�@�^5@�=q@��@��-@��@�p�@�G�@�9X@�(�@���@��F@�\)@��@�5?@��@�x�@��@�Ĝ@��D@�bN@�Z@�A�@��;@��@�t�@�K�@�;d@�S�@�C�G�O�@���@�V@��T@���@��D@y�7@o|�@h��@`Ĝ@Y�7@R~�@L�j@E�@<��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	B�B	A�B	A�B	@�B	@�B	@�B	A�B	A�B	A�B	A�B	A�B	A�B	A�B	@�B	A�B	A�B	A�B	@�B	@�B	D�B	O�B	Q�B	R�B	R�B	S�B	T�B	T�B	��B
VB
D�B
e`B-B8RBXBjBv�B�1B�JB�\B��B��B��B��B��B��B��B�FB�FBÖBȴB��B��B�/B�BB�ZB�/B��B��BB�LB��B��B��B�{B�bB�VB�B�+B}�Bv�Bq�BT�B1'BBŢBn�B0!B
�B
��B
�DB
W
B
33B
'�B
"�B
�B
  B	��B	�B	�5B	��B	ĜB	��B	�B	XB	D�B	8RB	-B	bB��B�B��B��B�B�;B��B��B��B��B�B�sB��B	
=B	{B	 �B	(�B	J�B	ZB	k�B	�B	�=B	�VB	��B	��B	��B	��B	��B	�?B	��B	�TB	�TB	�ZB	�mB	��B
+B
VB
oB
uB
�B
#�B
(�B
0!B
5?B
6FB
6FB
6FB
33B
7LB
=qB
;dB
9XB
8RB
9XB
?}B
B�B
B�B
E�B
E�B
E�B
C�B
C�B
E�B
H�B
H�B
G�B
E�B
C�B
B�B
C�B
B�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
>wB
<jB
:^B
9XB
9XB
:^B
:^B
7LB
5?B
6FB
6FB
6FB
8RB
9XB
:^B
:^B
:^B
8RB
9XB
9XB
8RB
6FB
7LB
6FB
6FB
5?B
6FB
5?B
7LB
9XB
;dB
;dB
<jB
<jB
<jB
<jB
;dB
:^B
9XB
8RB
6FB
5?B
49B
2-B
2-B
1'B
0!B
0!B
0!B
/B
.B
,B
+B
+B
)�B
)�B
(�B
'�B
(�B
'�B
&�B
&�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
hB
\B
JB
1B
+B
+B
+B
%B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
  B
B
B
B
B
B
%B
B
B
%B
1B
1B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
hB
hB
hB
hB
bB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
{B
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
,B
2-B
5?B
:^B
A�B
G�B
L�B
P�B
S�B
VB
[#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B	BpB	AjB	AkB	@fB	@dB	@fB	AlB	AlB	AjB	AjB	AjB	AjB	AmB	@dB	AjB	AmB	AjB	@eB	@dB	D~B	O�B	Q�B	R�B	R�B	S�B	T�B	T�B	��B
.B
DsB
e6B,�B8"BW�BjOBv�B��B�B�,B�QB�wB��B��B�yB��B��B�B�B�dBȃBҼBϫB� B�B�'B��B��B͝B�]B�B��B��B�JB�EB�.B�!B��B��B}�Bv�BqtBT�B0�B�B�kBn^B/�B
�OB
��B
�B
V�B
2�B
'�B
"�B
PB	��B	��B	�LB	�B	��B	�gB	��B	��B	W�B	DnB	8B	,�B	4B�B�qB��B��B�qB�B��B̡B˙BзB��B�@B��B	

B	GB	 �B	(�B	J�B	Y�B	kMB	��B	�B	�B	�SB	�kB	�~B	��B	��B	�B	ӺB	�B	�B	�B	�0B	��B
�B
B
.B
8B
hB
#�B
(�B
/�B
5 B
6B
6B
6B
2�B
7B
=2B
;#B
9B
8B
9B
?<B
BPB
BOB
EbB
E_B
EaB
CWB
CUB
EbB
HrB
HsB
GpB
E`B
CUB
BOB
CWB
BPB
AGB
BOB
BMB
BKB
BMB
BNB
AFB
@?B
>5B
<&B
:B
9B
9B
:B
:B
7	B
4�B
6B
6B
6B
8B
9B
:B
:B
:B
8B
9B
9B
8B
6B
7B
6B
6B
4�B
6B
4�B
7
B
9B
;!B
;!B
<'B
<&B
<'B
<'B
;#B
:B
9B
8B
6B
4�B
3�B
1�B
1�B
0�B
/�B
/�B
/�B
.�B
-�B
+�B
*�B
*�B
)�B
)�B
(�B
'�B
(�B
'�B
&�B
&�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
"�B
!�B
 �B
yB
rB
pB
qB
kB
kB
hB
cB
bB
bB
]B
\B
YB
]B
VB
WB
PB
CB
<B
>B
;B
CB
AB
5B
)B
#B
$B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
 �B	��B	��B	��B	��B	��B	�}B	��B	�~B	��B	��B	��B	�B	�xB	�}B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�}B	��B	��B	�|B	�yB	�rB	�pB	�qB	�xB	��B	��B	�B	�oB	�iB	�jB	�kB	�jB	�kB	�qB	�qB	�nB	�oB	�uB	�vB	�|B	�uB	�}B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

�B
 B

�B

�B

�B
B
B
B

B
B
B
	B

B
B
B
B
B
B
B
B
B
B
B
!B
!B
"B
B
"B
B
B
 B
B
"B
!B
(B
(B
&B
.B
/B
.B
-B
/B
/B
-B
3B
3B
4B
<B
8B
:B
1B
;B
:B
?B
AB
?B
AB
2B
-B
-B
-B
/B
2B
3B
2B
9B
BB
@B
TB
KB
MB
SB
XG�O�B
HB
oB
&�B
+�B
1�B
4�B
:B
AAB
GfB
L�B
P�B
S�B
U�B
Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417372016080714173720160807141737  AO  ARCAADJP                                                                    20150522021604    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150522021604  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150522021604  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141737  IP                  G�O�G�O�G�O�                