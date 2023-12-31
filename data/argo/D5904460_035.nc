CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-27T10:16:30Z AOML 3.0 creation; 2016-08-07T21:17:34Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
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
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150227101630  20160807141734  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               #A   AO  5285_8895_035                   2C  D   APEX                            6487                            072314                          846 @�=��?�1   @�=�W:�@,E�Q��c�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    #A   B   B   @�33@�  A   A   AA��A`  A�  A�  A�  A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD�  D�S3D�|�D�y�D�fD�@ D��fD�� D�3D�I�D�i�DǠ D�Y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�33A	��A)��AK34Ai��A���A���A���A�  A�  A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZ��BbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD� D&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq  Dq�fDr&fDr�fDs&fDs�fDt&fDt��Dy��D�33D�ffD�� D���D�)�D�S3D���D��3D�&fD�\�D�|�Dǳ3D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�t�AӺ^Aӛ�AӍPAӁA�l�A�\)A�VA�O�A���A�1'AЕ�A�v�A�G�A�-A���A�A�oAͣ�A�r�A�{A��/A̼jA̟�A̝�A̴9A̍PA�;dA�A���Aˏ\A�9XA�K�A���A��/A���A�|�A��yAȇ+A�x�A�hsA��
A��TA��AŇ+A�l�A��A�=qA� �A�&�A��TA��uA�bA�;dA���A���A�^5A��A�ffA�
=A�C�A�?}A��yA��`A�-A���A��#A�O�A�bA�z�A�|�A��
A�t�A�x�A�(�A�E�A��RA��A�5?A�?}A��A�33A�1A�A�A�bNA�33A�{A�$�A�;dA�\)A;dAwl�At�DAq�mAp$�Ao�Ak33Ahz�Ac�mA\�`AW�7AQ+AJ�AH�AG�AC�-A?|�A<��A:v�A8r�A7�7A6�uA6  A3�A2-A1G�A0A�A/oA+��A(A�A&�A&�A%+A$�/A#�A#;dA!��A �/A A�Az�A �A�uA
=A��A�A�+A�A�\A�A��A��A�uA��A��A\)A��A�9A�A+A�+Ar�AoA�-A�wA��A��AJA$�A�A�TA"�A �AC�A`BA5?A�^A �A�AA�AdZA��Ar�A$�A�-A�-A��A+An�A�Al�AĜAz�A��A	�FA	hsA	hsA�AffA�/A�AZA9XA�A�FA�A/A��AI�AA   @��@���@�hs@��!@��@��D@��@�9X@���@��!@�r�@�\)@�M�@���@�Q�@�F@�V@@�v�@�$�@��`@�bN@� �@��m@�ƨ@띲@�t�@�o@�ȴ@�ff@�p�@��@�9X@���@�7@��@�@���@��#@��@�j@�Z@�ȴ@��@���@�/@��/@ܴ9@�z�@܃@�1'@��m@ۍP@�+@�~�@�M�@���@���@�1'@���@׾w@�dZ@ָR@�=q@�X@Լj@�I�@�  @Ӆ@�\)@�
=@���@�V@��@�?}@�9X@ύP@�\)@�"�@���@Ώ\@�V@���@��@̋D@��;@�|�@�"�@ʏ\@�J@�@�?}@���@ȃ@�b@Ǯ@�;d@�ȴ@�^5@���@ŉ7@�X@��@�z�@�9X@�  @ÍP@��@§�@�ff@�@���@��h@�G�@�&�@�Ĝ@��w@�l�@�ȴ@��\@���@��!@�V@�J@��#@�7L@�z�@���@�|�@��@�~�@�@��T@��h@�Ĝ@��m@�o@��R@��+@�M�@���@���@���@�1'@��F@�t�@�
=@��@��@�^5@���@�?}@�Ĝ@�bN@��;@���@��@�K�@��H@�^5@�$�@���@��7@���@�1@���@�ff@�5?@��@�@��@�7L@���@�j@���@�C�@�C�@���@�=q@���@���@�Ĝ@�r�@�(�@��@�l�@�K�@��@��H@�n�@�J@���@��@�hs@��`@��@�bN@�b@�  @��@�ƨ@�|�@�+@��@�o@��H@��!@�~�@�$�@��-@��@��@��D@�bN@��@��m@�ƨ@���@�dZ@��@��@��+@��T@��-@��7@�hs@�O�@�?}@��@���@���@�A�@�1@��@���@�dZ@��@���@��+@�n�@�E�@�$�@���@���@��-@���@�`B@�/@��@���@���@�A�@� �@�1@���@���@�K�@��H@��@��\@�5?@�=q@�$�@���@�`B@�`B@�G�@�%@�r�@�A�@�b@��;@��@��H@���@�n�@�5?@��-@��h@�7L@��`@��j@�z�@�9X@��m@�ƨ@���@�hs@�9X@~ff@t�@o|�@h �@_;d@V5?@K�m@D�/@>�R@7
=@1X@+o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�7LA�t�AӺ^Aӛ�AӍPAӁA�l�A�\)A�VA�O�A���A�1'AЕ�A�v�A�G�A�-A���A�A�oAͣ�A�r�A�{A��/A̼jA̟�A̝�A̴9A̍PA�;dA�A���Aˏ\A�9XA�K�A���A��/A���A�|�A��yAȇ+A�x�A�hsA��
A��TA��AŇ+A�l�A��A�=qA� �A�&�A��TA��uA�bA�;dA���A���A�^5A��A�ffA�
=A�C�A�?}A��yA��`A�-A���A��#A�O�A�bA�z�A�|�A��
A�t�A�x�A�(�A�E�A��RA��A�5?A�?}A��A�33A�1A�A�A�bNA�33A�{A�$�A�;dA�\)A;dAwl�At�DAq�mAp$�Ao�Ak33Ahz�Ac�mA\�`AW�7AQ+AJ�AH�AG�AC�-A?|�A<��A:v�A8r�A7�7A6�uA6  A3�A2-A1G�A0A�A/oA+��A(A�A&�A&�A%+A$�/A#�A#;dA!��A �/A A�Az�A �A�uA
=A��A�A�+A�A�\A�A��A��A�uA��A��A\)A��A�9A�A+A�+Ar�AoA�-A�wA��A��AJA$�A�A�TA"�A �AC�A`BA5?A�^A �A�AA�AdZA��Ar�A$�A�-A�-A��A+An�A�Al�AĜAz�A��A	�FA	hsA	hsA�AffA�/A�AZA9XA�A�FA�A/A��AI�AA   @��@���@�hs@��!@��@��D@��@�9X@���@��!@�r�@�\)@�M�@���@�Q�@�F@�V@@�v�@�$�@��`@�bN@� �@��m@�ƨ@띲@�t�@�o@�ȴ@�ff@�p�@��@�9X@���@�7@��@�@���@��#@��@�j@�Z@�ȴ@��@���@�/@��/@ܴ9@�z�@܃@�1'@��m@ۍP@�+@�~�@�M�@���@���@�1'@���@׾w@�dZ@ָR@�=q@�X@Լj@�I�@�  @Ӆ@�\)@�
=@���@�V@��@�?}@�9X@ύP@�\)@�"�@���@Ώ\@�V@���@��@̋D@��;@�|�@�"�@ʏ\@�J@�@�?}@���@ȃ@�b@Ǯ@�;d@�ȴ@�^5@���@ŉ7@�X@��@�z�@�9X@�  @ÍP@��@§�@�ff@�@���@��h@�G�@�&�@�Ĝ@��w@�l�@�ȴ@��\@���@��!@�V@�J@��#@�7L@�z�@���@�|�@��@�~�@�@��T@��h@�Ĝ@��m@�o@��R@��+@�M�@���@���@���@�1'@��F@�t�@�
=@��@��@�^5@���@�?}@�Ĝ@�bN@��;@���@��@�K�@��H@�^5@�$�@���@��7@���@�1@���@�ff@�5?@��@�@��@�7L@���@�j@���@�C�@�C�@���@�=q@���@���@�Ĝ@�r�@�(�@��@�l�@�K�@��@��H@�n�@�J@���@��@�hs@��`@��@�bN@�b@�  @��@�ƨ@�|�@�+@��@�o@��H@��!@�~�@�$�@��-@��@��@��D@�bN@��@��m@�ƨ@���@�dZ@��@��@��+@��T@��-@��7@�hs@�O�@�?}@��@���@���@�A�@�1@��@���@�dZ@��@���@��+@�n�@�E�@�$�@���@���@��-@���@�`B@�/@��@���@���@�A�@� �@�1@���@���@�K�@��H@��@��\@�5?@�=q@�$�@���@�`B@�`B@�G�@�%@�r�@�A�@�b@��;@��@��H@���@�n�@�5?@��-@��h@�7L@��`@��j@�z�@�9X@��m@�ƨG�O�@�hs@�9X@~ff@t�@o|�@h �@_;d@V5?@K�m@D�/@>�R@7
=@1X@+o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	L�B	P�B	Q�B	Q�B	R�B	R�B	Q�B	S�B	ZB	u�B	�fB
�B(�B'�B(�B+B#�B{BhB�B#�B8RBA�BE�BR�Bl�B�B�JB��B�jB�/B�TB�NB1BoBuB�B�B)�B/B/B-B6FBN�BZB[#BT�BJ�BI�BB�BH�B`BBk�Bl�Bo�Bs�Bs�Bs�Bs�Bv�B�+B�PB�oB�hB�\B�bB�\B�PB�+Bz�B^5BG�B>wB�B�mB��B��BcTBC�BP�BffBiyB`BBM�B�B
�ZB
�3B
��B
�B
[#B
K�B
.B
B	�B	�)B	��B	ĜB	�B	��B	y�B	N�B	-B		7B�B�sB�BB��B��B�5B�)B�`B�B�B��B�yB�TB�HB�yB�mB�5B��B��B��B�NB�NB�B��B�B�TB�B�B��B��B�B�`B�ZB��B��B��B	
=B	 �B	1'B	,B	[#B	� B	�{B	�VB	�oB	�JB	�DB	�DB	�bB	��B	�LB	�dB	�wB	�}B	ŢB	��B	��B	ŢB	�qB	�wB	�wB	��B	B	B	�dB	�dB	ĜB	�BB	�5B	�)B	�B	�yB	�B	��B	��B	��B	��B	��B	��B
B	��B	�B	�B	�B	�B	�NB	�B	�B	�;B	�TB	�mB	�yB	�sB	�fB	�mB	�mB	�`B	�/B	��B	��B	ȴB	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�`B	�TB	�BB	�)B	�#B	�5B	�;B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�TB	�ZB	�`B	�ZB	�ZB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
+B
+B
+B
+B
+B
+B
+B
	7B

=B

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
PB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
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
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
&�B
&�B
.B
49B
9XB
?}B
B�B
K�B
O�B
W
B
[#B
^5B
cTB
gmB
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	L�B	P�B	Q�B	Q�B	R�B	R�B	Q�B	S�B	Y�B	u�B	�AB
�}B(�B'�B(�B*�B#�BLB9B�B#�B8$BAWBErBR�Bl[B��B�B��B�9B��B�#B�B�B<B@BOByB)�B.�B.�B,�B6BN�BY�BZ�BT�BJ�BI�BB]BH�B`BkNBlWBojBs�Bs�Bs�Bs�Bv�B��B�B�9B�2B�(B�+B�(B�B��Bz�B]�BGvB>?B{B�3BΠB�{Bc BC\BP�Bf,Bi?B`BM�BiB
� B
��B
�NB
��B
Z�B
K�B
-�B
 �B	�cB	��B	͟B	�iB	��B	�kB	y�B	N�B	,�B		B�TB�CB�B��B��B�B��B�,B�YB�B��B�EB�"B�B�FB�;B�B��B͡BϮB�B�B�cB�B�JB�B�nB�[B��B��B�bB�)B�$B��B��B��B	
B	 �B	0�B	+�B	Z�B	�B	�?B	�B	�1B	�B	�B	�B	�&B	��B	�B	�)B	�:B	�>B	�bB	ХB	̐B	�bB	�3B	�:B	�<B	�JB	�PB	�OB	�(B	�%B	�]B	�B	��B	��B	�=B	�7B	�lB	��B	��B	��B	��B	��B	��B
�B	��B	�VB	�gB	�vB	�PB	�B	��B	��B	��B	�B	�'B	�6B	�1B	�#B	�+B	�+B	� B	��B	ԼB	˄B	�qB	��B	�<B	�SB	�\B	�mB	��B	�kB	�~B	�zB	�oB	�TB	�OB	�FB	�HB	�5B	�HB	�OB	�ZB	�WB	�\B	�TB	�TB	�SB	�UB	�[B	�_B	�`B	�`B	�_B	�WB	�HB	�5B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�8B	�@B	�:B	�:B	�;B	�:B	�9B	�4B	�5B	�/B	�/B	�,B	�/B	�/B	�6B	�4B	�3B	�5B	�1B	�9B	�BB	�?B	�CB	�IB	�FB	�GB	�EB	�LB	�LB	�KB	�JB	�SB	�QB	�TB	�YB	�WB	�YB	�^B	�eB	�eB	�eB	�eB	�iB	�iB	�sB	�pB	�kB	�gB	�hB	�qB	�pB	�wB	�wB	�|B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B
 �B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

�B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
B
B
B
 B
!B
B
(B
(B
(B
-B
+B
.B
-B
.B
,B
/B
0B
.B
0B
:B
>B
EB
CB
DB
DB
BB
EB
DB
CB
LB
QB
WB
TB
`B
cB
jB
oB
oB
nB
pB
vB
vB
uB
tB
 ~B
uB
wB
uB
 {B
 |B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
$�G�O�B
&�B
-�B
3�B
9B
?2B
BFB
K~B
O�B
V�B
Z�B
]�B
cB
g&B
l@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417342016080714173420160807141734  AO  ARCAADJP                                                                    20150227101630    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150227101630  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150227101630  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141734  IP                  G�O�G�O�G�O�                