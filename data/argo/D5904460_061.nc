CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-13T19:17:22Z AOML 3.0 creation; 2016-08-07T21:17:38Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150713191722  20160807141738  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               =A   AO  5285_8895_061                   2C  D   APEX                            6487                            072314                          846 @�_�����1   @�_�����@+G�z�H�c�9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    =A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�ffB���B���B�  B�  B�33B�  B�  B�  B���B�  B�ffB�ffB�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D��D�FfD�p D�ɚD��D�FfD�� D��fD�	�D�6fD�� DǦfD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B���B�  B�  B�33B�33B�ffB�33B�33B�33B�  B�33B���B���B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD��D&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt��Dy�fD�,�D�Y�D��3D���D�  D�Y�D��3D���D��D�I�D��3Dǹ�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�uA�v�A�=qA�
=A��TA��mA�A���Aߺ^Aߣ�Aߙ�A߉7A߁A�t�A�p�A�p�A�XA�$�A�^5A�;dA���A��A�ƨAۧ�Aۡ�Aۡ�Aۛ�A�bNA�5?A�
=AځA� �A��TA�K�A�(�A�ƨA�?}A͛�A�?}A̩�A���A�|�A�oAƬA���AÑhA���A�VA��A�;dA�-A��DA�O�A���A�ƨA���A���A�5?A�oA�(�A��7A���A�A�A�ȴA��A� �A�G�A��A���A�dZA�E�A���A��HA���A�\)A��PA�Q�A���A��HA��TA�M�A� �A�=qA~JAxAq��Al��AdI�A_ƨA^{A\�A[G�AX�AVv�AShsAP�HAL��AI�FAG`BAD5?AA?}A?`BA> �A;��A9/A65?A4  A3�A1A.=qA-��A*�+A)�#A)��A(ȴA(z�A'��A'��A'A'�-A(1'A(r�A(M�A'��A'�A'ƨA'dZA'
=A&�HA&VA%�A%\)A%&�A%+A$�HA$z�A$A�A$�A$A#�A"��A"�A!��A!�wA ��A�A��A"�A�+A{A�A��A33AQ�AA��A�hA�9A$�AAC�A�Av�A��A��A�mA��Ap�A;dAȴA�A^5A��AS�AA�!A�A(�A�TAp�A�A �AA\)A33A"�A�jAM�A5?A{A�;Al�A"�A%A�A1'A��A�hAXA�A��Ar�AffA1'A��A��A\)A�A
=qA	ƨA	�A	/A�!A9XA1A�AƨAhsA+AA�AA�A��An�A-A��A�AVA��A�+AA�AXA ��A �\A Q�A M�A I�A  �@�o@�%@�A�@�  @��@�;d@��@���@�V@�{@�&�@�b@�F@�@�"�@�\@�@�p�@�?}@��@���@�r�@���@���@�(�@��@�X@��@�(�@�\)@�+@���@��@��@���@旍@�M�@�x�@�j@�w@�E�@�@�G�@��/@���@�  @�C�@ޏ\@���@�G�@ܼj@��
@��y@�n�@ٙ�@�Z@���@�"�@ָR@��#@Ԭ@���@���@љ�@�p�@�&�@�Q�@�;d@�
=@Ώ\@�J@�G�@��@̓u@�r�@�Q�@��@�;d@ʗ�@�=q@ɉ7@���@�(�@���@�;d@�ȴ@�^5@�{@ũ�@���@ă@�1'@��
@Ý�@�S�@��H@��@��-@��h@�G�@���@��D@�  @�"�@���@�{@�p�@�?}@���@�  @��@�|�@�l�@�S�@���@�ff@��-@���@���@��h@�p�@��@��@�  @�dZ@�;d@�@�-@��T@��-@��h@�/@�r�@�A�@���@�C�@��R@��T@�&�@���@�z�@�Z@�9X@� �@��w@�o@�$�@�`B@���@��9@��m@��@�;d@��@��#@���@��@�O�@��@��@��j@�bN@��@��;@�ƨ@��P@��@���@�v�@�5?@�J@��T@�hs@��9@��D@�Z@��m@��@�dZ@��y@���@���@�=q@�J@�`B@���@��j@��u@�Q�@���@��m@��@�o@��y@��R@�E�@�$�@��@�O�@��@��`@�I�@� �@�1@��
@��P@�33@�@��y@�ȴ@���@��+@�ff@�-@���@�hs@��@�7L@�?}@���@��@��D@��D@�Q�@�l�@�;d@�"�@��@�
=@��H@�v�@��@��7@�/@���@���@�I�@�ƨ@�dZ@�;d@�
=@��H@�n�@�{@��T@���@�&�@�%@���@�M�@��-@��D@{dZ@q��@i�@` �@V�R@Nff@H�`@B�@:��@5��@0��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A��A�uA�v�A�=qA�
=A��TA��mA�A���Aߺ^Aߣ�Aߙ�A߉7A߁A�t�A�p�A�p�A�XA�$�A�^5A�;dA���A��A�ƨAۧ�Aۡ�Aۡ�Aۛ�A�bNA�5?A�
=AځA� �A��TA�K�A�(�A�ƨA�?}A͛�A�?}A̩�A���A�|�A�oAƬA���AÑhA���A�VA��A�;dA�-A��DA�O�A���A�ƨA���A���A�5?A�oA�(�A��7A���A�A�A�ȴA��A� �A�G�A��A���A�dZA�E�A���A��HA���A�\)A��PA�Q�A���A��HA��TA�M�A� �A�=qA~JAxAq��Al��AdI�A_ƨA^{A\�A[G�AX�AVv�AShsAP�HAL��AI�FAG`BAD5?AA?}A?`BA> �A;��A9/A65?A4  A3�A1A.=qA-��A*�+A)�#A)��A(ȴA(z�A'��A'��A'A'�-A(1'A(r�A(M�A'��A'�A'ƨA'dZA'
=A&�HA&VA%�A%\)A%&�A%+A$�HA$z�A$A�A$�A$A#�A"��A"�A!��A!�wA ��A�A��A"�A�+A{A�A��A33AQ�AA��A�hA�9A$�AAC�A�Av�A��A��A�mA��Ap�A;dAȴA�A^5A��AS�AA�!A�A(�A�TAp�A�A �AA\)A33A"�A�jAM�A5?A{A�;Al�A"�A%A�A1'A��A�hAXA�A��Ar�AffA1'A��A��A\)A�A
=qA	ƨA	�A	/A�!A9XA1A�AƨAhsA+AA�AA�A��An�A-A��A�AVA��A�+AA�AXA ��A �\A Q�A M�A I�A  �@�o@�%@�A�@�  @��@�;d@��@���@�V@�{@�&�@�b@�F@�@�"�@�\@�@�p�@�?}@��@���@�r�@���@���@�(�@��@�X@��@�(�@�\)@�+@���@��@��@���@旍@�M�@�x�@�j@�w@�E�@�@�G�@��/@���@�  @�C�@ޏ\@���@�G�@ܼj@��
@��y@�n�@ٙ�@�Z@���@�"�@ָR@��#@Ԭ@���@���@љ�@�p�@�&�@�Q�@�;d@�
=@Ώ\@�J@�G�@��@̓u@�r�@�Q�@��@�;d@ʗ�@�=q@ɉ7@���@�(�@���@�;d@�ȴ@�^5@�{@ũ�@���@ă@�1'@��
@Ý�@�S�@��H@��@��-@��h@�G�@���@��D@�  @�"�@���@�{@�p�@�?}@���@�  @��@�|�@�l�@�S�@���@�ff@��-@���@���@��h@�p�@��@��@�  @�dZ@�;d@�@�-@��T@��-@��h@�/@�r�@�A�@���@�C�@��R@��T@�&�@���@�z�@�Z@�9X@� �@��w@�o@�$�@�`B@���@��9@��m@��@�;d@��@��#@���@��@�O�@��@��@��j@�bN@��@��;@�ƨ@��P@��@���@�v�@�5?@�J@��T@�hs@��9@��D@�Z@��m@��@�dZ@��y@���@���@�=q@�J@�`B@���@��j@��u@�Q�@���@��m@��@�o@��y@��R@�E�@�$�@��@�O�@��@��`@�I�@� �@�1@��
@��P@�33@�@��y@�ȴ@���@��+@�ff@�-@���@�hs@��@�7L@�?}@���@��@��D@��D@�Q�@�l�@�;d@�"�@��@�
=@��H@�v�@��@��7@�/@���@���@�I�@�ƨ@�dZ@�;d@�
=@��H@�n�@�{@��T@���@�&�@�%G�O�@�M�@��-@��D@{dZ@q��@i�@` �@V�R@Nff@H�`@B�@:��@5��@0��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	 �B	�B	�B	�B	�B	!�B	+B	)�B	,B	-B	2-B	;dB	@�B	C�B	G�B	I�B	L�B	M�B	L�B	R�B	l�B	t�B	|�B	}�B	~�B	~�B	~�B	~�B	�B	�B	�B	�B	~�B	�B	��B	��B	��B
�bB
�-B
ȴB
�B�B��B��B�ZB)�B2-B
=B�5Bp�BJ�B
��Bk�B�mB"�B7LBo�B]/BT�BR�BC�B�B��B�)B��B�uB�Bs�Be`BW
BO�B=qB!�BB
�mB
ÖB
��B
gmB
8RB
�B

=B
B	��B	�B	ŢB	�{B	e`B	>wB	�B	  B��B�B�B�TB�)B��B��B��B��B��B�B�5B�NB�NB�sB�B�B�B��B	+B	
=B	!�B	�B	!�B	,B	?}B	R�B	aHB	w�B	�DB	��B	��B	�B	�fB	�B	��B
B
B
JB
�B
�B
!�B
!�B
,B
/B
/B
1'B
2-B
2-B
2-B
33B
5?B
6FB
5?B
5?B
7LB
8RB
9XB
:^B
:^B
:^B
:^B
9XB
8RB
8RB
;dB
;dB
:^B
:^B
:^B
;dB
;dB
;dB
9XB
9XB
;dB
>wB
>wB
=qB
;dB
:^B
:^B
:^B
9XB
7LB
9XB
8RB
6FB
5?B
5?B
5?B
5?B
33B
1'B
1'B
1'B
1'B
1'B
33B
33B
49B
33B
2-B
33B
6FB
5?B
49B
49B
49B
33B
1'B
0!B
/B
0!B
1'B
1'B
2-B
2-B
1'B
0!B
/B
/B
.B
-B
-B
-B
,B
+B
)�B
(�B
'�B
&�B
'�B
'�B
&�B
%�B
$�B
$�B
$�B
$�B
#�B
"�B
"�B
!�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
oB
oB
oB
oB
oB
hB
hB
bB
bB
bB
\B
\B
VB
PB

=B
+B
1B
+B
%B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
%B
%B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
DB

=B
	7B

=B

=B

=B

=B

=B
JB
JB
JB
JB
JB
PB
PB
JB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
 �B
"�B
'�B
+B
0!B
5?B
;dB
A�B
G�B
N�B
Q�B
ZB
^5B
aHB
e`B
gm1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	 �B	�B	�B	�B	�B	!�B	*�B	)�B	+�B	,�B	2B	;IB	@iB	CwB	G�B	I�B	L�B	M�B	L�B	R�B	lnB	t�B	|�B	}�B	~�B	~�B	~�B	~�B	��B	��B	��B	��B	~�B	��B	��B	��B	��B
�5B
�B
ȈB
�RBxB�VB�QB�'B)�B1�B

B�BpqBJ�B
��BkPB�5B"�B7BogB\�BT�BR�BC`BqB�B��B��B�=B��Bs�Be'BV�BO�B=:B!�B �B
�:B
�`B
�\B
g7B
8B
gB

B
�B	��B	�SB	�nB	�JB	e-B	>GB	WB��B��B�qB�bB�'B��B��B��BͤBͤBΨB��B�B�B�B�BB�hB�qB�sB��B	�B	
B	!�B	�B	!�B	+�B	?HB	R�B	aB	w�B	�B	��B	ʅB	��B	�(B	�aB	��B
�B
�B
B
HB
aB
!�B
!�B
+�B
.�B
.�B
0�B
1�B
1�B
1�B
2�B
4�B
6B
4�B
4�B
7B
8B
9B
:B
:B
:B
:B
9B
8B
8B
;!B
;!B
:B
:B
:B
;"B
;"B
;$B
9B
9B
;$B
>6B
>7B
=0B
;#B
:B
:B
:B
9B
7
B
9B
8B
6B
4�B
4�B
4�B
4�B
2�B
0�B
0�B
0�B
0�B
0�B
2�B
2�B
3�B
2�B
1�B
2�B
6B
4�B
3�B
3�B
3�B
2�B
0�B
/�B
.�B
/�B
0�B
0�B
1�B
1�B
0�B
/�B
.�B
.�B
-�B
,�B
,�B
,�B
+�B
*�B
)�B
(�B
'�B
&�B
'�B
'�B
&�B
%�B
$�B
$�B
$�B
$�B
#�B
"�B
"�B
!�B
 �B
 �B
 �B
|B
tB
oB
hB
cB
dB
\B
OB
DB
7B
,B
,B
*B
+B
*B
,B
%B
&B
 B
B
B
B
B
B
B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�vB	�pB	�fB	�eB	�eB	�xB	�vB	�qB	�pB	�qB	�kB	�kB	�qB	�jB	�wB	�B	�|B	�}B	�xB	�yB	�xB	�vB	�xB	�wB	�pB	�jB	�nB	�vB	�zB	�wB	�xB	�~B	�uB	�uB	�{B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
	�B
�B
	�B
	�B
	�B
	�B
	�B
B
B
B
 B
B
B
	B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
B
(B
&B
0B
.B
-B
0B
.B
1B
3B
3B
4B
1B
:B
>B
@B
BB
>B
>B
?B
?B
@B
DB
DB
KB
WB
UB
^B
dB
fB
eB
_B
eB
cB
bB
jB
jB
cB
jB
bB
dB
cB
cB
eB
cB
kB
gB
vB
 |B
wB
uB
xB
wB
 ~B
 {B
 ~G�O�B
'�B
*�B
/�B
4�B
;B
AAB
GeB
N�B
Q�B
Y�B
]�B
`�B
eB
g$1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417382016080714173820160807141738  AO  ARCAADJP                                                                    20150713191722    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150713191722  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150713191722  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141738  IP                  G�O�G�O�G�O�                