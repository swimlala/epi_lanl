CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:02Z AOML 3.0 creation; 2016-08-07T21:17:32Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221302  20160807141732  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_023                   2C  D   APEX                            6487                            072314                          846 @�-͢�@1   @�-�33/�@-I�^5?}�c�Ƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dys3D��D�C3D�� D���D��D�C3D�� D�ٚD�	�D�P D�� DǼ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33A	��A)��AI��Ai��A���A���A���A���A���A���A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�  B�33B�ffB�ffB�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
  D
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD��D ,�D �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4� D5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG�fDH&fDH�fDI&fDI�fDJ&fDJ��DK&fDK�fDL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ&fDQ�fDR&fDR�fDS&fDS�fDT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDt�3Dy��D�,�D�VfD��3D�� D�  D�VfD��3D���D��D�c3D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A��yA��HA��AӬA�hsA�S�A�M�A�K�A�G�A�C�A�A�A�?}A�=qA�;dA�9XA�7LA�;dA�9XA�A�AӅAӃA�=qA�  A��#A���A���A�ȴAң�A҅A҉7AҾwA�A��A���A��A��A���A�ȴAҲ-Aң�Aҏ\A˙�AēuA�VA�oA�E�A��-A���A�1'A���A�  A�9XA�/A��yA��A�l�A��A��HA��#A�9XA�-A��A��!A��A�x�A��DA�bA�A�p�A�I�A�Q�A���A���A~��Au�FAml�Ah��Afr�Adn�A_ƨAV(�AN�AM�-AM`BALr�AI�#AGhsAD�`A@��A>ĜA=��A=
=A<�A:jA8�A7A5ƨA4~�A2I�A1+A/�A.E�A-A,1A+%A*r�A)�TA)l�A(��A'�;A&��A&=qA#�A#�A"�9A"�A!dZA �A�DA�mAt�AbNAp�A�An�A��A/A�A�!A�`A�hA%A�
A��A^5A��A
M�AA(�A��A��A`BA7LA��A�PA =q@��y@��;@��-@�j@��@��-@�n�@�@�@���@�;d@�l�@��P@�j@��@�z�@�z�@��/@�z�@�b@��;@��@��;@�33@�
=@�\)@���@�ff@� �@�-@�@��F@�D@�M�@�@�v�@��^@�p�@�?}@�7L@홚@�7@��
@�33@�J@�@陚@�R@�dZ@�
=@�~�@�$�@�x�@�Ĝ@�j@�A�@�I�@�Q�@�1@��@��;@��
@�S�@�!@�V@�V@�@�7@�p�@�@�p�@��m@�|�@�^5@�5?@ᙚ@�@�X@�/@���@�  @��;@��@��;@ߍP@�\)@�K�@�"�@�ȴ@ܛ�@�n�@���@ى7@ؼj@��@��y@�{@Ԭ@���@ҟ�@Ӿw@��@�b@ӶF@�t�@�S�@�33@�v�@�@��@���@�&�@���@�r�@�9X@ϝ�@�ȴ@·+@�V@�{@��@��T@��#@���@�G�@̃@�A�@�1@�@ʏ\@�v�@�ff@���@�G�@�Ĝ@�bN@ǶF@ǝ�@Ǖ�@ǅ@�t�@�o@��@Ɨ�@�n�@�E�@�@ŉ7@���@�I�@þw@���@��-@�?}@�Ĝ@�z�@��@�ƨ@�\)@�+@��@���@��@��!@��@��/@�Z@�b@���@��@��!@��+@�$�@���@�hs@�1'@���@�V@��@��^@�p�@�X@�O�@�G�@��@��@�Ĝ@��u@�1@���@�33@���@�ff@�E�@��@��-@�G�@���@�ƨ@���@�|�@�C�@�ȴ@���@�ff@�$�@���@��7@�/@���@��j@�Q�@���@���@�dZ@��@���@��+@�ff@�E�@�@��#@���@�`B@�%@�r�@��;@��@�;d@��y@���@�ȴ@��R@��+@���@�X@�/@��@��j@�A�@�\)@���@���@�V@���@�@��-@�p�@�V@��`@��/@��@��@�z�@�bN@��F@��@��\@�ff@�^5@�V@�V@�$�@�@��@��#@��#@��#@���@���@��-@��h@�G�@��@�V@��@���@��j@��@� �@�1@���@��
@���@��@�5?@���@�G�@�/@��@���@��`@��/@��9@���@�z�@�bN@�Q�@�I�@�A�@�(�@���@�ƨ@�S�@���@�ff@�M�@�{@���@���@�p�@�7L@��@�Ĝ@�z�@�I�@�b@���@��y@��R@�V@�{@���@��@���@��-@��7@�7L@��/@�bN@�9X@���@�"�@��H@���@��!@���@���@���@��\@�ff@���@�?}@��j@vff@m@e@]/@V@N$�@F��@?l�@9�@2��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  A���A��A��yA��HA��AӬA�hsA�S�A�M�A�K�A�G�A�C�A�A�A�?}A�=qA�;dA�9XA�7LA�;dA�9XA�A�AӅAӃA�=qA�  A��#A���A���A�ȴAң�A҅A҉7AҾwA�A��A���A��A��A���A�ȴAҲ-Aң�Aҏ\A˙�AēuA�VA�oA�E�A��-A���A�1'A���A�  A�9XA�/A��yA��A�l�A��A��HA��#A�9XA�-A��A��!A��A�x�A��DA�bA�A�p�A�I�A�Q�A���A���A~��Au�FAml�Ah��Afr�Adn�A_ƨAV(�AN�AM�-AM`BALr�AI�#AGhsAD�`A@��A>ĜA=��A=
=A<�A:jA8�A7A5ƨA4~�A2I�A1+A/�A.E�A-A,1A+%A*r�A)�TA)l�A(��A'�;A&��A&=qA#�A#�A"�9A"�A!dZA �A�DA�mAt�AbNAp�A�An�A��A/A�A�!A�`A�hA%A�
A��A^5A��A
M�AA(�A��A��A`BA7LA��A�PA =q@��y@��;@��-@�j@��@��-@�n�@�@�@���@�;d@�l�@��P@�j@��@�z�@�z�@��/@�z�@�b@��;@��@��;@�33@�
=@�\)@���@�ff@� �@�-@�@��F@�D@�M�@�@�v�@��^@�p�@�?}@�7L@홚@�7@��
@�33@�J@�@陚@�R@�dZ@�
=@�~�@�$�@�x�@�Ĝ@�j@�A�@�I�@�Q�@�1@��@��;@��
@�S�@�!@�V@�V@�@�7@�p�@�@�p�@��m@�|�@�^5@�5?@ᙚ@�@�X@�/@���@�  @��;@��@��;@ߍP@�\)@�K�@�"�@�ȴ@ܛ�@�n�@���@ى7@ؼj@��@��y@�{@Ԭ@���@ҟ�@Ӿw@��@�b@ӶF@�t�@�S�@�33@�v�@�@��@���@�&�@���@�r�@�9X@ϝ�@�ȴ@·+@�V@�{@��@��T@��#@���@�G�@̃@�A�@�1@�@ʏ\@�v�@�ff@���@�G�@�Ĝ@�bN@ǶF@ǝ�@Ǖ�@ǅ@�t�@�o@��@Ɨ�@�n�@�E�@�@ŉ7@���@�I�@þw@���@��-@�?}@�Ĝ@�z�@��@�ƨ@�\)@�+@��@���@��@��!@��@��/@�Z@�b@���@��@��!@��+@�$�@���@�hs@�1'@���@�V@��@��^@�p�@�X@�O�@�G�@��@��@�Ĝ@��u@�1@���@�33@���@�ff@�E�@��@��-@�G�@���@�ƨ@���@�|�@�C�@�ȴ@���@�ff@�$�@���@��7@�/@���@��j@�Q�@���@���@�dZ@��@���@��+@�ff@�E�@�@��#@���@�`B@�%@�r�@��;@��@�;d@��y@���@�ȴ@��R@��+@���@�X@�/@��@��j@�A�@�\)@���@���@�V@���@�@��-@�p�@�V@��`@��/@��@��@�z�@�bN@��F@��@��\@�ff@�^5@�V@�V@�$�@�@��@��#@��#@��#@���@���@��-@��h@�G�@��@�V@��@���@��j@��@� �@�1@���@��
@���@��@�5?@���@�G�@�/@��@���@��`@��/@��9@���@�z�@�bN@�Q�@�I�@�A�@�(�@���@�ƨ@�S�@���@�ff@�M�@�{@���@���@�p�@�7L@��@�Ĝ@�z�@�I�@�b@���@��y@��R@�V@�{@���@��@���@��-@��7@�7L@��/@�bN@�9X@���@�"�@��H@���@��!@���@���@���@��\G�O�@���@�?}@��j@vff@m@e@]/@V@N$�@F��@?l�@9�@2��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oBYBXBXBW
BW
BS�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BR�BR�BT�BW
B`BB	�B
B
�dB �B!�B"�B&�B,B(�B&�B,B8RBN�BI�BH�BL�BS�B`BB\)B^5B_;B_;B?}B_;BdZB�dB��B�B
=B�B&�B�BPB	7B:^BYB7LB�B!�B�BB�)B��B��BJ�B/B%B
�HB
��B
�bB
ffB
T�B
<jB
�B	�B	�B	x�B	\)B	M�B	@�B	"�B��B�BB�;B�5B�B��B��B��B��B��B��B��B��B��BǮBĜB��B�qB�qB�XB�FB�3B�9B�3B�-B�3B�9B�9B�?B�LB�XB�dB��BÖBÖBÖBÖBÖBÖBÖBB��B��B�dB�RB�-B�B�B��B��B��B��B��B��B��B��B��B��B�oB�bB�oB�oB��B��B��B��B��B��B��B��B��B��B�#B	B	B	B	DB	{B	�B	$�B	0!B	9XB	A�B	M�B	Q�B	S�B	VB	aHB	hsB	hsB	k�B	s�B	n�B	r�B	�B	��B	��B	�VB	�B	x�B	m�B	iyB	jB	l�B	n�B	y�B	�1B	�VB	�bB	��B	��B	��B	��B	�LB	ĜB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�HB	�TB	�TB	�TB	�yB	�B	�B	�B	�B	�B	�mB	�mB	�TB	�NB	�;B	�;B	�;B	�BB	�HB	�NB	�sB	�B	�B	�B	�B	�B	�yB	�yB	�`B	�NB	�BB	�5B	�)B	�#B	�B	�B	��B	��B	�B	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
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
PB
PB
PB
VB
VB
VB
VB
PB
VB
VB
VB
PB
PB
JB
DB
DB
DB
DB
PB
PB
VB
\B
\B
bB
bB
hB
hB
hB
bB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
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
"�B
"�B
!�B
!�B
!�B
!�B
!�B
#�B
$�B
/B
49B
=qB
B�B
H�B
N�B
P�B
VB
YB
^5B
bNB
gm1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  BX�BW�BW�BV�BV�BS�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BR�BR�BT�BV�B`B	qB
 �B
�4B �B!�B"�B&�B+�B(�B&�B+�B8#BN�BI�BH�BL�BS�B`B[�B^B_B_B?KB_Bd)B�2B̜B�PB
B�B&�BXBB	B:&BX�B7BsB!�BNB�B��BʉB�NBJ�B.�B�B
�B
͞B
�)B
f2B
T�B
<6B
eB	�RB	��B	x�B	[�B	M�B	@RB	"�B��B�B�B�B��B��B͢BΨBϮBеBϮBΨB̛BʒB�}B�jB�QB�?B�AB�)B�B�B�B�B��B�B�B�B�B�B�&B�2B�QB�bB�aB�cB�aB�cB�bB�bB�\B�VB�QB�1B�B��B��B��B��B��B��B��B�yB��B��B��B��B�^B�9B�.B�<B�:B��B��B��B��B��B�uB�QB�LB�oB��B��B	�B	�B	�B	
B	@B	dB	$�B	/�B	9B	ALB	M�B	Q�B	S�B	U�B	aB	h8B	h6B	kGB	swB	nZB	rpB	��B	�RB	�EB	�B	��B	x�B	mQB	i9B	j@B	lJB	nXB	y�B	��B	�B	�"B	�GB	�MB	�RB	��B	�
B	�ZB	�rB	�pB	�qB	ʁB	̊B	͑B	ΔB	ΕB	ϛB	ӴB	��B	��B	�B	�B	�B	�B	�5B	�;B	�:B	�;B	�BB	�;B	�)B	�)B	�B	�	B	��B	��B	��B	��B	�B	�
B	�/B	�9B	�>B	�CB	�AB	�9B	�6B	�5B	�B	�	B	��B	��B	��B	��B	��B	��B	ԹB	ӴB	��B	�B	�9B	�RB	�ZB	�`B	�^B	�_B	�fB	�gB	�aB	�_B	�]B	�_B	�^B	�]B	�YB	�XB	�[B	�XB	�WB	�XB	�XB	�RB	�RB	�NB	�JB	�EB	�FB	�EB	�GB	�GB	�IB	�KB	�LB	�KB	�LB	�SB	�SB	�QB	�RB	�NB	�SB	�RB	�OB	�SB	�PB	�YB	�XB	�_B	�dB	�eB	�jB	�qB	�pB	�vB	�uB	�~B	�|B	�}B	�{B	�{B	�{B	�zB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

�B

�B

�B

�B

�B
B

B
B

B
B
B
B
B
B
B
B
B
	B
B
B

�B

�B

�B

�B
B
B
B
B
B
B
B
 B
!B
B
B
!B
B
'B
&B
(B
&B
(B
(B
'B
'B
(B
&B
&B
&B
(B
&B
'B
&B
&B
$B
,B
.B
5B
2B
7B
7B
7B
9B
9B
8B
EB
FB
LB
KB
KB
SB
LB
RB
QB
RB
RB
QB
SB
TB
RB
PB
SB
QB
YB
_B
_B
^B
]B
_B
^B
aB
bB
cB
cB
iB
jB
jB
hB
oB
qB
sB
tB
tB
vB
wB
tB
uB
 |B
 {B
 }B
 B
!�B
!�B
"�B
"�B
!�B
!B
!�B
!�B
!�G�O�B
$�B
.�B
3�B
=(B
BDB
HlB
N�B
P�B
U�B
X�B
]�B
bB
g"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.6 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417322016080714173220160807141732  AO  ARCAADJP                                                                    20150226221302    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221302  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221302  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141732  IP                  G�O�G�O�G�O�                