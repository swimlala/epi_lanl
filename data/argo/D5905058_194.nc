CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-07T00:37:21Z creation;2019-12-07T00:37:26Z conversion to V3.1;2023-06-29T05:50:32Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191207003721  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_194                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��x��X�1   @��y�I�@7u�8�YK�b��)^�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DQ��DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�3D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @Y��@�  @�33A	��A)��AI��Ai��A���A���A���A���A���A�  A���A���BffB
ffBffBffB"ffB*ffB2ffB:ffBBffBJffBRffBZffBbffBjffBrffBzffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB���B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>�3C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�D &fD �fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD	&fD	�fD
&fD
�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD&fD�fD &fD �fD!&fD!�fD"&fD"�fD#&fD#�fD$&fD$�fD%&fD%�fD&&fD&�fD'&fD'�fD(&fD(�fD)&fD)�fD*&fD*�fD+&fD+�fD,&fD,�fD-&fD-�fD.&fD.�fD/&fD/�fD0&fD0�fD1&fD1�fD2&fD2�fD3&fD3�fD4&fD4�fD5&fD5�fD6&fD6�fD7&fD7�fD8&fD8�fD9&fD9�fD:&fD:�fD;&fD;�fD<&fD<�fD=&fD=�fD>&fD>�fD?&fD?�fD@&fD@�fDA&fDA�fDB&fDB�fDC&fDC�fDD&fDD�fDE&fDE�fDF&fDF�fDG&fDG��DH&fDH�fDI&fDI�fDJ&fDJ�fDK&fDK� DL&fDL�fDM&fDM�fDN&fDN�fDO&fDO�fDP&fDP�fDQ,�DQ�fDR  DR�fDS&fDS� DT&fDT�fDU&fDU�fDV&fDV�fDW&fDW�fDX&fDX�fDY&fDY�fDZ&fDZ�fD[&fD[�fD\&fD\�fD]&fD]�fD^&fD^�fD_&fD_�fD`&fD`�fDa&fDa�fDb&fDb�fDc&fDc�fDd&fDd�fDe&fDe�fDf&fDf�fDg&fDg�fDh&fDh�fDi&fDi�fDj&fDj�fDk&fDk�fDl&fDl�fDm&fDm�fDn&fDn�fDo&fDo�fDp&fDp�fDq&fDq�fDr&fDr�fDs&fDs�fDt&fDt�fDu&fDu�fDv&fDv�fDw&fDw�fDx&fDx�fDy&fDy�fDz&fDz�fD{&fD{�fD|&fD|�fD},�D}�fD~&fD~�fD&fD�fD�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D�� D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�fD�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��fD��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D�� D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D3D��3D�3D�S3DÓ3D��3D�3D�S3Dē3D��3D�3D�S3Dœ3D��3D�3D�S3DƓ3D��3D�3D�S3DǓ3D��3D�fD�S3Dȓ3D��3D�3D�S3Dɓ3D��3D�3D�S3Dʓ3D��3D�3D�S3D˓3D��3D�3D�S3D̓3D��3D�3D�S3D͓3D��3D�3D�S3DΓ3D��3D�3D�S3Dϓ3D��3D�3D�S3DГ3D��3D�3D�S3Dѓ3D��3D�3D�S3Dғ3D��3D�3D�S3Dӓ3D��3D�3D�S3Dԓ3D��3D�3D�S3DՓ3D��3D�3D�S3D֓3D��3D�3D�S3Dד3D��3D�3D�S3Dؓ3D��3D�3D�S3Dٓ3D��3D�3D�S3Dړ3D��3D�3D�S3Dۓ3D��3D�3D�S3Dܓ3D��3D�3D�S3Dݓ3D��3D�3D�S3Dޓ3D��3D�3D�S3Dߓ3D��3D�3D�S3D��3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D�3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�S3D��3D��3D�3D�VfD��fD��3D�fD�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��yA��`A��HA���Aʲ-Aʡ�Aʏ\A�|�A�t�A�r�A�ZA�Q�A�I�A�=qA�33A�-A�-A�+A��A��/A��;A��A��A�"�A�x�A�S�A���A�;dA��A��DA��A���A��mA�-A���A�^5A�l�A��A���A���A�$�A��A�ffA�bA�9XA�x�A��A�
=A�hsA�1A�(�A��`A��A�Q�A�~�A�ĜA�+A���A�JA��A�p�A�jA�/A�?}A��RA��A�`BA���A�bNA��#A�^5A��A�x�A�S�A�hsA��uA��;A��`A��-A��A�K�A�A�z�A�1'A�^5A�ĜA�1A���A�l�A��AO�A{?}Ax��AvjAr��Ap5?An�Ak��Ah-Af�Ac��AbQ�Aa/A^I�A]�A[�mAYC�AWp�AV^5AU��ASx�AO�ANI�AL�HAJ�HAHAE
=AC/AA
=A?��A>v�A=ƨA;�PA9`BA8��A7�A6bNA4�uA2�RA1�wA0ȴA/��A.-A,I�A+�
A+��A+"�A*1A)�hA(n�A'G�A%�-A$^5A#?}A"�A!dZA r�A��A+A�Ax�A/AĜAVAJA��A��A�hA1'A�hA�A�
A
=An�Ap�A��A��AM�A�hAz�A�A�PA%A��A�DA�AhsA
ĜA	�-A�`A�FA+A��Ar�AA�A�A�A+A�uAA33A v�@�C�@���@�(�@��@���@���@��!@�G�@�9X@��@�+@�E�@���@�ƨ@�+@���@���@�w@�-@�%@�E�@�7L@��@��@��@�ƨ@�|�@�K�@���@�&�@� �@�1@۾w@ڧ�@��#@��;@��y@ӥ�@Ѻ^@�/@�Z@�
=@�`B@��
@���@�p�@�V@�;d@�?}@�o@�^5@�V@��D@�9X@�(�@��@��P@�\)@�@��@�-@�J@�G�@��h@�b@��+@��^@�$�@��!@�{@��D@��\@�hs@�p�@���@���@��@��@���@��`@�1'@��
@�K�@�l�@�S�@�o@�b@�j@�1@�K�@�/@���@��@�?}@��9@���@���@�J@�?}@��@���@�;d@�A�@��D@���@��@��P@���@���@�&�@��@�b@�+@�ȴ@�S�@��@�=q@�5?@�=q@�~�@�{@���@��^@���@���@�/@��j@�I�@��@��@���@�|�@�K�@��@��@��\@�M�@�{@��#@���@���@��^@��h@�hs@�bN@�K�@�dZ@�@��R@���@�z�@��
@��@���@���@���@�7L@��@��9@�r�@��@�I�@���@�;d@���@�n�@��7@���@��@��@�
=@�-@��#@���@��#@���@���@���@�@��T@�p�@���@���@�Ĝ@��j@���@��\@��#@���@���@��;@�  @���@�C�@�ȴ@�M�@��+@���@��\@���@�ȴ@��!@���@�^5@���@�G�@��j@�b@�|�@���@��#@���@��@��@��`@��j@�r�@�b@� �@��F@�"�@��R@���@�"�@��@�o@�"�@��\@���@�`B@�/@��@��@�r�@� �@���@�ƨ@��F@��@�dZ@��y@���@���@�~�@�~�@��+@�v�@��@���@���@�hs@�X@�?}@���@�Q�@��@��m@��m@��;@���@��F@�\)@�
=@��!@�V@�E�@�5?@���@��-@�x�@�O�@�/@��@��9@��u@�I�@��@���@��
@��F@���@��@�l�@�K�@�33@�~�@�5?@��@��^@���@�`B@�7L@���@���@��j@�z�@�Q�@�A�@�1'@�b@��@��@�@~ȴ@~@}�h@}p�@}V@|�@|�@|z�@|9X@|�@{�m@{�@{@y��@x�`@xĜ@x��@x�@x�@xr�@xbN@xb@w|�@w+@v�R@vff@v5?@u�-@t�/@t��@t�@tj@t1@s��@so@r��@rn�@rM�@r�@qhs@p�`@pbN@p1'@p1'@pA�@p1'@o�w@ol�@o;d@o�@n�y@n$�@m�-@m?}@l�j@kƨ@k33@j�H@j�!@jM�@i�@ihs@h�`@hbN@h1'@hb@g�;@g�w@g��@g\)@f�y@f�+@f5?@e�@e�@eO�@eV@d��@dZ@c�m@c@bn�@a�@a�7@a�@`��@`r�@_�@_�P@_l�@_K�@_�@^ȴ@^�R@^�R@^��@^{@]/@\�/@\�j@\j@\I�@\(�@\1@[C�@Z�@Z�!@Z�!@Z�\@Z~�@Z^5@Z�@ZJ@Y��@Yx�@Y&�@XĜ@X�@X �@W��@W\)@V��@V��@VE�@U�@U�@U@U�h@U�h@Up�@U?}@T�@T9X@S��@Sƨ@SdZ@R��@R��@R^5@Q��@QG�@Q&�@P��@PĜ@PQ�@O��@O+@N�y@N�+@NV@N$�@M�T@M��@M�@L��@L�j@L��@Lz�@LI�@K��@K�F@Kt�@KS�@KC�@K33@K@J�!@Jn�@J=q@I�#@Ihs@IX@H��@HbN@Hb@G|�@G
=@F��@F5?@E�-@E�@Ep�@Ep�@E`B@E`B@EO�@D�/@Dz�@DZ@DI�@C�
@C��@C�@Ct�@CdZ@C"�@B��@B=q@A�^@A�7@Ax�@AX@A&�@A%@@Ĝ@@b@?�;@?�w@?\)@?K�@?;d@?
=@>�@>��@>@=@=�@=V@<�j@<z�@<(�@;��@;��@;o@:��@:~�@:n�@9��@9��@9x�@9G�@97L@9%@8��@8�`@8�@8A�@81'@8b@7��@7K�@7+@7�@7
=@6v�@6@5��@5p�@5�@4�/@4��@4Z@4�@3�m@3��@3C�@3o@2�!@2^5@1�@1��@1��@1�@0��@0�u@0A�@/�@/��@/l�@/;d@.ȴ@.ff@.E�@.$�@-��@-`B@-�@,�@,Z@,�@+��@+ƨ@+��@+t�@+dZ@+S�@+33@+o@*�@*��@*��@*��@*~�@*n�@*n�@*=q@*-@*�@)��@)��@)�7@)7L@(��@(�9@(bN@(1'@(  @'�@'�P@'|�@'|�@'l�@'\)@';d@&�y@&�@&�R@&��@&��@&ff@&{@%�T@%@%�-@%�h@%V@$��@$�@$��@$�j@$��@$Z@$1@$1@#ƨ@#��@#C�@#o@#@"�@"�H@"��@"^5@"^5@"^5@"�@!�@!�^@!�7@!hs@!7L@!&�@!%@ �`@ Ĝ@ ��@ r�@ bN@ Q�@ A�@  �@��@�P@+@�@�R@ȴ@ȴ@��@v�@ff@$�@{@@�@@O�@�@��@��@z�@Z@I�@I�@9X@(�@ƨ@��@�@t�@dZ@S�@@�!@n�@=q@-@J@��@��@��@hs@X@&�@��@��@Q�@��@��@|�@\)@;d@;d@+@�@�y@�R@v�@V@V@{@��@�-@�@O�@�@��@�/@�@�D@9X@ƨ@��@dZ@C�@"�@@�H@��@~�@M�@=q@�@J@�#@��@�7@X@�@��@Ĝ@�9@�u@A�@  @�;@��@�w@�w@�@�@�@��@�P@|�@+@�R@��@ff@V@E�@$�@{@�@�T@�T@@�h@p�@�@�@�/@�/@�/@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��yA��`A��HA���Aʲ-Aʡ�Aʏ\A�|�A�t�A�r�A�ZA�Q�A�I�A�=qA�33A�-A�-A�+A��A��/A��;A��A��A�"�A�x�A�S�A���A�;dA��A��DA��A���A��mA�-A���A�^5A�l�A��A���A���A�$�A��A�ffA�bA�9XA�x�A��A�
=A�hsA�1A�(�A��`A��A�Q�A�~�A�ĜA�+A���A�JA��A�p�A�jA�/A�?}A��RA��A�`BA���A�bNA��#A�^5A��A�x�A�S�A�hsA��uA��;A��`A��-A��A�K�A�A�z�A�1'A�^5A�ĜA�1A���A�l�A��AO�A{?}Ax��AvjAr��Ap5?An�Ak��Ah-Af�Ac��AbQ�Aa/A^I�A]�A[�mAYC�AWp�AV^5AU��ASx�AO�ANI�AL�HAJ�HAHAE
=AC/AA
=A?��A>v�A=ƨA;�PA9`BA8��A7�A6bNA4�uA2�RA1�wA0ȴA/��A.-A,I�A+�
A+��A+"�A*1A)�hA(n�A'G�A%�-A$^5A#?}A"�A!dZA r�A��A+A�Ax�A/AĜAVAJA��A��A�hA1'A�hA�A�
A
=An�Ap�A��A��AM�A�hAz�A�A�PA%A��A�DA�AhsA
ĜA	�-A�`A�FA+A��Ar�AA�A�A�A+A�uAA33A v�@�C�@���@�(�@��@���@���@��!@�G�@�9X@��@�+@�E�@���@�ƨ@�+@���@���@�w@�-@�%@�E�@�7L@��@��@��@�ƨ@�|�@�K�@���@�&�@� �@�1@۾w@ڧ�@��#@��;@��y@ӥ�@Ѻ^@�/@�Z@�
=@�`B@��
@���@�p�@�V@�;d@�?}@�o@�^5@�V@��D@�9X@�(�@��@��P@�\)@�@��@�-@�J@�G�@��h@�b@��+@��^@�$�@��!@�{@��D@��\@�hs@�p�@���@���@��@��@���@��`@�1'@��
@�K�@�l�@�S�@�o@�b@�j@�1@�K�@�/@���@��@�?}@��9@���@���@�J@�?}@��@���@�;d@�A�@��D@���@��@��P@���@���@�&�@��@�b@�+@�ȴ@�S�@��@�=q@�5?@�=q@�~�@�{@���@��^@���@���@�/@��j@�I�@��@��@���@�|�@�K�@��@��@��\@�M�@�{@��#@���@���@��^@��h@�hs@�bN@�K�@�dZ@�@��R@���@�z�@��
@��@���@���@���@�7L@��@��9@�r�@��@�I�@���@�;d@���@�n�@��7@���@��@��@�
=@�-@��#@���@��#@���@���@���@�@��T@�p�@���@���@�Ĝ@��j@���@��\@��#@���@���@��;@�  @���@�C�@�ȴ@�M�@��+@���@��\@���@�ȴ@��!@���@�^5@���@�G�@��j@�b@�|�@���@��#@���@��@��@��`@��j@�r�@�b@� �@��F@�"�@��R@���@�"�@��@�o@�"�@��\@���@�`B@�/@��@��@�r�@� �@���@�ƨ@��F@��@�dZ@��y@���@���@�~�@�~�@��+@�v�@��@���@���@�hs@�X@�?}@���@�Q�@��@��m@��m@��;@���@��F@�\)@�
=@��!@�V@�E�@�5?@���@��-@�x�@�O�@�/@��@��9@��u@�I�@��@���@��
@��F@���@��@�l�@�K�@�33@�~�@�5?@��@��^@���@�`B@�7L@���@���@��j@�z�@�Q�@�A�@�1'@�b@��@��@�@~ȴ@~@}�h@}p�@}V@|�@|�@|z�@|9X@|�@{�m@{�@{@y��@x�`@xĜ@x��@x�@x�@xr�@xbN@xb@w|�@w+@v�R@vff@v5?@u�-@t�/@t��@t�@tj@t1@s��@so@r��@rn�@rM�@r�@qhs@p�`@pbN@p1'@p1'@pA�@p1'@o�w@ol�@o;d@o�@n�y@n$�@m�-@m?}@l�j@kƨ@k33@j�H@j�!@jM�@i�@ihs@h�`@hbN@h1'@hb@g�;@g�w@g��@g\)@f�y@f�+@f5?@e�@e�@eO�@eV@d��@dZ@c�m@c@bn�@a�@a�7@a�@`��@`r�@_�@_�P@_l�@_K�@_�@^ȴ@^�R@^�R@^��@^{@]/@\�/@\�j@\j@\I�@\(�@\1@[C�@Z�@Z�!@Z�!@Z�\@Z~�@Z^5@Z�@ZJ@Y��@Yx�@Y&�@XĜ@X�@X �@W��@W\)@V��@V��@VE�@U�@U�@U@U�h@U�h@Up�@U?}@T�@T9X@S��@Sƨ@SdZ@R��@R��@R^5@Q��@QG�@Q&�@P��@PĜ@PQ�@O��@O+@N�y@N�+@NV@N$�@M�T@M��@M�@L��@L�j@L��@Lz�@LI�@K��@K�F@Kt�@KS�@KC�@K33@K@J�!@Jn�@J=q@I�#@Ihs@IX@H��@HbN@Hb@G|�@G
=@F��@F5?@E�-@E�@Ep�@Ep�@E`B@E`B@EO�@D�/@Dz�@DZ@DI�@C�
@C��@C�@Ct�@CdZ@C"�@B��@B=q@A�^@A�7@Ax�@AX@A&�@A%@@Ĝ@@b@?�;@?�w@?\)@?K�@?;d@?
=@>�@>��@>@=@=�@=V@<�j@<z�@<(�@;��@;��@;o@:��@:~�@:n�@9��@9��@9x�@9G�@97L@9%@8��@8�`@8�@8A�@81'@8b@7��@7K�@7+@7�@7
=@6v�@6@5��@5p�@5�@4�/@4��@4Z@4�@3�m@3��@3C�@3o@2�!@2^5@1�@1��@1��@1�@0��@0�u@0A�@/�@/��@/l�@/;d@.ȴ@.ff@.E�@.$�@-��@-`B@-�@,�@,Z@,�@+��@+ƨ@+��@+t�@+dZ@+S�@+33@+o@*�@*��@*��@*��@*~�@*n�@*n�@*=q@*-@*�@)��@)��@)�7@)7L@(��@(�9@(bN@(1'@(  @'�@'�P@'|�@'|�@'l�@'\)@';d@&�y@&�@&�R@&��@&��@&ff@&{@%�T@%@%�-@%�h@%V@$��@$�@$��@$�j@$��@$Z@$1@$1@#ƨ@#��@#C�@#o@#@"�@"�H@"��@"^5@"^5@"^5@"�@!�@!�^@!�7@!hs@!7L@!&�@!%@ �`@ Ĝ@ ��@ r�@ bN@ Q�@ A�@  �@��@�P@+@�@�R@ȴ@ȴ@��@v�@ff@$�@{@@�@@O�@�@��@��@z�@Z@I�@I�@9X@(�@ƨ@��@�@t�@dZ@S�@@�!@n�@=q@-@J@��@��@��@hs@X@&�@��@��@Q�@��@��@|�@\)@;d@;d@+@�@�y@�R@v�@V@V@{@��@�-@�@O�@�@��@�/@�@�D@9X@ƨ@��@dZ@C�@"�@@�H@��@~�@M�@=q@�@J@�#@��@�7@X@�@��@Ĝ@�9@�u@A�@  @�;@��@�w@�w@�@�@�@��@�P@|�@+@�R@��@ff@V@E�@$�@{@�@�T@�T@@�h@p�@�@�@�/@�/@�/@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B,B-B-B-B-B-B-B-B-B-B.B.B.B/B.B.B/B1'B1'B1'B33B6FB9XB;dBF�BS�BVBW
BXBYBYB[#B[#B\)B^5BaHBbNBcTBdZBffBe`B^5BdZB��BɺBBbBJB�B&�B6FBC�BC�BB�B<jB<jBJ�BJ�BQ�B[#BR�BM�BC�B<jBA�B=qB>wBG�BB�B7LB.B&�B�BuB �B#�B�B\B�B�BB��B�B�BȴB�^B�RB�3B��B��B�oB�+B� Bt�B]/BI�B9XB,B�BuB\B
=B  B
�B
��B
B
�LB
��B
�=B
r�B
hsB
ZB
9XB
%�B
VB	�B	�B	ƨB	�3B	��B	�hB	�B	~�B	w�B	l�B	aHB	\)B	N�B	@�B	5?B	+B	$�B	%B��B�B�ZB��B�wB�FB��B��B��B��B��B�hB�bB�DB�+B�B{�Bx�Bu�Bs�Bq�Bn�Bl�Bl�Bk�BiyBgmBe`BdZB`BB^5B\)BZBXBW
BS�BT�BR�BP�BO�BO�BN�BL�BM�BL�BN�BK�BI�BI�BH�BE�BE�BC�BB�B@�B?}B<jB9XB2-B-B)�B(�B'�B'�B&�B$�B#�B"�B&�B)�B+B)�B(�B&�B"�B"�B!�B#�B'�B(�B'�B'�B%�B%�B#�B"�B"�B"�B#�B&�B(�B+B.B33B1'B0!B/B-B+B)�B)�B'�B'�B)�B+B,B-B-B/B49B7LB8RB:^B<jB;dB<jB9XB?}B<jB;dB;dB;dB:^B=qBB�B?}B@�B>wB<jB>wB?}BA�BB�BB�BB�BC�BF�BG�BH�BI�BK�BW
B[#BiyBgmBgmBhsBo�Bu�Bz�Bz�Bv�Bu�Bz�B�JB��B��B��B�B�!B�3B�3B�9B�XB�}B�}BɺB��B��B��B��B��B�
B�;B�;B�;B�;B�BB�;B�BB�yB��B	+B	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	)�B	-B	6FB	<jB	@�B	C�B	D�B	G�B	I�B	N�B	Q�B	R�B	T�B	XB	ZB	[#B	]/B	_;B	aHB	bNB	dZB	e`B	gmB	iyB	jB	jB	jB	l�B	m�B	l�B	iyB	k�B	o�B	n�B	n�B	l�B	l�B	l�B	o�B	r�B	|�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�1B	�1B	�DB	�DB	�DB	�DB	�JB	�JB	�VB	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�FB	�RB	�XB	�XB	�RB	�XB	�XB	�^B	�dB	�dB	�wB	�}B	��B	��B	��B	��B	B	ĜB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
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
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
PB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
1'B
1'B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
XB
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B+�B,�B,�B,�B,�B,�B,�B,�B,�B,�B-�B-�B-�B.�B-�B-�B/ B0�B0�B0�B2�B6+B9>B;dBF�BS�BU�BV�BW�BX�BYB[	B[	B\B^Ba-Bb4BcnBe,Bi_Br|BjBkB�RB˒B�B�B�B!bB*�B8�BD�BE9BD�B?}B?BLJBK�BTaB]dBTFBO�BD�B=qBC-B?B@BIBDMB:B0;B)�BBaB"hB%�B�B�BOByB�B 4B��B��BɆB��B�	B�TB�nB��B�[B��B��Bv�B_;BKxB:�B-�B	B�BB�B�B
�B
��B
�MB
��B
�0B
�6B
t�B
k�B
^5B
<PB
)B
TB	��B	��B	��B	��B	�BB	��B	��B	��B	z�B	m�B	c B	^�B	P�B	A�B	6�B	-�B	(XB	fB��B�ZB��B�$B��B��B�yB�8B�B�-B��B�TB��B��B�RB��B}"By�Bw2BuZBshBo Bl�Bm)Bl�Bj0Bh�Bf�BfBa�B_pB]dBZ�BX�BW�BT�BVBS[BQBPHBPHBO(BMjBN�BN<BPHBLdBJrBKBI�BFYBF�BDBB�BAB@iB=�B:�B3�B-wB*B)_B(XB(�B'�B%�B$�B#�B'mB*KB+QB*KB)�B(sB#�B#nB"hB$�B(�B)�B(�B(�B&�B&�B$�B#nB#�B#TB$ZB'B)B+QB/B3hB1vB0�B0UB-�B+�B+kB*�B(�B)DB*B+B,B-)B-wB/�B4�B72B8�B:�B<�B<�B=<B;0B@iB<�B;�B<6B<jB;0B=�BC-B?�BA�B?�B=qB>�B@ BA�BB�BB�BB�BC�BF�BG�BH�BI�BK�BWYB[#BjBh$Bg�Bh
BoOBv+B{�B{�BwBu?By�B�xB�B��B��B�B�UB�3B�MB�B�$B�.B��B�lB��B�BBϫB��BңB��B�VBߊBߊB��B�\B��B�pB�sB�"B	�B	 B	�B	�B	B	�B	�B	�B	�B	�B	yB	7B	"�B	*0B	,�B	5�B	<B	@�B	C�B	D�B	GzB	I�B	N�B	Q�B	R�B	UB	W�B	Y�B	[	B	]B	_!B	a-B	bNB	d@B	eFB	gRB	iDB	jKB	jKB	jeB	l�B	m�B	l�B	iDB	k�B	o�B	oB	n�B	l�B	lqB	l=B	oB	q�B	|�B	��B	�B	�B	��B	�B	�SB	�%B	�KB	�KB	��B	��B	�xB	�^B	�^B	�~B	�0B	�B	�"B	�4B	� B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	��B	�xB	��B	��B	��B	��B	�WB	�|B	��B	��B	��B	��B	��B	�B	�AB	�5B	�IB	�CB	�"B	�QB	�KB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	��B	��B	��B	��B	��B	�B	��B	��B	�8B	�>B	�>B	�^B	�JB	�dB	�]B	�HB	�OB	�iB	�iB	��B	�[B	�gB	�tB	�tB	�tB	ƎB	ǮB	ɆB	ɠB	ɠB	�rB	ʦB	��B	��B	͹B	ΊB	ΊB	ΥB	ϑB	��B	��B	�B	��B	��B	ּB	��B	��B	�B	�B	��B	�B	�B	�;B	�B	�'B	�B	�B	� B	� B	�&B	�&B	�&B	�,B	�FB	�B	�XB	�_B	�eB	�QB	�QB	�qB	�qB	�]B	�cB	�}B	�iB	�iB	�oB	�oB	�oB	�B	�B	�B	�B	�B	�B	��B	��B	�zB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
�B
�B
�B
B
	B
	B
	B
	7B

=B

=B

#B

=B
DB
)B
B
B
6B
6B
6B
6B
6B
"B
"B
"B
(B
(B
(B
BB
BB
.B
.B
HB
NB
NB
NB
:B
TB
oB
[B
aB
gB
mB
SB
mB
sB
yB
_B
_B
_B
_B
KB
eB
eB
�B
�B
qB
qB
qB
qB
qB
qB
�B
xB
xB
dB
xB
dB
�B
~B
~B
~B
dB
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
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
/ B
/ B
.�B
/B
0!B
1B
1B
2B
2B
2B
3B
3�B
4�B
4�B
4�B
4�B
5B
5%B
5�B
6B
6B
6FB
7B
6�B
72B
8B
8B
88B
9>B
9>B
9	B
:*B
:*B
:*B
:*B
:*B
;0B
;0B
;0B
<6B
<B
<6B
<6B
<6B
<6B
<PB
=<B
=<B
=VB
=VB
=<B
=<B
=<B
>BB
>wB
>]B
>BB
>BB
>]B
?.B
?HB
?HB
?HB
?.B
?.B
?HB
?cB
@iB
A;B
AUB
AUB
AoB
B[B
B[B
B[B
BuB
C{B
C{B
DgB
DgB
DgB
EmB
EmB
EmB
EmB
EmB
FtB
FtB
F�B
GzB
G�B
GzB
GzB
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[	B
[�B
[�B
[�B
[�B
\B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^�B
^�B
^�B
_B
_B
_B
_B
`B
_�B
`B
`B
`'B
aB
aB
`�B
aB
aB
`�B
`�B
`�B
aB
aB
bB
a�B
a�B
a�B
bB
bB
bB
b4B
bB
cB
c B
d&B
d&B
d@B
d@B
dB
d&B
e,B
e,B
e,B
eFB
e,B
e,B
e,B
eB
eB
e,B
fB
f2B
f2B
f2B
f2B
fB
f2B
f2B
f2B
g8B
g8B
g8B
g8B
g8B
g8B
g8B
g8B
gRB
gB
hXB
hXB
h>B
h>B
h>B
h>B
h>B
iDB
i*B
iDB
iDB
iDB
j0B
jKB
jKB
jKB
jKB
jKB
jKB
jKB
jKB
kQB
kkB
k6B
kQB
l=B
l=B
l"B
l"B
mCB
mCB
m]B
m]B
mwB
nIB
m]B
nIB
nIB
n}B
nIB
n}B
nIB
ncB
n}B
ncB
oiB
oiB
oiB
pUB
pUB
pUB
pUB
po11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Uh<G�b<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.6(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912120037062019121200370620191212003706202306231719252023062317192520230623171925201912130030342019121300303420191213003034  JA  ARFMdecpA19c                                                                20191207093718  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191207003721  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191207003724  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191207003724  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191207003725  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191207003725  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191207003725  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191207003725  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191207003726  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191207003726                      G�O�G�O�G�O�                JA  ARUP                                                                        20191207005501                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191207153445  CV  JULD            G�O�G�O�Fǋ�                JM  ARCAJMQC2.0                                                                 20191211153706  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191211153706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191212153034  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081925  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                