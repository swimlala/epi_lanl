CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-14T09:43:09Z creation;2023-01-14T09:43:10Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20230114094309  20230114102801  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�z��1   @�W:�@4ݲ-V�c�7KƧ�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�33A��A!��A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0ffB8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DLy�DM  DM�fDN  DNy�DO  DO�fDPfDP� DQ  DQ� DR  DR� DS  DS� DTfDT�fDU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� DdfDd�fDe  Dey�De��Df� DgfDg�fDh  Dh� Di  Di� Dj  Djy�Dj��Dk� DlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dv��Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÃ3D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dσ3D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D��3D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�C3D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�C3D�|�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33A��A%��AD  Ad  A���A�  A�  A�  A�  A�  A�  A�  B  B	  B  BffB!  B)  B1ffB9  BA  BH��BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�L�B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ B�L�B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ CY�C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0Y�C2Y�C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CXY�CZ@ C\@ C^@ C`Y�CbY�Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  D  D � D D� DfD� D D� D D� D D� D D� D D� D D� D	fD	� D
 D
� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D	�D� D D� D D� D D� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$�fD% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D,	�D,� D- D-� D. D.� D/ D/� D0 D0�fD1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ�fDK DK� DL DL��DM DM�fDN DN��DO DO�fDPfDP� DQ DQ� DR DR� DS DS� DTfDT�fDU DU� DV DV� DW DW� DX DX� DY	�DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db	�Db� Dc Dc� DdfDd�fDe De��Df	�Df� DgfDg�fDh Dh� Di Di� Dj Dj��Dk	�Dk� DlfDl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr� Ds Ds� Dt Dt� Du	�Du� Dv Dv� Dw	�Dw� DxfDx� Dy Dy� Dz Dz� D{ D{� D| D|� D} D}� D~ D~� D D� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D��3D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�D�D���D�� D� D�D�D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D�3D�H D�� D�� D�3D�H D�� D�� D� D�H D�� D��3D�3D�H D�� D�� D� D�H D�� D�� D� D�H D��3D�� D� D�H D���D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�K3D�� D�� D� D�H D�� D�� D� D�H D���D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D D�� D� D�H DË3D�� D� D�H DĈ D�� D� D�H Dň D�� D� D�H Dƈ D�� D� D�H Dǈ D�� D� D�H DȈ D�� D� D�H DɈ D�� D� D�H Dʈ D�� D� D�H Dˈ D�� D� D�H D̈ D�� D� D�H D͈ D�� D� D�H DΈ D�� D� D�H Dϋ3D�� D� D�H DЈ D�� D� D�H Dш D�� D� D�H D҈ D�� D� D�H Dӈ D��3D� D�H DԈ D�� D� D�H DՈ D�� D� D�H Dֈ D�� D� D�H D׈ D�� D� D�H D؈ D�� D� D�H Dو D�� D� D�H Dڈ D�� D� D�H Dۈ D�� D� D�H D܈ D�� D� D�H D݈ D�� D� D�H Dވ D�� D� D�K3D߈ D�� D� D�H D�� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D��D�� D� D�H D� D�� D� D�H D�3D�� D� D�H D� D�� D� D�D�D� D�� D� D�D�D��D�� D� D�H D� D�� D� D�H D�� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D�3D�H D��D�� D� D�H D�� D�� D� D�H D�� D��3D� D�H D�� D�� D� D�K3D���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��
A��A��A��A���A̶FA̅A�jA�oA�p�A�-A���A���Aʧ�A�S�A��A�A�ĜAɕ�A�|�A�n�A�ffA�Q�A�{A�ĜA�jA��TAǑhA�/A���A�9XA�ZA�/A���A��A��!A�{A��#A���A�bNA��RA���A��`A�-A��A��A�9XA�-A�dZA��PA��A�ȴA��A�n�A�v�A���A���A�bA��A�-A�bNA�z�A���A��yA�~�A��#A�A�A��A��A�7LA��A�l�A�/A�bNA�C�A��/A���A��A��A�;dA��-A�K�A��/A�|�A�ƨA���A� �A�G�A��A�dZA�7LA��A�`BA}�;Azv�Ay�TAy�AxbNAt�Aq��ApbAnn�Al-Aj��AhjAfĜAeA_�A]�PA\�A\�uA\jA\1'A[�
A[7LAY��AV�/AS/AQ�;AP{AK�AK+AJ��AJ1AH=qAE%AC�ABbNAA33A@A>1A<��A;�A:I�A9hsA8jA5"�A3dZA2�`A2~�A1|�A.��A-�A-�FA,ĜA+�FA+�A(�/A'��A&ffA%/A$��A$M�A#�A"�`A!G�A�A��AbA�A1AM�A�/AZA�-AoAA�A\)A�AȴAJAx�A�A�A�A$�A��A~�A%A��A?}A
�9A	dZAȴAr�A �A��A�^A��A\)A&�AE�A�HAQ�A��A|�A ��A 9X@���@��R@���@��@�1'@��w@��@��@��m@� �@��T@�j@��w@�33@��H@�^5@���@�
=@�Q�@�b@�\)@�%@�?}@�A�@�ƨ@�V@�&�@�S�@�^@�j@��m@�
=@ޗ�@އ+@�=q@�`B@ܼj@�9X@���@���@ڧ�@ڗ�@ڏ\@�~�@���@��@ׅ@�"�@�n�@ա�@�j@�33@�V@�x�@�j@�l�@�$�@͡�@͡�@͑h@�`B@���@�z�@�(�@˝�@��@�E�@���@�ƨ@��@�n�@�@��@���@�V@öF@�o@�~�@�{@���@�X@�G�@��@��`@��m@�|�@�
=@��@�ȴ@�{@��9@�Q�@�b@��m@���@�;d@�ȴ@�~�@�E�@�-@��@�7L@�Ĝ@��@���@�j@��@���@�n�@�=q@��7@��`@�9X@�dZ@���@���@��!@��R@�ȴ@��@���@�~�@�n�@�$�@�O�@��@��@�b@�|�@�
=@�=q@��-@���@��@���@�J@���@�`B@�%@�z�@�bN@�A�@���@��@�@�ȴ@�V@��@��#@��@���@��9@�r�@��@���@�K�@�
=@�
=@�
=@�
=@��H@�~�@�E�@�{@�J@��#@�p�@�G�@��@��/@���@�Ĝ@��@�Z@��
@�\)@�"�@���@��+@�V@���@��h@�O�@��@��@��j@���@�bN@�9X@�b@��@��F@�S�@�o@��7@��/@���@�V@��/@�bN@��@�v�@��@���@�7L@�Ĝ@��D@�z�@�r�@�(�@�+@���@���@���@��+@�M�@�@�?}@���@�I�@��
@��@���@�t�@�C�@�o@�ȴ@�=q@��-@��@�x�@�7L@���@�Ĝ@��D@���@��j@���@��@��u@��@��@�r�@��@��F@�l�@�C�@���@��@�ȴ@��\@�n�@�-@��@���@��-@���@�`B@��@�%@���@��j@���@�r�@�bN@�A�@� �@�1@���@��@�l�@�K�@�"�@��@�
=@��@��R@�~�@�=q@�J@���@�@��@��@��`@��9@��D@�j@�A�@�9X@�(�@� �@��m@��@��P@�t�@�C�@�@�ȴ@��\@�v�@�v�@�v�@�=q@��@��7@�X@�&�@��@��/@���@��9@���@��D@�Z@�(�@�1@��
@���@�\)@�C�@�33@��y@���@�v�@�{@�@��@��T@��7@�O�@���@��9@��@�I�@�b@�@~��@~v�@}�@}V@|�@{dZ@zM�@y�^@x�9@w��@w�@v�+@v@u��@tj@s"�@r��@r^5@q��@q��@q��@qX@p�u@p�@p  @o;d@o
=@n��@n��@n��@n��@m�T@mV@l�D@k�m@kt�@k33@ko@j��@jJ@ix�@h��@h1'@g�@g;d@g
=@f�y@f�+@e@e��@eV@d�D@dI�@d�@cƨ@c�@c33@b��@b~�@b=q@a�#@ax�@`r�@`  @_�@_�@_�P@_;d@_�@^�R@^5?@]�h@]?}@]�@\z�@\1@[ƨ@[��@[�@[C�@Z�@Z�H@Z��@Z��@Z=q@Y��@X��@X �@W�;@W��@W��@W�@V�y@V��@Vv�@VE�@U�-@T��@S��@S33@R�!@R�\@R~�@Rn�@R^5@R^5@RM�@R-@R�@RJ@Q��@Q�@P�u@Pr�@P  @O\)@N�+@NE�@N@M�@M�h@M`B@M`B@M?}@M/@M�@L�/@L��@L��@L��@L�@L(�@Kƨ@KdZ@KC�@K"�@Ko@J�@J��@J~�@JM�@J-@I�^@I&�@H�`@HĜ@H�9@H�u@H�u@HbN@H �@G�w@G\)@E�T@E�@Ep�@Ep�@Ep�@EO�@D�j@C�F@C�@Ct�@CS�@C"�@B��@B�@A��@Ahs@@��@@�9@@��@@�u@@�@@Q�@@ �@?l�@>�y@>ȴ@>�+@>$�@>@=�T@=@=��@=p�@=`B@=O�@=O�@=�@<�@<(�@<�@;��@;�m@;�
@;�
@;C�@:�!@:n�@:J@9�7@9X@97L@9%@8Ĝ@8�9@8��@8�u@8b@7\)@7
=@6ȴ@6E�@6$�@6{@6@5�T@5�h@5?}@5�@4��@4�D@4Z@4(�@3�
@3�F@3S�@2�H@2�!@2~�@2^5@2M�@2M�@2-@1��@0Ĝ@0r�@0 �@0  @0  @0  @/�;@/��@/�@/\)@/K�@/K�@/+@/+@.�y@.ff@.@-@-p�@-`B@-O�@-V@,�@,��@,�j@,�@,��@,j@,9X@+��@+�
@+�
@+ƨ@+ƨ@+��@+t�@+33@+o@+o@+"�@+o@*�H@*��@)�7@(��@(Ĝ@(�9@(�@(bN@(Q�@(1'@(b@'�w@'K�@&�y@&�R@&ff@&E�@&5?@&$�@&{@%�T@%�-@%�h@%?}@$�@$�@$�/@$�/@$��@$��@$�D@$j@$(�@$1@#�m@#�m@#�m@#�m@#�m@#ƨ@#t�@#"�@"�H@"�\@"n�@"=q@"J@!��@!��@!�7@!X@!&�@ r�@ A�@ A�@ 1'@�@�;@�@l�@�@
=@�y@�R@�+@v�@V@@��@�@O�@��@�@j@I�@(�@��@�
@�F@S�@�H@�\@^5@M�@-@-@J@�@�^@��@�7@x�@x�@7L@��@Ĝ@�9@�u@bN@1'@  @�w@�P@l�@K�@
=@�y@�@�R@��@E�@�-@�@p�@?}@��@�@��@�D@z�@(�@1@�m@�
@�F@�@S�@o@@�H@�H@�\@n�@^5@M�@��@��@x�@�@%@��@�`@�`@��@��@��@�`@Ĝ@Q�@1'@b@�@�@l�@;d@+@�@�@��@ȴ@v�@$�@�@�@�T@�-@p�@?}@��@�/@��@z�@Z@(�@��@�m@�F@dZ@33@
�H@
��@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��
A��A��A��A���A̶FA̅A�jA�oA�p�A�-A���A���Aʧ�A�S�A��A�A�ĜAɕ�A�|�A�n�A�ffA�Q�A�{A�ĜA�jA��TAǑhA�/A���A�9XA�ZA�/A���A��A��!A�{A��#A���A�bNA��RA���A��`A�-A��A��A�9XA�-A�dZA��PA��A�ȴA��A�n�A�v�A���A���A�bA��A�-A�bNA�z�A���A��yA�~�A��#A�A�A��A��A�7LA��A�l�A�/A�bNA�C�A��/A���A��A��A�;dA��-A�K�A��/A�|�A�ƨA���A� �A�G�A��A�dZA�7LA��A�`BA}�;Azv�Ay�TAy�AxbNAt�Aq��ApbAnn�Al-Aj��AhjAfĜAeA_�A]�PA\�A\�uA\jA\1'A[�
A[7LAY��AV�/AS/AQ�;AP{AK�AK+AJ��AJ1AH=qAE%AC�ABbNAA33A@A>1A<��A;�A:I�A9hsA8jA5"�A3dZA2�`A2~�A1|�A.��A-�A-�FA,ĜA+�FA+�A(�/A'��A&ffA%/A$��A$M�A#�A"�`A!G�A�A��AbA�A1AM�A�/AZA�-AoAA�A\)A�AȴAJAx�A�A�A�A$�A��A~�A%A��A?}A
�9A	dZAȴAr�A �A��A�^A��A\)A&�AE�A�HAQ�A��A|�A ��A 9X@���@��R@���@��@�1'@��w@��@��@��m@� �@��T@�j@��w@�33@��H@�^5@���@�
=@�Q�@�b@�\)@�%@�?}@�A�@�ƨ@�V@�&�@�S�@�^@�j@��m@�
=@ޗ�@އ+@�=q@�`B@ܼj@�9X@���@���@ڧ�@ڗ�@ڏ\@�~�@���@��@ׅ@�"�@�n�@ա�@�j@�33@�V@�x�@�j@�l�@�$�@͡�@͡�@͑h@�`B@���@�z�@�(�@˝�@��@�E�@���@�ƨ@��@�n�@�@��@���@�V@öF@�o@�~�@�{@���@�X@�G�@��@��`@��m@�|�@�
=@��@�ȴ@�{@��9@�Q�@�b@��m@���@�;d@�ȴ@�~�@�E�@�-@��@�7L@�Ĝ@��@���@�j@��@���@�n�@�=q@��7@��`@�9X@�dZ@���@���@��!@��R@�ȴ@��@���@�~�@�n�@�$�@�O�@��@��@�b@�|�@�
=@�=q@��-@���@��@���@�J@���@�`B@�%@�z�@�bN@�A�@���@��@�@�ȴ@�V@��@��#@��@���@��9@�r�@��@���@�K�@�
=@�
=@�
=@�
=@��H@�~�@�E�@�{@�J@��#@�p�@�G�@��@��/@���@�Ĝ@��@�Z@��
@�\)@�"�@���@��+@�V@���@��h@�O�@��@��@��j@���@�bN@�9X@�b@��@��F@�S�@�o@��7@��/@���@�V@��/@�bN@��@�v�@��@���@�7L@�Ĝ@��D@�z�@�r�@�(�@�+@���@���@���@��+@�M�@�@�?}@���@�I�@��
@��@���@�t�@�C�@�o@�ȴ@�=q@��-@��@�x�@�7L@���@�Ĝ@��D@���@��j@���@��@��u@��@��@�r�@��@��F@�l�@�C�@���@��@�ȴ@��\@�n�@�-@��@���@��-@���@�`B@��@�%@���@��j@���@�r�@�bN@�A�@� �@�1@���@��@�l�@�K�@�"�@��@�
=@��@��R@�~�@�=q@�J@���@�@��@��@��`@��9@��D@�j@�A�@�9X@�(�@� �@��m@��@��P@�t�@�C�@�@�ȴ@��\@�v�@�v�@�v�@�=q@��@��7@�X@�&�@��@��/@���@��9@���@��D@�Z@�(�@�1@��
@���@�\)@�C�@�33@��y@���@�v�@�{@�@��@��T@��7@�O�@���@��9@��@�I�@�b@�@~��@~v�@}�@}V@|�@{dZ@zM�@y�^@x�9@w��@w�@v�+@v@u��@tj@s"�@r��@r^5@q��@q��@q��@qX@p�u@p�@p  @o;d@o
=@n��@n��@n��@n��@m�T@mV@l�D@k�m@kt�@k33@ko@j��@jJ@ix�@h��@h1'@g�@g;d@g
=@f�y@f�+@e@e��@eV@d�D@dI�@d�@cƨ@c�@c33@b��@b~�@b=q@a�#@ax�@`r�@`  @_�@_�@_�P@_;d@_�@^�R@^5?@]�h@]?}@]�@\z�@\1@[ƨ@[��@[�@[C�@Z�@Z�H@Z��@Z��@Z=q@Y��@X��@X �@W�;@W��@W��@W�@V�y@V��@Vv�@VE�@U�-@T��@S��@S33@R�!@R�\@R~�@Rn�@R^5@R^5@RM�@R-@R�@RJ@Q��@Q�@P�u@Pr�@P  @O\)@N�+@NE�@N@M�@M�h@M`B@M`B@M?}@M/@M�@L�/@L��@L��@L��@L�@L(�@Kƨ@KdZ@KC�@K"�@Ko@J�@J��@J~�@JM�@J-@I�^@I&�@H�`@HĜ@H�9@H�u@H�u@HbN@H �@G�w@G\)@E�T@E�@Ep�@Ep�@Ep�@EO�@D�j@C�F@C�@Ct�@CS�@C"�@B��@B�@A��@Ahs@@��@@�9@@��@@�u@@�@@Q�@@ �@?l�@>�y@>ȴ@>�+@>$�@>@=�T@=@=��@=p�@=`B@=O�@=O�@=�@<�@<(�@<�@;��@;�m@;�
@;�
@;C�@:�!@:n�@:J@9�7@9X@97L@9%@8Ĝ@8�9@8��@8�u@8b@7\)@7
=@6ȴ@6E�@6$�@6{@6@5�T@5�h@5?}@5�@4��@4�D@4Z@4(�@3�
@3�F@3S�@2�H@2�!@2~�@2^5@2M�@2M�@2-@1��@0Ĝ@0r�@0 �@0  @0  @0  @/�;@/��@/�@/\)@/K�@/K�@/+@/+@.�y@.ff@.@-@-p�@-`B@-O�@-V@,�@,��@,�j@,�@,��@,j@,9X@+��@+�
@+�
@+ƨ@+ƨ@+��@+t�@+33@+o@+o@+"�@+o@*�H@*��@)�7@(��@(Ĝ@(�9@(�@(bN@(Q�@(1'@(b@'�w@'K�@&�y@&�R@&ff@&E�@&5?@&$�@&{@%�T@%�-@%�h@%?}@$�@$�@$�/@$�/@$��@$��@$�D@$j@$(�@$1@#�m@#�m@#�m@#�m@#�m@#ƨ@#t�@#"�@"�H@"�\@"n�@"=q@"J@!��@!��@!�7@!X@!&�@ r�@ A�@ A�@ 1'@�@�;@�@l�@�@
=@�y@�R@�+@v�@V@@��@�@O�@��@�@j@I�@(�@��@�
@�F@S�@�H@�\@^5@M�@-@-@J@�@�^@��@�7@x�@x�@7L@��@Ĝ@�9@�u@bN@1'@  @�w@�P@l�@K�@
=@�y@�@�R@��@E�@�-@�@p�@?}@��@�@��@�D@z�@(�@1@�m@�
@�F@�@S�@o@@�H@�H@�\@n�@^5@M�@��@��@x�@�@%@��@�`@�`@��@��@��@�`@Ĝ@Q�@1'@b@�@�@l�@;d@+@�@�@��@ȴ@v�@$�@�@�@�T@�-@p�@?}@��@�/@��@z�@Z@(�@��@�m@�F@dZ@33@
�H@
��@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bo�Bo�Bo�Bn�Bn�Bs�B{�B~�B�=B��B�'BǮB�
B�yB�B�B�B��B��B��B��BBBBBPB�B6FBF�BF�BA�B33B&�B�B�BPBhBhB\BVBDB%B��B��B��B�B�B��B��B��B��B�B�ZB��B��BŢB�^B��B��B��B��B��B�hB|�BdZB\)BE�B2-B�BB�B�
BÖB�?B�-B�B��B�PB|�B]/BC�B-BuB
�B
��B
�}B
�9B
��B
�{B
�%B
p�B
iyB
YB
:^B
 �B
�B
�B
PB
  B	�ZB	�B	��B	��B	�LB	��B	��B	�\B	� B	iyB	e`B	cTB	bNB	`BB	_;B	[#B	R�B	D�B	1'B	'�B	�B	VB	B	B��B��B�B�BB�)B�B��B��BƨB��B�qB�XB�RB�!B��B��B��B��B��B��B��B��B��B�uB��B�bB�VB�=B�7B�+B�%B�B�B�B�B�B~�B~�B� B� B~�B�B�B�B�B� B~�B� B~�B}�B� B~�B~�B� B�B�+B�%B�+B�%B�+B�B�B�B�B�B�B�B� B�B�+B�%B�%B�+B�DB�JB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�9B�9B�FB�FB�^B�qB��BBŢBƨBƨBǮB��B��B��B��B��B��B��B��B��B�B�BB�ZB�ZB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	1B	DB	PB	\B	hB	hB	hB	{B	�B	�B	�B	!�B	$�B	&�B	'�B	+B	-B	49B	7LB	9XB	8RB	9XB	=qB	F�B	H�B	I�B	J�B	K�B	N�B	Q�B	R�B	T�B	T�B	W
B	\)B	^5B	_;B	_;B	`BB	hsB	iyB	hsB	iyB	iyB	l�B	n�B	m�B	o�B	q�B	r�B	s�B	u�B	x�B	{�B	|�B	� B	�B	�+B	�+B	�+B	�%B	�%B	�=B	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�9B	�?B	�FB	�RB	�XB	�qB	�wB	�}B	�}B	�}B	��B	ĜB	ŢB	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�HB	�ZB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
DB
VB
hB
hB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
#�B
#�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
.B
/B
/B
/B
/B
/B
/B
0!B
1'B
2-B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
8RB
8RB
8RB
9XB
9XB
9XB
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
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
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
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
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
YB
YB
YB
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
[#B
[#B
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
^5B
^5B
]/B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
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
hsB
hsB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  Bo�Bo�Bo�Bn�Bn�Bs�B{�B~�B�=B��B�'BǮB�
B�yB�B�B�B��B��B��B��BBBBBPB�B6FBF�BF�BA�B33B&�B�B�BPBhBhB\BVBDB%B��B��B��B�B�B��B��B��B��B�B�ZB��B��BŢB�^B��B��B��B��B��B�hB|�BdZB\)BE�B2-B�BB�B�
BÖB�?B�-B�B��B�PB|�B]/BC�B-BuB
�B
��B
�}B
�9B
��B
�{B
�%B
p�B
iyB
YB
:^B
 �B
�B
�B
PB
  B	�ZB	�B	��B	��B	�LB	��B	��B	�\B	� B	iyB	e`B	cTB	bNB	`BB	_;B	[#B	R�B	D�B	1'B	'�B	�B	VB	B	B��B��B�B�BB�)B�B��B��BƨB��B�qB�XB�RB�!B��B��B��B��B��B��B��B��B��B�uB��B�bB�VB�=B�7B�+B�%B�B�B�B�B�B~�B~�B� B� B~�B�B�B�B�B� B~�B� B~�B}�B� B~�B~�B� B�B�+B�%B�+B�%B�+B�B�B�B�B�B�B�B� B�B�+B�%B�%B�+B�DB�JB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�9B�9B�FB�FB�^B�qB��BBŢBƨBƨBǮB��B��B��B��B��B��B��B��B��B�B�BB�ZB�ZB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	1B	DB	PB	\B	hB	hB	hB	{B	�B	�B	�B	!�B	$�B	&�B	'�B	+B	-B	49B	7LB	9XB	8RB	9XB	=qB	F�B	H�B	I�B	J�B	K�B	N�B	Q�B	R�B	T�B	T�B	W
B	\)B	^5B	_;B	_;B	`BB	hsB	iyB	hsB	iyB	iyB	l�B	n�B	m�B	o�B	q�B	r�B	s�B	u�B	x�B	{�B	|�B	� B	�B	�+B	�+B	�+B	�%B	�%B	�=B	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�9B	�?B	�FB	�RB	�XB	�qB	�wB	�}B	�}B	�}B	��B	ĜB	ŢB	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�HB	�ZB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
DB
VB
hB
hB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
#�B
#�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
.B
/B
/B
/B
/B
/B
/B
0!B
1'B
2-B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
8RB
8RB
8RB
9XB
9XB
9XB
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
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
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
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
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
YB
YB
YB
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
[#B
[#B
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
^5B
^5B
]/B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
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
hsB
hsB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230114184254  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230114094309  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230114094309  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230114094310                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230114094311  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230114094311  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230114102801                      G�O�G�O�G�O�                