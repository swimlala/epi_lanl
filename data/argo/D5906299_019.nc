CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-08-23T22:40:55Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    9�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    :   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  :$   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  :�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ;�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        <d   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    <p   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    <t   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  <|   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    <�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    <�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  <�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  =L   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  =�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    >   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           >   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    >0   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            >4   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           >L   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           >d   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    >|   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    >�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    >�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        A�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    A�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    A�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    A�   PROFILE_TEMP_CNDC_QC               	long_name         (Global quality flag of TEMP_CNDC profile   conventions       Argo reference table 2a    
_FillValue                    A�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        .�  A�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     .�  |L   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     .�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .�  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .�  \   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � O<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� Z�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� �   	TEMP_CNDC            
         	long_name         -Internal temperature of the conductivity cell      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� -�   TEMP_CNDC_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                 @ h|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 $  j�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 $  ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 $  ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                 � ּ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ش   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ٤   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �0Argo profile    3.1 1.2 19500101000000  20220823224055  20221004115546  5906299 5906299 5906299 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER,                                                  STEPHEN RISER,                                                  STEPHEN RISER,                                                  PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC                AAA AOAOAO  8443                            8443                            8443                            2C  2C  2C  DDD APEX                            APEX                            APEX                            8815                            8815                            8815                            052520                          052520                          052520                          877 877 877 @�O����@�O����@�O����111 @�O�q�4�@�O�q�4�@�O�q�4�@5���l�@5���l�@5���l��c�G�z��c�G�z��c�G�z�111 GPS     GPS     GPS     Primary sampling: averaged []                                                                                                                                                                                                                                   Secondary sampling: discrete []                                                                                                                                                                                                                                 Secondary sampling: discrete [1Hz data]                                                                                                                                                                                                                                  AAB AAC FFF     >���@@  @�  @�  A  A0  AP  Ap  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B$  B,  B4  B<  BD  BL  BT  B\  Bd  Bl  Bt  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C	  C  C  C  C  C  C  C  C  C  C  C  C!  C#  C%  C'  C)  C+  C-  C/  C1  C3  C5  C7  C9  C;  C=  C?  CA  CC  CE  CH  CK  CM  CO  CQ  CS  CU  CW  CY  C[  C]�C_  Ca  Cc  Ce  Cg  Ci  Ck  Cm  Co  Cq  Cs  Cu  Cw  Cy  C{  C}  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C CÀ CĀ Cŀ Cƀ Cǀ CȀ Cɀ Cʀ Cˀ C̀ C̀ C΀ Cπ CЀ Cр CҀ CӀ CԀ CՀ Cր C׀ C؀ Cـ Cڀ Cۀ C܀ C݀ Cހ C߀ C�� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C�� C� C� C� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D @ D � D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D	@ D	� D
@ D
� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D @ D � D!@ D!� D"@ D"� D#@ D#� D$@ D$� D%@ D%� D&@ D&� D'@ D'� D(@ D(� D)@ D)� D*@ D*� D+@ D+� D,@ D,� D-@ D-� D.@ D.� D/@ D/� D0@ D0� D1@ D1� D2@ D2� D3@ D3� D4@ D4� D5@ D5� D6@ D6� D7@ D7� D8@ D8� D9@ D9� D:@ D:� D;@ D;� D<@ D<� D=@ D=� D>@ D>� D?@ D?� D@@ D@� DA@ DA� DB@ DB� DC@ DC� DD@ DD� DE@ DE� DF@ DF� DG@ DG� DH@ DH� DI@ DI� DJ@ DJ� DK@ DK� DL@ DL� DM@ DM� DN@ DN� DO@ DO� DP@ DP� DQ@ DQ� DR@ DR� DS@ DS� DT@ DT� DU@ DU� DV@ DV� DW@ DW� DX@ DX� DY@ DY� DZ@ DZ� D[@ D[� D\@ D\� D]@ D]� D^@ D^� D_@ D_� D`@ D`� Da@ Da� Db@ Db� Dc@ Dc� Dd@ Dd� De@ De� Df@ Df� Dg@ Dg� Dh@ Dh� Di@ Di� Dj@ Dj� Dk@ Dk� Dl@ Dl� Dm@ Dm� Dn@ Dn� Do@ Do� DpFfDp� Dq@ Dq� Dr@ Dr� Ds@ Ds� Dt@ Dt� Du@ Du� Dv@ Dv� Dw@ Dw� Dx@ Dx� Dy@ Dy� Dz@ Dz� D{@ D{� D|@ D|� D}@ D}� D~@ D~� D@ D� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D��D�` D  D�� D�  D�` Dà D�� D�  D�` DĠ D�� D�  D�` DŠ D�� D�  D�` DƠ D�� D�  D�` DǠ D�� D�  D�` DȠ D�� D�  D�` Dɠ D�� D�  D�` Dʠ D�� D�  D�` Dˠ D�� D�  D�` D̠ D�� D�  D�` D͠ D�� D�  D�` DΠ D�� D�  D�` DϠ D�� D�  D�` DР D�� D�  D�` DѠ D�� D�  D�` DҠ D�� D�  D�` DӠ D�� D�  D�` DԠ D�� D�  D�` Dՠ D�� D��D�\�D֠ D�� D�  D�` Dנ D�� D�  D�` Dؠ D�� D�  D�` D٠ D�� D�  D�` Dڠ D�� D�  D�` D۠ D�� D�  D�` Dܠ D�� D�  D�` Dݠ D�� D�  D�` Dޠ D�� D�  D�` Dߠ D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D�� D�� D�  D�` D�3D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D��D�` D� D�� D�  D�` D� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�\�D�� D�� D�  D�` D�� D�� D�  D�` D�� D��3D�6f@��A\)A�{A�=qB#33BI�Bo�B�8RB���B���B��)B���B�Cz�C��CQ�C +�C(��C4z�C>޸CH� CQ��C[�Cf��CpB�CzxRC�33C�)C�T{C�(�C�s3C�:�C�#�C�8RC��C��C�.C��{C�j=C�G�C��{D	��D3D"�)D/"�D;�DH�DT��D`�Dm�)Dz
D�Y�D�
D��D��D�K�D���D���D� �D�PRD��
D�ϮD�ÅD�P�D�
D��3D��D�J=D��D��qD�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=���>L��=���=���>L��=���>L��=���=���=���=���>L��=���>���>L��=���>L��>L��=���=���>L��=���>L��>L��>L��>L��>L��=���=���=���    =���=���=���>L��>L��=���>L��>���>L��>L��=���=���=���>L��=���>L��>L��>���=���>���>L��=���>L��>L��>L��>���>���>���>L��>���    =���=���=���=���>L��>L��=���=���=���>L��=���>L��>L��=���    =���=���=���>L��>L��=���=���>���>L��=���>L��=���=���>L��=���>L��>L��=���=���=���>L��=���>L��=���=���>L��=���=���=���>L��=���=���>L��>���>L��=���>���>L��    =���>L��>L��=���>L��=���>L��>���>L��>L��=���=���>L��>L��=���>L��>L��>���>L��=���>L��=���=���>L��>L��=���>L��=���>L��    =���=���=���>L��>L��=���>L��>L��>L��=���=���=���>L��=���>L��=���=���>L��=���=���>L��>L��>L��>L��>L��>L��=���>L��=���=���=���=���=���>L��>L��>L��>L��>L��>L��>L��=���=���=���>L��>L��>L��>���>���>���>���?   >���?   ?��?��?   ?   >���>���=���=���=���=���>���=���=���>L��=���>L��>���>L��>L��>L��=���>L��=���=���>L��>L��=���>L��>���=���=���=���>���=���=���>���=���=���>���=���    >L��>L��=���=���    >���>L��>L��=���>���    >L��=���>L��>L��>���=���>L��>L��>L��>L��>L��>L��>���>L��>L��=���    >���=���=���>���=���>���>���>L��>L��=���=���>L��>L��>L��>L��=���=���=���>L��=���=���>L��=���=���=���=���>L��>L��>���>���>L��>���>L��>���>L��>���>L��=���=���=���>L��>L��>L��>L��>L��=���>L��>���>���>L��>L��>L��>���>L��=���=���>L��=���>���=���>L��>L��=���=���>L��=���>L��=���=���>L��=���>L��>L��=���=���>���>���=���>L��>L��>L��>L��>L��>L��>���>L��>L��>L��=���=���>L��>L��>���>���?   ?   ?��?��?333?fff?fff?fff?���?���?���?�ff?�ff?�33?���?ٙ�?ٙ�?�ff?�33@   @ff@��@33@��@   @&ff@,��@333@@  @@  @Fff@L��@S33@Y��@`  @fff@l��@s33@y��@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@ٙ�@�  @�  @�33@�ff@���@�  @�33@�ff@���@���A��A33A��AffA  A	��A33AffA  A��A33A��A  A��A33A��AffA!��A#33A$��A&ffA)��A+33A,��A.ffA1��A333A4��A6ffA8  A9��A;33A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAQ��AS33AT��AVffAX  A[33A\��A^ffA`  Aa��Ac33Ad��Ah  Ai��Ak33Al��AnffAp  Aq��At��AvffAx  Ay��A{33A|��A~ffA���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���Ař�A�ffA�33A���Aə�A�ffA�33A�  A͙�A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aݙ�A�33A�  A���AᙚA�33A�  A���A噚A�33A�  A���A陚A�ffA�33A���A홚A�ffA�33A�  A�A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�  A���A���A�ffA�33B   B ffB33B��B  BffB��B33B  BffB��B33B��BffB��B33B��BffBffB	33B	��B
  B
ffB33B��B  BffB��B��B  BffB33B��B  BffB��B��B  BffB33B��B  BffB��B33B  BffB��B33B  BffB��B33B��B  B��B33B��B  BffB��B��B  BffB��B33B��B   B ffB!33B!��B"  B"ffB"��B#33B$  B$  B$ffB%33B%��B&  B&ffB&��B'33B'��B(  B(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2ffB2��B3��B3��B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B;��B<  B<ffB<��B=33B=��B>  B>ffB>��B?33B?��B@  B@ffB@��BA33BA��BB  BBffBB��BC33BD  BD  BD��BE33BE��BF  BFffBF��BG33BG��BH  BHffBH��BI33BI��BJ  BJffBJ��BK33BK��BL  BLffBL��BM33BM��BN  BNffBN��BO33BO��BP  BPffBP��BQ33BQ��BR  BRffBR��BS33BS��BT  BTffBT��BU33BU��BVffBVffBW33BW��BX  BXffBX��BY33BY��BZ  BZffBZ��B[33B[��B\  B\ffB\��B]33B]��B^  B^ffB^��B_33B_��B`  B`ffB`��Ba33Ba��Bb  BbffBb��Bc33Bc��Bd  BdffBd��Be33Bf  Bf  BfffBf��Bg33Bg��Bh  BhffBi33Bi��Bj  BjffBj��Bk33Bk��Bl  BlffBl��Bm33Bm��Bn  BnffBn��Bo��Bp  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 111111111111111111111111111111141111111111111111111111111111114111111111111114111111111111111111111111111111111111114111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411114111114111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    >�\)@>�R@�\)@�\)A�A/�AO�Ao�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B#�B+�B3�B;�BC�BK�BS�B[�Bc�Bk�Bs�B{�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B�B���B���B���B���B���B���B���C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CG��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C]{C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qC�}qD >�D ��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D	>�D	��D
>�D
��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D>�D��D >�D ��D!>�D!��D">�D"��D#>�D#��D$>�D$��D%>�D%��D&>�D&��D'>�D'��D(>�D(��D)>�D)��D*>�D*��D+>�D+��D,>�D,��D->�D-��D.>�D.��D/>�D/��D0>�D0��D1>�D1��D2>�D2��D3>�D3��D4>�D4��D5>�D5��D6>�D6��D7>�D7��D8>�D8��D9>�D9��D:>�D:��D;>�D;��D<>�D<��D=>�D=��D>>�D>��D?>�D?��D@>�D@��DA>�DA��DB>�DB��DC>�DC��DD>�DD��DE>�DE��DF>�DF��DG>�DG��DH>�DH��DI>�DI��DJ>�DJ��DK>�DK��DL>�DL��DM>�DM��DN>�DN��DO>�DO��DP>�DP��DQ>�DQ��DR>�DR��DS>�DS��DT>�DT��DU>�DU��DV>�DV��DW>�DW��DX>�DX��DY>�DY��DZ>�DZ��D[>�D[��D\>�D\��D]>�D]��D^>�D^��D_>�D_��D`>�D`��Da>�Da��Db>�Db��Dc>�Dc��Dd>�Dd��De>�De��Df>�Df��Dg>�Dg��Dh>�Dh��Di>�Di��Dj>�Dj��Dk>�Dk��Dl>�Dl��Dm>�Dm��Dn>�Dn��Do>�Do��DpEDp��Dq>�Dq��Dr>�Dr��Ds>�Ds��Dt>�Dt��Du>�Du��Dv>�Dv��Dw>�Dw��Dx>�Dx��Dy>�Dy��Dz>�Dz��D{>�D{��D|>�D|��D}>�D}��D~>�D~��D>�D��D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�)D�_\D\D��\D�\D�_\Dß\D��\D�\D�_\Dğ\D��\D�\D�_\Dş\D��\D�\D�_\DƟ\D��\D�\D�_\Dǟ\D��\D�\D�_\Dȟ\D��\D�\D�_\Dɟ\D��\D�\D�_\Dʟ\D��\D�\D�_\D˟\D��\D�\D�_\D̟\D��\D�\D�_\D͟\D��\D�\D�_\DΟ\D��\D�\D�_\Dϟ\D��\D�\D�_\DП\D��\D�\D�_\Dџ\D��\D�\D�_\Dҟ\D��\D�\D�_\Dӟ\D��\D�\D�_\Dԟ\D��\D�\D�_\D՟\D��\D�)D�\)D֟\D��\D�\D�_\Dן\D��\D�\D�_\D؟\D��\D�\D�_\Dٟ\D��\D�\D�_\Dڟ\D��\D�\D�_\D۟\D��\D�\D�_\Dܟ\D��\D�\D�_\Dݟ\D��\D�\D�_\Dޟ\D��\D�\D�_\Dߟ\D��\D�\D�_\D��\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\DD��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D�\D��\D�)D�_\D�\D��\D�\D�_\D�\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��\D�\D�\)D��\D��\D�\D�_\D��\D��\D�\D�_\D��\D��D�5�@�G�A
=A��A�{B#�BI�
Bo�
B�.B�B��{B���B�B��Cu�C��CL�C &fC(��C4u�C>ٙCH��CQ��C[�Cf��Cp=pCzs3C�0�C��C�Q�C�&gC�p�C�8RC�!HC�5�C�C�RC�+�C���C�g�C�EC���D	�fD�D"��D/!GD;��DHfDT�RD`��Dm��Dz�D�YHD�~fD��{D��D�K3D��D�� D� RD�O�D��fD��
D���D�PRD�~fD�ҏD�
�D�I�D�)D���D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=��
=��
>8Q�=��
=��
>8Q�=��
>8Q�=��
=��
=��
=��
>8Q�=��
>�\)>8Q�=��
>8Q�>8Q�=��
=��
>8Q�=��
>8Q�>8Q�>8Q�>8Q�>8Q�=��
=��
=��
G�O�=��
=��
=��
>8Q�>8Q�=��
>8Q�>\>8Q�>8Q�=��
=��
=��
>8Q�=��
>8Q�>8Q�>�\)=��
>�\)>8Q�=��
>8Q�>8Q�>8Q�>\>�\)>�\)>8Q�>\G�O�=��
=��
=��
=��
>8Q�>8Q�=��
=��
=��
>8Q�=��
>8Q�>8Q�=��
G�O�=��
=��
=��
>8Q�>8Q�=��
=��
>�\)>8Q�=��
>8Q�=��
=��
>8Q�=��
>8Q�>8Q�=��
=��
=��
>8Q�=��
>8Q�=��
=��
>8Q�=��
=��
=��
>8Q�=��
=��
>8Q�>�\)>8Q�=��
>�\)>8Q�G�O�=��
>8Q�>8Q�=��
>8Q�=��
>8Q�>�\)>8Q�>8Q�=��
=��
>8Q�>8Q�=��
>8Q�>8Q�>�\)>8Q�=��
>8Q�=��
=��
>8Q�>8Q�=��
>8Q�=��
>8Q�G�O�=��
=��
=��
>8Q�>8Q�=��
>8Q�>8Q�>8Q�=��
=��
=��
>8Q�=��
>8Q�=��
=��
>8Q�=��
=��
>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�=��
>8Q�=��
=��
=��
=��
=��
>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�=��
=��
=��
>8Q�>8Q�>8Q�>�\)>�\)>\>�\)>�>\>�?z�?z�>�>�>\>�\)=��
=��
=��
=��
>�\)=��
=��
>8Q�=��
>8Q�>�\)>8Q�>8Q�>8Q�=��
>8Q�=��
=��
>8Q�>8Q�=��
>8Q�>�\)=��
=��
=��
>�\)=��
=��
>�\)=��
=��
>�\)=��
G�O�>8Q�>8Q�=��
=��
G�O�>�\)>8Q�>8Q�=��
>�\)G�O�>8Q�=��
>8Q�>8Q�>�\)=��
>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>8Q�=��
G�O�>�\)=��
=��
>�\)=��
>�\)>�\)>8Q�>8Q�=��
=��
>8Q�>8Q�>8Q�>8Q�=��
=��
=��
>8Q�=��
=��
>8Q�=��
=��
=��
=��
>8Q�>8Q�>�\)>�\)>8Q�>�\)>8Q�>�\)>8Q�>�\)>8Q�=��
=��
=��
>8Q�>8Q�>8Q�>8Q�>8Q�=��
>8Q�>�\)>�\)>8Q�>8Q�>8Q�>�\)>8Q�=��
=��
>8Q�=��
>�\)=��
>8Q�>8Q�=��
=��
>8Q�=��
>8Q�=��
=��
>8Q�=��
>8Q�>8Q�=��
=��
>�\)>�\)=��
>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>8Q�=��
=��
>8Q�>8Q�>�\)>�\)>�>�?z�?z�?.{?aG�?aG�?aG�?�=q?�=q?�
>?��
?��
?���?�=q?�
>?�
>?��
?��?�p�@�@�@�@Q�@�R@%�@+�@1�@>�R@>�R@E�@K�@Q�@XQ�@^�R@e�@k�@q�@xQ�@~�R@��\@�@���@�(�@�\)@��\@�@���@�(�@�\)@��\@�@�(�@�\)@��\@�@���@�(�@�\)@\@���@�(�@�\)@ҏ\@�@���@�\)@�\)@�\@�@�(�@�\)@�\@�@���@�(�AG�A�GAz�AzA�A	G�A
�GAzA�AG�A�GAz�A�AG�A�GAz�AzA!G�A"�GA$z�A&zA)G�A*�GA,z�A.zA1G�A2�GA4z�A6zA7�A9G�A:�GA>zA?�AAG�AB�GAFzAG�AIG�AJ�GALz�ANzAQG�AR�GATz�AVzAW�AZ�GA\z�A^zA_�AaG�Ab�GAdz�Ag�AiG�Aj�GAlz�AnzAo�AqG�Atz�AvzAw�AyG�Az�GA|z�A~zA���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�
=A��
A���A�=pA�=pA��
A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
Aģ�A�p�A�=pA�
=Aȣ�A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
AУ�A�=pA�
=A��
Aԣ�A�=pA�
=A��
Aأ�A�=pA�
=A��
Aܣ�A�p�A�
=A��
A��A�p�A�
=A��
A��A�p�A�
=A��
A��A�p�A�=pA�
=A��A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
B Q�B�B�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�BQ�BQ�B	�B	�B	�B
Q�B�B�B�BQ�B�RB�B�BQ�B�B�B�BQ�B�RB�B�BQ�B�B�B�BQ�B�RB�B�BQ�B�RB�B�BQ�B�RB�B�B�B�RB�B�B�BQ�B�RB�B�BQ�B�RB�B�B�B Q�B!�B!�B!�B"Q�B"�RB#�B#�B#�B$Q�B%�B%�B%�B&Q�B&�RB'�B'�B'�B(�RB)�B)�B)�B*Q�B*�RB+�B+�B+�B,Q�B,�RB-�B-�B.Q�B.�RB/�B/�B/�B0Q�B0�RB1�B1�B1�B2Q�B2�RB3�B3�B4Q�B4�RB5�B5�B5�B6Q�B6�RB7�B7�B7�B8Q�B8�RB9�B9�B9�B:Q�B:�RB;�B;�B<Q�B<�RB=�B=�B=�B>Q�B>�RB?�B?�B?�B@Q�B@�RBA�BA�BA�BBQ�BB�RBC�BC�BC�BD�RBE�BE�BE�BFQ�BF�RBG�BG�BG�BHQ�BH�RBI�BI�BI�BJQ�BJ�RBK�BK�BK�BLQ�BL�RBM�BM�BM�BNQ�BN�RBO�BO�BO�BPQ�BP�RBQ�BQ�BQ�BRQ�BR�RBS�BS�BS�BTQ�BT�RBU�BU�BVQ�BVQ�BW�BW�BW�BXQ�BX�RBY�BY�BY�BZQ�BZ�RB[�B[�B[�B\Q�B\�RB]�B]�B]�B^Q�B^�RB_�B_�B_�B`Q�B`�RBa�Ba�Ba�BbQ�Bb�RBc�Bc�Bc�BdQ�Bd�RBe�Be�Be�BfQ�Bf�RBg�Bg�Bg�BhQ�Bi�Bi�Bi�BjQ�Bj�RBk�Bk�Bk�BlQ�Bl�RBm�Bm�Bm�BnQ�Bn�RBo�Bo�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 111111111111111111111111111111141111111111111111111111111111114111111111111114111111111111111111111111111111111111114111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411114111114111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�O�A�Q�A�VA�ZA�^5A�\)A�\)A�\)A�ZA�\)A�\)A�^5A�^5A�bNA�bNA�dZA�dZA�bNA�dZA�dZA�hsA�hsA�ffA�ffA�ffA�ffA�hsA�hsA�jA�jA�l�A�l�A�n�A�n�A�p�A�p�A�p�A�p�A�ffA�\)A�33A�$�A�%A���A��mA�x�A�A�r�A�A�7LAŰ!A®A�bA�%A�33A���A�ƨA��wA���A�&�A�A��A���A�C�A��A��A���A��yA���A�9XA���A�XA��RA��;A�VA�VA��
A�`BA��\A���A��RA��A���A��A��A�ȴA��mA��A��A���A��;A�^5A�E�A��A��+A��;A�p�A� �A�  A�A�A�n�A��PA���A�ȴA���A�ZA���A�&�A��A�A�A�JA`BA~bA|M�A{�Az�HAzQ�AzJAyl�Ax  As�;AqO�An1Al1'Ai�Af��Ad�yAc&�A`jA^  AY��AVȴAVAU��AU�7AT��AS�FARbNAPA�AM�AJffAH1AG��AF��AEƨAD~�ABbA?��A=�FA=\)A<��A<�A;�-A:�jA:  A8jA7t�A7G�A6�9A5��A4��A49XA3�A37LA3VA2n�A1A1;dA17LA0��A.��A.n�A.-A,�A+��A*��A*9XA)S�A(�+A'�A&ĜA%p�A$9XA"�yA!t�A �RA �+A�
A/A`BAp�AQ�AA+AI�At�A;dA�A-A��A�PA��A~�An�AJA��AS�A��A�PA�#A	dZA�A^5AQ�A-A�7A n�@��T@��y@���@��`@���@�{@�V@�r�@� �@��@�9@�P@�l�@�S�@�+@�ff@���@�u@�$�@��/@睲@�E�@�p�@�9X@�(�@� �@�b@��@�5?@�@��@�{@�C�@�t�@�+@�R@��#@���@�C�@��@噚@�&�@��`@��
@�^5@��@�@�r�@�  @ߝ�@�@�bN@��@�/@؛�@ם�@��@�l�@Ѻ^@�`B@�dZ@�  @ҸR@��#@ѡ�@�Z@�t�@�"�@���@·+@�E�@ͺ^@�?}@���@̣�@�j@�;d@ʧ�@�7L@�I�@ǝ�@���@Ɨ�@�@���@��@�~�@��H@�;d@��\@�ff@�E�@�J@��@��w@�~�@��@�G�@��@�Z@��
@���@�;d@��H@��+@�M�@�@�V@��9@�A�@��P@��@�J@�&�@��/@�A�@�\)@��\@���@�X@�&�@��D@��
@�|�@�"�@���@��+@��@���@���@��h@��`@��u@�z�@��@�\)@�|�@���@��w@�ƨ@��w@���@�\)@�o@�ȴ@��7@�p�@���@��h@�O�@���@�z�@�Q�@�9X@���@�33@���@��\@�=q@�{@��@��@�O�@�O�@���@�x�@��h@�hs@���@�V@���@�  @��@�|�@��P@��P@��P@��P@��P@��P@��@�K�@�33@��R@�n�@�n�@�v�@�@�t�@�"�@��H@��H@�
=@��@�Z@�`B@���@�J@��-@��h@��@��D@�9X@��@�o@�o@�o@��H@��R@��!@���@�ȴ@�v�@�-@�{@�p�@���@���@�A�@��m@��@�+@���@���@��R@���@�n�@�-@���@���@�p�@�%@��`@���@���@��D@���@�l�@�K�@�;d@�+@�@��!@���@�~�@�^5@�V@�M�@�5?@�@���@��T@���@�p�@��@�V@��`@��9@��D@�j@�Q�@��
@��w@��@��P@�;d@��R@�v�@�=q@�{@��@���@�O�@�/@���@��@��u@�Q�@�1@��;@���@��@��@�v�@���@���@�x�@�G�@�7L@�7L@�&�@��`@�r�@�I�@��@�b@�@|�@l�@K�@~��@~��@~$�@}��@|��@{�
@{��@{��@{��@{dZ@{@z��@z��@z=q@y�@xr�@w�@wl�@v�@vE�@v$�@uO�@t�@tz�@s�
@sC�@r�\@q��@qG�@q�@p�`@p�u@p�@pbN@p1'@o�w@o|�@n��@m��@l9X@k��@k"�@k"�@k"�@j~�@i��@i�7@i7L@hQ�@g�w@gl�@g;d@g
=@fȴ@f5?@e�@e@e��@e�h@ep�@eO�@eV@d�j@d�@dz�@dz�@dj@dZ@dI�@dI�@d(�@cƨ@cdZ@b��@a�^@a7L@`r�@_��@_�@_;d@^��@^��@^��@^�@^��@^�+@^ff@^V@]@]`B@]?}@]?}@]�@]�@\�/@\�j@\Z@\�@[ƨ@[��@[C�@["�@Z��@Z��@Z~�@Zn�@Zn�@Z^5@Z=q@Y�@Y�#@Y�#@Y�#@Y�#@Y�#@Y�#@Y��@Y��@Y�^@Y�7@Y�@X��@XĜ@XĜ@X�@W�P@V�y@V�+@V$�@V@V@V@V@U�@U��@U�h@U`B@U`B@U`B@U/@T1@SdZ@R��@Rn�@RJ@Q��@Q�#@Q��@Q�^@Qhs@Pr�@O
=@NE�@N@M��@MO�@MV@MV@MV@MV@L��@Kƨ@K�@KC�@K@J�H@J��@J��@J��@J~�@J�@I��@H��@H��@H�@Hr�@Hr�@HbN@HQ�@H1'@Hb@G�@G\)@GK�@F�@E�-@EO�@EO�@D��@D�/@D��@D��@D��@D�D@Dz�@DZ@D�@C��@CC�@C@B��@B~�@BM�@Ax�@A�@@�`@@�u@?��@?+@>ȴ@>��@>��@>�+@>$�@>@=�-@=p�@<�/@<z�@<z�@<I�@<9X@<(�@<1@;�
@;ƨ@;t�@;@:�@9�^@9X@9&�@8�@8  @7�;@7�;@7�;@7l�@6�y@6�R@6��@6�+@6�+@6ff@5�@5��@5�-@5�-@5��@5�h@5�@5?}@4��@4�@4��@4I�@3��@3ƨ@3�F@3��@3dZ@2��@2n�@2^5@2M�@2=q@2�@2J@1��@1G�@1�@0�`@0r�@0b@/|�@.�R@.{@-�T@-`B@-p�@-p�@-p�@-`B@-O�@-O�@-O�@-O�@-O�@,��@*�\@*=q@)�#@)7L@(��@(�@'�@'+@'
=@&��@&�y@&�@&V@%�T@%��@%O�@$��@$�/@$��@$��@$�j@$�j@$��@$�D@$z�@$Z@#��@#�F@#�F@#�F@#��@#S�@#o@"�H@"^5@"�@!�#@!��@!x�@!X@!7L@!7L@!�@!�@!�@!%@!%@ ��@ �`@ ��@ �9@ ��@ ��@ ��@ ��@ �u@ �u@ bN@�;@\)@�@�@
=@��@�@�R@��@��@�+@�+@�+@�+@ff@ff@5?@{@{@$�@�@��@p�@/@�@��@��@dZ@S�@S�@S�@S�@C�@33@"�@@�@��@-@�^@hs@G�@7L@��@Ĝ@�u@r�@�@l�@�@�@ȴ@E�@$�@{@{@�T@�h@/@j@I�@I�@1@ƨ@o@n�@hs@Ĝ@r�@�@;d@�@�+@ff@�T@`B@?}@/@�@/@V@�@��@�@��@z�@(�@��@�m@�F@��@C�@o@
�H@
��@
��@
��@
^5@
J@	�^@	x�@	G�@�`@�@Q�@A�@1'@1'@ �@ �@ �@1'@1'@A�@A�@A�@A�@A�@A�@A�@A�@A�@1'@�@�w@�@�@�w@�w@�@�w@�w@�@��@\)A�VmA�]dA�[WA�bNA�c�A�e�A�k�A�o5A�_;A��yA�ޞA��A�33A�P}A��A�6FA��A�7�A�+A���A�ǮA�:A��$A|N<Ay�6Alk�A`rGAUںAP�AG#�A@u�A:֡A7�A3P�A1-wA-v�A)�A�A,=@�x�@��@�C-@��X@���@�Y@��@�X�@�C@�E9@�;@��@{�*@p��@e�@^��@Zl�@U��@M!�@G��@@�U@92a@4G@-rG@$��@!(�@�F@�@��@��@�@n/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A���A�\)A�
=A�"�A��A�  A���A�+Aȧ�A��
A�A��A�$�A��AͰ!A��
A��#A˙�ÃA���A˾wA��A�-A�+A�VA�33Aɩ�A�E�A�S�A��#A�1A�;dAǡ�A��A��A��A��A��A���A̴9A�A�jA�|�A���A�=qA�(�A�&�A�-A�$�A� �A���A�|�A���A�33A�(�A�&�A�33A�+A�"�A���A�;dA�9XA��A���A�|�A͟�A�oA��mA�jA�1'A�ȴA˰!A�"�A��A��TA���A��A˺^A��A�A�/A� �Aͧ�A�{A�ȴA���A�jA�I�A�r�A�VAͺ^Aͧ�A�r�A���A�-A�+A� �A�^5A�hsA�M�A�ĜA�C�A͗�A̴9A�9XA˟�A�C�A��;A�33A�-A�(�A�  A���Aͺ^A�1A�/A� �A�
=A�ffA�hsA�oA��A�&�A��A���A�;dA͓uA���A���A˅Aʧ�A�(�A�&�A���A��#A��TA���AͼjA�JA͙�Aɧ�A̸RA�{A�hsA��
A�bA���A���A͝�A�JA�C�A�/A���A�33A̺^A�/A���A�K�A�+A̧�AȋDA�C�A�&�A�A�A�ƨA� �A��yA�/A�/A�"�A��A��HAͥ�A�x�A˺^A�"�A��A͙�Aˣ�A�1'A�+A�+A�5?A�"�A���A˴9A�t�A̧�A��`A͓uA�+A� �A�7LA�5?A�-A�/A�-A�+A�/A�-A�-A�(�A��A��;A��mA�C�A�;dȦ+A�^5Ȁ\A�bAͺ^A��`A�$�A�+A� �A��
A�VA��#A��;A;wA��#A͍PA;wA���A��A��/A��A�t�A��HA��A�ZA�XA� �Aͺ^A��A���A�-A���A�A�oA͑hA�bA�G�A��A�33A�(�A�&�A�oA�=qA�
=A�JA�C�A�|�A��A�C�A͑hA�+A�33A�33A�&�A�33A�+A�VA�A�A�A�-A�$�A�bA��A�"�A�XA�{A�;dA�-A��yA�O�A�r�A�"�A��A�9XA�/A�ĜA̅A���A�(�AͬA�n�A͝�A�ƨA�+A�9XA�jA�;dA�;dA�7LA�?}A�?}A�=qA�9XA�9XA�7LA�&�AͮA�33A��A�&�A��mA��;A�7LA�/A��A�n�A�9XA�=qA�5?A��mA��A�7LA�33A��A�x�A�1'A���A̓A͍PA͟�A�hsA�
=A��A�?}A��#A�"�A�bA�K�A�9XA�(�A�%A���A��A�C�A���A�XA�-Aʹ9A�~�A�-A��A��A�9XA�33A�-A��A��HA��A���A��A��;A�33A�=qA�=qA�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�A�A�?}A�=qA�?}A�C�A�E�A�K�A�G�A�G�A�G�A�I�A�I�A�K�A�O�A�M�A�I�A�M�A�Q�A�S�A�S�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�Q�A�O�A�O�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�M�A�M�A�O�A�Q�A�Q�A�M�A�M�A�O�A�O�A�Q�A�XA�ZA�XA�VA�S�A�Q�A�Q�A�VA�ZA�ZA�ZA�XA�S�A�VA�XA�S�A�VA�ZA�VA�ZA�XA�ZA�ZA�XA�XA�\)A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�^5A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�\)A�\)A�\)A�^5A�^5A�^5A�\)A�ZA�ZA�ZA�\)A�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�ZA�ZA�ZA�\)A�\)A�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�^5A�^5A�^5A�^5A�^5A�`BA�`BA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�bNA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�bNA�dZA�bNA�dZA�dZA�ffA�ffA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�ffA�dZA�dZA�dZA�bNA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�dZA�dZA�bNA�bNA�ffA�ffA�dZA�dZA�ffA�dZA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�hsA�jA�hsA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�dZA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�hsA�ffA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�jA�jA�jA�hsA�hsA�hsA�hsA�hsA�hsA�jA�jA�jA�l�A�l�A�l�A�jA�jA�jA�jA�jA�jA�jA�jA�l�A�l�A�l�A�l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    A�1'A�O�A�Q�A�VA�ZA�^5A�\)A�\)A�\)A�ZA�\)A�\)A�^5A�^5A�bNA�bNA�dZA�dZA�bNA�dZA�dZA�hsA�hsA�ffA�ffA�ffA�ffA�hsA�hsA�jA�jA�l�A�l�A�n�A�n�A�p�A�p�A�p�A�p�A�ffA�\)A�33A�$�A�%A���A��mA�x�A�A�r�A�A�7LAŰ!A®A�bA�%A�33A���A�ƨA��wA���A�&�A�A��A���A�C�A��A��A���A��yA���A�9XA���A�XA��RA��;A�VA�VA��
A�`BA��\A���A��RA��A���A��A��A�ȴA��mA��A��A���A��;A�^5A�E�A��A��+A��;A�p�A� �A�  A�A�A�n�A��PA���A�ȴA���A�ZA���A�&�A��A�A�A�JA`BA~bA|M�A{�Az�HAzQ�AzJAyl�Ax  As�;AqO�An1Al1'Ai�Af��Ad�yAc&�A`jA^  AY��AVȴAVAU��AU�7AT��AS�FARbNAPA�AM�AJffAH1AG��AF��AEƨAD~�ABbA?��A=�FA=\)A<��A<�A;�-A:�jA:  A8jA7t�A7G�A6�9A5��A4��A49XA3�A37LA3VA2n�A1A1;dA17LA0��A.��A.n�A.-A,�A+��A*��A*9XA)S�A(�+A'�A&ĜA%p�A$9XA"�yA!t�A �RA �+A�
A/A`BAp�AQ�AA+AI�At�A;dA�A-A��A�PA��A~�An�AJA��AS�A��A�PA�#A	dZA�A^5AQ�A-A�7A n�@��T@��y@���@��`@���@�{@�V@�r�@� �@��@�9@�P@�l�@�S�@�+@�ff@���@�u@�$�@��/@睲@�E�@�p�@�9X@�(�@� �@�b@��@�5?@�@��@�{@�C�@�t�@�+@�R@��#@���@�C�@��@噚@�&�@��`@��
@�^5@��@�@�r�@�  @ߝ�@�@�bN@��@�/@؛�@ם�@��@�l�@Ѻ^@�`B@�dZ@�  @ҸR@��#@ѡ�@�Z@�t�@�"�@���@·+@�E�@ͺ^@�?}@���@̣�@�j@�;d@ʧ�@�7L@�I�@ǝ�@���@Ɨ�@�@���@��@�~�@��H@�;d@��\@�ff@�E�@�J@��@��w@�~�@��@�G�@��@�Z@��
@���@�;d@��H@��+@�M�@�@�V@��9@�A�@��P@��@�J@�&�@��/@�A�@�\)@��\@���@�X@�&�@��D@��
@�|�@�"�@���@��+@��@���@���@��h@��`@��u@�z�@��@�\)@�|�@���@��w@�ƨ@��w@���@�\)@�o@�ȴ@��7@�p�@���@��h@�O�@���@�z�@�Q�@�9X@���@�33@���@��\@�=q@�{@��@��@�O�@�O�@���@�x�@��h@�hs@���@�V@���@�  @��@�|�@��P@��P@��P@��P@��P@��P@��@�K�@�33@��R@�n�@�n�@�v�@�@�t�@�"�@��H@��H@�
=@��@�Z@�`B@���@�J@��-@��h@��@��D@�9X@��@�o@�o@�o@��H@��R@��!@���@�ȴ@�v�@�-@�{@�p�@���@���@�A�@��m@��@�+@���@���@��R@���@�n�@�-@���@���@�p�@�%@��`@���@���@��D@���@�l�@�K�@�;d@�+@�@��!@���@�~�@�^5@�V@�M�@�5?@�@���@��T@���@�p�@��@�V@��`@��9@��D@�j@�Q�@��
@��w@��@��P@�;d@��R@�v�@�=q@�{@��@���@�O�@�/@���@��@��u@�Q�@�1@��;@���@��@��@�v�@���@���@�x�@�G�@�7L@�7L@�&�@��`@�r�@�I�@��@�b@�@|�@l�@K�@~��@~��@~$�@}��@|��@{�
@{��@{��@{��@{dZ@{@z��@z��@z=q@y�@xr�@w�@wl�@v�@vE�@v$�@uO�@t�@tz�@s�
@sC�@r�\@q��@qG�@q�@p�`@p�u@p�@pbN@p1'@o�w@o|�@n��@m��@l9X@k��@k"�@k"�@k"�@j~�@i��@i�7@i7L@hQ�@g�w@gl�@g;d@g
=@fȴ@f5?@e�@e@e��@e�h@ep�@eO�@eV@d�j@d�@dz�@dz�@dj@dZ@dI�@dI�@d(�@cƨ@cdZ@b��@a�^@a7L@`r�@_��@_�@_;d@^��@^��@^��@^�@^��@^�+@^ff@^V@]@]`B@]?}@]?}@]�@]�@\�/@\�j@\Z@\�@[ƨ@[��@[C�@["�@Z��@Z��@Z~�@Zn�@Zn�@Z^5@Z=q@Y�@Y�#@Y�#@Y�#@Y�#@Y�#@Y�#@Y��@Y��@Y�^@Y�7@Y�@X��@XĜ@XĜ@X�@W�P@V�y@V�+@V$�@V@V@V@V@U�@U��@U�h@U`B@U`B@U`B@U/@T1@SdZ@R��@Rn�@RJ@Q��@Q�#@Q��@Q�^@Qhs@Pr�@O
=@NE�@N@M��@MO�@MV@MV@MV@MV@L��@Kƨ@K�@KC�@K@J�H@J��@J��@J��@J~�@J�@I��@H��@H��@H�@Hr�@Hr�@HbN@HQ�@H1'@Hb@G�@G\)@GK�@F�@E�-@EO�@EO�@D��@D�/@D��@D��@D��@D�D@Dz�@DZ@D�@C��@CC�@C@B��@B~�@BM�@Ax�@A�@@�`@@�u@?��@?+@>ȴ@>��@>��@>�+@>$�@>@=�-@=p�@<�/@<z�@<z�@<I�@<9X@<(�@<1@;�
@;ƨ@;t�@;@:�@9�^@9X@9&�@8�@8  @7�;@7�;@7�;@7l�@6�y@6�R@6��@6�+@6�+@6ff@5�@5��@5�-@5�-@5��@5�h@5�@5?}@4��@4�@4��@4I�@3��@3ƨ@3�F@3��@3dZ@2��@2n�@2^5@2M�@2=q@2�@2J@1��@1G�@1�@0�`@0r�@0b@/|�@.�R@.{@-�T@-`B@-p�@-p�@-p�@-`B@-O�@-O�@-O�@-O�@-O�@,��@*�\@*=q@)�#@)7L@(��@(�@'�@'+@'
=@&��@&�y@&�@&V@%�T@%��@%O�@$��@$�/@$��@$��@$�j@$�j@$��@$�D@$z�@$Z@#��@#�F@#�F@#�F@#��@#S�@#o@"�H@"^5@"�@!�#@!��@!x�@!X@!7L@!7L@!�@!�@!�@!%@!%@ ��@ �`@ ��@ �9@ ��@ ��@ ��@ ��@ �u@ �u@ bN@�;@\)@�@�@
=@��@�@�R@��@��@�+@�+@�+@�+@ff@ff@5?@{@{@$�@�@��@p�@/@�@��@��@dZ@S�@S�@S�@S�@C�@33@"�@@�@��@-@�^@hs@G�@7L@��@Ĝ@�u@r�@�@l�@�@�@ȴ@E�@$�@{@{@�T@�h@/@j@I�@I�@1@ƨ@o@n�@hs@Ĝ@r�@�@;d@�@�+@ff@�T@`B@?}@/@�@/@V@�@��@�@��@z�@(�@��@�m@�F@��@C�@o@
�H@
��@
��@
��@
^5@
J@	�^@	x�@	G�@�`@�@Q�@A�@1'@1'@ �@ �@ �@1'@1'@A�@A�@A�@A�@A�@A�@A�@A�@A�@1'@�@�w@�@�@�w@�w@�@�w@�w@�@��@\)A�VmA�]dA�[WA�bNA�c�A�e�A�k�A�o5A�_;A��yA�ޞA��A�33A�P}A��A�6FA��A�7�A�+A���A�ǮA�:A��$A|N<Ay�6Alk�A`rGAUںAP�AG#�A@u�A:֡A7�A3P�A1-wA-v�A)�A�A,=@�x�@��@�C-@��X@���@�Y@��@�X�@�C@�E9@�;@��@{�*@p��@e�@^��@Zl�@U��@M!�@G��@@�U@92a@4G@-rG@$��@!(�@�F@�@��@��@�@n/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�?}A�A�A�?}A�=qA�?}A�C�A�E�A�K�A�G�A�G�A�G�A�I�A�I�A�K�A�O�A�M�A�I�A�M�A�Q�A�S�A�S�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�Q�A�O�A�O�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�M�A�M�A�O�A�Q�A�Q�A�M�A�M�A�O�A�O�A�Q�A�XA�ZA�XA�VA�S�A�Q�A�Q�A�VA�ZA�ZA�ZA�XA�S�A�VA�XA�S�A�VA�ZA�VA�ZA�XA�ZA�ZA�XA�XA�\)A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�^5A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�\)A�\)A�\)A�^5A�^5A�^5A�\)A�ZA�ZA�ZA�\)A�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�ZA�ZA�ZA�\)A�\)A�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�^5A�^5A�^5A�^5A�^5A�`BA�`BA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�bNA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�bNA�dZA�bNA�dZA�dZA�ffA�ffA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�dZA�dZA�ffA�dZA�dZA�dZA�bNA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�dZA�dZA�bNA�bNA�ffA�ffA�dZA�dZA�ffA�dZA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�hsA�jA�hsA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�dZA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�hsA�ffA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�jA�jA�jA�hsA�hsA�hsA�hsA�hsA�hsA�jA�jA�jA�l�A�l�A�l�A�jA�jA�jA�jA�jA�jA�jA�jA�l�A�l�A�l�A�l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aě�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bm�Bl�BjBn�Bp�Bu�Bw�B�B�PB�hB�
B��B��B��B��B��B�B8RBH�BP�BYB]/B\)B_;B\)BYBN�BK�BI�BM�BK�BQ�B�BJ�BK�BL�BM�BL�BD�B2-B�BB�B�)B�B�FB��B�bB�B�DB�hB�%B�B}�By�Bq�BbNBD�B-B{B
��B
�ZB
�B
�qB
��B
�uB
�=B
~�B
iyB
YB
N�B
I�B
C�B
:^B
/B
'�B
!�B
�B
�B
JB	��B	�fB	��B	�qB	�B	��B	�hB	�B	w�B	gmB	S�B	A�B	6FB	1'B	/B	+B	'�B	"�B	�B	\B	B��B��B�B�B�mB�5B��B��BɺB��B��B��BȴBŢB��B�dB�XB�FB�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�JB�=B�1B�B� B�B�B� B{�By�Br�Bo�Bl�BjBjBl�Bk�Bk�BiyBe`BdZBdZBcTBaHB`BB]/B[#B\)BZBW
BVBP�BI�BF�BL�BK�BE�B>wB<jB;dB:^B9XB:^B9XB9XB8RB7LB8RB:^B:^B9XB9XB8RB7LB7LB9XB:^B;dB=qB>wBE�BI�BL�BP�B\)BgmBp�B}�B�JB��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�RB�XB�XB�^B�qB�jB�jB�dB�XB�?B�!B�B��B�B�qBŢBÖBȴB��B��B��B��B��B�B�/B�NB�fB�mB�yB�mB�B�B�B�B�B�B�B�`B�;B�;B�B�/B��BɺBȴBǮBŢBBBĜBŢBɺB��B��B��B��B��B��B��B�B�B�#B�/B�/B�5B�5B�BB�TB�TB�`B�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	1B	1B	PB	\B	hB	uB	�B	�B	�B	�B	 �B	 �B	�B	�B	%�B	,B	0!B	1'B	33B	33B	33B	33B	49B	7LB	8RB	;dB	?}B	G�B	H�B	H�B	K�B	R�B	W
B	]/B	bNB	dZB	iyB	l�B	n�B	p�B	s�B	w�B	� B	�B	�B	�B	�%B	�+B	�1B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�RB	�qB	��B	ÖB	ĜB	ƨB	ŢB	ƨB	ƨB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�NB	�TB	�fB	�mB	�sB	�mB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
%B
B
%B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B
	7B
	7B
	7B
	7B
	7B

=B
	7B

=B
	7B

=B

=B
DB
DB

=B
DB
DB
DB
JB
JB
PB
PB
PB
VB
PB
VB
VB
VB
VB
VB
VB
VB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
VB
PB
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
\B
bB
bB
hB
hB
oB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
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
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
1'B
1'B
33B
33B
49B
49B
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
;dB
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
@�B
?}B
@�B
A�B
@�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
C�B
C�B
C�B
C�B
C�B
B�B
C�B
C�B
C�B
C�B
B�B
E�B
E�B
F�B
G�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
`BB
aHB
bNB
bNB
bNB
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
cTB
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
iyB
iyB
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
iyB
iyB
iyB
iyB
jBr|Bq�Bq�Bq�Bq�Bq�Bq�BoOBm)Bu�B��B�OBWYB[	BIBUgB#�B��B��Br�B<B
��B
WYB
/�B
4B	��B	tB	.�B	�B��B�fBżB��B��B�FB��B�NB}�BiyB8B��B�;B�B�B	�B	7�B	��B	�#B	޸B	�MB	�xB	��B
�B

�B
jB
[B
�B
IB
&fB
+B
5ZB
>(B
C�B
K)B
OB
RoB
T�B
Y�B
b�B
h
B
iDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�|�>�%Bs�>š�A<�\B�?|�B��?	�^B�>�(�@���B�;>��Bw�B��?�
B�\B\)>�ƨ?�z�BH�?M�hBjBXB[#BƨA<�>և+?Tz�A�Z>t�jA-
=>B�\>���B?}B�>��TBL�B�B��B>-V>hr�>�K�B�@l�BXBgmBt�A���B�BW
>�ffBN�Bq�BffBr�Bm�Bv�B1'B�>k�A���AT�\?.{A�B
hsB6F? ĜA�S�?���B  ? A�Bt�B�A�K�>��^@;�?�P>�ZBr�B�By�A�jB��B�R?$��B��?^5?>�/BZ?+B�uB��>�V?]�?�S�B?�B�}>r�!>���A�t�Bt�A�K�>���Bgm>`A�A��B
=Bs�B]/?���B��B��>�K�B.Bn�BT�>�"�BJ�?�;dB
	7Bx�B~�B�>�V?/�By�BL�>`A�B��Bk�B�B�B��B��>Õ�B
�7B1B�>�
=Bq�BD�B�>�ƨ?��P?|j@T�B{�B
��Bp�By�B�3B	{>�7LBE�>�`BB�X>�n�Bk�>o��A�l�B@�>��?/\)Bz�BcTBp�BK�B�B|�A7oB?M��>�-B�^AZ��A���B1Bp�BjB/Bq�B
ŢBY?�D>��/?�yAK�B	9XBiyBp�Bo�Bs�Bn�Br�Br�Bo�Br�Bq�Br�Bt�B�VBL�>�?}?��?&$�>�E�B��>C��>�5?A)�B	�Bl�Bu�B�7B&�BB�>�A�B�V>ۥ�@z��Bm�B�d>���Bw�BH�>�%B\)?n�B��?��@�RB��A�+A�G�B��>5?}>ɺ^B
�-B�?2�@	G�>�9XBk�Br�Bn�A��;B��>�x�B��>���B	@�Be`B��?4�jB	p�BjBA�BdZBgmBn�B~�BD�A�j>{�m?&ffB�Bp�?f�yBƨ>��yBo�Bv�B
��B�>�Z?���Bo�BiyB
�mB_;A�&�>��+A��B� A7L?��B|�?���?��u?.�@D(�BaHBjBm�Bo�B^5Bo�B\)Bs�B
�RB��B��>t�jAxĜ?JB��B�jB
�9Bx�B�J?��^B]/Bt�B�o?��B
ȴBk�B{�B
\)A��>��B��>��/B�A�p�B
�B�`?$�>ǮB0!?�!Bɺ?9X?T�jBm�?2�!B��B
,B�T>�Br�B��>��B	�B�B�PBJB]/B7LBw�BcTB��@�V>�>���Bl�B>wBq�Bo�Bq�Br�Bs�Br�Br�Bq�Br�Bq�Br�Bs�Br�Br�Br�Br�Br�Br�Br�Bp�Br�Bp�Bq�Bt�Bs�Bq�Bq�Bq�Bq�Bq�Br�Bq�Br�Br�Br�Br�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Br�Br�Br�Br�Br�Bp�Bp�Bq�Bq�Bq�Br�Br�Bq�Bq�Br�Br�Br�Bq�Br�Bp�Bo�Bq�Br�Br�Bq�Br�Bp�Bp�Bn�Bp�Bq�Bs�Br�Bq�Br�Bs�Bq�Bo�Bq�Bp�Bq�Br�Bq�Br�Bp�Bo�Bq�Bq�Bq�Bq�Br�Br�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Bq�Bq�Br�Br�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Br�Bq�Bq�Br�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bq�Bq�Bp�Bq�Br�Bq�Bq�Bq�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bq�Bq�Bq�Bq�Br�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Br�Bq�Br�Br�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bp�Bq�Br�Br�Bq�Bq�Bp�Br�Bq�Bq�Bq�Br�Bq�Bq�Br�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Bq�Br�Bq�Br�Br�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Br�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Br�Bq�Br�Br�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Br�Br�Bq�Bq�Bq�Br�Br�Bq�Bq�Br�Bp�Br�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Br�Bq�Br�Bs�Br�Bq�Br�Br�Bq�Bq�Br�Bq�Bq�Bq�Br�Br�Bq�Br�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Bq�Br�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bp�Bp�Bq�Bq�Bq�Bq�Bp�Bq�Br�Bq�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bp�Bq�Bp�Bp�Bq�Bq�Bq�Bq�Bp�Bp�Bq�Bp�Bq�Bq�Bp�Bp�Bq�Bq�Bp�Bq�Bp�Bq�Bq�Bq�Br�Bq�Bp�Bp�Bq�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�AΣ�AμjAθRAΉ7AΥ�AΕ�AΛ�A�ffAΟ�A�p�A�hsA�dZAΑhA�ZA�x�A�33A�7LA�33A�\)A�1'A�A�A�E�A�VA�jA�XA�XA� �A�5?A�"�A��A͓uAʹ9A͋DA�t�A�S�A�/A�1A�ĜA̮A�bNA�VAˍPA�1A�A�{A�VA��`Ať�A�ĜA��A��jA��A�7LA��^A�C�A��A���A�p�A���A�{A��A���A�jA� �A�1A�ƨA��!A��uA���A��uA�v�A���A��!A�jA��7A���A��/A��uA��RA��A���A���A���A��HA�l�A���A��RA���A���A�hsA��A���A�A��PA�ZA���A��HA���A��A�v�A�
=A��A�I�A���A�O�A~��A}�A|E�A{hsAz-Ax�9AwAuC�At5?Ar�\Aq%Ao%Al��Aj  Af�/Ad(�AaVA^�`A\�/A[;dAY&�AWdZAUAS;dAQ?}AQVAPI�AN�AL��AK�PAI�
AG��AF��AEXACp�ABȴAA�AAC�A@  A?`BA=O�A<�jA<$�A:��A:A8��A8M�A6�RA5��A4jA4�\A3p�A3�A2ZA1�A0�A0�+A/�A/S�A/�A.M�A-+A,^5A+\)A)��A)&�A'�A'�A%ƨA$�!A$VA#\)A"ĜA!��A M�A�;A(�AK�AVA�
An�A�
A��A�A�A�/AM�AdZAv�AA�Ax�AjA�PA9XA��AdZA�+A��A
�9A	+Av�A�AƨA�A��A�#A E�@�5?@���@�Z@�hs@�33@�E�@��@���@��@��m@�@�ȴ@��@��@�ƨ@�@�1'@�t�@�@���@�p�@�9X@�D@���@�"�@�33@�G�@�~�@�E�@��@�"�@�1@�A�@�v�@��/@�(�@�C�@�dZ@�^@��m@�C�@�l�@�V@�$�@��@��m@ާ�@�-@އ+@ݲ-@��y@�ff@�X@�9X@���@Ӯ@�Z@��@�
=@��@�`B@�9X@�"�@Ь@Ϯ@Ϯ@�K�@υ@�7L@�%@��@�dZ@�ff@�z�@�{@�?}@��`@�C�@���@�r�@§�@�p�@�7L@�@��@��D@���@���@�^5@�O�@�C�@���@�M�@��@�-@��@�;d@��y@���@�~�@�dZ@�$�@�Ĝ@�Ĝ@���@�K�@��h@�?}@�?}@��@�ƨ@�;d@�r�@��\@���@���@���@�;d@�K�@��@���@��^@���@���@���@���@�p�@��`@��R@��@�n�@�1@���@�Z@�I�@�K�@��y@��H@�
=@�V@���@�1'@�M�@�|�@�E�@�{@�%@��`@���@��w@�(�@�A�@�9X@��
@�bN@��@�ȴ@�n�@�$�@�5?@�$�@�M�@�/@��u@�7L@���@���@�p�@� �@�`B@��-@��@�?}@�(�@��@�@�9X@�+@�l�@��
@�9X@�J@��@�~�@��;@��\@�(�@���@���@�1@�bN@��j@��@���@�b@�V@���@��@���@��@�`B@��D@� �@��F@���@�C�@���@���@���@���@��D@�1'@���@��@��`@��@��;@�o@��@�;d@���@�=q@��@���@��@���@�1@���@�(�@�9X@�Z@��w@�I�@�bN@�V@�&�@���@��P@���@�^5@�/@�9X@�V@��h@�Z@���@�I�@��@��j@�b@���@��P@�r�@�ff@�b@��P@��H@�t�@�M�@��m@���@���@�{@��-@��T@�@��@�v�@���@�n�@�Ĝ@�1'@�I�@���@��@��@�33@�^5@�E�@�"�@��7@�V@�{@���@�@�7L@��@� �@~�y@��@��@��@�@}�h@zM�@|�/@{t�@|�/@{t�@{��@|I�@}/@z��@z~�@|�j@y�@y�@y�7@z�H@x1'@y&�@x�@z�@y�#@uO�@v�+@v�y@v�R@uV@t1@p�u@uV@t�@r��@s"�@r�@q��@nv�@o�@nv�@pr�@o;d@n��@o�@o��@lZ@m�T@k�
@m/@mp�@n$�@i�@m��@i&�@i�#@i&�@hr�@gl�@fȴ@eO�@f��@i7L@f��@gl�@f�+@f��@e�@e�T@d9X@d�@b�H@b��@d1@g�P@a��@co@b�@`��@a��@^�R@aG�@b�@ax�@b��@a��@bn�@_|�@`A�@^��@a��@a�@`A�@^V@^ȴ@`��@_l�@]p�@^@_;d@\j@_�P@]V@]�h@Z�@[�
@[��@[�
@Y7L@[��@Y�#@[��@X��@Z�@Y%@Y%@Z~�@Y&�@Vȴ@Y�@[C�@Z��@X�@Vȴ@Y�@V�y@V$�@Y��@U`B@W;d@Y�@X�u@Xr�@V�@Vff@V@Up�@W|�@U��@U�h@T1@Q7L@Tj@V�+@Rn�@V��@Rn�@Rn�@T�j@R�@PbN@Q�^@PbN@O�@P�u@O�@L�/@M@O;d@N@M��@L��@L�@L��@L��@Lz�@M�-@I�#@NV@M�h@KC�@K��@J�\@I�7@J��@Gl�@HQ�@FV@H  @G�P@G�@L�@H�u@Fȴ@HĜ@E?}@E��@HQ�@F@Ep�@D�D@D9X@E��@E�@Fff@B��@Ep�@C�F@Cƨ@A�7@A�@DI�@A��@B~�@C�@>��@?K�@BJ@@�u@?
=@@�9@=@BM�@?
=@?�P@<��@>�+@?|�@;ƨ@;C�@=/@;"�@;�@>@<�D@<��@<(�@<I�@<��@9G�@;dZ@:�!@;�@8�9@8�9@8Q�@:��@;dZ@6�R@:=q@8��@9�7@6ȴ@:-@7K�@7�;@5O�@7+@9�7@9hs@7K�@6v�@6{@7+@5?}@5��@5�@2�\@8��@3�
@4I�@4�j@6ȴ@5�T@7��@5`B@4�@6V@2-@3t�@2��@3@0��@2��@2-@3��@2J@17L@.E�@1X@1�7@0b@-��@0�`@0�@/�w@.�R@.$�@,�D@-p�@+S�@-p�@.v�@.@+t�@+��@,�D@,�D@+��@/
=@,��@+�F@+"�@)��@)�7@(��@%��@&�@&{@%�@&�@(1'@'�@%@%��@$1@$z�@&�@$�@%�@"�@#S�@!�@#�F@"��@$�j@&@"��@"n�@#��@#�m@"-@#�@!%@!�^@!G�@$9X@ȴ@��@ȴ@"�\@!��@{@ �`@!hs@ Q�@#�F@"^5@"��@�y@�@�w@l�@v�@   @!hs@$�@�y@ 1'@l�@�y@!%@ �@�
@"�!@�w@o@7L@(�@��@��@{@J@{@��@��@@�D@9X@j@5?@�@��@��@@&�@��@bN@M�@�w@��@�
@�
@�H@�m@ƨ@1'@  @r�@1@��@ff@��@/@;d@ƨ@�@/@��@��@�@o@�T@�@(�@n�@��@��@�!@�\@�@%@�h@bN@�@$�@|�@|�@��@S�@$�@��@33@V@
�@	�@	7L@@;d@\)@��@	X@	��@z�@��@
M�@	X@	�@	�@;d@	�@��@�P@ff@	7L@{@	7L@ff@�@	X@��@ff@dZ@��@�/@5?@@O�@V@�@�;@��@�@�@+@@�@�u@�@�-@	%@�@�@$�@@��@@�/@�@�@��@�@�D@��@p�@1'@�@��A͢�A�UgA�UgA��A�g8A�UgA��dA��jA�+�A�D3A��A���A���A���A���A�>A��lA��_A�qAA�	7A�:A�$A{N�Ata�An|A\�AUe�AN-�AFVA?�"A=H�A5�#A1��A.�A) \A&�XA$r�A֡A	�@�?}@�1@��@�@�@�iD@���@���@�>B@�<�@��:@���@���@x,=@x,=@\�@Z6�@bkQ@O;d@I��@Ap�@;�m@9 \@+6z@+6z@kQ@:�@:�@�)@�@خ@�@�DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AΑhA�ffA�ffA��;A�ffA�ffA�VA�ffA��Aͣ�A�C�A��Aͣ�Aͣ�Aͣ�A��;A�VA�VAͣ�A���A��;A�ffAͣ�A�ffA��;A�VA�VA��Aͣ�A�ffAΑhA��;A�VA�ffA�VA�ffA�VA�ffA��;Aͣ�Aͣ�A��;A�ffA�ffA�ffA��Aͣ�A��A��;Aͣ�A��A��;A�ffAͣ�A�ffA�ffAͣ�A��A�ffA��;A�VA�VA��;A�ffA�VA���A�VAͣ�A�C�A���A��;A��AΑhAΑhA��;A�ffA�ffAͣ�A�VAͣ�Aͣ�A��;A��;A��;A�ffA��;A�ffA�ffA�VA�VA���A��A��;A��Aͣ�A�ffAͣ�Aͣ�A�C�A�VA��;A��A��AΑhA�VA�ffA��;Aͣ�A��;A�ffA�VAΑhA��;AΑhAΑhAͣ�A��Aͣ�A��Aͣ�A��A��A�VAͣ�A��AΑhA�VA�VAͣ�AΑhA���A�VA��A��;A�ffA�ffA��Aͣ�A��;A��A��;A�VAΑhA��A��A��;A�ffAͣ�Aͣ�A�ffAͣ�A��A��A��A�ffAͣ�Aͣ�A�VAͣ�Aͣ�Aͣ�Aͣ�Aͣ�Aͣ�A��;AΑhAͣ�A��A��A�VA��;AΑhAͣ�A�VA�ffAΑhA�VAͣ�A��A�ffA��A��;A���A���A�ffAͣ�A�ffA�VAͣ�Aͣ�A�1A��;A��A���A��Aͣ�AΑhAΑhAΑhAͣ�A��;A�ffA�VA��;A���A���Aͣ�Aͣ�AΑhAΑhA���Aͣ�A�VA��;A��;A��Aͣ�A��;A�VA�ffA��;AΑhA�ffA��A�VA�VA�ffA���AΑhA��;Aͣ�AΑhAͣ�A�ffA���A��A�1A��;Aͣ�A��A�VA��A��A��Aͣ�A��AΑhA�VA��;A�VAΑhA��A��;AΑhA��AΑhAͣ�A��;AΑhA�1A��A�VA�~�A���A��A��;A��;Aͣ�A�VA���A���AΑhA���A��A�1AΑhA���AΑhA��A�VA�1A��;A��A�VAΑhA��;A��;AΑhA�C�Aͣ�A��A�1AΑhAΑhA��AΑhA�VA��;A���A���A�VA�1A���A���A�C�A���A��Aͣ�A�VAΑhA�VA��A�1A�1A��;AΑhAΑhA�1A�1A�1A�1A�VAΑhA���A���A�VA�1A�1A���A���A�VA�1A�VA�1A�C�AΑhA�VA���A�1A���A�VA��A��;A���A�VA��A��A�VA�VAΑhA�1A�C�A��A�VA�~�A�1AΑhA�1A�VA�C�A��A�C�A�C�A���A�1AΑhA���AΑhA���AΑhA��A���A�C�A�VA�1AΑhA��AΑhA�~�A�VA��;A�1A�VAΑhA���A�VA�C�A���A�1A�VA�1A���A�1AΑhA�VA��A�VA�1A�VA�C�AΑhAΑhA��A���AΑhA�VA�VA�1A��A�1AΑhAͣ�A�VA�C�A�VA���A�C�A�1A�C�AΑhA���A�C�A�VA�1A�VA�1A���AΑhAͣ�A���AΑhAΑhAΑhA�1A�VA�~�AΑhA���A��A�VA�1AΑhA�VA��A��A�VA���A�VAΑhAΑhA��A���A���A�VA��A���A��;A���A�1A�VAΑhA�ffA�C�A�VA��A���A�VAΑhAΑhA�VA���A�~�A�C�A��AΑhAΑhAΑhAΑhAΑhA�1A�VA�1A���A�1AΑhAΑhA�VA�VA�VA��A�1A�VA��;A�VA�C�A���A���A�ffAΑhA��A��;A���A���A�1A�1A���A���A��;A�VA���AΑhAͣ�Aͣ�A�VA�C�A��;A�VA�VA�VA�1A�1AΑhA�1A��;AΑhA�VA��AΑhA�VA��A��AΑhA�~�A�VA�~�A���A�~�A��A�C�AΑhA�VAϺ^A���A��A�VA�VAΑhAΑhA��Aͣ�A�ffA��A���A�VA��A���A���A�1A��;A�~�A�VA��;A���AΑhA��A�VA�VA��A�VAΑhA��AΑhA�1A��;A��A�1A���A��;AΑhA��A���A�VA��A��A�1A�~�A�VAΑhA��;A��A��A��A���A���A�C�A�C�A���A�VA�1A���A�VA�1A�VA��AΑhA��;Aͣ�A��;A���A�VA�VAΑhA��;A�VA��AΑhA��;A���A���AΑhA��A��A���A��A�VA�1A�C�AΑhAΑhA���AΑhA�VA��;AΑhA�1A��;A�VA��;AΑhA���A�VAͣ�AΑhA��AΑhA���A��A�VA��;A��A��;A�VA��;A��;A�VA��;A���A��;A��A��A��;A��;Aͣ�A��Aͣ�A���A��;AΑhA���A�1Aͣ�A�1A�1A��;Aͣ�A��;A�VAΑhA���Aͣ�A�VAͣ�A��;A��;AΑhA��;AΑhAΑhAΑhA��Aͣ�AΑhA�VAΑhA�VA��A��;A�C�A�VA���A�VA��AΑhA���A�1Aͣ�A��;A�VA��;A��A��;A�VA��A��Aͣ�A��;A�ffA��A�1A�VA��A�VA�1AΑhA��A�VA��A�~�AΑhA��;A��A��;A���A��;Aͣ�A�VAΑhA�ffA�VA��;A�VAΑhAΑhAΑhA��;A��;A�1A��A��A��;A�VA��A��A��;A��A���A���A�1A�VA��A��A��;AΑhA��A��A��A��A�VA�VA��A��A�C�A��A�VA���A���A��A��A���A��A�VAΑhA��;A�1A�VAΑhA�1Aͣ�A��A�VA�1A�1A��A��AΑhA�VA��;A��;A�ffA�C�A�1A��AΑhAΑhA�VA��A��A�VA��A��;A�VA�VAΑhA�VA��A�VA��;A�1A�VAΑhA���A��A���A��;A��;AΑhA��;A��AΑhA��A�1A���A��A�1A�VAΑhA�VA��AΑhA��;A��A���A���A�VA��;A��;A�VA��A��A��;A���A��;A�1AΑhA��;A��Aͣ�A��;Aͣ�A��;A��A��;Aͣ�Aͣ�Aͣ�A�VAΑhA��Aͣ�Aͣ�AΑhA���Aͣ�A�~�AΑhAΑhAͣ�A���A�VAΑhA��;Aͣ�A�VAΑhA��;A��A��;AΑhAͣ�A��;Aͣ�A�VA��AΑhA�VA��A��A��;A�VA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                    PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            202208232240552022082322405520220823224055    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232240552022082322405520220823224055  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B83E            383E            383E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232240552022082322405520220823224055  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�8800            800             800             UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202210041155462022100411554620221004115546  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                