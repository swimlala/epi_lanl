CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-08-23T22:40:59Z creation      
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
_FillValue                  0 �0Argo profile    3.1 1.2 19500101000000  20220823224059  20221004115546  5906299 5906299 5906299 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER,                                                  STEPHEN RISER,                                                  STEPHEN RISER,                                                  PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC                AAA AOAOAO  8443                            8443                            8443                            2C  2C  2C  DDD APEX                            APEX                            APEX                            8815                            8815                            8815                            052520                          052520                          052520                          877 877 877 @�P���z�@�P���z�@�P���z�111 @�P����@�P����@�P����@5�����@5�����@5������c�j~��#�c�j~��#�c�j~��#111 GPS     GPS     GPS     Primary sampling: averaged []                                                                                                                                                                                                                                   Secondary sampling: discrete []                                                                                                                                                                                                                                 Secondary sampling: discrete [1Hz data]                                                                                                                                                                                                                                  AAB AAC FFF     >���@9��@�  @�  A  A0  AP  Ap  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B$  B,  B4  B<  BD  BL  BT  B\  Bd  Bl  Bt  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C	  C  C  C  C  C  C  C  C  C  C  C  C!  C#  C%  C'  C)  C+  C-  C/  C1  C3  C5  C7  C9  C;  C=  C?  CA  CC  CE  CH  CK  CM  CO  CQ  CS�CU  CW  CY  C[  C]  C_  Ca  Cc  Ce  Cg  Ci  Ck  Cm  Co  Cq  Cs  Cu  Cw  Cy  C{  C}  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C CÀ CĀ Cŀ Cƀ Cǀ CȀ Cɀ Cʀ Cˀ C̀ C̀ C΀ Cπ CЀ Cр CҀ CӀ CԀ CՀ Cր C׀ C؀ Cـ Cڀ Cۀ C܀ C݀ Cހ C߀ C�� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C�� C� C� C� C� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� D @ D � D@ D� DFfD� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D	@ D	� D
@ D
� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D @ D � D!@ D!� D"@ D"� D#@ D#� D$@ D$� D%@ D%� D&@ D&� D'@ D'� D(@ D(� D)@ D)� D*@ D*� D+@ D+� D,@ D,� D-@ D-� D.@ D.� D/@ D/� D0@ D0� D1@ D1� D2@ D2� D3@ D3� D4@ D4� D5@ D5� D6@ D6� D7@ D7� D8@ D8� D9@ D9� D:@ D:� D;@ D;� D<@ D<� D=@ D=� D>@ D>� D?@ D?� D@@ D@� DA@ DA� DB@ DB� DC@ DC� DD@ DD� DE@ DE� DF@ DF� DG@ DG� DH@ DH� DI@ DI� DJ@ DJ� DK@ DK� DL@ DL� DM@ DM� DN@ DN� DO@ DO� DP@ DP� DQ@ DQ� DR@ DR� DS@ DS� DT@ DT� DU@ DU� DV@ DV� DW@ DW� DX@ DX� DY@ DY� DZ@ DZ� D[@ D[� D\@ D\� D]@ D]� D^@ D^� D_@ D_� D`@ D`� Da@ Da� Db@ Db� Dc@ Dc� Dd@ Dd� De@ De� Df@ Df� Dg@ Dg� Dh@ Dh� Di@ Di� DjFfDj� Dk@ Dk� Dl@ Dl� Dm@ Dm� Dn@ Dn� Do@ Do� Dp@ Dp� Dq@ Dq� Dr@ Dr� Ds@ Ds� Dt@ Dt� Du@ Du� Dv@ Dv� Dw@ Dw� Dx@ Dx� Dy@ Dy� Dz@ Dz� D{@ D{� D|@ D|� D}@ D}� D~@ D~�fD@ D� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D���D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D��3D�#3D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D���D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D��3D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D  D�� D�  D�` Dà D�� D�  D�` DĠ D�� D�  D�` DŠ D�� D�  D�` DƠ D�� D�  D�` DǠ D�� D�  D�` DȠ D�� D�  D�` Dɠ D�� D�  D�` Dʠ D�� D�  D�` Dˠ D�� D�  D�` D̠ D�� D�  D�` D͠ D�� D�  D�` DΠ D�� D�  D�` DϠ D�� D�  D�` DР D�� D�  D�` DѠ D�� D�  D�` DҠ D�� D�  D�` DӠ D�� D�  D�` DԠ D�� D�  D�` Dՠ D�� D�  D�` D֠ D�� D�  D�` Dנ D�� D�  D�` Dؠ D���D�  D�` Dٜ�D�� D�  D�` Dڠ D�� D�  D�` D۠ D�� D�  D�` Dܠ D�� D�  D�` Dݠ D�� D�  D�` Dޠ D�� D�  D�` Dߠ D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�\�D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D�� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D���D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�<�@��A!�A��\A�=qB �BH
=Br�B��B�{B�=qB���B���B�(�CaHC�=CٚC u�C*��C4�)C=�{CH�CR.C[��Cd��CpW
CzC�*=C�5�C��C�  C�j=C�S3C�3C�*=C�
=C��C�C�C�%C�C�C��D	�HD��D"�D/{D;�fDH �DT��Da33Dm�RDy�qD�I�D���D�θD��D�J�D���D���D�3D�\�D��D��
D�
�D�NfDԔ�Dڊ=D�{D�>�D��D���D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>���>���>���>L��=���=���>L��>L��>L��>L��=���>L��=���=���=���=���=���=���>L��?   >���>���>���>L��>���=���=���>L��>L��=���>L��>���>L��=���=���>���=���=���>L��=���=���>L��=���=���=���=���>L��>L��=���>���>L��=���=���=���=���>L��=���=���>L��>���>L��>���>���=���=���=���=���>L��=���=���=���>L��>���>���>L��=���=���>L��>L��>L��>L��>���>L��>L��>L��=���=���=���>L��=���>L��=���>���>���=���=���>L��=���>L��>L��=���>L��>���>���>���>���>L��=���=���=���>L��>L��?   >L��>L��=���>���=���=���>���>L��>���>L��>L��>���>L��=���=���>L��=���>L��=���>L��>L��>L��=���=���>L��=���>L��>���>L��>L��>L��>���>L��=���>���>L��>L��>L��>L��>L��=���=���>L��=���=���=���>���=���>L��>���>L��>���>L��>L��>L��>L��=���>L��=���=���>L��>L��>���>L��=���=���>���>L��>L��=���=���>L��>���>L��>L��>L��>���=���=���>L��>���>L��=���>���>L��>L��>L��=���>L��=���>L��>L��>L��>���>L��=���>���>L��>���>L��=���>L��=���=���=���>L��=���=���>L��=���>L��>L��>L��=���>L��>L��>L��>L��=���>L��=���>L��>���>���>L��>���>���>���>���>���>L��=���>L��>L��>L��=���=���>L��=���=���>L��>L��=���>L��=���>L��>L��>L��>L��>���>���=���>L��>L��>L��>���>L��=���>L��>L��>���>���>���>���>L��>���>L��=���>L��>L��>L��>���=���>���=���=���>L��>L��=���=���>L��>L��>L��>L��>���>L��=���>L��    >L��>L��=���=���>L��>L��=���=���>���>L��>L��>L��>���>L��=���=���>L��=���>L��>���>���>���?��?   ?333?L��?�  ?�  ?���?���?�ff?�33?�  ?���?ٙ�?�ff?�33@   @ff@ff@33@��@   @   @,��@333@9��@9��@@  @Fff@L��@S33@Y��@`  @fff@l��@s33@y��@�33@�ff@�ff@���@���@�  @�ff@�ff@���@���@�  @�ff@���@���@�  @�  @�ff@���@���@�  @�33@�ff@ə�@���@�  @�ff@�ff@���@�  @�33@�ff@陚@���@�  @�33@�ff@���@���A��A��A33A��A  A	��A33A��A��A  A��A33A��AffA  A��A33A��AffA   A!��A#33A$��A&ffA(  A)��A+33A,��A.ffA1��A1��A4��A6ffA8  A9��A;33A<��A>ffAA��AC33AD��AFffAH  AI��AK33ANffAP  AQ��AS33AT��AVffAX  A[33A\��A^ffA`  Aa��Ac33Ad��AfffAi��Ak33Al��AnffAq��As33At��AvffAx  Ay��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�  A���A͙�A�ffA�33A�  A���A�ffA�33A�  A���Aՙ�A�ffA�33A�  A�  Aٙ�A�ffA�33A�  A���Aݙ�A�ffA�33A�  A���AᙚA�ffA�  A�  A噚A�ffA�33A�  A���A陚A�ffA�33A�  A���A�ffA�ffA�  A���A�A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33B   B ffB ��B33B��B  BffB��B��B��BffB��B33B��B  BffB��B33B��B  BffB��B	33B
  B
ffB
��B33B��B  BffB��B33B��B  BffB��B33B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffB��B33B��B  BffBffB��B��B  B  BffB��B33B��B   B ffB ��B!33B!��B"  B"ffB#33B#��B$  B$  B$ffB%33B%��B&  B&ffB&��B'33B'��B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-33B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B0��B133B1��B2  B2ffB2��B333B3��B4  B4ffB4��B533B5��B6  B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:  B:ffB:��B;33B;��B<  B<ffB<��B=33B=��B>  B>ffB>��B?33B?��B@  B@ffB@��B@��BA��BA��BB  BBffBC33BC33BC��BD  BDffBD��BE33BE��BF  BFffBF��BG33BG��BH  BHffBH��BI33BI��BJ  BJffBJ��BK33BK��BL  BLffBL��BM33BM��BN  BNffBN��BO33BO��BP  BPffBP��BQ33BQ��BR  BRffBR��BS33BS��BT  BTffBT��BU33BU��BV  BVffBV��BW33BW��BX  BXffBX��BY33BY��BZ  BZffBZ��B[33B[��B\  B\��B]33B]��B^  B^ffB^��B_33B_��B`  B`ffB`��Ba33Ba��Bb  BbffBc33Bc��Bd  BdffBd��Be33Be��Bf  BfffBf��Bg��Bh  BhffBh��Bi33Bi��Bj  BjffBj��Bk33Bl  BlffBl��Bm33Bm��Bn  BnffBn��Bo33Bo��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    >��@7
>@��R@޸RA\)A/\)AO\)Ao\)A��A��A��A��AǮA׮A�A��B�
B�
B�
B�
B#�
B+�
B3�
B;�
BC�
BK�
BS�
B[�
Bc�
Bk�
Bs�
B{�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CG��CJ��CL��CN��CP��CS]CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C���C�z�C�z�C�z�C�z�C�z�C�z�D =qD �qD=qD�qDC�D�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD	=qD	�qD
=qD
�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD =qD �qD!=qD!�qD"=qD"�qD#=qD#�qD$=qD$�qD%=qD%�qD&=qD&�qD'=qD'�qD(=qD(�qD)=qD)�qD*=qD*�qD+=qD+�qD,=qD,�qD-=qD-�qD.=qD.�qD/=qD/�qD0=qD0�qD1=qD1�qD2=qD2�qD3=qD3�qD4=qD4�qD5=qD5�qD6=qD6�qD7=qD7�qD8=qD8�qD9=qD9�qD:=qD:�qD;=qD;�qD<=qD<�qD==qD=�qD>=qD>�qD?=qD?�qD@=qD@�qDA=qDA�qDB=qDB�qDC=qDC�qDD=qDD�qDE=qDE�qDF=qDF�qDG=qDG�qDH=qDH�qDI=qDI�qDJ=qDJ�qDK=qDK�qDL=qDL�qDM=qDM�qDN=qDN�qDO=qDO�qDP=qDP�qDQ=qDQ�qDR=qDR�qDS=qDS�qDT=qDT�qDU=qDU�qDV=qDV�qDW=qDW�qDX=qDX�qDY=qDY�qDZ=qDZ�qD[=qD[�qD\=qD\�qD]=qD]�qD^=qD^�qD_=qD_�qD`=qD`�qDa=qDa�qDb=qDb�qDc=qDc�qDd=qDd�qDe=qDe�qDf=qDf�qDg=qDg�qDh=qDh�qDi=qDi�qDjC�Dj�qDk=qDk�qDl=qDl�qDm=qDm�qDn=qDn�qDo=qDo�qDp=qDp�qDq=qDq�qDr=qDr�qDs=qDs�qDt=qDt�qDu=qDu�qDv=qDv�qDw=qDw�qDx=qDx�qDy=qDy�qDz=qDz�qD{=qD{�qD|=qD|�qD}=qD}�qD~=qD~��D=qD�qD��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�ۅD��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D���D�!�D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D���D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D�D�޸D��D�^�DÞ�D�޸D��D�^�DĞ�D�޸D��D�^�DŞ�D�޸D��D�^�Dƞ�D�޸D��D�^�DǞ�D�޸D��D�^�DȞ�D�޸D��D�^�Dɞ�D�޸D��D�^�Dʞ�D�޸D��D�^�D˞�D�޸D��D�^�D̞�D�޸D��D�^�D͞�D�޸D��D�^�DΞ�D�޸D��D�^�DϞ�D�޸D��D�^�DО�D�޸D��D�^�Dў�D�޸D��D�^�DҞ�D�޸D��D�^�DӞ�D�޸D��D�^�DԞ�D�޸D��D�^�D՞�D�޸D��D�^�D֞�D�޸D��D�^�Dמ�D�޸D��D�^�D؞�D�ۅD��D�^�Dٛ�D�޸D��D�^�Dڞ�D�޸D��D�^�D۞�D�޸D��D�^�Dܞ�D�޸D��D�^�Dݞ�D�޸D��D�^�Dޞ�D�޸D��D�^�Dߞ�D�޸D��D�^�D���D�޸D��D�^�DីD�޸D��D�[�D➸D�޸D��D�^�D㞸D�޸D��D�^�D䞸D�޸D��D�^�D垸D�޸D��D�^�D枸D�޸D��D�^�D瞸D�޸D��D�^�D螸D�޸D��D�^�D鞸D�޸D��D�^�DꞸD�޸D��D�^�D랸D�޸D��D�^�D잸D�޸D��D�^�DힸD�޸D��D�^�DD�޸D��D�^�DD�޸D��D�^�D�D�޸D��D�^�D�D�޸D��D�^�D�D�޸D��D�^�D�D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�ۅD��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D�;�@��
A!G�A�=pA��B \)BG�GBrB��
B�  B�(�B��fBڽqB�{CWC� C�]C k�C*��C4��C=�>CH�CR#�C[޹Cd�CpL�Cz�RC�%C�0�C��C��C�eC�NC�C�%C�C��C�  C��C�  C�>�C��D	��D�D"��D/�D;��DG�DT�Da0�Dm��Dy��D�H�D���D��pD�{D�IGD��RD���D��D�[�D�~fD���D�	GD�MDԓ�Dڈ�D�3D�=pD횏D���D�GG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��>�Q�>�Q�>��>#�
=u=u>#�
>#�
>#�
>#�
=u>#�
=u=u=u=u=u=u>#�
>�>��>��>��>#�
>��=u=u>#�
>#�
=u>#�
>��>#�
=u=u>��=u=u>#�
=u=u>#�
=u=u=u=u>#�
>#�
=u>�Q�>#�
=u=u=u=u>#�
=u=u>#�
>��>#�
>��>��=u=u=u=u>#�
=u=u=u>#�
>��>��>#�
=u=u>#�
>#�
>#�
>#�
>��>#�
>#�
>#�
=u=u=u>#�
=u>#�
=u>��>��=u=u>#�
=u>#�
>#�
=u>#�
>��>��>��>�Q�>#�
=u=u=u>#�
>#�
>�>#�
>#�
=u>�Q�=u=u>��>#�
>��>#�
>#�
>��>#�
=u=u>#�
=u>#�
=u>#�
>#�
>#�
=u=u>#�
=u>#�
>��>#�
>#�
>#�
>��>#�
=u>��>#�
>#�
>#�
>#�
>#�
=u=u>#�
=u=u=u>��=u>#�
>��>#�
>��>#�
>#�
>#�
>#�
=u>#�
=u=u>#�
>#�
>��>#�
=u=u>�Q�>#�
>#�
=u=u>#�
>��>#�
>#�
>#�
>�Q�=u=u>#�
>��>#�
=u>��>#�
>#�
>#�
=u>#�
=u>#�
>#�
>#�
>�Q�>#�
=u>��>#�
>��>#�
=u>#�
=u=u=u>#�
=u=u>#�
=u>#�
>#�
>#�
=u>#�
>#�
>#�
>#�
=u>#�
=u>#�
>��>�Q�>#�
>��>��>��>��>��>#�
=u>#�
>#�
>#�
=u=u>#�
=u=u>#�
>#�
=u>#�
=u>#�
>#�
>#�
>#�
>�Q�>��=u>#�
>#�
>#�
>�Q�>#�
=u>#�
>#�
>��>�Q�>��>��>#�
>�Q�>#�
=u>#�
>#�
>#�
>��=u>�Q�=u=u>#�
>#�
=u=u>#�
>#�
>#�
>#�
>��>#�
=u>#�
G�O�>#�
>#�
=u=u>#�
>#�
=u=u>��>#�
>#�
>#�
>�Q�>#�
=u=u>#�
=u>#�
>�Q�>��>�Q�?\)>�?(��?B�\?u?u?��?�z�?�G�?�{?��H?Ǯ?�z�?�G�?�{?��H@�
@�
@��@
>@p�@p�@*=q@0��@7
>@7
>@=p�@C�
@J=q@P��@W
>@]p�@c�
@j=q@p��@w
>@��@��@��@�Q�@��@��R@��@��@��@��@��R@��@�Q�@��@��R@��R@��@�Q�@��@��R@��@��@�Q�@˅@θR@��@��@ۅ@޸R@��@��@�Q�@�@�R@��@��@�Q�@��A ��A ��A�\A(�A\)A��A
�\A(�A(�A\)A��A�\A(�AA\)A��A�\A(�AA\)A ��A"�\A$(�A%A'\)A(��A*�\A,(�A-A0��A0��A4(�A5A7\)A8��A:�\A<(�A=A@��AB�\AD(�AEAG\)AH��AJ�\AMAO\)AP��AR�\AT(�AUAW\)AZ�\A\(�A]A_\)A`��Ab�\Ad(�AeAh��Aj�\Al(�AmAp��Ar�\At(�AuAw\)Ax��Az�\A|(�A}A\)A�z�A�zA��GA��A�z�A�G�A�zA��GA�z�A�G�A�zA�zA��A�z�A�G�A�zA��GA��A�z�A�G�A��GA��A��A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�G�A��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�G�A�zA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GAîA�z�A�G�A�zA��GAǮA�z�A�G�A�zA��GAˮA�z�A�G�A�zA��GAϮA�z�A�zA��GAӮA�z�A�G�A�zA��GA׮A׮A�G�A�zA��GAۮA�z�A�G�A�zA��GA߮A�z�A�G�A�zA�A�A�G�A�zA��GA�A�z�A�G�A�zA��GA�A�z�A�zA�zA�A�z�A�G�A�zA��GA�A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��B =pB ��B
=Bp�B�
B=pB��Bp�Bp�B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B	
=B	�
B
=pB
��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB=pB��Bp�B�
B�
B=pB��B
=Bp�B�
B =pB ��B!
=B!p�B!�
B"=pB#
=B#p�B#�
B#�
B$=pB%
=B%p�B%�
B&=pB&��B'
=B'p�B'�
B(=pB(��B)
=B)p�B)�
B*=pB*��B+
=B+p�B+�
B,=pB,��B-
=B-p�B-�
B.=pB.��B/
=B/p�B/�
B0=pB0��B0��B1
=B1p�B1�
B2=pB2��B3
=B3p�B3�
B4=pB4��B5
=B5p�B5�
B5�
B6=pB6��B7
=B7p�B7�
B8=pB8��B9
=B9p�B9�
B9�
B:=pB:��B;
=B;p�B;�
B<=pB<��B=
=B=p�B=�
B>=pB>��B?
=B?p�B?�
B@=pB@��B@��BAp�BAp�BA�
BB=pBC
=BC
=BCp�BC�
BD=pBD��BE
=BEp�BE�
BF=pBF��BG
=BGp�BG�
BH=pBH��BI
=BIp�BI�
BJ=pBJ��BK
=BKp�BK�
BL=pBL��BM
=BMp�BM�
BN=pBN��BO
=BOp�BO�
BP=pBP��BQ
=BQp�BQ�
BR=pBR��BS
=BSp�BS�
BT=pBT��BU
=BUp�BU�
BV=pBV��BW
=BWp�BW�
BX=pBX��BY
=BYp�BY�
BZ=pBZ��B[
=B[p�B[�
B\��B]
=B]p�B]�
B^=pB^��B_
=B_p�B_�
B`=pB`��Ba
=Bap�Ba�
Bb=pBc
=Bcp�Bc�
Bd=pBd��Be
=Bep�Be�
Bf=pBf��Bgp�Bg�
Bh=pBh��Bi
=Bip�Bi�
Bj=pBj��Bk
=Bk�
Bl=pBl��Bm
=Bmp�Bm�
Bn=pBn��Bo
=Bop�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A̸RA��/A��HA��`A��TA��TA��yA��A��A��yA��`A��`A��mA��mA��`A��A��A��A��A���A���A���A���A���A��mA��
A͸RA�bNA�VA�E�A�;dA� �A�
=A�
=A�
=A�JA�
=A��A�A̾wA̬A̕�A�v�A�?}A�+A���Aʉ7A��HA�O�A�VA��A�ZA���A�A���A���A��A���A�l�A��;A�%A��^A�x�A��\A�  A��A�x�A�K�A�bA�-A��A���A�O�A���A��A���A�n�A�+A���A�I�A���A�I�A���A��A��!A���A�oA��A��-A���A�/A��7A��A��A��TA�^5A��jA�A�+A���A�Q�A�ƨA�A���A��7A���A��`A�A�VA�bA���A��A��FA��A���A��/A�VA~��A|^5Ax�uAv$�As�Arz�Aq�#Ap�uAnZAk�TAhQ�AfJAdr�Ac�Ab1'A`�RA_x�A_&�A\�AX=qAU�mAUS�AT{AQ�AP�AO;dANA�AM�AL��AL �AKt�AJ��AIS�AH��AG��AF��AFAC�FAA�;A@bNA=�
A<��A;��A9hsA7�A5��A4E�A3�A3&�A0�A/��A.��A.1'A-�A-"�A,ZA+x�A)%A'�A&�!A%�-A%%A$�!A#��A"�A!|�A!
=A!%A �`A ffAx�A��A �A��AC�AAJA�+A��A�PA1'AdZA33AoAA�A\)A�AĜA��A�`A�A�Ap�A��A��A�A
n�A
A	��A�A�AO�An�A��AƨA��A�AV@�
=@�-@�%@���@�|�@���@�J@�\)@�X@�j@�F@���@�5?@�r�@���@�V@�X@�I�@�  @㝲@���@�
=@�u@�@�F@�j@�l�@�~�@���@�h@���@�"�@��@��@�`B@��D@�  @�+@�$�@ܛ�@���@ڏ\@ٙ�@�V@ج@�ƨ@֏\@�dZ@ҸR@��@�?}@���@�Z@�1@�t�@�33@��@Η�@�ff@�{@ͺ^@̴9@˅@ˍP@�dZ@ʸR@�E�@�&�@� �@�\)@�^5@�7L@�Q�@�;d@���@�&�@�V@��/@�j@�
=@�@��-@�V@���@���@�~�@�ff@�1'@���@��@�j@�O�@�&�@��w@�\)@���@�S�@��@��@�l�@�
=@�ȴ@��+@�V@�{@�x�@�7L@�V@�bN@���@�@��!@�n�@�@��^@���@�%@��9@�I�@��m@��;@��m@��m@��w@�C�@�@�ff@��@��/@�@�ff@�$�@��7@��`@�bN@�1@�dZ@��w@��@��;@�l�@�33@��@��\@�v�@�n�@�n�@�@��7@��h@��7@�O�@��@��@�9X@�dZ@�K�@�C�@�+@��@��@���@���@��!@��H@�@��@���@�o@�^5@�hs@�O�@�X@�X@�O�@���@���@�z�@�A�@�A�@�  @��@�|�@�33@��@�@��`@�z�@�z�@���@��@�V@��@��@��@�z�@�bN@�(�@�1@�  @�b@�I�@�z�@�Ĝ@��`@���@���@��/@���@��j@��j@��@���@��@�Z@�A�@�1'@��@�  @��m@��;@��;@�ƨ@��F@��F@��@���@�dZ@�+@���@��y@��!@�v�@�^5@�M�@�M�@�M�@���@��#@���@���@�V@���@���@�|�@�K�@�"�@�
=@��@�ȴ@��\@�{@���@�`B@�G�@�G�@�?}@���@��j@���@�Z@�  @�dZ@��@���@�v�@�E�@�5?@�J@��@�%@���@���@�1'@�1@��m@�ƨ@��P@�\)@�;d@�"�@��H@��\@�=q@��T@��@��u@�j@�  @�@��@�@|�@~@}/@}V@|�@|��@{�F@z^5@zn�@zJ@yhs@y�@x1'@w�@v�y@vff@u�h@u�@u`B@t�j@tI�@t(�@s��@s�F@s�m@s��@s�F@st�@s@so@r�@r��@r�\@r��@r�!@r-@r~�@r-@q7L@p�`@p�@pr�@o��@n�y@nE�@m�T@m�h@lj@k��@j�H@j��@j^5@j-@i��@i��@i%@h��@hA�@g|�@fȴ@e�h@d�/@dI�@d�@eV@d�@d�@dz�@cS�@a�#@cdZ@e�h@d�@b-@a7L@a&�@a%@`�`@`Ĝ@`A�@^�R@^��@^ȴ@^�y@^�@^�y@_K�@_l�@_;d@^��@^ȴ@^�+@^$�@]/@\�@[�m@[dZ@[33@[@Z�H@Z��@ZM�@Z-@Y��@Y��@Y��@Yx�@Y&�@X��@X�9@XQ�@W�w@W��@Wl�@W\)@WK�@Wl�@W��@W�P@V�y@U�@T��@UV@UV@T��@T�@T�@T�/@T�j@T�@Tz�@TZ@S�m@S33@S@S@R�H@Rn�@R�@Q�@Q�^@Qx�@Q%@PA�@O�@OK�@O
=@N��@N�R@N�+@NV@N{@M��@M?}@M/@L�@L�j@L��@L�D@Lj@LI�@L�@K��@Kt�@KC�@K"�@J�H@J��@J��@JM�@I�@I��@Ix�@IX@I&�@H��@H �@G�@G�P@G\)@F�R@F�+@FV@E�-@E`B@EO�@E?}@D�/@Dj@DZ@D9X@D(�@D1@C�m@C�
@C��@C"�@B��@B��@B^5@B-@BJ@BJ@A��@Ax�@A�@@��@@ �@?�@?�;@?�w@?��@?l�@>�y@>�+@=��@=/@<�j@<j@<1@;�F@;�@;dZ@;"�@:�!@:n�@:-@9��@9��@9��@8��@8�u@8bN@7�@7�@7|�@7K�@6�@6�R@6��@6v�@65?@6{@6@5�@5@5��@5��@5`B@4��@4�j@4��@4j@4I�@4I�@49X@4(�@4�@41@41@3�
@3�@3@2n�@1��@17L@0�`@0�@0Q�@0A�@01'@0b@/�@/�;@/��@/�@/�P@/�@.ȴ@.E�@.$�@.$�@.{@.@-�-@,�@,�D@,�D@,z�@,z�@,z�@,9X@+��@+�F@+dZ@*�H@*~�@*n�@*^5@*^5@*M�@*M�@*�@)��@(��@(�9@(�u@(A�@(1'@(1'@(b@'��@'��@'��@'�w@'�P@'\)@';d@&��@&�y@&�@&�@&ff@&{@&{@&@%�@%�T@%@%@%�-@%��@%�@%�@%`B@%/@%�@%�@%V@$�@$Z@#dZ@#S�@#33@"�!@"n�@"M�@"-@!��@!��@!��@!��@!x�@!G�@!&�@ ��@ ��@ ��@ �@ bN@ A�@ b@��@��@|�@\)@��@�R@�+@{@@z�@�
@�F@��@�@�@dZ@@=q@7L@�`@�`@�u@A�@�;@|�@+@��@��@��@ff@��@�-@�h@p�@/@�@j@��@�
@�
@�
@ƨ@ƨ@��@t�@t�@t�@dZ@33@o@�@��@~�@J@�#@�^@G�@�`@��@�@bN@�@;d@��@V@@`B@V@9X@��@ƨ@dZ@
�H@
��@
�!@
��@
^5@
=q@
J@	�@	�#@	��@	�^@	��@	hs@�`@Q�@b@  @�@��@��@\)@;d@+@+@
=@��@�y@�@ȴ@��@E�@{@��@��@/@Z@(�@��@ƨ@�F@��@�F@�F@�F@�F@�F@�F@�F@�FA��A��A�� A��A���A�ߤA�;dA�A̹�A�!A�Q�A���A���A��A�JA���A��_A�A�E9A��FA��KA�MA���A�s�Az&�Aq	lAd�'A_T�AT�AM��AI_ADt�A<K�A4	A.\�A*]dA$یA�	AA A��@�`�@�($@@���@�[W@��:@�[W@��@���@�Ɇ@�$@z^5@r�@hz�@^��@Y<6@T7�@L��@Fz@@�@8m�@4�@.	@(V�@%�"@ �v@�@s@��@&@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A͸RA;wA͸RA�|�A˃A�E�A��`A�x�Aͩ�A�C�A�p�A�|�A�v�A�ƨA̲-A��A�|�A�7LAͲ-A�A���A;wAͺ^Aͧ�A�?}A��A���A��A͸RAͶFAͺ^Aͺ^Aͣ�A��A�%A�v�A˟�A��A͟�A�
=AʮA˝�A��A���A�S�A��Aͥ�Aʹ9A̓A�x�A�;dA�?}A���A�I�A��
A��/A���A�A�33Aͥ�A��`A͡�A�O�A���A�A�1A˝�A�{A�A��/A�l�A�5?Aͩ�A�l�AˍPA��DA�ĜA�C�A͡�A͙�Aʹ9A;wAͶFAͮA�jA�"�AʍPA�1AˬÁAͬA�hsA���A� �A�n�A�I�A�;dA�{A�bNAͶFAͩ�Aʹ9A͸RAͶFA͸RA͙�A���A���A��HA�%AͲ-AͼjAͰ!A̓A�n�A�O�A͓uA���A�v�A͑hA�JA͙�A�;dAʹ9AͬÁA�E�A���A�ffA̮A�1A�p�A���AͬÁA���A�t�A�~�A̓uAͲ-AͬAͮA�C�A͍PAʹ9Aͣ�A�E�AͲ-AͶFAͺ^Aͥ�A�A�ÁA�VA�bNA�ƨA�{A� �A�bNAͅA�z�A�z�A;wAͰ!AͲ-AͲ-A͙�A�+A�"�A͟�A͍PA�O�A��/A͙�AͶFAͺ^AͶFA͍PA�VAͼjAͩ�A� �A�A�jAͶFAͼjA͸RAͬA�I�AͅA��HA��TA�x�A̋DÁA˅A��TAͮA�=qA�M�A�`BAͥ�A�oA͟�A�A�A���A͇+A˾wA�ĜA�ƨA���ÁA��`A�  A�ffA�/AͶFA͗�A�;dA�I�A�S�A���A˾wAͺ^A͏\A�A�A�Aͺ^A�dZA���Aʕ�A��Aɲ-A�z�A�ĜA�ȴA�AͶFA�ĜA�ĜA�AͮA���AȍPA�~�Aͩ�AͅA���A�l�A�ĜA�t�Aǰ!A�Q�A͉7A�(�Aʹ9A�7LA���A���A�ĜA͉7A�  A͏\A���A���A���A���A���Aͩ�A���A�ȴA�ȴA;wA���A���A�ȴA�ȴAͼjA�G�A�$�A���A���A�ȴAͮA�x�AͮA��A�^5A͓uA͗�A�/AȃA�ĜA�p�AˁA��
A���A�v�Aˣ�A͟�A�?}A�K�ÁA˝�A��A�ȴA���AͬA���A�p�Aʹ9A��A�ȴAͺ^A���AƍPAǰ!A�^5A�ĜA�O�A��HA��HA��;A��HA��HA��HA��HA��;A��/A��/A��/A��A��#A��;A��HA��;A��;A��;A��/A��#A��/A��TA��TA��TA��TA��TA��TA��;A��#A��/A��;A��/A��/A��#A��#A��A��A��#A��/A��/A��;A��HA��HA��HA��TA��TA��HA��HA��HA��HA��HA��HA��TA��TA��HA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��`A��mA��`A��`A��`A��mA��mA��mA��mA��mA��mA��mA��mA��mA��mA��mA��TA��TA��TA��TA��TA��HA��HA��HA��HA��HA��TA��HA��;A��;A��;A��;A��;A��HA��HA��HA��HA��;A��HA��HA��HA��HA��TA��TA��TA��mA��mA��mA��mA��`A��mA��mA��TA��TA��yA��A��mA��TA��TA��`A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��yA��yA��mA��yA��yA��`A��`A��TA��TA��`A��TA��TA��`A��mA��mA��`A��`A��TA��TA��TA��`A��`A��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��`A��TA��`A��`A��`A��`A��`A��`A��`A��`A��`A��mA��mA��mA��`A��mA��`A��`A��mA��mA��mA��mA��mA��mA��`A��mA��mA��mA��mA��mA��mA��yA��mA��mA��mA��mA��mA��mA��`A��`A��mA��mA��mA��mA��mA��mA��mA��mA��`A��HA��HA��HA��HA��HA��HA��HA��TA��TA��TA��TA��TA��HA��HA��TA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A���A��A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A��A��mA��mA��yA��mA��`A��`A��TA��TA��TA��TA��TA��HA��HA��HA��HA��HA��;A��;A��/A��/A��#A��#A��A��A��A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ƨA�ƨA�A���A���A;wA͸RAʹ9AͲ-AͰ!AͬAͩ�Aͣ�A͙�AͅA�r�A�ffA�dZA�dZA�bNA�bNA�bNA�bNA�`BA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�^5A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�ZA�XA�VA�S�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�M�A�I�A�I�A�G�A�G�A�E�A�C�A�E�A�C�A�C�A�C�A�A�A�C�A�A�A�A�A�A�A�A�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    A̸RA��/A��HA��`A��TA��TA��yA��A��A��yA��`A��`A��mA��mA��`A��A��A��A��A���A���A���A���A���A��mA��
A͸RA�bNA�VA�E�A�;dA� �A�
=A�
=A�
=A�JA�
=A��A�A̾wA̬A̕�A�v�A�?}A�+A���Aʉ7A��HA�O�A�VA��A�ZA���A�A���A���A��A���A�l�A��;A�%A��^A�x�A��\A�  A��A�x�A�K�A�bA�-A��A���A�O�A���A��A���A�n�A�+A���A�I�A���A�I�A���A��A��!A���A�oA��A��-A���A�/A��7A��A��A��TA�^5A��jA�A�+A���A�Q�A�ƨA�A���A��7A���A��`A�A�VA�bA���A��A��FA��A���A��/A�VA~��A|^5Ax�uAv$�As�Arz�Aq�#Ap�uAnZAk�TAhQ�AfJAdr�Ac�Ab1'A`�RA_x�A_&�A\�AX=qAU�mAUS�AT{AQ�AP�AO;dANA�AM�AL��AL �AKt�AJ��AIS�AH��AG��AF��AFAC�FAA�;A@bNA=�
A<��A;��A9hsA7�A5��A4E�A3�A3&�A0�A/��A.��A.1'A-�A-"�A,ZA+x�A)%A'�A&�!A%�-A%%A$�!A#��A"�A!|�A!
=A!%A �`A ffAx�A��A �A��AC�AAJA�+A��A�PA1'AdZA33AoAA�A\)A�AĜA��A�`A�A�Ap�A��A��A�A
n�A
A	��A�A�AO�An�A��AƨA��A�AV@�
=@�-@�%@���@�|�@���@�J@�\)@�X@�j@�F@���@�5?@�r�@���@�V@�X@�I�@�  @㝲@���@�
=@�u@�@�F@�j@�l�@�~�@���@�h@���@�"�@��@��@�`B@��D@�  @�+@�$�@ܛ�@���@ڏ\@ٙ�@�V@ج@�ƨ@֏\@�dZ@ҸR@��@�?}@���@�Z@�1@�t�@�33@��@Η�@�ff@�{@ͺ^@̴9@˅@ˍP@�dZ@ʸR@�E�@�&�@� �@�\)@�^5@�7L@�Q�@�;d@���@�&�@�V@��/@�j@�
=@�@��-@�V@���@���@�~�@�ff@�1'@���@��@�j@�O�@�&�@��w@�\)@���@�S�@��@��@�l�@�
=@�ȴ@��+@�V@�{@�x�@�7L@�V@�bN@���@�@��!@�n�@�@��^@���@�%@��9@�I�@��m@��;@��m@��m@��w@�C�@�@�ff@��@��/@�@�ff@�$�@��7@��`@�bN@�1@�dZ@��w@��@��;@�l�@�33@��@��\@�v�@�n�@�n�@�@��7@��h@��7@�O�@��@��@�9X@�dZ@�K�@�C�@�+@��@��@���@���@��!@��H@�@��@���@�o@�^5@�hs@�O�@�X@�X@�O�@���@���@�z�@�A�@�A�@�  @��@�|�@�33@��@�@��`@�z�@�z�@���@��@�V@��@��@��@�z�@�bN@�(�@�1@�  @�b@�I�@�z�@�Ĝ@��`@���@���@��/@���@��j@��j@��@���@��@�Z@�A�@�1'@��@�  @��m@��;@��;@�ƨ@��F@��F@��@���@�dZ@�+@���@��y@��!@�v�@�^5@�M�@�M�@�M�@���@��#@���@���@�V@���@���@�|�@�K�@�"�@�
=@��@�ȴ@��\@�{@���@�`B@�G�@�G�@�?}@���@��j@���@�Z@�  @�dZ@��@���@�v�@�E�@�5?@�J@��@�%@���@���@�1'@�1@��m@�ƨ@��P@�\)@�;d@�"�@��H@��\@�=q@��T@��@��u@�j@�  @�@��@�@|�@~@}/@}V@|�@|��@{�F@z^5@zn�@zJ@yhs@y�@x1'@w�@v�y@vff@u�h@u�@u`B@t�j@tI�@t(�@s��@s�F@s�m@s��@s�F@st�@s@so@r�@r��@r�\@r��@r�!@r-@r~�@r-@q7L@p�`@p�@pr�@o��@n�y@nE�@m�T@m�h@lj@k��@j�H@j��@j^5@j-@i��@i��@i%@h��@hA�@g|�@fȴ@e�h@d�/@dI�@d�@eV@d�@d�@dz�@cS�@a�#@cdZ@e�h@d�@b-@a7L@a&�@a%@`�`@`Ĝ@`A�@^�R@^��@^ȴ@^�y@^�@^�y@_K�@_l�@_;d@^��@^ȴ@^�+@^$�@]/@\�@[�m@[dZ@[33@[@Z�H@Z��@ZM�@Z-@Y��@Y��@Y��@Yx�@Y&�@X��@X�9@XQ�@W�w@W��@Wl�@W\)@WK�@Wl�@W��@W�P@V�y@U�@T��@UV@UV@T��@T�@T�@T�/@T�j@T�@Tz�@TZ@S�m@S33@S@S@R�H@Rn�@R�@Q�@Q�^@Qx�@Q%@PA�@O�@OK�@O
=@N��@N�R@N�+@NV@N{@M��@M?}@M/@L�@L�j@L��@L�D@Lj@LI�@L�@K��@Kt�@KC�@K"�@J�H@J��@J��@JM�@I�@I��@Ix�@IX@I&�@H��@H �@G�@G�P@G\)@F�R@F�+@FV@E�-@E`B@EO�@E?}@D�/@Dj@DZ@D9X@D(�@D1@C�m@C�
@C��@C"�@B��@B��@B^5@B-@BJ@BJ@A��@Ax�@A�@@��@@ �@?�@?�;@?�w@?��@?l�@>�y@>�+@=��@=/@<�j@<j@<1@;�F@;�@;dZ@;"�@:�!@:n�@:-@9��@9��@9��@8��@8�u@8bN@7�@7�@7|�@7K�@6�@6�R@6��@6v�@65?@6{@6@5�@5@5��@5��@5`B@4��@4�j@4��@4j@4I�@4I�@49X@4(�@4�@41@41@3�
@3�@3@2n�@1��@17L@0�`@0�@0Q�@0A�@01'@0b@/�@/�;@/��@/�@/�P@/�@.ȴ@.E�@.$�@.$�@.{@.@-�-@,�@,�D@,�D@,z�@,z�@,z�@,9X@+��@+�F@+dZ@*�H@*~�@*n�@*^5@*^5@*M�@*M�@*�@)��@(��@(�9@(�u@(A�@(1'@(1'@(b@'��@'��@'��@'�w@'�P@'\)@';d@&��@&�y@&�@&�@&ff@&{@&{@&@%�@%�T@%@%@%�-@%��@%�@%�@%`B@%/@%�@%�@%V@$�@$Z@#dZ@#S�@#33@"�!@"n�@"M�@"-@!��@!��@!��@!��@!x�@!G�@!&�@ ��@ ��@ ��@ �@ bN@ A�@ b@��@��@|�@\)@��@�R@�+@{@@z�@�
@�F@��@�@�@dZ@@=q@7L@�`@�`@�u@A�@�;@|�@+@��@��@��@ff@��@�-@�h@p�@/@�@j@��@�
@�
@�
@ƨ@ƨ@��@t�@t�@t�@dZ@33@o@�@��@~�@J@�#@�^@G�@�`@��@�@bN@�@;d@��@V@@`B@V@9X@��@ƨ@dZ@
�H@
��@
�!@
��@
^5@
=q@
J@	�@	�#@	��@	�^@	��@	hs@�`@Q�@b@  @�@��@��@\)@;d@+@+@
=@��@�y@�@ȴ@��@E�@{@��@��@/@Z@(�@��@ƨ@�F@��@�F@�F@�F@�F@�F@�F@�F@�FA��A��A�� A��A���A�ߤA�;dA�A̹�A�!A�Q�A���A���A��A�JA���A��_A�A�E9A��FA��KA�MA���A�s�Az&�Aq	lAd�'A_T�AT�AM��AI_ADt�A<K�A4	A.\�A*]dA$یA�	AA A��@�`�@�($@@���@�[W@��:@�[W@��@���@�Ɇ@�$@z^5@r�@hz�@^��@Y<6@T7�@L��@Fz@@�@8m�@4�@.	@(V�@%�"@ �v@�@s@��@&@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A��/A��/A��/A��A��#A��;A��HA��;A��;A��;A��/A��#A��/A��TA��TA��TA��TA��TA��TA��;A��#A��/A��;A��/A��/A��#A��#A��A��A��#A��/A��/A��;A��HA��HA��HA��TA��TA��HA��HA��HA��HA��HA��HA��TA��TA��HA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��`A��mA��`A��`A��`A��mA��mA��mA��mA��mA��mA��mA��mA��mA��mA��mA��TA��TA��TA��TA��TA��HA��HA��HA��HA��HA��TA��HA��;A��;A��;A��;A��;A��HA��HA��HA��HA��;A��HA��HA��HA��HA��TA��TA��TA��mA��mA��mA��mA��`A��mA��mA��TA��TA��yA��A��mA��TA��TA��`A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��yA��yA��mA��yA��yA��`A��`A��TA��TA��`A��TA��TA��`A��mA��mA��`A��`A��TA��TA��TA��`A��`A��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��TA��`A��TA��`A��`A��`A��`A��`A��`A��`A��`A��`A��mA��mA��mA��`A��mA��`A��`A��mA��mA��mA��mA��mA��mA��`A��mA��mA��mA��mA��mA��mA��yA��mA��mA��mA��mA��mA��mA��`A��`A��mA��mA��mA��mA��mA��mA��mA��mA��`A��HA��HA��HA��HA��HA��HA��HA��TA��TA��TA��TA��TA��HA��HA��TA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A���A��A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A���A���A��A��mA��mA��yA��mA��`A��`A��TA��TA��TA��TA��TA��HA��HA��HA��HA��HA��;A��;A��/A��/A��#A��#A��A��A��A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ƨA�ƨA�A���A���A;wA͸RAʹ9AͲ-AͰ!AͬAͩ�Aͣ�A͙�AͅA�r�A�ffA�dZA�dZA�bNA�bNA�bNA�bNA�`BA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�^5A�^5A�^5A�^5A�\)A�\)A�\)A�\)A�ZA�XA�VA�S�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�M�A�I�A�I�A�G�A�G�A�E�A�C�A�E�A�C�A�C�A�C�A�A�A�C�A�A�A�A�A�A�A�A�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ŢB
ǮB
�)B
�BB0!B5?B;dB?}BL�B`BBdZBffBhsBk�Br�Bu�Bu�Bu�Bt�Bx�B�1B�oB��B�qB��B�ZBɺB�XB��B��B�FB�}B�3B�BɺBƨB�B��B�`B�HB�BB�NB�`B�B��BB{B)�B1'B0!B.B(�B%�B �B�BuBB��B�B�)B�BB�B�`B�TB�#B��B��B��B�wB�B��B|�Bp�Bo�BZBM�B=qB%�BbBB
��B
�ZB
�B
��B
�qB
�-B
��B
��B
�+B
v�B
iyB
]/B
O�B
C�B
5?B
!�B
&�B	��B	�B	�fB	�5B	��B	ŢB	�3B	��B	�{B	�JB	�B	{�B	r�B	l�B	ffB	XB	D�B	9XB	49B	-B	#�B	�B	�B	oB	bB	PB	
=B	%B	B��B��B�B�B�B�fB�;B�B��B��BƨBB�jB�FB�?B�-B�B��B��B��B��B��B��B��B�bB�\B�VB�JB�JB�DB�7B�%B�%B�%B�B�B�B�B�B�B� B~�B~�B{�By�Bx�B{�B}�B}�B~�B~�B|�B}�B}�B|�B|�B|�B{�B�B�B�B�B�+B�+B�1B�B�B�B� B{�Bz�By�B}�B}�Bw�BjBffBgmBbNBYBT�BXBZBW
BO�BL�BG�BA�B<jB;dB<jB>wB?}BB�BE�BF�BH�BN�BdZBx�B�1B�uB��B��B��B��B��B�B�'B�3B�?B�LB�LB�LB�XB�dB�qB�wB��B��B�}B��B�}B�}B�wB��BĜB��B��B�B�B�B�)B�;B�;B�HB�NB�yB�B�B�B�B�B�B�yB�sB�mB�mB�B�B�B�B�B�B�B�B�B�B��B��B�B�sB�HB�#B�
B��B�B�5B�5B�/B�;B�BB�/B�#B�)B�5B�;B�;B�BB�BB�BB�HB�NB�HB�TB�fB�sB�sB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	+B	hB	�B	�B	�B	�B	�B	!�B	&�B	/B	5?B	@�B	C�B	D�B	E�B	H�B	I�B	J�B	K�B	N�B	Q�B	Q�B	Q�B	R�B	T�B	VB	ZB	]/B	^5B	_;B	`BB	aHB	dZB	gmB	hsB	jB	l�B	m�B	n�B	o�B	o�B	n�B	m�B	m�B	n�B	q�B	t�B	x�B	y�B	}�B	�B	�B	�B	�7B	�JB	�\B	�\B	�VB	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�FB	�RB	�dB	�wB	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�;B	�5B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�BB	�HB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B	��B
B
	7B
+B
B
B
B
+B
	7B
	7B
+B
%B
1B
	7B
	7B
	7B

=B
VB
bB
hB
oB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
hB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
%�B
%�B
%�B
&�B
$�B
#�B
#�B
$�B
%�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
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
?}B
?}B
?}B
@�B
@�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
A�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
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
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
YB
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
]/B
]/B
^5B
_;B
_;B
\)B
]/B
]/B
]/B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
cTB
dZB
dZB
dZB
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
gmB
hsB
hsB
hsB
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
n�B
n�B
n�B
n�B
��B
�B
ƎB
��B
�B
��B?�Be`Bv+B�SB�cB��B��BޞB'�B'�B�0B�B�{Bu�B=�B
�DB
�B
i�B
)�B	ՁB	��B	hsB	/�B	NB��B��BǮB�B��B��B��BHB.BlBzB��B�CB��B�xB	Q�B	r�B	��B	��B	��B	��B	��B	�B	��B
	�B
[B
B
VB
#B
,B
6zB
;�B
A B
E�B
G�B
L�B
U�B
[�B
d@B
j0B
n/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ĜB
ŢB
ȴB
�;B��@(1'>�r�A�33B�A�RB?}A���A��`>�9XB
J@�VB)�>��?�ffB��B
ƨB
�LB
ÖB
��B
�B1'>333>�;dB
��@��?�ZB
B
��B
��A��T@���B�h>Y�?*��B
�5@��>w��B	��>�V>��j>��T?'l�B
�dB
��?�XBX?�/?{>m�h>R�>��^B
�A\)>�VB
�XB�A�/B
�sB�H>�>+>�+?��
B�A�ff>?|�>��BdZB
�#B�{Aӟ�>o��>�z�B
ɺ@pr�B
�dB
,B
ǮB
�3B
�)BR�>��?}�->�l�B
_;?QhsB
>�;dB  A�p�>Ձ?*��B$�>�M�B
�bB
�d@�l�B
�3B
ƨB
ǮB
��B�B�>���?V@W�PB
�XB	�B
�B
�mB
��@�Q�B	7A�?-O�BB
1'B
�A�bB
B
�B
��A�K�>�p�B
�!?��Bv�>���B
�RB
�B]/>�Ĝ>�VB
��@�
=B��B
ǮB
�B
ĜB
q�B
��A�?7
=B
ǮBu�B
�=B
�B2-A�K�>cS�>��yBY>7K�>��w?��B:^@� �B��B
ɺB
��B
ƨB�
B
��A��^A�1A��yBO�>l�D?��B
��B
�LB
ǮB
��@ڟ�?�VB
��A�jBgm>��j?�+B
ĜB
ǮB	�%B
�B=qB|�>�V>��A��\B
�B��>�9XB
��B�1B

=B?�jB
��>�~�B
ŢB
��B	�B
�BB|�Aw�wB
ĜB
�RB
�#Bp�?�BÖ>j~�?FffBbNB<j>}�?�hB�m>��B
�wB
ȴ?L�D?�-B
�}B
�mB
,B�3?8��@��>���B
ŢB
B
ɺB
��B
ĜB
ŢB
ŢB
��B{A��>���BiyB
�RBX?a%?A%B
v�>��\?��B
�BX?*��A��?��uB
�9B
�wB
�)?��B
�BB�=>�n�B	O�B
�B
!�B
��B
�NA�\B
��B
ĜB
ŢB
ƨB
ǮB
ɺB
��B
��A�{>�`BB
	7B
�}B;dBD�?B��B
=?�>ě�B
�B^5>���?M�hB
��A���B
�-B
�qB
�B�1@>ȴB
Ǯ>�|�B
�dB�>�{?v�B	.B
��A��>ݲ-B
��BJA���B
�PB�A�r�>�|�?��B�>���B�B
ƨB
ƨB
ĜB
ǮB
ĜB
ǮB
ǮB
ƨB
ŢB
ǮB
ǮB
ŢB
ƨB
ƨB
ǮB
ȴB
ȴB
ǮB
ȴB
ƨB
ƨB
ƨB
ƨB
ŢB
ǮB
ƨB
ƨB
ǮB
ŢB
ǮB
ȴB
ȴB
ǮB
ǮB
ȴB
ȴB
ȴB
ǮB
ǮB
ǮB
ǮB
ǮB
ƨB
ƨB
ǮB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ǮB
ƨB
ǮB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ƨB
ŢB
ŢB
ŢB
ƨB
ǮB
ĜB
ƨB
ƨB
ŢB
ƨB
ƨB
ŢB
ŢB
ƨB
ƨB
ƨB
ǮB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ƨB
ǮB
ƨB
ƨB
ǮB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ĜB
ƨB
ǮB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ǮB
ƨB
ƨB
ŢB
ƨB
ƨB
ǮB
ƨB
ǮB
ǮB
ƨB
ǮB
ƨB
ƨB
ǮB
ȴB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ƨB
ǮB
ǮB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ŢB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ǮB
ƨB
ȴB
ŢB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ŢB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ǮB
ŢB
ƨB
ÖB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ǮB
ŢB
ŢB
ƨB
ƨB
ŢB
ŢB
ŢB
ƨB
ƨB
ŢB
ƨB
ƨB
ǮB
ƨB
ŢB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ŢB
ǮB
ƨB
ƨB
ǮB
ƨB
ƨB
ŢB
ƨB
ŢB
ƨB
ƨB
ǮB
ǮB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ǮB
ǮB
ƨB
ǮB
ƨB
ĜB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ǮB
ƨB
ƨB
ŢB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ŢB
ǮB
ǮB
ǮB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ǮB
ƨB
ƨB
ǮB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ǮB
ǮB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ŢB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ŢB
ĜB
ŢB
ŢB
ŢB
ĜB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ĜB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ǮB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�)B
�#B
�)B
�5B
�/B
�;B
�BB
�HB
�HB
�HB
�HB
�NB
�NB
�TB
�TB
�TB
�ZB
�`B
�fB
�mB
�sB
�yB
�B
�B
�yB
�B
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
��B
��B
��B
��B
��B
��B
��BBB+B1B1BDBPBhB�B�B(�B/B0!B/B0!B1'B0!B1'B0!B1'B1'B1'B1'B1'B1'B1'B1'B1'B2-B1'B2-B33B33B33B33B49B5?B5?B6FB7LB6FB7LB6FB7LB7LB6FB7LB7LB9XB9XB9XB:^B:^B;dB<jB;dB;dB<jB;dB;dB<jB;dB<jB<jB=qB=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�C�A�1'A�7LA�+A�oA��A��A��A��;A��#A��;A��AͰ!A�A�ȴA͛�A͸RA;wA͑hA͍PA�v�A�ffA�A�A��A�(�A���A���A̝�A�S�A�S�A��HA��HA˥�A˶FA�x�A�5?Aʺ^Aʩ�A�%A�S�A��`A�"�A�ffA���A��TA���A�M�A�ȴA���A���A�JA��A��
A�9XA���A�VA�K�A�bA��wA��+A�ffA��A��TA���A��A��/A���A�"�A�\)A��\A�JA��A���A�ȴA�1A�^5A���A���A�p�A�K�A��A��jA���A�  A�ƨA�A�$�A��A��hA���A��A���A�O�A��/A���A�VA�A�=qA��A��A� �A�ĜA���A�A�S�A��hA�bNA�n�A��+AoA}/A{Ay��Aw�AwoAt�+Arz�ApbAm|�Akt�Ai�#Ag��AehsAcK�Aa%A_hsA]ƨA]
=A[\)AY%AWO�AU�^ATI�AR-APJAN�AM�AK�AJ��AI�hAH�AHbNAGO�AE��AD�AC��ABȴAA�A@-A>�/A=O�A<M�A:1'A9�PA8�A6�A5ƨA4�`A3�A1��A0�yA/�A/S�A-p�A,�A+&�A*�!A)33A(�jA'��A&Q�A$�A$JA"�9A"E�A!p�A �DA �+Al�A�PA��A�A �Av�A�yA�\A$�A/AA�A�A�A��A��A�/A�hA�AAr�A��AffA��A��A��A�wA?}A	��A	l�Ar�A�
AS�A��A1'A-AȴAbA{AA ��A �D@�v�@���@�X@���@���@��/@�K�@��@�G�@��@���@�/@�!@�^5@���@�&�@�@�\@���@�{@�bN@�bN@���@���@䛦@�1'@䛦@���@�E�@�R@��@���@�9@ݺ^@���@��@���@�hs@�v�@��@���@�(�@��@���@�@�7L@ԣ�@ӕ�@���@��@�V@Л�@�Ĝ@��y@ͺ^@�`B@���@�{@�j@� �@�v�@�dZ@˥�@��@ʏ\@��/@�ƨ@�dZ@ř�@�?}@�(�@�ff@�hs@�Z@�b@�^5@�&�@���@�;d@�|�@���@��@��R@���@���@�1@��@�/@���@���@��@�X@���@�/@�z�@���@��@�o@�5?@�x�@�/@�(�@��D@��F@��;@�r�@�ff@��R@�/@���@�;d@���@���@�I�@�b@��
@��
@���@���@�o@�ff@��@��y@�+@���@�K�@���@���@�7L@���@�%@��;@�7L@�z�@�7L@��/@��w@�C�@��@��H@��!@���@�+@�`B@�+@��@�n�@�z�@��@�?}@���@��@���@��D@�"�@�x�@�K�@��@��@��@��H@�\)@�@�v�@�G�@���@���@�X@�K�@��h@�X@��#@�bN@�Q�@��;@�\)@��;@��@��@�
=@���@�V@�=q@�dZ@��y@��!@��m@��u@��@�Q�@��u@���@�bN@��/@��P@�z�@��@�1'@���@���@��u@��;@�/@�z�@�z�@�7L@���@��@�`B@���@��@���@��F@��
@��@��D@��
@�^5@���@��y@��y@�;d@�K�@���@���@���@�J@�=q@�;d@��h@��!@��/@��@��@�7L@�p�@��j@�(�@��@��h@�1@��D@��H@�^5@�C�@��!@���@�=q@�-@�5?@��
@��9@� �@� �@� �@�~�@�33@�ȴ@��`@�-@��H@�$�@���@��u@�{@�1'@�p�@�S�@��y@�7L@��u@�5?@���@��^@�O�@�J@�/@��h@��T@�{@�?}@�X@��@�b@��/@��9@��9@���@�/@~�@{33@y�^@{dZ@z^5@{o@y�7@y�@y�^@x�u@z�@v�R@xA�@v�+@v{@t�@vȴ@u�-@s�@u@s�F@r�H@u`B@r�H@t1@qG�@sS�@pb@sƨ@qx�@q�#@r-@t��@s��@s@qx�@o�@q��@p�@pbN@r=q@pA�@p �@p�@m�@o�@m��@n��@mV@l�@lI�@i%@l�@h �@kƨ@iG�@f�R@f�@cdZ@hA�@f�y@d��@hbN@e`B@gl�@b��@h  @d(�@ep�@dI�@c�@c�@a�7@c��@cC�@b�!@`��@c��@_�@`Q�@ax�@`Ĝ@_;d@`Q�@`b@`bN@_+@]?}@^{@]�T@]V@^ff@^��@_��@\�@[��@]@\��@Z�\@Yx�@Z-@ZM�@X��@Y��@\��@Z�@[��@W��@XA�@V5?@XbN@T��@ZJ@T(�@W\)@R^5@T��@XQ�@St�@U/@V��@Rn�@V�R@S@Q��@Tj@Q7L@Q��@R�@PĜ@PQ�@Q�@U��@T��@P��@Q&�@O�@Sƨ@R��@PbN@O
=@Q��@Q��@P1'@O�P@P�9@N��@O\)@O;d@M�h@Q�@N��@P1'@PQ�@L��@J�@J�H@KC�@I�#@K��@Hr�@J^5@JJ@Ko@IG�@FV@H��@F{@J~�@G��@FE�@F5?@F�+@H��@D��@F��@Dj@F�R@E/@E�h@G�w@F��@F�R@CC�@D�D@E�-@AX@A��@D�D@A��@B=q@@�@A��@?|�@A�7@A�@A��@?|�@?�w@>ȴ@Ahs@@�u@>�y@=�h@=�-@=�@=��@@�9@?|�@?
=@=V@<��@;�@9��@>E�@=p�@;��@<Z@;ƨ@:M�@<1@;�m@8�@:=q@:�H@7\)@9�#@8�9@9X@8��@5�h@5�-@:��@9&�@6��@1�@7K�@6�@5p�@7+@5`B@4��@8��@4�@3�F@6v�@7�@4��@7��@5��@4�/@2�!@1��@1��@5O�@4�@5/@17L@0��@3"�@3C�@0��@2��@3dZ@1hs@2�@1�7@3�m@2~�@-��@1�#@/�@1�^@2-@/��@-��@1x�@/;d@/�P@0�u@.��@,j@1hs@-�T@+�@+ƨ@.5?@*��@+�@-��@.V@+��@*n�@,��@,z�@,�j@+��@*M�@*-@)hs@*J@(1'@+�F@*J@)hs@'
=@)��@(A�@(Q�@(��@'�;@+S�@$Z@(bN@*=q@(A�@(�9@&V@(�u@)G�@(  @%/@)&�@%�T@&V@'|�@"�\@(A�@#ƨ@"��@$1@#�m@#��@"^5@#�F@#t�@%O�@#@$�/@!�@$Z@&{@"��@%p�@�@#��@"^5@#t�@
=@ ��@!��@ ��@ 1'@!��@
=@!�@#33@!X@;d@+@#��@ ��@@��@�@��@p�@ �`@?}@��@�@V@S�@��@��@�D@9X@x�@M�@Ĝ@Ĝ@x�@X@X@@b@
=@r�@Q�@�9@��@;d@ff@�@?}@(�@�@�D@��@��@�+@�H@(�@=q@�m@1@n�@�D@l�@�T@ �@j@�#@=q@dZ@~�@�^@��@ȴ@�@��@�;@�+@��@�j@
^5@@
��@��@`B@	��@"�@
�@
~�@�P@�/@�9@
=@�T@��@�T@�y@��@�@��@�w@E�@�9@9X@�@O�@�y@@�y@�H@�D@E�@�@�+@O�@�T@Ĝ@�@M�@;d@�/@;d@�h@�@�@��@�\@��@�h@/@��@I�@�j@�!@�T@�j@��@dZ@��@��@��@V@&�@�T@�H@(�A�g8A�g8A��jA�g8A�g8A͢�A̴nAˊ	A�#nA��)A� �A�@OA��A��pA���A���A��fA��A�}�A���A� �A��A�Z�Ay��Am�AcOA\�AS��AI�AE�?A>�"A;�uA5G�A-2�A,�A �A&�A֡A��@�qv@���@��j@���@��@���@���@�>B@�<�@�7�@���@�$�@z�H@uw2@_��@Wx@O;d@Wx@O;d@>��@>��@0Ɇ@+6z@3�:@%�'@"�,@�)@�D@:�@�}@خ?�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�ffA�ffA��A��;A��A�ffA��;A�ffA��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�VA��;A��A�ffA��A�ffA��A�ffA�+A�ffA��Aͣ�A��A�ffA�ffA�ffAͣ�A�ffA̴9A��A��A�+Aͣ�A�ffA�ffA��;A��AΑhA�ffA�ffA�ffA�ffA�ffA�ffA��;A��;A�ffA�ffA��A�ffA�ffA�ffA�ffA��Aͣ�A��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffAͣ�A�ffAͣ�A��Aͣ�A�ffAͣ�A��A��A�ffA�ffA��;A��;A�ffA�VA��;A�VA�ffA�ffA��;A̴9A�ffA�ffA�ffA�ffA��;Aͣ�A��Aͣ�A�+A�ffAΑhA��A��A��;A��A�VA�ffAΑhA��Aͣ�A�ffAͣ�A�ffAͣ�Aͣ�A��;Aͣ�A�ffAͣ�A��A�ffA�VA�ffA�ffA��A���A��;A��;AΑhA�ffA�ffA�ffAͣ�A�ffA�ffA��A��;A��A�VAͣ�A�ffAͣ�A�ffAΑhA�VA��AΑhAͣ�A�ffAͣ�Aͣ�Aͣ�A�ffAͣ�A�ffA�VA��;A�ffAΑhA�ffAͣ�A���A��A�ffA��;A��;A�ffA�ffAͣ�Aͣ�A�ffAͣ�Aͣ�A��Aͣ�A�ffAͣ�A��Aͣ�Aͣ�A�VA��;A�VA�ffA��;A�VA�VA��A�VA��;A��;Aͣ�Aͣ�Aͣ�A�ffAͣ�A�ffA�VA�ffA�ffA�VA��A��;A�VAͣ�Aͣ�A�VA��;A�ffAͣ�Aͣ�Aͣ�A�ffA��A��;A�VA��;Aͣ�A�ffAΑhA��A��;Aͣ�Aͣ�A���A��A�VA���A��;A�+A�VA�ffA��A�VA�ffA�VA��A��A��;A��;Aͣ�A��Aͣ�A�1A�VAͣ�A�ffAΑhAΑhA�VAͣ�A�VA��A��;A��;A��A��;A��A���Aͣ�A��;A�~�A��A��;AΑhA��AΑhA��;Aͣ�Aͣ�AΑhAͣ�A�VA�C�A��A�VA��;A�VA�ffA�ffAͣ�A��;A��A��;A��;A�VA��A�C�A���A��A�VA�ffA��;A��A��A���AΑhA�VA�1AΑhAͣ�A��A��;A��Aͣ�A�VA��;A�VAΑhA��A�1A��;Aͣ�A�ffA��A��;A��;A��;Aͣ�A�VA�VAͣ�AΑhA��;A��;AΑhA��;A���A��Aͣ�A�~�AΑhAΑhAΑhA��;A��;A��;AΑhAͣ�A��;A��;A�VA�1A�VA�1AΑhA�VA��A�1A�1AΑhA��A��A��A��;A��;AΑhAͣ�Aͣ�Aͣ�AΑhA�VAͣ�A��;A��;Aͣ�A��;Aͣ�A�1A���A�VAΑhA��A��A���AΑhA��A��;AΑhA���A���A��;A�ffA�~�A��A���A��AΑhA��Aͣ�A��A��;A��;AΑhA�ffAͣ�A���A�ffA��;A�1A���A��A�ffA�1A���A�ffAΑhAΑhA��A�ffAͣ�AΑhA��AΑhA��A�ffAΑhA�ffA��;Aͣ�A�ffA��;A��A��;A��;AΑhA�VAΑhAΑhA�VAͣ�A�1Aͣ�A�VA�VA��AΑhA�1A�ffA��;A�VA��;A�ffAͣ�A��A�ffAΑhA��;Aͣ�Aͣ�A��;A�ffA��;A�VA�VAͣ�A�VAͣ�A�ffA�ffAͣ�A��AΑhA��;A�VA��A��A��Aͣ�Aͣ�AΑhA��A�VAΑhAͣ�Aͣ�A�ffA��Aͣ�A�VA��;A��;A��A��Aͣ�A�ffA��;A��;Aͣ�A��;A��;A��Aͣ�A��Aͣ�A�ffA�ffAͣ�A�ffAͣ�A��A�1A�ffAͣ�AΑhAͣ�A���A��;Aͣ�A�VA�ffAΑhA�VA��;A�ffA��A�VAΑhAͣ�Aͣ�Aͣ�A�VAͣ�Aͣ�Aͣ�A��;Aͣ�Aͣ�A���Aͣ�A�ffAͣ�A��A��A���Aͣ�A�ffA��Aͣ�Aͣ�Aͣ�AΑhA��;A�ffA�ffA��A���A��;Aͣ�A��A��A��A��A�ffA�ffAͣ�AΑhA�VA�+A��;A��Aͣ�A���A�ffA�ffA��;A��;A�ffA�ffAͣ�A�ffA�1A�ffA�ffA�ffAͣ�Aͣ�A�VA�ffAͣ�Aͣ�A��;Aͣ�A��A��;A��;A��A�VA�ffA�ffA��;A��;AΑhAͣ�A���A��;A�ffA�ffAͣ�Aͣ�A�ffA�ffA�ffAͣ�AΑhA��;A�ffA�ffA�ffA�ffA��A�VAͣ�A̴9A�ffA���Aͣ�A��;A��;Aͣ�A���A�ffA�ffA�ffA�ffA�ffAͣ�A��Aͣ�A��;A�ffA�1A�ffA�ffA�ffA�ffA��;A�ffA�ffAͣ�A�VA�ffA�ffA��;A�VA��;A�ffA�ffAͣ�Aͣ�A��A��;A��Aͣ�Aͣ�A�ffA��;A�ffA�ffA�ffAͣ�A�ffAͣ�A��A��A��A��A��;A�ffAͣ�A��;A�ffA��A��;A��;A��;A��A��;A�VA��;Aͣ�A��A�ffA��A��A��;Aͣ�A�ffA��A̴9A�+A��;A�ffA�ffA��A�x�Aͣ�Aͣ�A�VA��;A�ffA�ffAͣ�A�ffAͣ�A��A��Aͣ�A�ffA�VA�ffAͣ�A�ffA�+A��A��A�+A�ffA��Aͣ�A�ffA̴9AΑhA�ffA��Aͣ�A�ffA�+A�ffAͣ�A�+A��Aͣ�A�ffA�+Aͣ�Aͣ�A�+A�ffA�ffA��A�ffA�+A��A�ffA��A��;A��A�+Aͣ�A�+A̴9A��A�+Aͣ�Aͣ�A��A�+A�VA̴9A�+A�+A�+A�ffA��;A̴9A��A�+A��A��A��;A��;A��A�+A��A̴9A��;A�+A��A��A�ffA�+A��Aͣ�A��A̴9A�+Aͣ�A�+A�+A�+A��Aͣ�A̴9A�+A�+A�+A̴9A��A��A̴9A�+Aͣ�A�+A��A̴9A̴9A�+A̴9A�+A�ffA̴9Aͣ�A�VAͣ�A��A�+A��A̴9A̴9A�x�A�+A�+A̴9A�+A̴9A�x�A�+A�x�A�x�A��A�x�A�+A�x�A�x�A�x�A��A�x�A�ffA�ffA�x�A�+A�+A�+A�+A̴9A�x�A̴9A�=qA̴9A��A�x�A̴9A�+A̴9A�x�A�=qA̴9A�x�A��A�+A�x�A�+A�x�A�+A̴9A�x�A�=qA̴9A�A�+A�x�A�x�A�x�A�=qA̴9A̴9A�x�A̴9A��A�A��A�ƨA�=qA�ƨA�A��A�=qA�=qA�x�A�=qA�x�A�=qA�x�A̴9A�=qA̴9A�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                    PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546202210041155462022100411554620221004115546AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            202208232240592022082322405920220823224059    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232240592022082322405920220823224059  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B83E            383E            383E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232240592022082322405920220823224059  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�8800            800             800             UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202210041155462022100411554620221004115546  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                