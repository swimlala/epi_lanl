CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-08-23T22:44:54Z creation      
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
resolution        =���     .�  |\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     .�  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .�  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .�  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � O�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� [@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� �|   	TEMP_CNDC            
         	long_name         -Internal temperature of the conductivity cell      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     .� .h   TEMP_CNDC_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                 @ i   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 $  kP   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 $  �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 $  �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                 � �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ڠ   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ڬ   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ڸ   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20220823224454  20221004115607  5906299 5906299 5906299 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER,                                                  STEPHEN RISER,                                                  STEPHEN RISER,                                                  PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC          �   �   �AAA AOAOAO  8443                            8443                            8443                            2C  2C  2C  DDD APEX                            APEX                            APEX                            8815                            8815                            8815                            052520                          052520                          052520                          877 877 877 @ٔ�u�2q@ٔ�u�2q@ٔ�u�2q111 @ٔ�$�B@ٔ�$�B@ٔ�$�B@5���R@5���R@5���R�d7�;dZ�d7�;dZ�d7�;dZ111 GPS     GPS     GPS     Primary sampling: averaged []                                                                                                                                                                                                                                   Secondary sampling: discrete []                                                                                                                                                                                                                                 Secondary sampling: discrete [1Hz data]                                                                                                                                                                                                                            �   �   �AAB AAB FFF     >L��@@  @�  @�  A��A0  ANffAp  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B$  B,  B4  B<  BD  BL  BT��B\ffBb  Bl  Bt  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C	  C  C  C  C  C  C  C  C�fC  C  C  C!  C#  C%  C'�C)�C+  C-  C/  C1  C3  C5  C7  C9  C;  C=  C?�CA�CC  CE  CG�fCK  CM  CO  CQ  CS  CU  CW  CY  C[  C]  C_  Ca  Cc  Ce  Cg  Ci  Ck  Cm  Co  Cq  Cs  Cu  Cw  Cy  C{  C}  C  C�� C�� C�s3C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C CÀ CĀ Cŀ Cƀ Cǀ CȌ�Cɀ Cʀ Cˀ C̀ C̀ C΀ Cπ CЀ Cр CҀ CӀ CԀ CՀ Cր C׀ C؀ Cـ Cڀ Cۀ C܀ C݀ Cހ C߀ C�� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C�� C� C� C� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D @ D � D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D	@ D	� D
@ D
� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D @ D � D!@ D!� D"@ D"� D#@ D#� D$@ D$� D%@ D%� D&@ D&� D'@ D'� D(@ D(� D)9�D)� D*@ D*� D+@ D+� D,@ D,� D-@ D-� D.@ D.� D/@ D/� D0@ D0� D1@ D1� D2@ D2� D3@ D3� D4@ D4� D5@ D5� D6@ D6� D7@ D7� D8@ D8� D9@ D9� D:@ D:� D;@ D;� D<@ D<� D=@ D=� D>@ D>� D?@ D?� D@@ D@� DA@ DA� DB@ DB� DC@ DC� DD@ DD� DE@ DE� DF@ DF� DG@ DG� DH@ DH� DI@ DI� DJ@ DJ� DK@ DK� DL@ DL� DM@ DM� DN@ DN� DO@ DO� DP@ DP� DQ@ DQ� DR@ DR� DS@ DS� DT@ DT� DU@ DU� DV@ DV� DW@ DW� DX@ DX� DY@ DY� DZ@ DZ� D[@ D[� D\@ D\� D]@ D]� D^@ D^� D_@ D_� D`@ D`� Da@ Da� Db@ Db� Dc@ Dc� Dd@ Dd� De@ De� Df@ Df� Dg@ Dg� Dh@ Dh� Di@ Di� Dj@ Dj� Dk@ Dk� Dl@ Dl� Dm@ Dm� Dn@ Dn� Do@ Do� Dp@ Dp� Dq@ Dq� Dr@ Dr� Ds@ Ds� Dt@ Dt� Du@ Du� Dv@ Dv� Dw@ Dw� Dx@ Dx� Dy@ Dy� Dz@ Dz� D{@ D{� D|@ D|� D}@ D}� D~@ D~� D@ D� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D��D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�c3D��3D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D��3D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D���D�� D�#3D�` D�� D��3D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D  D�� D�  D�` Dà D�� D�  D�` DĠ D�� D�  D�` DŠ D�� D�  D�` DƠ D�� D�  D�` DǠ D�� D�  D�` DȠ D�� D�  D�` Dɠ D�� D�  D�` Dʠ D�� D�  D�` Dˠ D�� D�  D�\�D̠ D�� D�  D�` D͠ D�� D�  D�` DΠ D�� D�  D�` DϠ D�� D�  D�` DР D�� D�  D�` DѠ D�� D�  D�` DҠ D�� D�  D�` DӠ D�� D�  D�` DԠ D�� D�  D�` Dՠ D�� D�  D�` D֠ D�� D�  D�` Dנ D�� D�  D�` Dؠ D�� D�  D�` D٠ D�� D�  D�\�Dڠ D�� D�  D�` D۠ D�� D�  D�` Dܠ D�� D�  D�` Dݠ D�� D�  D�` Dޠ D�� D�  D�` Dߠ D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D�� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�@ D��f@ǮA*ffA��RA�{B\)BK{Br��B��{B��)B�\)B���B�  B��C��CECJ=C��C*�
C4}qC>�
CH��CR��C\��Cf��Cp�)Czk�C�HC�0�C��C�T{C�` C�u�C�AHC��C�aHC���C�K�C��Cǜ)C�P�C�RD	�HD5�D"��D/"�D;�=DH�DT��Da8�Dm}qDz�D�L�D�}D��HD��D�AHD���D�D�D�V�D��D�ӅD�
D�[�D�yHD�ɚD�	�D�@�D�)D��D�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=���    >L��>L��>���=���=���=���=���>L��>L��>���>L��>���>���>L��    =���=���>L��>L��>���>���>���?   >���    >L��=���=���>L��>L��>���>L��=���=���=���=���=���=���    =���=���=���>���=���>L��=���=���>L��=���>���>L��>L��>L��    >L��=���=���>L��=���    =���>L��?   >���>���>���>���=���>L��>L��>���>���>L��=���=���>L��=���=���>L��>L��>���>���    >���>L��=���>���    >L��>���?   ?��?333?fff?�  ?���?�ff?�  ?ٙ�?�ff@   @ff@��@��@&ff@333@9��@Fff@L��@Y��@fff@s33@y��@�33@�ff@���@�33@�ff@���@�33@�ff@���@�33@���@�  @�ff@�ff@���@�33@ٙ�@�  @�33@陚@���@�33@���A   A33AffA  A33AffA  A33AffA��A33AffA!��A$��A(  A)��A,��A0  A1��A4��A8  A;33A<��A@  AC33AD��AH  AK33ANffAP  AS33AVffAX  A[33A^ffA`  Ac33AfffAh  Ak33AnffAq��As33AvffAy��A|��A~ffA���A�ffA�33A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���Ař�A�33A���Aə�A�ffA�  A���A�ffA�  A���Aљ�A�33A�  Aՙ�A�ffA�  A���A�ffA�33A�  Aݙ�A�ffA�  A���AᙚA�33A�  A噚A�ffA�33A���A陚A�33A�  A홚A�ffA�33A�  A�A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33B   B ffB ��B��B  BffB33B��B  BffB��B33B  BffB��B33B��B  B��B	33B	��B
  B
ffB
��B��B  BffB��B33B��B  BffB33B��B  BffB��B33B��B  BffB��B33B��B  BffB33B��B  BffB��B33B��B  BffB��B33B��B  B��B33B��B  BffB��B33B��B  BffB��B33B   B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB%33B%��B&  B&ffB&��B'33B'��B(  B(ffB(��B)33B)��B*  B*ffB*��B+��B+��B,  B,��B,��B-��B.  B.ffB.��B/33B/��B0  B0ffB0��B133B1��B2  B2ffB2��B333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B7��B8  B8ffB8��B933B9��B:  B:ffB:��B:��B;33B;��B<  B<ffB<��B=33B=��B>  B>ffB>��B?33B?��B@  B@  B@ffB@��BA33BA��BB  BBffBB��BC33BC��BC��BD  BDffBD��BE33BE��BF  BF  BFffBF��BG33BG��BH  BH  BHffBH��BI33BI��BI��BJ  BJffBJ��BK33BK33BK��BL  BLffBL��BL��BM33BM��BN  BN  BNffBN��BO33BO��BO��BP  BPffBP��BP��BQ33BQ��BQ��BR  BRffBR��BR��BS33BS��BS��BT  BTffBTffBT��BU33BU33BU��BV  BV  BVffBV��BV��BW33BW��BW��BX  BX  BXffBX��BX��BY33BY33BY��BZ  BZ  BZ  BZffBZ��BZ��B[33B[33B[��B[��B\  B\  B\ffB\ffB\��B\��B\��B]33B]33B]��B]��B]��B^  B^  B^  B^ffB^ffB^ffB^��B^��B^��B^��B_33B_33B_33B_33B_��B_��B_��B_��B_��B`  B`  B`  B`  B`  B`  B`  B`  B`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`ffB`  B`  B`  B`  B`  B`  B`  B`  B_��B_��B_��B_��B_��B_��B_��B_33B_33B_33B_33B^��B^��B^��B^��B^��B^ffB^ffB^ffB^ffB^ffB^ffB^  B^  B^  B^  B^  B]��B]��B]��B]��B]��B]33B]33B]33B]33B]33B]33B\��B\��B\��B\��B\ffB\ffB\ffB\ffB\ffB\  B\  B\  B\  B\  B[��B[��B[��B[��B[33B[33B[33B[33B[33BZ��BZ��BZ��BZ��BZffBZffBZffBZffBZ  BZ  BZ  BZ  BZ  BY��BY��BY��BY��BY33BY33BY33BX��BX��BX��BX��BXffBXffBXffBXffBX  BX  BX  BX  BW��BW��BW��BW��BW33BW33BW33BW33BV��BV��BV��BVffBVffBVffBVffBV  BV  BV  BU��BU��BU��BU��BU��BU33BU33BU33BU33BU33BT��BT��BT��BT��BT��BTffBTffBTffBTffBTffBT  BT  BT  BT  BT  BT  BT  BT  BT  BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BS��BT  BT  BT  BT  BT  BT  BT  BTffBTffBTffBTffBT��BT��BT��BT��BU33BU33BU33BU33BU��BU��BU��BU��BV  BV  BVffBVffBVffBV��BV��BV��BW33BW33BW33BW��BW��BX  BX  BX  BXffBXffBX��BX��BY33BY33BY��BY��BY��BZ  BZ  BZffBZffBZ��B[33B[33B[��B[��B\  B\  B\ffB\ffB\��B]33B]33B]��B]��B^  B^ffB^ffB^��B^��B_33B_��B_��B`  B`ffB`ffB`��B`��Ba33Ba��Ba��Bb  BbffBb��Bb��Bc33Bc��Bc��Bd  BdffBdffBd��Be33Be��Be��Bf  BfffBfffBf��Bg33Bg33Bg��Bh  BhffBhffBh��Bi33Bi��Bi��Bj  BjffBj��Bj��Bk33Bk��Bk��Bl  BlffBlffBl��Bm33Bm��Bn  Bn  BnffBn��Bn��Bo33Bo��Bp  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  114111111111111114111111111411111111111114111111111111114111114111111111111111111111141111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      =u@7
=@��@ۅA\)A-AL(�AmA��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B#p�B+p�B3p�B;p�BCp�BKp�BT=qB[�
Bap�Bkp�Bsp�B{p�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��B��B��RB��RB��RB��RB��RBŸRBɸRB͸RBѸRBոRBٸRBݸRB�RB�RB�RB��RB�RB��RB��RB��RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)CC�)C�)C�)C �)C"�)C$�)C&��C(��C*�)C,�)C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>��C@��CB�)CD�)CGCJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�nC�nC�aGC�nC�nC�nC�nC�nC�nC�nC�nC�z�C�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�z�C�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nC�nD 7
D �
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D	7
D	�
D
7
D
�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D7
D�
D 7
D �
D!7
D!�
D"7
D"�
D#7
D#�
D$7
D$�
D%7
D%�
D&7
D&�
D'7
D'�
D(7
D(�
D)0�D)�
D*7
D*�
D+7
D+�
D,7
D,�
D-7
D-�
D.7
D.�
D/7
D/�
D07
D0�
D17
D1�
D27
D2�
D37
D3�
D47
D4�
D57
D5�
D67
D6�
D77
D7�
D87
D8�
D97
D9�
D:7
D:�
D;7
D;�
D<7
D<�
D=7
D=�
D>7
D>�
D?7
D?�
D@7
D@�
DA7
DA�
DB7
DB�
DC7
DC�
DD7
DD�
DE7
DE�
DF7
DF�
DG7
DG�
DH7
DH�
DI7
DI�
DJ7
DJ�
DK7
DK�
DL7
DL�
DM7
DM�
DN7
DN�
DO7
DO�
DP7
DP�
DQ7
DQ�
DR7
DR�
DS7
DS�
DT7
DT�
DU7
DU�
DV7
DV�
DW7
DW�
DX7
DX�
DY7
DY�
DZ7
DZ�
D[7
D[�
D\7
D\�
D]7
D]�
D^7
D^�
D_7
D_�
D`7
D`�
Da7
Da�
Db7
Db�
Dc7
Dc�
Dd7
Dd�
De7
De�
Df7
Df�
Dg7
Dg�
Dh7
Dh�
Di7
Di�
Dj7
Dj�
Dk7
Dk�
Dl7
Dl�
Dm7
Dm�
Dn7
Dn�
Do7
Do�
Dp7
Dp�
Dq7
Dq�
Dr7
Dr�
Ds7
Ds�
Dt7
Dt�
Du7
Du�
Dv7
Dv�
Dw7
Dw�
Dx7
Dx�
Dy7
Dy�
Dz7
Dz�
D{7
D{�
D|7
D|�
D}7
D}�
D~7
D~�
D7
D�
D��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD�RD�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�^�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�޸D��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D��RD�ۅD��D�[�D���D�޸D��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D�D�ۅD��D�[�DÛ�D�ۅD��D�[�Dě�D�ۅD��D�[�Dś�D�ۅD��D�[�Dƛ�D�ۅD��D�[�DǛ�D�ۅD��D�[�Dț�D�ۅD��D�[�Dɛ�D�ۅD��D�[�Dʛ�D�ۅD��D�[�D˛�D�ۅD��D�XRD̛�D�ۅD��D�[�D͛�D�ۅD��D�[�DΛ�D�ۅD��D�[�Dϛ�D�ۅD��D�[�DЛ�D�ۅD��D�[�Dћ�D�ۅD��D�[�Dқ�D�ۅD��D�[�Dӛ�D�ۅD��D�[�Dԛ�D�ۅD��D�[�D՛�D�ۅD��D�[�D֛�D�ۅD��D�[�Dכ�D�ۅD��D�[�D؛�D�ۅD��D�[�Dٛ�D�ۅD��D�XRDڛ�D�ۅD��D�[�Dۛ�D�ۅD��D�[�Dܛ�D�ۅD��D�[�Dݛ�D�ۅD��D�[�Dޛ�D�ۅD��D�[�Dߛ�D�ۅD��D�[�D���D�ۅD��D�[�DᛅD�ۅD��D�[�D⛅D�ۅD��D�[�D㛅D�ۅD��D�[�D䛅D�ۅD��D�[�D囅D�ۅD��D�[�D曅D�ۅD��D�[�D盅D�ۅD��D�[�D蛅D�ۅD��D�[�D雅D�ۅD��D�[�DꛅD�ۅD��D�[�D뛅D�ۅD��D�[�D웅D�ۅD��D�[�D훅D�ۅD��D�[�DD�ۅD��D�[�DD�ۅD��D�[�D�D�ۅD��D�[�D�D�ۅD��D�[�D�D�ۅD��D�[�D�D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD��D�[�D���D�ۅD�;�D���@�33A((�A���A���B��BJ�Br{B�L�B��{B�{BȅBܸRB�\)Cc�C!HC&fC�C*�3C4Y�C>s3CH�CRh�C\��Cf^�CpxRCzG�C��\C��C���C�B�C�NC�c�C�/\C���C�O\C��C�9�C��CǊ=C�>�C�fD	�RD,�D"��D/�D;�GDG��DT��Da0 Dmt{Dz�D�HRD�x�D���D�HD�<�D��\D��D��D�R=D���D��
D��D�W
D�t�D��D�qD�<{D��D�љD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��#�
�#�
G�O�=u=u>#��#�
�#�
�#�
�#�
=u=u>��=u>#�>#�=uG�O��#�
�#�
=u=u>��>��>#�>�Q�>#�G�O�=u�#�
�#�
=u=u>#�=u�#�
�#�
�#�
�#�
�#�
�#�
G�O��#�
�#�
�#�
>#��#�
=u�#�
�#�
=u�#�
>#�=u=u=uG�O�=u�#�
�#�
=u�#�
G�O��#�
=u>�Q�>#�>��>#�>���#�
=u=u>��>��=u�#�
�#�
=u�#�
�#�
=u=u>��>#�G�O�>#�=u�#�
>#�G�O�=u>��>�Q�>� ?\)?B�\?\(�?u?�z�?�{?Ǯ?�z�?�{?��G@�
@��@p�@*=p@0��@=p�@C�
@P��@]p�@j=p@p��@}p�@��@�Q�@��R@��@�Q�@��R@��@�Q�@��R@��@��@��@��@�Q�@θR@��@ۅ@޸R@��@�Q�@�R@��@��A ��A(�AA��A(�AA��A(�A\)A��A(�A\)A"�\A%A'\)A*�\A-A/\)A2�\A5A8��A:�\A=A@��AB�\AEAH��AL(�AMAP��AT(�AUAX��A\(�A]A`��Ad(�AeAh��Al(�Ao\)Ap��At(�Aw\)Az�\A|(�A\)A�G�A�{A��A�G�A�{A��A�G�A�{A��A�G�A�{A��A�G�A�{A��A�z�A�{A��A�z�A�{A��HA�z�A�G�A��HA�z�A�G�A��HA�z�A�G�A��HA��A�G�A��HA��A�G�A�{A��A�z�A�{A��A�z�A�{A��HA�z�A�G�A��HA��A�G�A��HA��A�G�A�{AîA�z�A�{AǮA�z�A�G�A��HAˮA�G�A��HAϮA�z�A�{A��HA�z�A�G�A��HA׮A�G�A�{A��HA�z�A�G�A��HA߮A�z�A�{A��HA�z�A�G�A�{A�A�z�A�{A��HA�z�A�G�A�{A��HA�z�A�G�A��HA�A�z�A�{A��HA��A�z�A�{A��HA��A�G�A�{A��HA��B =qB
>Bp�B�
B��B
>Bp�B�
B=qB��Bp�B�
B=qB��B
>Bp�B=qB��B	
>B	p�B	�
B
=qB
>Bp�B�
B=qB��B
>Bp�B�
B��B
>Bp�B�
B=qB��B
>Bp�B�
B=qB��B
>Bp�B�
B��B
>Bp�B�
B=qB��B
>Bp�B�
B=qB��B
>Bp�B=qB��B
>Bp�B�
B=qB��B
>Bp�B�
B=qB��Bp�B�
B =qB ��B!
>B!p�B!�
B"=qB"��B#
>B#p�B#�
B$��B%
>B%p�B%�
B&=qB&��B'
>B'p�B'�
B(=qB(��B)
>B)p�B)�
B*=qB+
>B+
>B+p�B,=qB,=qB-
>B-p�B-�
B.=qB.��B/
>B/p�B/�
B0=qB0��B1
>B1p�B1�
B2=qB2��B3
>B3p�B3�
B4=qB4��B5
>B5p�B5�
B6=qB6��B7
>B7p�B7�
B8=qB8��B9
>B9p�B9�
B:=qB:=qB:��B;
>B;p�B;�
B<=qB<��B=
>B=p�B=�
B>=qB>��B?
>B?p�B?p�B?�
B@=qB@��BA
>BAp�BA�
BB=qBB��BC
>BC
>BCp�BC�
BD=qBD��BE
>BEp�BEp�BE�
BF=qBF��BG
>BGp�BGp�BG�
BH=qBH��BI
>BI
>BIp�BI�
BJ=qBJ��BJ��BK
>BKp�BK�
BL=qBL=qBL��BM
>BMp�BMp�BM�
BN=qBN��BO
>BO
>BOp�BO�
BP=qBP=qBP��BQ
>BQ
>BQp�BQ�
BR=qBR=qBR��BS
>BS
>BSp�BS�
BS�
BT=qBT��BT��BU
>BUp�BUp�BU�
BV=qBV=qBV��BW
>BW
>BWp�BWp�BW�
BX=qBX=qBX��BX��BY
>BYp�BYp�BYp�BY�
BZ=qBZ=qBZ��BZ��B[
>B[
>B[p�B[p�B[�
B[�
B\=qB\=qB\=qB\��B\��B]
>B]
>B]
>B]p�B]p�B]p�B]�
B]�
B]�
B^=qB^=qB^=qB^=qB^��B^��B^��B^��B_
>B_
>B_
>B_
>B_
>B_p�B_p�B_p�B_p�B_p�B_p�B_p�B_p�B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_�
B_p�B_p�B_p�B_p�B_p�B_p�B_p�B_p�B_
>B_
>B_
>B_
>B_
>B_
>B_
>B^��B^��B^��B^��B^=qB^=qB^=qB^=qB^=qB]�
B]�
B]�
B]�
B]�
B]�
B]p�B]p�B]p�B]p�B]p�B]
>B]
>B]
>B]
>B]
>B\��B\��B\��B\��B\��B\��B\=qB\=qB\=qB\=qB[�
B[�
B[�
B[�
B[�
B[p�B[p�B[p�B[p�B[p�B[
>B[
>B[
>B[
>BZ��BZ��BZ��BZ��BZ��BZ=qBZ=qBZ=qBZ=qBY�
BY�
BY�
BY�
BYp�BYp�BYp�BYp�BYp�BY
>BY
>BY
>BY
>BX��BX��BX��BX=qBX=qBX=qBX=qBW�
BW�
BW�
BW�
BWp�BWp�BWp�BWp�BW
>BW
>BW
>BW
>BV��BV��BV��BV��BV=qBV=qBV=qBU�
BU�
BU�
BU�
BUp�BUp�BUp�BU
>BU
>BU
>BU
>BU
>BT��BT��BT��BT��BT��BT=qBT=qBT=qBT=qBT=qBS�
BS�
BS�
BS�
BS�
BSp�BSp�BSp�BSp�BSp�BSp�BSp�BSp�BSp�BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BS
>BSp�BSp�BSp�BSp�BSp�BSp�BSp�BS�
BS�
BS�
BS�
BT=qBT=qBT=qBT=qBT��BT��BT��BT��BU
>BU
>BU
>BU
>BUp�BUp�BU�
BU�
BU�
BV=qBV=qBV=qBV��BV��BV��BW
>BW
>BWp�BWp�BWp�BW�
BW�
BX=qBX=qBX��BX��BY
>BY
>BY
>BYp�BYp�BY�
BY�
BZ=qBZ��BZ��B[
>B[
>B[p�B[p�B[�
B[�
B\=qB\��B\��B]
>B]
>B]p�B]�
B]�
B^=qB^=qB^��B_
>B_
>B_p�B_�
B_�
B`=qB`=qB`��Ba
>Ba
>Bap�Ba�
Bb=qBb=qBb��Bc
>Bc
>Bcp�Bc�
Bc�
Bd=qBd��Be
>Be
>Bep�Be�
Be�
Bf=qBf��Bf��Bg
>Bgp�Bg�
Bg�
Bh=qBh��Bi
>Bi
>Bip�Bi�
Bj=qBj=qBj��Bk
>Bk
>Bkp�Bk�
Bk�
Bl=qBl��Bm
>Bmp�Bmp�Bm�
Bn=qBn=qBn��Bo
>Bop�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  114111111111111114111111111411111111111114111111111111114111114111111111111111111111141111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�oA�bA��A� �A� �A� �A�"�A�"�A�"�A�$�A�$�A�&�A�&�A�(�A�(�A�+A�+A�+A�+A�-A�-A�/A�/A�+A�+A�K�A�n�Aְ!A�ĜA�p�A�v�A�x�AʑhAɧ�Aǉ7A�G�Ař�A��A��`AîA�r�A��A���A�p�A��A�XA�VA�"�A��A�-A��9A�hsA���A���A�ffA�M�A�z�A���A��^A��PA�dZA��-A�n�A��-A��`A�p�A�A���A��A��A���A�\)A�ffA��A� �A���A���A��A�A�1'A��PA�XA���A���A�Q�A��PA�33A�VA�`BA���A�A�A��A��A��7A�-A��DA��A�(�A�  A���A���A��A�C�A���A�^5A�ƨA�x�A��A�~�A�;dA���A�VA�r�A~�Az^5Ax9XAu�7At��AtZAr�Ao%AmG�Aj�DAf��Ab�!A_�
A^1A\M�AY"�AX��AW�AT��AR��AN��AL�!AK�7AJ1'AGhsAE�AA��A?"�A=��A;�
A9�
A8��A6z�A5��A4��A2ffA0��A/�A.��A-�^A+�
A*��A)+A(=qA'�-A&�A&-A%�A$��A#XA!��A!K�A ��A��A/A��A
=A�9A�A�!A�A�7A�A��AjA�AbAC�A�\A�TAt�Ar�A�A^5A$�A�mA�wAp�A�`A7LA�#A
�!A
=qA	��A�AA�A�A�A1'A�A�TA��AK�A?}A r�@�;d@���@�A�@�C�@��7@���@�33@�M�@�hs@�Q�@�@�\@�@�9@��@��-@��/@�Z@�\)@�X@�D@�z�@��@��m@��H@�p�@� �@���@⟾@���@�V@��@ߝ�@�ȴ@�5?@��@��@��H@�ff@�{@��/@�1@ם�@�l�@��H@��@�V@Ӯ@҇+@�@�&�@��m@��@�?}@�%@�z�@˶F@�C�@�X@�ƨ@�S�@��@őh@�z�@��@��@�7L@��@�1'@��@���@�O�@�`B@��y@�n�@���@�ff@��\@��!@�ȴ@�~�@���@�X@�{@���@���@���@��H@�Z@�33@�/@���@�I�@�l�@�\)@�(�@�%@�=q@�5?@��;@��@���@�A�@�(�@���@��u@�1'@�t�@�5?@���@���@�|�@�
=@��R@�5?@��@��@��u@��@�t�@��!@�ff@��@�hs@�?}@���@�Ĝ@�r�@��@�ƨ@�S�@�+@�"�@�o@��y@���@�v�@�=q@�{@���@��@���@��7@�hs@�X@�&�@�z�@��@�  @�  @���@�~�@��@���@���@�1'@��+@�=q@��T@��@�^5@�-@���@��@��@��@�^5@�-@��@���@��@��7@�?}@�hs@�X@�O�@�%@��/@��9@���@�j@�A�@�9X@�  @��@�l�@�;d@��@���@��\@���@�X@�hs@�7L@�j@�I�@�r�@�dZ@��H@��@���@��F@�|�@�S�@�o@��H@���@���@�$�@���@�O�@��/@�A�@�r�@�ƨ@�|�@���@���@�v�@�5?@���@�x�@��7@�=q@�^5@�M�@��!@�n�@���@��h@��@���@�$�@�$�@�J@���@�7L@��9@�z�@�9X@��@���@�C�@���@�ff@�5?@���@���@�O�@��@�Q�@�t�@�\)@��P@�C�@�@���@�
=@���@�\)@�C�@��y@�$�@��@���@�@���@��@���@�z�@�  @��w@�|�@�33@�
=@�
=@���@���@���@���@���@�M�@�-@��@���@��@���@���@��h@��7@�`B@���@��u@�bN@�(�@�1@��
@���@�t�@�C�@��y@��R@���@�~�@�ff@�E�@��@��@��T@��#@���@���@�`B@�?}@��@���@�Ĝ@��@��@�j@�I�@�1'@�b@�P@;d@~��@~@}��@}��@}V@|�@|I�@{�m@{"�@zM�@zJ@zJ@y�^@x��@x�@xbN@xQ�@xr�@wl�@v�R@v@u@t��@tI�@s�m@s�@sC�@r�H@r�H@r�\@q��@q�@q&�@p�u@p�@p1'@o�@n�@n@n@m�T@l�@l�@l��@k�F@i��@hr�@hĜ@h��@hQ�@h1'@h  @f�y@f5?@f$�@e�@f{@e@e/@eO�@e?}@d�@dj@c��@cC�@b�!@b�\@b^5@b=q@b�@a��@a�7@aG�@`�9@`�u@`�@`�u@`��@`�9@a�@a%@`bN@_��@_;d@^�y@_
=@_;d@^�y@^��@^E�@^E�@]��@]�@]�@]`B@]�@\z�@[��@Z�H@Z�@Y�#@Y�^@Y��@Y7L@X��@X�u@XQ�@W��@W;d@V�@Vff@U@UO�@UV@TI�@S�m@S��@S�F@SdZ@S"�@S33@R�@Qx�@O�;@Ol�@O+@N��@N��@N�R@N�+@Nv�@Nff@NE�@M��@Mp�@M�@MV@L�@L��@L�@K��@KdZ@KS�@K"�@J��@J~�@JM�@I��@I��@IG�@H�`@HbN@H �@Hb@G�;@Gl�@G+@G
=@F��@FE�@E�-@E��@EO�@D��@D�/@D��@D�D@DI�@D9X@D�@C�F@C��@C�@Ct�@CC�@C"�@C@B��@BM�@A��@A�^@A&�@@bN@@  @?�@?K�@?;d@>�y@>��@>�+@>5?@=�@=��@=O�@=/@<��@<��@<��@<z�@<Z@<(�@;ƨ@;�@;t�@;C�@;33@;"�@;@:�H@:�!@:~�@:^5@:^5@:=q@9��@9��@9x�@9x�@9hs@97L@9%@8�@81'@8  @7�;@7�@7K�@6��@6��@6V@6E�@65?@6@5�T@5�-@5�@5O�@5?}@5V@4�j@4j@49X@41@3��@3t�@3dZ@3C�@3o@2��@2��@2~�@2^5@1��@1�^@1x�@1�@0��@0�`@0��@0�9@0�u@0r�@0 �@/�@/\)@/;d@.�@.�+@.{@-@-p�@-p�@-`B@,��@,�D@,j@,Z@,I�@,�@+�m@+�m@+ƨ@+��@+t�@+@*�!@*�\@*^5@*�@)��@)��@)x�@)�@(��@(�u@(Q�@(  @'�w@'�P@'K�@&��@&�@&ȴ@&��@&V@%�-@%�h@%�@%�@%?}@%V@$�@$��@$�@$��@$z�@$j@#��@#�F@#�@#dZ@#"�@"��@"�\@"^5@"=q@"J@!��@!G�@!�@ ��@ �9@ �@ r�@ r�@ Q�@ 1'@ 1'@ 1'@   @�;@��@\)@�y@�R@ff@�@��@@�-@p�@O�@?}@��@�D@Z@I�@�@ƨ@��@�@dZ@C�@o@@�!@~�@^5@J@��@��@��@x�@�@�`@�@A�@b@�;@�@�P@;d@��@ȴ@�+@v�@v�@5?@�@�@��@��@?}@��@�@Z@(�@�m@t�@C�@"�@@�H@~�@^5@=q@J@��@x�@G�@�@��@�9@r�@1'@b@��@�@�P@l�@K�@+@�@�@ȴ@�+@V@E�@$�@{@�T@�-@�@O�@/@��@��@��@�D@j@Z@1@�
@��@dZ@S�@"�@o@
�H@
��@
�!@
��@
~�@
M�@
-@
�@	��@	��@	��@	��@	x�@	hs@	7L@�9@bNA��A�!-A�$tA�)_A�,qA�+6A��Aǳ3AÙ1A��A�]/A�[�A���A�=A��A�m�A��JA�h>A��A��?A�C�A��-A�-A��_Au�Aj�`A]��AS��AI�[A<P�A4�8A-�A'�9A$2aAw�AA�A�zA<�A	��@��@�xl@��U@��F@�Ĝ@�M@��K@��@�a|@���@�1@���@�B�@y&�@na|@c�r@^�c@U7L@L�U@E@>�6@:Ov@5@@/�@*_@$Ɇ@ 7�@�@��@��@��@�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aװ!A�{A��`A���Aڣ�A�7LAϬA�(�A�-A��A��mA�JA�A��mAڡ�A�^5A��/AϼjA�JA��A�  A�bA�bA�JA���Aڕ�A�n�A�S�AՋDA�K�Aڣ�A�A���A�r�A��A�`BA�ƨAղ-A�hsA�A�t�A��AָRA�t�A��TA�t�A�S�AمA�XAڝ�A���A�A�A��;A��A�
=Aڗ�A�jA�=qA�K�A�/A�I�A�?}A� �A�%A���A�A���A�ƨA�&�A׃A��HA���A��A�5?A�K�A�v�Aا�A�K�A���A�^5A��`A�ĜAڑhA�?}AڬA���A���A��AӁA�A�VA��A��A��A�bA�bA�{A�
=A�
=A�JA�VA�bA�VA�bA�bA�bA�oA�{A�oA�{A�{A�oA�oA�{A�bA�bA�VA�VA�VA�bA�bA�bA�VA�VA�bA�oA�{A�{A�oA�oA�bA�{A��A��A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A�"�A� �A� �A� �A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�+A�+A�-A�+A�-A�-A�-A�-A�+A�-A�-A�+A�+A�-A�+A�-A�-A�-A�-A�-A�-A�+A�-A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�/A�/A�-A�-A�/A�/A�/A�/A�/A�1'A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�-A�/A�/A�/A�-A�-A�-A�-A�-A�-A�-A�+A�+A�-A�-A�-A�+A�(�A�(�A�+A�+A�+A�+A�(�A�(�A�(�A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�+A�+A�+A�(�A�(�A�+A�+A�(�A�+A�+A�(�A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�&�A�$�A�$�A�$�A�$�A�&�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�oA�VA�JA�
=A�1A�1A�1A�1A�%A�A�A���A���A���A���A��A��A��A��A��`A��;A��A���A�ƨA�AھwAں^AڶFAڴ9Aڴ9Aڲ-Aڰ!AڮAڬAڧ�Aڥ�Aڡ�Aڟ�Aڟ�Aڟ�Aڟ�Aڝ�Aڝ�Aڙ�Aڙ�Aڛ�Aڛ�Aڙ�Aڙ�Aڙ�Aڗ�Aڕ�AړuAڑhAڏ\Aڏ\AڍPAڍPAڍPAڋDAڋDAډ7AڅAځA�|�A�~�AڃAڃAڃAځA�|�A�x�A�v�A�t�A�r�A�t�A�v�A�x�A�z�A�z�A�|�A�z�A�z�A�v�A�t�A�t�A�z�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�~�AڃAڅAڅAڇ+Aڇ+Aڇ+Aڇ+Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7AڋDAډ7Aډ7Aډ7Aڇ+AڃA�~�A�x�A�x�A�z�A�z�AڃAڏ\AڑhAڑhAڏ\AڋDAڇ+AځA�|�A�|�AڅAڗ�Aڝ�Aڝ�Aڝ�Aڙ�AړuAڇ+AڃAڇ+AڍPAڙ�Aڣ�Aڣ�Aڟ�Aڙ�Aډ7AڅAړuAڛ�Aڧ�AڬAڬAڬAڬAڬAڮAڲ-Aڴ9AڶFAڶFAڸRAڸRAں^AڼjAڼjAڼjAڸRAڴ9Aڰ!Aک�Aڡ�Aڝ�Aڝ�Aڡ�Aڧ�Aڰ!AڶFAڸRAں^Aں^Aں^Aں^AڼjAڼjAں^AڶFAڰ!AڬAک�Aک�Aڧ�Aڧ�Aڧ�Aڥ�Aڥ�Aڧ�Aڥ�Aڣ�Aڣ�Aڟ�Aڙ�AڑhAڇ+A�|�A�t�A�n�A�hsA�hsA�jA�hsA�bNA�`BA�S�A�A�A� �A���A�A�hsA���A�33A׺^A�n�A�`BA�v�A�r�A�ffA�ffA�Q�A�/A�A��/Aְ!A�x�A�5?A�AծAՃA�z�A�l�A�ffA�ffA�dZA�r�AՉ7A�~�A�x�A�hsA�E�A��A��mA���Aԛ�Aԇ+A�z�A�dZA�Q�A�5?A�1A��#Aӣ�A�5?A��A��`A���A�1A��A�"�A�+A�$�A���Aҧ�AҍPA���A�A�JA�bA�%A���A��A��mA��
A�AҴ9Aҩ�Aҟ�Aҏ\A�~�A�dZA�G�A�&�A�A��yA���Aѩ�Aћ�AэPA�x�A�\)A�I�A�;dA�5?A�+A�"�A��A�%A���A��A��A��yA��;A��
A���A�AжFAв-Aа!AЮAЮAХ�AЧ�AУ�AЩ�AЙ�AБhAЋDAЃA�z�A�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      A���A�oA�bA��A� �A� �A� �A�"�A�"�A�"�A�$�A�$�A�&�A�&�A�(�A�(�A�+A�+A�+A�+A�-A�-A�/A�/A�+A�+A�K�A�n�Aְ!A�ĜA�p�A�v�A�x�AʑhAɧ�Aǉ7A�G�Ař�A��A��`AîA�r�A��A���A�p�A��A�XA�VA�"�A��A�-A��9A�hsA���A���A�ffA�M�A�z�A���A��^A��PA�dZA��-A�n�A��-A��`A�p�A�A���A��A��A���A�\)A�ffA��A� �A���A���A��A�A�1'A��PA�XA���A���A�Q�A��PA�33A�VA�`BA���A�A�A��A��A��7A�-A��DA��A�(�A�  A���A���A��A�C�A���A�^5A�ƨA�x�A��A�~�A�;dA���A�VA�r�A~�Az^5Ax9XAu�7At��AtZAr�Ao%AmG�Aj�DAf��Ab�!A_�
A^1A\M�AY"�AX��AW�AT��AR��AN��AL�!AK�7AJ1'AGhsAE�AA��A?"�A=��A;�
A9�
A8��A6z�A5��A4��A2ffA0��A/�A.��A-�^A+�
A*��A)+A(=qA'�-A&�A&-A%�A$��A#XA!��A!K�A ��A��A/A��A
=A�9A�A�!A�A�7A�A��AjA�AbAC�A�\A�TAt�Ar�A�A^5A$�A�mA�wAp�A�`A7LA�#A
�!A
=qA	��A�AA�A�A�A1'A�A�TA��AK�A?}A r�@�;d@���@�A�@�C�@��7@���@�33@�M�@�hs@�Q�@�@�\@�@�9@��@��-@��/@�Z@�\)@�X@�D@�z�@��@��m@��H@�p�@� �@���@⟾@���@�V@��@ߝ�@�ȴ@�5?@��@��@��H@�ff@�{@��/@�1@ם�@�l�@��H@��@�V@Ӯ@҇+@�@�&�@��m@��@�?}@�%@�z�@˶F@�C�@�X@�ƨ@�S�@��@őh@�z�@��@��@�7L@��@�1'@��@���@�O�@�`B@��y@�n�@���@�ff@��\@��!@�ȴ@�~�@���@�X@�{@���@���@���@��H@�Z@�33@�/@���@�I�@�l�@�\)@�(�@�%@�=q@�5?@��;@��@���@�A�@�(�@���@��u@�1'@�t�@�5?@���@���@�|�@�
=@��R@�5?@��@��@��u@��@�t�@��!@�ff@��@�hs@�?}@���@�Ĝ@�r�@��@�ƨ@�S�@�+@�"�@�o@��y@���@�v�@�=q@�{@���@��@���@��7@�hs@�X@�&�@�z�@��@�  @�  @���@�~�@��@���@���@�1'@��+@�=q@��T@��@�^5@�-@���@��@��@��@�^5@�-@��@���@��@��7@�?}@�hs@�X@�O�@�%@��/@��9@���@�j@�A�@�9X@�  @��@�l�@�;d@��@���@��\@���@�X@�hs@�7L@�j@�I�@�r�@�dZ@��H@��@���@��F@�|�@�S�@�o@��H@���@���@�$�@���@�O�@��/@�A�@�r�@�ƨ@�|�@���@���@�v�@�5?@���@�x�@��7@�=q@�^5@�M�@��!@�n�@���@��h@��@���@�$�@�$�@�J@���@�7L@��9@�z�@�9X@��@���@�C�@���@�ff@�5?@���@���@�O�@��@�Q�@�t�@�\)@��P@�C�@�@���@�
=@���@�\)@�C�@��y@�$�@��@���@�@���@��@���@�z�@�  @��w@�|�@�33@�
=@�
=@���@���@���@���@���@�M�@�-@��@���@��@���@���@��h@��7@�`B@���@��u@�bN@�(�@�1@��
@���@�t�@�C�@��y@��R@���@�~�@�ff@�E�@��@��@��T@��#@���@���@�`B@�?}@��@���@�Ĝ@��@��@�j@�I�@�1'@�b@�P@;d@~��@~@}��@}��@}V@|�@|I�@{�m@{"�@zM�@zJ@zJ@y�^@x��@x�@xbN@xQ�@xr�@wl�@v�R@v@u@t��@tI�@s�m@s�@sC�@r�H@r�H@r�\@q��@q�@q&�@p�u@p�@p1'@o�@n�@n@n@m�T@l�@l�@l��@k�F@i��@hr�@hĜ@h��@hQ�@h1'@h  @f�y@f5?@f$�@e�@f{@e@e/@eO�@e?}@d�@dj@c��@cC�@b�!@b�\@b^5@b=q@b�@a��@a�7@aG�@`�9@`�u@`�@`�u@`��@`�9@a�@a%@`bN@_��@_;d@^�y@_
=@_;d@^�y@^��@^E�@^E�@]��@]�@]�@]`B@]�@\z�@[��@Z�H@Z�@Y�#@Y�^@Y��@Y7L@X��@X�u@XQ�@W��@W;d@V�@Vff@U@UO�@UV@TI�@S�m@S��@S�F@SdZ@S"�@S33@R�@Qx�@O�;@Ol�@O+@N��@N��@N�R@N�+@Nv�@Nff@NE�@M��@Mp�@M�@MV@L�@L��@L�@K��@KdZ@KS�@K"�@J��@J~�@JM�@I��@I��@IG�@H�`@HbN@H �@Hb@G�;@Gl�@G+@G
=@F��@FE�@E�-@E��@EO�@D��@D�/@D��@D�D@DI�@D9X@D�@C�F@C��@C�@Ct�@CC�@C"�@C@B��@BM�@A��@A�^@A&�@@bN@@  @?�@?K�@?;d@>�y@>��@>�+@>5?@=�@=��@=O�@=/@<��@<��@<��@<z�@<Z@<(�@;ƨ@;�@;t�@;C�@;33@;"�@;@:�H@:�!@:~�@:^5@:^5@:=q@9��@9��@9x�@9x�@9hs@97L@9%@8�@81'@8  @7�;@7�@7K�@6��@6��@6V@6E�@65?@6@5�T@5�-@5�@5O�@5?}@5V@4�j@4j@49X@41@3��@3t�@3dZ@3C�@3o@2��@2��@2~�@2^5@1��@1�^@1x�@1�@0��@0�`@0��@0�9@0�u@0r�@0 �@/�@/\)@/;d@.�@.�+@.{@-@-p�@-p�@-`B@,��@,�D@,j@,Z@,I�@,�@+�m@+�m@+ƨ@+��@+t�@+@*�!@*�\@*^5@*�@)��@)��@)x�@)�@(��@(�u@(Q�@(  @'�w@'�P@'K�@&��@&�@&ȴ@&��@&V@%�-@%�h@%�@%�@%?}@%V@$�@$��@$�@$��@$z�@$j@#��@#�F@#�@#dZ@#"�@"��@"�\@"^5@"=q@"J@!��@!G�@!�@ ��@ �9@ �@ r�@ r�@ Q�@ 1'@ 1'@ 1'@   @�;@��@\)@�y@�R@ff@�@��@@�-@p�@O�@?}@��@�D@Z@I�@�@ƨ@��@�@dZ@C�@o@@�!@~�@^5@J@��@��@��@x�@�@�`@�@A�@b@�;@�@�P@;d@��@ȴ@�+@v�@v�@5?@�@�@��@��@?}@��@�@Z@(�@�m@t�@C�@"�@@�H@~�@^5@=q@J@��@x�@G�@�@��@�9@r�@1'@b@��@�@�P@l�@K�@+@�@�@ȴ@�+@V@E�@$�@{@�T@�-@�@O�@/@��@��@��@�D@j@Z@1@�
@��@dZ@S�@"�@o@
�H@
��@
�!@
��@
~�@
M�@
-@
�@	��@	��@	��@	��@	x�@	hs@	7L@�9@bNA��A�!-A�$tA�)_A�,qA�+6A��Aǳ3AÙ1A��A�]/A�[�A���A�=A��A�m�A��JA�h>A��A��?A�C�A��-A�-A��_Au�Aj�`A]��AS��AI�[A<P�A4�8A-�A'�9A$2aAw�AA�A�zA<�A	��@��@�xl@��U@��F@�Ĝ@�M@��K@��@�a|@���@�1@���@�B�@y&�@na|@c�r@^�c@U7L@L�U@E@>�6@:Ov@5@@/�@*_@$Ɇ@ 7�@�@��@��@��@�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�bA�bA�{A�
=A�
=A�JA�VA�bA�VA�bA�bA�bA�oA�{A�oA�{A�{A�oA�oA�{A�bA�bA�VA�VA�VA�bA�bA�bA�VA�VA�bA�oA�{A�{A�oA�oA�bA�{A��A��A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A� �A�"�A� �A� �A� �A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�$�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�+A�+A�-A�+A�-A�-A�-A�-A�+A�-A�-A�+A�+A�-A�+A�-A�-A�-A�-A�-A�-A�+A�-A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�/A�/A�-A�-A�/A�/A�/A�/A�/A�1'A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�-A�/A�/A�/A�-A�-A�-A�-A�-A�-A�-A�+A�+A�-A�-A�-A�+A�(�A�(�A�+A�+A�+A�+A�(�A�(�A�(�A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�+A�+A�+A�(�A�(�A�+A�+A�(�A�+A�+A�(�A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�&�A�$�A�$�A�$�A�$�A�&�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�oA�VA�JA�
=A�1A�1A�1A�1A�%A�A�A���A���A���A���A��A��A��A��A��`A��;A��A���A�ƨA�AھwAں^AڶFAڴ9Aڴ9Aڲ-Aڰ!AڮAڬAڧ�Aڥ�Aڡ�Aڟ�Aڟ�Aڟ�Aڟ�Aڝ�Aڝ�Aڙ�Aڙ�Aڛ�Aڛ�Aڙ�Aڙ�Aڙ�Aڗ�Aڕ�AړuAڑhAڏ\Aڏ\AڍPAڍPAڍPAڋDAڋDAډ7AڅAځA�|�A�~�AڃAڃAڃAځA�|�A�x�A�v�A�t�A�r�A�t�A�v�A�x�A�z�A�z�A�|�A�z�A�z�A�v�A�t�A�t�A�z�A�|�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�~�AڃAڅAڅAڇ+Aڇ+Aڇ+Aڇ+Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7Aډ7AڋDAډ7Aډ7Aډ7Aڇ+AڃA�~�A�x�A�x�A�z�A�z�AڃAڏ\AڑhAڑhAڏ\AڋDAڇ+AځA�|�A�|�AڅAڗ�Aڝ�Aڝ�Aڝ�Aڙ�AړuAڇ+AڃAڇ+AڍPAڙ�Aڣ�Aڣ�Aڟ�Aڙ�Aډ7AڅAړuAڛ�Aڧ�AڬAڬAڬAڬAڬAڮAڲ-Aڴ9AڶFAڶFAڸRAڸRAں^AڼjAڼjAڼjAڸRAڴ9Aڰ!Aک�Aڡ�Aڝ�Aڝ�Aڡ�Aڧ�Aڰ!AڶFAڸRAں^Aں^Aں^Aں^AڼjAڼjAں^AڶFAڰ!AڬAک�Aک�Aڧ�Aڧ�Aڧ�Aڥ�Aڥ�Aڧ�Aڥ�Aڣ�Aڣ�Aڟ�Aڙ�AڑhAڇ+A�|�A�t�A�n�A�hsA�hsA�jA�hsA�bNA�`BA�S�A�A�A� �A���A�A�hsA���A�33A׺^A�n�A�`BA�v�A�r�A�ffA�ffA�Q�A�/A�A��/Aְ!A�x�A�5?A�AծAՃA�z�A�l�A�ffA�ffA�dZA�r�AՉ7A�~�A�x�A�hsA�E�A��A��mA���Aԛ�Aԇ+A�z�A�dZA�Q�A�5?A�1A��#Aӣ�A�5?A��A��`A���A�1A��A�"�A�+A�$�A���Aҧ�AҍPA���A�A�JA�bA�%A���A��A��mA��
A�AҴ9Aҩ�Aҟ�Aҏ\A�~�A�dZA�G�A�&�A�A��yA���Aѩ�Aћ�AэPA�x�A�\)A�I�A�;dA�5?A�+A�"�A��A�%A���A��A��A��yA��;A��
A���A�AжFAв-Aа!AЮAЮAХ�AЧ�AУ�AЩ�AЙ�AБhAЋDAЃA�z�A�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�uB
�9B
��Bq�B�DB��B�B{�Br�BO�BN�BA�BE�BgmB}�BǮB�B�)B��B�}B�!B�wB�NB�ZB�;B�)B�B�B��B%B�B�B#�B�B  BƨB�9B��B��B�PB�B�B�Bx�Bu�B}�B�%B�1B�1B~�B�+B��B��B��B��B�uB�VB�Bp�B^5BR�BF�B(�B@�B{B\B+B
��B
�B
��B
�RB
�dB
��B
�B
u�B
cTB
T�B
A�B
5?B
'�B
�B
PB
1B
B	��B	��B	�B	�`B	�B	ȴB	�LB	�B	��B	��B	�bB	�B	r�B	e`B	R�B	>wB	-B	�B	hB		7B��B��B�B�NB�BǮB�dB�?B�B��B�JB� Bt�Bq�Bl�BcTB\)BR�BM�BH�BB�B>wB>wB=qB<jB=qB;dB8RB6FB49B2-B0!B.B)�B,B)�B'�B%�B$�B#�B!�B�B�B�B�B�B�B�B�B�BuBuBhB\BPBDB
=BDB
=B
=B
=B
=B	7BB+B	7B
=B	7B	7BVBDBDB
=B
=B
=B	7B	7B1B+BJB
=BBB��B��B��B��B��B��B  BB+B1B+B1B1B+BBBB+BDBhBuBbBuB�B�B�B�B�B�B�B!�B%�B%�B%�B%�B&�B(�B'�B+B.B.B-B,B)�B+B0!B49B5?B6FB>wB?}B@�B?}B>wB@�BC�BH�BL�BM�BM�BQ�BT�BVBT�BVB_;BaHBaHBbNBffBk�B~�B�B�=B��B��B��B��B��B��B��B�oB�=B�+B�DB��B��B��B��B��B��B��B��B��B��B�-B�?BÖBŢB��B�B�B�)B�5B�;B�/B�/B�/B�B�B�#B�)B�;B�NB�ZB�mB�B�B�B�B��B��B��B��B��B	B	B	%B		7B	
=B	
=B	
=B	JB	VB	VB	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	!�B	"�B	�B	�B	�B	�B	 �B	#�B	#�B	!�B	#�B	$�B	%�B	'�B	,B	-B	/B	1'B	2-B	6FB	8RB	;dB	;dB	>wB	@�B	A�B	B�B	D�B	G�B	G�B	J�B	M�B	P�B	Q�B	R�B	R�B	Q�B	R�B	S�B	VB	T�B	T�B	XB	YB	XB	XB	ZB	^5B	`BB	aHB	bNB	dZB	e`B	e`B	e`B	ffB	e`B	bNB	aHB	aHB	bNB	bNB	aHB	_;B	^5B	]/B	\)B	ZB	ZB	]/B	bNB	dZB	e`B	hsB	iyB	jB	l�B	l�B	m�B	p�B	q�B	s�B	t�B	u�B	v�B	u�B	v�B	w�B	x�B	w�B	w�B	w�B	w�B	v�B	v�B	u�B	s�B	t�B	s�B	t�B	u�B	u�B	u�B	v�B	x�B	z�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�%B	�%B	�%B	�+B	�+B	�+B	�1B	�7B	�7B	�=B	�DB	�DB	�JB	�VB	�\B	�bB	�bB	�hB	�oB	�oB	�oB	�uB	�uB	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�dB	�jB	�jB	�dB	�jB	�wB	�qB	�qB	�wB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	�}B	�}B	�wB	�wB	�wB	�}B	�wB	�}B	�wB	�}B	�}B	�}B	�}B	�}B	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ƨB	ƨB	ƨB	ĜB	B	B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
%B
%B
+B
�IB
�=B
��B
�)B
��B
�kBmBLdBk6BƨB��B�}B�B�HBu�B�B��B_�BB
�CB
}<B
8RB
{B	�IB	��B	YKB	JB�#B�FBoiBG�B<B2-B'�B!HBBB	�B1B	lB)BTaB�eB��B	B	#B	R�B	a�B	vzB	�-B	�^B	�B	�!B	�B	��B	��B	�BB	�?B	�~B	�4B	�SB	��B	�B	��B	�B	�B	�B	��B	�HB
B
?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BF�>�ȴ?|�B��B
��B�?&$�>�o?�A�`BBoB
5?B
�B
��B
��B��A�V>���>ؓu?]p�B
��B
}�B
�B
�B
�3B
��B��>��mB7L>��?��B
�DB
�!BB�B�T>�~�>�+A�b>�~�@���>�I�>�l�B+?��?<jB
�T@�"�Bb>�hB
B�B�#?F$�B
�B
�!BDB�`?��B
�NA�`B?v�+A�\)>�=q?��>���B
�uB
�'B
�B
�!B
ÖBY>�B
v�B
hsB
�FB
��BÖ@�V>�FB��>�$�>��mB
�B	�B
�B\?$�/B
��B
�?8bBbN>�C�BZB
�B
�B
�B
�-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
�B
�B
�B
�B
�B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
�B
�B
�B
�B
�B
�B
�B
��B
��B
�B
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
�B
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
�uB
�oB
�bB
�VB
�JB
�=B
�7B
�B
�B
|�B
x�B
t�B
p�B
jB
dZB
]/B
YB
W
B
W
B
W
B
XB
XB
ZB
[#B
\)B
^5B
_;B
`BB
aHB
bNB
dZB
e`B
ffB
iyB
jB
k�B
jB
k�B
m�B
m�B
o�B
r�B
t�B
u�B
w�B
x�B
w�B
w�B
w�B
w�B
x�B
z�B
|�B
~�B
� B
� B
{�B
{�B
|�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�%B
�1B
�7B
�7B
�1B
�+B
�+B
�%B
�%B
�+B
�7B
�DB
�PB
�bB
�\B
�PB
�PB
�1B
�B
� B
�B
�B
�B
�B
�%B
�%B
�B
~�B
u�B
p�B
o�B
o�B
q�B
s�B
v�B
u�B
r�B
o�B
jB
ffB
e`B
e`B
cTB
cTB
`BB
VB
P�B
M�B
K�B
L�B
M�B
N�B
M�B
H�B
@�B
9XB
33B
/B
/B
0!B
2-B
49B
6FB
49B
5?B
49B
6FB
:^B
?}B
D�B
H�B
J�B
L�B
N�B
P�B
S�B
W
B
ZB
\)B
^5B
_;B
bNB
ffB
jB
o�B
u�B
x�B
y�B
|�B
~�B
�B
�B
�B
�B
�%B
�1B
�=B
�JB
�\B
�uB
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
�B
�jB
�#BhBG�Bn�B�B~�BhsBK�B<jB.B�B�B&�B0!B<jBL�B`BBk�By�Bw�BdZBR�BI�BD�B=qB/B{BPBPBbB�B$�B.B8RBA�BE�BG�BM�BQ�BZBe`Bq�B�B��BɺB�}B�B��B�Bt�Bk�Bk�Bu�B�VB�VBp�BXBP�BJ�BI�BI�BJ�BJ�BN�BP�BQ�BQ�BR�BR�BS�BW
BZB]/B_;B_;BcTBdZBbNBdZBhsBjBiyBiyBgmBiyBiyBjBl�Bn�Bn�Bo�Bp�Bq�Bq�Br�Bq�Bu�Bv�Bu�Bv�Bt�Bv�Bt�Bt�Bp�Bt�Bt�Bt�Bs�Bs�Bq�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A�S�A���A���A��AڑhA���AڑhA�{A���A�A��A��A���A���A���Aک�Aڇ+Aڇ+A�M�A�9XA���A���A�x�A�1A��/A�x�A�{A���A��A�A��/A���A��yA��`A��A¶FA��A���A��A�
=A�v�A��jA��uA�%A�  A��!A�l�A�33A��HA���A�t�A�p�A��-A��A��A��A�JA�"�A� �A���A�?}A�v�A��A�l�A��;A�O�A���A�=qA���A�dZA�JA�bNA�x�A� �A���A��A��PA��A�&�A��!A�A�ƨA�(�A�oA���A��;A��uA���A��RA���A�v�A��A�z�A��A� �A�ȴA���A��RA�=qA��9A�1'A�`BA���A�A��yA�  A~��A|�`A{Ay|�Aw&�Au/As"�AqoAn��Al�9AjJAhr�Afn�Ab��A`A]?}AZ��AXE�AVE�AS��AQx�AO|�AM�AK��AI|�AG�AE��AC&�AAp�A@�A=�#A<r�A;�7A8��A7��A6�!A5�A4bA2ĜA1oA0-A.  A,r�A+%A*z�A)A(r�A'S�A%�FA$�!A#ƨA#�#A"I�A!�mA �uA`BA��A�Ar�AG�A=qA�-A%Az�A�!A��A��AoA��A�jA(�A&�AM�A;dA�uA�PAO�A�#A�`A�HAZAJA�A
(�A	�A	&�A�TA�uAZAC�A��A�PA/An�A  A&�A�A $�@�"�@��@��@�~�@�G�@�hs@�I�@��^@��@��j@�1@�n�@�&�@�  @�J@��H@��@�@���@�G�@��@�u@�ȴ@�w@��@⟾@�+@��@�A�@�l�@�+@���@ܬ@��;@��@��@�dZ@��@ج@���@��@���@�dZ@��@��/@�hs@��@�v�@ҸR@��#@щ7@�;d@�"�@�M�@̛�@��
@��@�@� �@�/@ǅ@�;d@Ɨ�@�A�@�C�@��@�M�@���@�Ĝ@�@���@�-@���@���@��9@�Q�@�O�@��@���@�|�@��!@� �@��!@��7@�O�@�ƨ@���@���@�-@��@�O�@���@��@��u@��@�t�@��
@��F@�9X@�K�@��F@���@�x�@��@�hs@���@�7L@��m@�S�@�@���@�v�@���@�J@�Ĝ@�+@�%@���@��D@�t�@�?}@��^@���@�/@�V@�X@�\)@��@�r�@�(�@���@�|�@�Q�@�o@��-@��#@���@�"�@�ff@���@��@��#@�5?@�Ĝ@��@�A�@��@��@�33@�9X@�+@��^@���@�x�@�(�@�Z@�V@��@�ff@��@��D@�Q�@�A�@�1@�1'@�~�@�o@�7L@���@���@���@�/@��@�n�@���@��@�  @�C�@��
@�%@�E�@���@�Q�@�b@�;d@���@�$�@��@�%@�(�@��@�r�@��;@��@���@��+@���@�33@���@�S�@�=q@���@��^@�~�@�9X@�p�@��@� �@���@�z�@���@���@���@�33@�J@�{@�E�@��-@��9@�-@���@���@�I�@��D@�j@�o@�j@�b@�K�@�1'@��@�Z@�l�@�\)@��@��@�  @���@���@�K�@���@�M�@��@�M�@�p�@��h@���@��/@�
=@���@��@�C�@�K�@��w@���@�@�x�@���@�/@���@��/@�33@�
=@�X@�hs@�r�@��
@��#@��@�r�@�hs@��@�dZ@��@��@�=q@�Z@��@�33@�p�@�t�@�=q@�t�@���@�x�@�5?@��^@��@���@��@�V@���@��@�7L@�9X@��@�r�@��+@�v�@�dZ@���@��#@���@��T@�5?@��H@�%@��@���@��^@�`B@�Ĝ@|�@~��@�r�@��@�1'@K�@}��@~ȴ@�b@��@~{@~�y@~�@~5?@z��@{C�@x�u@z��@z=q@y�#@x��@yX@y�@wK�@w�@w��@t�D@uV@w+@t�@r�H@uV@qx�@t��@s��@s��@t�@tz�@q��@pb@p  @pr�@q�7@mp�@p��@o�@o�@nv�@l�@j�H@i&�@mp�@k"�@hbN@j�H@iX@jJ@i%@g�w@dj@g+@d��@gK�@e�@e�@d�/@b�H@dI�@a�7@c@c�m@a��@`�9@d�D@`r�@a��@b��@e�-@c��@g|�@ahs@_;d@`b@]�T@]`B@^ȴ@]�h@_;d@^{@[�
@]�@\�j@]O�@\�/@\I�@\�@]`B@]@[�F@\Z@\z�@^��@]p�@Y��@[S�@Z�\@]V@\j@W�w@Z^5@Y�^@V$�@U�T@Yhs@Y�7@U��@U@V��@Y�7@T��@X�@U�@V@Vff@T��@R�\@Vȴ@Qhs@R�!@St�@Qhs@Qhs@O��@T(�@O|�@O��@Q��@Nȴ@O��@Nȴ@M�h@MO�@O|�@O;d@Q��@O|�@Nȴ@O�@L�@M��@Ko@LZ@P  @MV@H�@J��@I�7@HĜ@K�F@G�P@J�\@H��@IG�@G�@HA�@G��@G|�@H�9@F�@F�@G�@Fff@@Ĝ@E�@E��@C�
@E�@E�@Fv�@C�@D�@C@Ct�@DZ@AX@C�F@@��@B�!@B~�@A��@A��@>�+@B=q@A&�@D��@?\)@<��@?�@=@?�@=��@=/@;@<9X@<1@>�+@:~�@<��@<1@=�@:�!@:J@:�@;"�@:=q@8Q�@9&�@;"�@9��@9x�@=�h@7\)@81'@:�!@8��@:^5@7��@97L@8�9@6��@8��@97L@9��@5�@:M�@7�@5�T@6@7;d@7�P@4�D@7�@65?@7�@4�@3��@5p�@5O�@2��@5p�@3�
@4Z@5V@4I�@4z�@3�m@1��@0�`@3�F@1��@1%@2�!@2�@2��@/\)@17L@1%@1��@1&�@/;d@1&�@0�@1&�@/��@-V@2�@-�@,1@.�+@+�
@+��@)��@.v�@,z�@,�/@+o@*�!@(�9@,Z@+��@+t�@)�7@-�@'K�@'��@*n�@+o@&E�@&�@&E�@(�u@*M�@'\)@%�@&��@$I�@(��@%O�@&�+@$j@( �@#ƨ@)G�@&�@$��@!��@&ff@#��@!�^@%@"n�@"�!@!%@ Q�@#�
@ 1'@ ��@5?@"=q@"�\@"=q@ ��@   @�P@ff@�P@�w@   @K�@ 1'@{@�@"�@ 1'@K�@?}@ �@��@�-@`B@�@v�@��@$�@�h@�;@��@V@j@$�@��@ Ĝ@  �@��@C�@��@n�@ƨ@z�@~�@��@�@Z@o@X@t�@�H@��@��@��@�H@t�@\)@�@hs@��@1'@��@Q�@��@M�@|�@ �@�;@�@��@�;@G�@��@p�@��@O�@��@�R@  @�H@��@V@�@�9@ff@o@�H@=q@��@hs@�@�@9X@Z@K�@�\@A�@��@z�@��@�;@/@�@E�@  @V@$�@Ĝ@7L@�w@1@�-@l�@?}@@��@
M�@@
�@��@��@�+@�F@V@	��@
��@�
@
n�@dZ@	X@Z@?}@	X@	X@	7L@S�@�@	�#@1'@Q�@�@�@�@�;@��@Ĝ@�@��@@�+@
=@
M�@��@V@��@S�@`B@?}A��A��A���A��A�A��+AǐbA��]A�5?A�F?A�JXA��A�\]A�2�A���A��A��fA���A�<�A���A��hA��|A{�9Ar�Afa|AYϫAN��AE�?A8�RA2��A-2�A%��A#FA T�AiDA�A�A
͟AS&@��z@�"�@��@���@���@�B�@���@���@�8�@��@�ߤ@�/�@�ߤ@x,=@g�;@g�;@e&�@Lz�@Q��@Ap�@;�m@3�:@6Z�@3�:@%�'@%�'@kQ@�)@)_@	��@�D@U2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���AڑhA�VAڑhAڑhA�|�A�|�A�|�A���A���A�%A�|�AڑhAڑhA���A�%AڑhA��A�A�AڑhA�|�AڑhAڑhA�|�AڑhA�A�A�%A�%A�%A۶FAڑhA���A�A�A�|�A۶FA���A���A�%AڑhA���A�|�A�%AڑhAڑhA���A�%A���A���A���A�%A���A���A���A���A�|�A۶FA�%A�|�A���A�|�A�+A�%A۶FA���A���A���A�|�AڑhA�|�A���A�%A�%A���A���A���A�|�A�%AڑhA�|�A۶FA�%A�%A���A�A�A���A�%A��A�%A�%A���A���A�%A��AڑhA���A�%A��A�|�A���A��A�%A���A�%A۶FA�%A���A��A���A��A�+A�A�A�A�A���A�|�A���A�VA�A�AڑhAڑhA���A�%A۶FA���A�|�A��AڑhAڑhA�A�A�%AڑhA�A�A�A�A�VA�|�A�%A�VA���A��A�VA�|�A���A�%A�A�A�A�A�A�A�VA���A�A�A�%A�|�A�VAڑhA�VA�%AڑhA���A���AڑhA���A�%A�l�A�%AڑhA���AڑhA�VA�%A�VA��A�A�A��A�VA�|�A�VA���AڑhA�%AڑhA�A�AڑhA��A���A�l�AڑhA���A�|�A���A���A��A�|�A�|�AڑhA���A�%A�%A�|�A��A�|�A۶FA�%AڑhA���A���A۶FA���A���A���A���A�VAڑhA���A���AڑhA�VA�VA�%A��AڑhAڑhA���AڑhA�%A���A�|�A��HA�%A���A�%A��A���A�%A���A���AڑhA�|�AڑhAڑhA۶FAڑhA�|�A�|�A�|�AڑhA�|�A���A�|�A���A�|�A�%A��A�|�A�|�AڑhA�%AڑhA���A�VA�VAڑhA�|�A�|�AڑhA���AڑhAڑhAڑhAڑhAڑhAڑhA�|�AڑhAڑhA�%A�VA۶FAڑhA���A�|�A�A�AڑhA�%A�A�A�%A۶FAڑhAڑhAڑhA۶FAڑhA�VA�%A���A�VAڑhAڑhAڑhA�VA���A�%A�VAڑhA�A�A�VA�VAڑhA�VA�A�A�A�A�%A���A�A�A�VA�VAڑhA�%A٥�AڑhA���A���A�%A��A٥�A�VA�VA���A��A�VA�%A�%AڑhA��A��HA�VAڑhA�VA�A�A�VAڑhAڑhAڑhA��AڑhAڑhA�VA�%A�VA�%A�VA��HA�VA��AڑhA��HA�l�A���A��A���AڑhAڑhA��AڑhA��A��HA�VA���AڑhA��HA��HA��A�VAڑhA���A��A٥�A���A��HA��A��HA�l�A���A��A���A��HA٥�A���A�VA��HA��HA�l�A��HA��A٥�A��HA�VA�VA�l�A�l�A��A��HAڑhA�1'A٥�A��A�1'A��HA��A��HA��A��HA�l�AڑhA�l�A��A�1'A٥�A�l�A��A�VA�l�A��HA٥�A��HA�l�A�1'A�1'A�l�A���A�l�A�l�A�1'A�l�A�l�A��HA�1'A��HA���A�1'A٥�A�l�A�l�A�l�A�1'AؼjA؁A٥�A���A���AؼjA���A���A�1'AؼjA��HA٥�AؼjAؼjA٥�AؼjA�l�A���A���A�l�A���A؁AؼjA؁AؼjA��A�l�AؼjA���A���A؁AؼjA���A٥�A���A؁AؼjAؼjA�l�A�E�A؁A�E�A؁A؁AؼjA�1'A�1'AؼjA�1'A�1'A؁AؼjA�l�A�ZAؼjA٥�A�1'A�l�AؼjAؼjAؼjAؼjA�E�A�E�A؁A�E�A�1'A���A�E�A�E�A���A�E�A�E�A���A�
=AؼjA�1'A�E�A�
=AؼjA�E�A�E�A�
=A���A�1'AؼjAו�AؼjA�
=A؁AؼjA���A��A���Aו�A�E�A؁A�E�A���A�ZA؁Aו�A؁Aו�A���A�E�A�ZA�
=A�E�A�E�A�ZA�E�A�ZA�
=A�ZA؁A�E�A�ZA�
=A��A�ZA��`A���A��`A�ZA���A��`A��A��`A�ZA��`A��`A֩�A���A֩�A��A�
=A֩�A�33A��`A�ZA��`A��`A֩�A֩�A��`A֩�A��A�n�A�n�A�n�A�ZA�33A�ZA�33A�33A�33A�33A֩�A�n�A֩�A�33AվwA�n�A���A���A�n�A��`A֩�AվwA���A�33AվwA�33A���A�33A���AվwA���A�ZAվwAՃA�G�A�G�A�n�A�n�A�G�A���A���AվwA�33AվwA�JA�n�A���A���A�G�AՃA���A���A���A�JA���A���A���A���AՃA���AՃA�\)A�JAվwA���AվwAՃA���Aԕ�A�JA���A�JA� �A��`A� �A��`A� �A�\)A���A�JA�\)A�\)A��`A��`A�JA� �A�\)A���A��`Aԕ�A��`Aԕ�A��`Aө�A� �A���Aԕ�A� �A�33A�\)A���A��`A��`A�\)A���Aө�Aө�A�\)A�n�Aө�A�n�A���A�n�A���A� �A�G�A��`A��`A�33A�G�A҃AҼjAҼjA҃A�JA�33A�33AҼjA�G�A�G�A҃A���A��AҼjAҼjAѕ�A���A�G�A�G�A҃A�JA�JA�G�AҼjA҃A���AҼjAѕ�A�JA���A�ZA�JA�JA�ZA�ZA�JA��A��AЧ�AЧ�A���A�1'AЧ�AЧ�A�ZAЧ�A�1'A��TA�l�A�l�A�1'A���A���AЧ�A�l�A��TAϺ^A��TA���A�1'A�~�A���A�~�A�~�A�1'AϺ^A�C�A���A�C�A���AΑhA�1A�~�A�VA�C�A�VA��A��;A��A��A��A��;AΑhA��;A�VAΑhA�ffA�VA�ffAͣ�A�ffA��Aͣ�A�ffA�ffA�ffA�ffA�ffA��A�x�A�ffA�ffA�+A̴9A�x�A�x�A�x�A̴9A�=qA�x�A̴9A�x�A̴9A�ƨA�A�ƨA̴9A�=qA�ƨA�oAˉ7A̴9A�M�A�M�Aˉ7A��
A�oAˉ7A�A�=qA�M�A�M�A�M�Aˉ7A��
A��
A�`BA�`BA�`BA�M�A�oA��
A�oA�oA�"�A�"�A�oAʛ�A�M�A�`BAɬA�M�A�"�A�`BAɬA�`BAɬAʛ�AɬA�p�A�p�A�p�AɬAɬA�p�A�p�AɬA���A�p�A�33A���A���A�33A�33AɬA���A��mA�33A�33A�p�AȼjA���AɬAȁAȁAȁAȼjA���A�C�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                      PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607202210041156072022100411560720221004115607AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            202208232244542022082322445420220823224454    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232244542022082322445420220823224454  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B83E            383E            383E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232244542022082322445420220823224454  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�8800            800             800             UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202210041156072022100411560720221004115607  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                