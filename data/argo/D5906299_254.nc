CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-08-23T22:47:43Z creation      
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
_FillValue                  0 �0Argo profile    3.1 1.2 19500101000000  20220823224743  20221004115622  5906299 5906299 5906299 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER,                                                  STEPHEN RISER,                                                  STEPHEN RISER,                                                  PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC          �   �   �AAA AOAOAO  8443                            8443                            8443                            2C  2C  2C  DDD APEX                            APEX                            APEX                            8815                            8815                            8815                            052520                          052520                          052520                          877 877 877 @��0�\�@@��0�\�@@��0�\�@111 @��1[��@��1[��@��1[��@5U�$�/@5U�$�/@5U�$�/�dFz�G��dFz�G��dFz�G�111 GPS     GPS     GPS     Primary sampling: averaged []                                                                                                                                                                                                                                   Secondary sampling: discrete []                                                                                                                                                                                                                                 Secondary sampling: discrete [1Hz data]                                                                                                                                                                                                                            �   �   �AAA AAB FFF     >���@@  @�  @�  A  A0  AP  Ap  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B$  B,  B4  B<  BD  BLffBT  B\  Bd  Bl  Bt  B|  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C	  C  C  C  C  C  C  C  C  C  C  C  C!  C#  C%  C'  C)  C+  C-  C/  C1  C3  C5  C7  C9  C;  C=  C?  CA  CB�fCE  CH  CK  CM  CO  CQ  CS  CU  CW  CY  C[  C]  C_  Ca  Cc  Ce  Cg  Ci  Ck  Cm  Co  Cq  Cs  Cu  Cw  Cy  C{  C}  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C CÀ CĀ Cŀ Cƀ Cǀ CȀ Cɀ Cʀ Cˀ C̀ C̀ C΀ Cπ CЀ Cр CҀ CӀ CԀ CՀ Cր C׀ C؀ Cـ Cڀ Cۀ C܀ C݀ Cހ C߀ C�� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C�� C� C� C� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D @ D � D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D	@ D	� D
@ D
� DFfD� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D @ D � D!@ D!� D"@ D"� D#@ D#� D$@ D$� D%@ D%� D&@ D&� D'@ D'� D(@ D(� D)@ D)� D*@ D*� D+@ D+� D,@ D,� D-@ D-� D.@ D.� D/@ D/� D0@ D0� D1@ D1� D2@ D2� D3@ D3� D4@ D4� D5@ D5� D6@ D6� D7@ D7� D8@ D8� D9@ D9� D:@ D:� D;@ D;� D<@ D<� D=@ D=� D>@ D>� D?@ D?� D@@ D@� DA@ DA� DB@ DB� DC@ DC� DD@ DD� DE@ DE� DF@ DF� DG@ DG� DH@ DH� DI@ DI� DJ@ DJ� DK@ DK� DL@ DL� DM@ DM� DN@ DN� DO@ DO� DP@ DP� DQ@ DQ� DR@ DR� DS@ DS� DT@ DT� DU@ DU� DV@ DV� DW@ DW� DX@ DX� DY@ DY� DZ@ DZ� D[@ D[� D\@ D\� D]@ D]� D^@ D^� D_@ D_� D`@ D`� Da@ Da� Db@ Db� Dc@ Dc� Dd@ Dd� De@ De� Df@ Df� Dg@ Dg� Dh@ Dh� Di@ Di� Dj@ Dj� Dk@ Dk� Dl@ Dl� Dm@ Dm� Dn@ Dn� Do@ Do� Dp@ Dp� Dq@ Dq� Dr@ Dr� Ds@ Ds��Dt@ Dt� Du@ Du� Dv@ Dv� Dw@ Dw� Dx@ Dx� Dy@ Dy� Dz@ Dz� D{@ D{� D|@ D|� D}@ D}� D~@ D~� D@ D� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D���D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D  D�� D�  D�` Dà D�� D�  D�` DĠ D�� D�  D�` DŠ D�� D�  D�` DƠ D�� D�  D�` DǠ D�� D�  D�` DȠ D�� D�  D�` Dɠ D�� D�  D�` Dʠ D�� D�  D�` Dˠ D�� D�  D�` D̠ D�� D�  D�` D͠ D�� D�  D�` DΜ�D�� D�  D�` DϠ D�� D�  D�` DР D�� D�  D�` DѠ D�� D�  D�` DҠ D�� D�  D�` DӠ D�� D�  D�` DԠ D�� D�  D�` Dՠ D�� D�  D�` D֠ D�� D�  D�` Dנ D�� D�  D�` Dؠ D�� D�  D�` D٠ D�� D�  D�` Dڠ D�� D�  D�` D۠ D�� D�  D�` Dܠ D�� D�  D�` Dݠ D�� D�  D�` Dޠ D�� D�  D�c3Dߠ D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�c3D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�#3D�` D� D�� D�  D�` D�� D�� D�  D�` D� D�� D�  D�\�D��D�� D�  D�` D� D���D�  D�` D� D�� D�  D�` D� D�� D�  D�` D� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�` D�� D�� D�  D�c3D�� D��3D�S3@�z�A(Q�A��A�
=B"ffBD�Bq�
B�u�B��HB�B��fB�B�3CECǮC�fC33C*�C4��C>h�CHٚCP�C\��Cf}qCpCz��C�T{C��C�+�C�s3C�J=C��C�nC�)C�` C��C�Q�C�C�C�=qC�C�b�D	�fD�D"��D/\D;��DH%�DT��Da(RDm� Dz33D�K�D��
D���D�D�I�D��D��=D�HD�S�D���D��HD���D�D)D�w
D���D��{D�?\D풏D�ҏD��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>L��>L��=���>L��>���>L��>���>L��>L��=���>���>���>���=���=���>���>L��>L��>L��>L��=���=���=���=���>L��>L��>L��>L��=���=���>L��=���>���>L��>���>L��>���=���=���>L��>L��=���>L��=���=���>L��>���>L��>L��>L��=���>���=���=���>���>L��=���>L��>L��>L��>L��>���>L��=���>L��>L��=���>L��=���=���>L��=���>���>L��=���>L��>���>L��>���>���>���>L��=���>L��=���>L��>L��=���>���=���>L��>���>���>���>���>���>���>���>���>���>L��>���>L��=���>L��=���>L��>L��=���=���=���>L��>L��=���>L��>L��>L��=���>L��>���=���>L��=���>L��>L��>L��>L��>���>L��>L��=���>���=���=���>L��>L��>���>L��>L��>L��>L��>���>���>���>L��>L��>L��>L��>���>L��>L��>L��=���>L��>L��>L��=���>L��>���>���>L��>L��>���>L��=���>L��>L��>���>L��>L��>L��>L��>���>���>���>L��>���=���>L��>���>L��>L��=���=���>L��>L��=���>���>���>L��=���>L��>L��=���>L��=���>���=���=���=���>L��=���>���>L��=���=���>L��=���>���>L��=���>L��=���=���>L��>���>���?   ?��?��?333?L��?L��?L��?fff?fff?�  ?���?���?�ff?�ff?�33?�  ?���?���?ٙ�?ٙ�?�33?�33?�33@ff@��@��@��@   @   @&ff@&ff@,��@333@333@@  @Fff@L��@L��@S33@Y��@Y��@`  @`  @l��@s33@y��@�  @�  @�33@�33@���@���@���@�  @�  @�33@�ff@���@���@���@�  @�33@�33@�ff@���@���@�  @�33@�33@�ff@���@���@���@�  @�33@�ff@�ff@ə�@���@�  @�33@�ff@�ff@ٙ�@���@�  @�  @�33@�ff@陚@陚@���@�  @�33@�ff@�ff@���@���A   A��A��A33A��AffA  A  A	��A33A��AffAffA  A��A��A33A��A  A  A��A33A33A��A��AffA   A!��A#33A#33A$��A&ffA(  A(  A)��A+33A,��A,��A.ffA0  A1��A333A333A4��A4��A6ffA6ffA8  A9��A;33A<��A<��A<��A>ffA@  A@  AA��AC33AD��AD��AFffAFffAH  AI��AI��AK33AL��ANffANffAP  AQ��AQ��AS33AT��AVffAVffAX  AX  AY��A[33A\��A^ffA^ffA`  Aa��Aa��Ac33Ad��AfffAh  Ah  Ai��Ai��Ak33Al��AnffAnffAp  Aq��As33At��At��AvffAx  Ay��A{33A{33A|��A~ffA�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�ffA�33A�  A���A�ffA�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���A�ffA�33A�  A���A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�33A�33A�  A���A�ffA�33A�  A���Aݙ�A�ffA�33A�  A���AᙚA�ffA�33A�  A噚A�ffA�33A�  A���A陚A�ffA�33A�  A���A홚A�ffA�  A���A�A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffB   B   B ��B33B��B  BffB��B33B��BffB��B33B��B  BffB��B33B��B  BffB��B	33B	��B
  B
��B33B��B  BffB��B33B��B  BffB��B��B  BffB��B33B��B  BffB��B33B��B  B��B33B��B  BffB��B33B  BffB��B33B��B  BffB��B��B  BffB��B33B��B  BffB��B33B   B ffB ��B!33B!��B"  B"ffB"��B#33B#��B$  B$ffB$��B%33B%��B&  B&ffB&��B'��B(  B(ffB(��B)33B)��B*  B*ffB*��B+33B+��B,  B,ffB,��B-33B-��B.  B.��B/33B/��B0  B0ffB0��B133B1��B2  B2ffB333B3��B4  B4ffB4��B533B5��B6  B6ffB6��B733B8  B8ffB8��B933B9��B:  B:ffB:��B;33B;��B<  B<ffB<��B=33B=��B>  B>ffB>��B?33B?��B?��B@  B@ffB@��BA33BA��BB  BBffBB��BB��BC33BC��BD  BDffBD��BE33BE33BE��BF  BFffBF��BG33BG33BG��BH  BHffBH��BH��BI33BI��BJ  BJ  BJffBJ��BJ��BK33BK��BK��BL  BLffBLffBL��BM33BM33BM��BN  BN  BNffBN��BN��BO33BO33BO��BP  BP  BPffBPffBP��BP��BQ33BQ��BQ��BR  BR  BRffBR��BR��BS33BS��BS��BT  BT  BTffBTffBT��BU33BU33BU��BU��BV  BV  BVffBV��BV��BW33BW��BW��BX  BX  BXffBX��BX��BY33BY33BY��BZ  BZ  BZffBZffBZ��B[33B[33B[��B\  B\  B\ffB\ffB\��B]33B]33B]��B^  B^  B^ffB^��B^��B_33B_��B_��B`  B`ffB`ffB`��Ba33Ba33Ba��Bb  Bb  BbffBb��Bb��Bc33Bc��Bd  Bd  BdffBd��Bd��Be33Be��Be��Bf  BfffBf��Bf��Bg33Bg��Bg��Bh  BhffBhffBh��Bi33Bi��Bi��Bj  BjffBj��Bj��Bk33Bk��Bl  Bl  BlffBl��Bm33Bm��Bn  Bn  BnffBn��Bn��Bo33Bo��Bp  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    >��@=p�@��R@޸RA\)A/\)AO\)Ao\)A��A��A��A��AǮA׮A�A��B�
B�
B�
B�
B#�
B+�
B3�
B;�
BC�
BL=pBS�
B[�
Bc�
Bk�
Bs�
B{�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB�)CD��CG��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�C�z�D =qD �qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD	=qD	�qD
=qD
�qDC�D�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD=qD�qD =qD �qD!=qD!�qD"=qD"�qD#=qD#�qD$=qD$�qD%=qD%�qD&=qD&�qD'=qD'�qD(=qD(�qD)=qD)�qD*=qD*�qD+=qD+�qD,=qD,�qD-=qD-�qD.=qD.�qD/=qD/�qD0=qD0�qD1=qD1�qD2=qD2�qD3=qD3�qD4=qD4�qD5=qD5�qD6=qD6�qD7=qD7�qD8=qD8�qD9=qD9�qD:=qD:�qD;=qD;�qD<=qD<�qD==qD=�qD>=qD>�qD?=qD?�qD@=qD@�qDA=qDA�qDB=qDB�qDC=qDC�qDD=qDD�qDE=qDE�qDF=qDF�qDG=qDG�qDH=qDH�qDI=qDI�qDJ=qDJ�qDK=qDK�qDL=qDL�qDM=qDM�qDN=qDN�qDO=qDO�qDP=qDP�qDQ=qDQ�qDR=qDR�qDS=qDS�qDT=qDT�qDU=qDU�qDV=qDV�qDW=qDW�qDX=qDX�qDY=qDY�qDZ=qDZ�qD[=qD[�qD\=qD\�qD]=qD]�qD^=qD^�qD_=qD_�qD`=qD`�qDa=qDa�qDb=qDb�qDc=qDc�qDd=qDd�qDe=qDe�qDf=qDf�qDg=qDg�qDh=qDh�qDi=qDi�qDj=qDj�qDk=qDk�qDl=qDl�qDm=qDm�qDn=qDn�qDo=qDo�qDp=qDp�qDq=qDq�qDr=qDr�qDs=qDs�Dt=qDt�qDu=qDu�qDv=qDv�qDw=qDw�qDx=qDx�qDy=qDy�qDz=qDz�qD{=qD{�qD|=qD|�qD}=qD}�qD~=qD~�qD=qD�qD��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D�D�޸D��D�^�DÞ�D�޸D��D�^�DĞ�D�޸D��D�^�DŞ�D�޸D��D�^�Dƞ�D�޸D��D�^�DǞ�D�޸D��D�^�DȞ�D�޸D��D�^�Dɞ�D�޸D��D�^�Dʞ�D�޸D��D�^�D˞�D�޸D��D�^�D̞�D�޸D��D�^�D͞�D�޸D��D�^�DΛ�D�޸D��D�^�DϞ�D�޸D��D�^�DО�D�޸D��D�^�Dў�D�޸D��D�^�DҞ�D�޸D��D�^�DӞ�D�޸D��D�^�DԞ�D�޸D��D�^�D՞�D�޸D��D�^�D֞�D�޸D��D�^�Dמ�D�޸D��D�^�D؞�D�޸D��D�^�Dٞ�D�޸D��D�^�Dڞ�D�޸D��D�^�D۞�D�޸D��D�^�Dܞ�D�޸D��D�^�Dݞ�D�޸D��D�^�Dޞ�D�޸D��D�a�Dߞ�D�޸D��D�^�D���D�޸D��D�^�DីD�޸D��D�^�D➸D�޸D��D�^�D㞸D�޸D��D�a�D䞸D�޸D��D�^�D垸D�޸D��D�^�D枸D�޸D��D�^�D瞸D�޸D��D�^�D螸D�޸D��D�^�D鞸D�޸D��D�^�DꞸD�޸D��D�^�D랸D�޸D�!�D�^�D잸D�޸D��D�^�DힸD�޸D��D�^�DD�޸D��D�[�DD�޸D��D�^�D�D�ۅD��D�^�D�D�޸D��D�^�D�D�޸D��D�^�D�D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�^�D���D�޸D��D�a�D���D���D�Q�@�33A'�A���A��QB"=pBD�Bq�B�aHB���B��B���BڮB�C:�C�qC�)C(�C*z�C4�{C>^�CH�]CP��C\��Cfs4Cp�RCz�{C�O\C��C�&fC�nC�EC�fC�h�C�
C�Z�C��C�L�C�>�C�8RC�  C�]pD	��D]D"� D/�D;�gDH#4DT�)Da%�Dm�qDz0�D�J�D���D��\D��D�HRD���D���D�  D�R�D���D�� D��pD�B�D�u�D�ϮD��3D�>D�GD��GD��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>#�
>#�
=u>#�
>��>#�
>�Q�>#�
>#�
=u>�Q�>��>��=u=u>��>#�
>#�
>#�
>#�
=u=u=u=u>#�
>#�
>#�
>#�
=u=u>#�
=u>��>#�
>��>#�
>�Q�=u=u>#�
>#�
=u>#�
=u=u>#�
>�Q�>#�
>#�
>#�
=u>��=u=u>�Q�>#�
=u>#�
>#�
>#�
>#�
>�Q�>#�
=u>#�
>#�
=u>#�
=u=u>#�
=u>�Q�>#�
=u>#�
>��>#�
>�Q�>��>��>#�
=u>#�
=u>#�
>#�
=u>��=u>#�
>��>��>��>��>�Q�>��>��>��>��>#�
>��>#�
=u>#�
=u>#�
>#�
=u=u=u>#�
>#�
=u>#�
>#�
>#�
=u>#�
>��=u>#�
=u>#�
>#�
>#�
>#�
>��>#�
>#�
=u>��=u=u>#�
>#�
>��>#�
>#�
>#�
>#�
>��>�Q�>��>#�
>#�
>#�
>#�
>��>#�
>#�
>#�
=u>#�
>#�
>#�
=u>#�
>��>�Q�>#�
>#�
>��>#�
=u>#�
>#�
>��>#�
>#�
>#�
>#�
>��>�Q�>��>#�
>��=u>#�
>��>#�
>#�
=u=u>#�
>#�
=u>��>��>#�
=u>#�
>#�
=u>#�
=u>��=u=u=u>#�
=u>�Q�>#�
=u=u>#�
=u>��>#�
=u>#�
=u=u>#�
>�Q�>�Q�>�?\)?\)?(��?B�\?B�\?B�\?\(�?\(�?u?��?��?�G�?�G�?�{?��H?Ǯ?Ǯ?�z�?�z�?�{?�{?�{@�
@
=q@
=q@
>@p�@p�@#�
@#�
@*=q@0��@0��@=p�@C�
@J=q@J=q@P��@W
>@W
>@]p�@]p�@j=q@p��@w
>@}p�@}p�@��@��@�Q�@��@��@��R@��R@��@��@�Q�@��@��@��R@��@��@��@�Q�@��@��R@��@��@��@�Q�@��@��@��R@��@��@��@�Q�@˅@θR@��@��@��@�Q�@ۅ@޸R@޸R@��@��@�Q�@�Q�@�@�R@��@��@��@�Q�@��@��RA ��A ��A�\A(�AA\)A\)A��A
�\A(�AAA\)A��A��A�\A(�A\)A\)A��A�\A�\A(�A(�AA\)A ��A"�\A"�\A$(�A%A'\)A'\)A(��A*�\A,(�A,(�A-A/\)A0��A2�\A2�\A4(�A4(�A5A5A7\)A8��A:�\A<(�A<(�A<(�A=A?\)A?\)A@��AB�\AD(�AD(�AEAEAG\)AH��AH��AJ�\AL(�AMAMAO\)AP��AP��AR�\AT(�AUAUAW\)AW\)AX��AZ�\A\(�A]A]A_\)A`��A`��Ab�\Ad(�AeAg\)Ag\)Ah��Ah��Aj�\Al(�AmAmAo\)Ap��Ar�\At(�At(�AuAw\)Ax��Az�\Az�\A|(�A}A\)A�z�A�G�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�G�A�zA��GA��A�z�A�G�A�zA��GA�z�A�G�A�zA�zA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�zA��GA��GA�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�zA�zA��GA��A�G�A�zA�zA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�G�A��GA��A�z�A�G�A�zA��GAîA�z�A�G�A�zA��GAǮA�z�A�zA��GAˮA�z�A�G�A�zA��GAϮA�z�A�G�A�zA��GAӮA�z�A�G�A��GA��GA׮A�z�A�zA��GAۮA�z�A�G�A�zA��GA߮A�z�A�G�A�zA��GA�A�G�A�zA��GA�A�z�A�G�A�zA��GA�A�z�A�G�A�zA�A�z�A�G�A�zA��GA�A�z�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��A��B ��B
=Bp�B�
B=pB��B
=Bp�B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��B	
=B	p�B	�
B
��B
=Bp�B�
B=pB��B
=Bp�B�
B=pB��Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=Bp�B�
B��B
=Bp�B�
B=pB��B
=B�
B=pB��B
=Bp�B�
B=pB��Bp�B�
B=pB��B
=Bp�B�
B=pB��B
=B�
B =pB ��B!
=B!p�B!�
B"=pB"��B#
=B#p�B#�
B$=pB$��B%
=B%p�B%�
B&=pB&��B'p�B'�
B(=pB(��B)
=B)p�B)�
B*=pB*��B+
=B+p�B+�
B,=pB,��B-
=B-p�B-�
B.��B/
=B/p�B/�
B0=pB0��B1
=B1p�B1�
B2=pB3
=B3p�B3�
B4=pB4��B5
=B5p�B5�
B6=pB6��B7
=B7�
B8=pB8��B9
=B9p�B9�
B:=pB:��B;
=B;p�B;�
B<=pB<��B=
=B=p�B=�
B>=pB>��B?
=B?p�B?p�B?�
B@=pB@��BA
=BAp�BA�
BB=pBB��BB��BC
=BCp�BC�
BD=pBD��BE
=BE
=BEp�BE�
BF=pBF��BG
=BG
=BGp�BG�
BH=pBH��BH��BI
=BIp�BI�
BI�
BJ=pBJ��BJ��BK
=BKp�BKp�BK�
BL=pBL=pBL��BM
=BM
=BMp�BM�
BM�
BN=pBN��BN��BO
=BO
=BOp�BO�
BO�
BP=pBP=pBP��BP��BQ
=BQp�BQp�BQ�
BQ�
BR=pBR��BR��BS
=BSp�BSp�BS�
BS�
BT=pBT=pBT��BU
=BU
=BUp�BUp�BU�
BU�
BV=pBV��BV��BW
=BWp�BWp�BW�
BW�
BX=pBX��BX��BY
=BY
=BYp�BY�
BY�
BZ=pBZ=pBZ��B[
=B[
=B[p�B[�
B[�
B\=pB\=pB\��B]
=B]
=B]p�B]�
B]�
B^=pB^��B^��B_
=B_p�B_p�B_�
B`=pB`=pB`��Ba
=Ba
=Bap�Ba�
Ba�
Bb=pBb��Bb��Bc
=Bcp�Bc�
Bc�
Bd=pBd��Bd��Be
=Bep�Bep�Be�
Bf=pBf��Bf��Bg
=Bgp�Bgp�Bg�
Bh=pBh=pBh��Bi
=Bip�Bip�Bi�
Bj=pBj��Bj��Bk
=Bkp�Bk�
Bk�
Bl=pBl��Bm
=Bmp�Bm�
Bm�
Bn=pBn��Bn��Bo
=Bop�Bo�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�9XA�?}A�C�A�C�A�C�A�?}A�=qA�=qA�?}A�?}A�A�A�A�A�=qA�=qA�?}A�?}A�;dA�;dA�9XA�$�A�A�A�  A��yA˰!Aˉ7A�p�A�S�A�O�A�hsA�G�A�z�A�O�A��
A���AǶFA�bNA�S�A���Aŕ�AŁA��Aď\A�  A���A�ffA�Q�A��A�M�A�l�A�r�A��A�%A���A��A���A�x�A�33A��A��+A�G�A��A�^5A��mA�VA�=qA�ƨA��DA�^5A��+A���A�ĜA��-A�VA�{A�A�jA�VA���A��/A��A��HA���A��hA�dZA�~�A�v�A�S�A��;A��A�1A�^5A��/A��A��TA��
A�^5A��A��;A�VA�=qA��7A�p�A��A���A�S�A��A���A�ZA��/A���A��Az�yAwdZAtA�Aq�AlI�Aj��Ah�jAgVAe�Ab�HA`�RA_A\�+AY%AW��AW;dAV��AVȴAV^5AU�hAT1'AS`BARbNAPȴAL�`AK7LAJ�AI�AI`BAH�/AG�FAF�AE"�AAA?\)A<�!A:  A8�DA8(�A8bA8  A7�mA7A5G�A3XA2��A1�A0�`A0ȴA05?A/l�A.ĜA.�+A-&�A+�^A*��A)�A(M�A&ffA%�#A%��A$n�A#�A"ffA!�A �A33A��A��A�AbNA�HA�mA��A �A��AĜA��AAE�A��A�-A��A%A-A&�A��A5?A`BAA�9A�A�PA
��A
��A	�;A	��A	�hA	�A�#A�jA9XA��A�RA�AO�A�!A��A �HA $�@��@�E�@��@�C�@�Q�@���@�33@�=q@��@�%@�z�@� �@��@��;@�7@��@�@�(�@�ff@���@���@�(�@ާ�@�J@�&�@܃@�
=@�x�@��@�t�@�C�@��H@պ^@Ӿw@�|�@�dZ@�dZ@ҧ�@�~�@��@Ѻ^@ѡ�@�?}@�bN@ϕ�@�-@�r�@� �@�b@�1@ˮ@ʇ+@ɩ�@���@��/@Ȭ@�A�@�dZ@�ff@���@�7L@��/@��@öF@�K�@��@§�@�5?@��@�hs@�hs@�Ĝ@�ƨ@�t�@�o@�V@���@�7L@�V@���@���@��!@�?}@���@��u@�9X@��
@���@���@�^5@��T@�p�@��/@�dZ@�{@�=q@�5?@��^@�x�@�7L@�\)@���@�5?@��#@���@��^@��7@��@�&�@���@�9X@�ƨ@��@�+@��@�;d@��P@�;d@��@�&�@�7L@���@��`@��h@��T@��@��u@��@���@�t�@�ff@�J@��T@�7L@�%@��`@��`@�z�@�(�@�ƨ@�S�@���@�n�@���@���@�@��@���@��R@��@�@�;d@�33@��@�v�@�^5@�$�@�@���@���@��@��@���@���@���@��j@�z�@�j@�Z@���@�ȴ@�^5@�@�r�@��P@�|�@�|�@�\)@�ȴ@�p�@�G�@��@�Ĝ@�r�@�Z@�9X@��@�l�@�C�@��@��y@�ȴ@���@�5?@�J@��-@�7L@���@��@��/@���@��@�Z@��
@�|�@�C�@�@���@�{@��-@�X@��`@���@�O�@�n�@�-@��@���@�Q�@��
@�ƨ@�1'@�z�@��@���@�-@�{@�@���@��@�9X@� �@��@���@���@��@�t�@���@��!@�5?@�{@��-@���@���@��h@��@�O�@�j@��@�C�@�@�^5@���@�  @�l�@��@�33@���@���@�33@�ȴ@���@�ff@�-@��@��@�@�@���@���@���@��7@�p�@�x�@�x�@�`B@���@���@���@��@�j@� �@���@���@�|�@�dZ@�+@�
=@��H@�v�@�$�@���@���@���@���@���@���@���@��u@�z�@�bN@�Q�@�Q�@�Q�@� �@�  @|�@+@~�y@~v�@}�T@}�h@}�@}�@|z�@{dZ@z~�@z�@yG�@x�9@x�u@xr�@xbN@xQ�@x �@w�@v��@vV@vV@vv�@vV@u�h@uV@t��@tI�@t9X@s��@s�
@s�F@s��@sS�@s@r��@rM�@q��@qX@p��@pĜ@p�@pA�@p  @o��@n��@n��@nv�@n5?@m�T@m��@m�-@mV@l�@kƨ@kt�@kS�@k@j�\@i��@ix�@h�`@hbN@h  @g��@g
=@f��@f�y@f�@f��@fE�@f{@f@e�@e��@ep�@d��@c��@cƨ@ct�@c@bn�@a�^@aX@`�`@`�u@_�@_
=@^5?@^@]�@]V@\�@[�m@[ƨ@[�F@[�F@[��@[o@Z��@Z�@Y&�@X��@X��@Xr�@X1'@W�;@W��@Wl�@W;d@Vȴ@V�+@V5?@V@U�@T��@T(�@S��@S��@R��@R�\@Q��@Q�@PĜ@O�@Ol�@O
=@N�y@N�R@N��@Nff@N{@M�T@M�-@L��@LZ@L1@Kƨ@KdZ@J��@J-@J�@JJ@I��@H�`@H�u@HbN@HQ�@HA�@G�@GK�@G�@G�@F��@F��@FE�@F$�@F@E�T@E@E`B@D��@D�j@D�D@DZ@DZ@DZ@D�@CS�@C33@B��@B^5@B=q@A�@A��@A��@Ahs@Ahs@AX@A%@@�@@A�@@b@?�@?�;@?�;@?�;@?�;@?��@?+@>�+@=�T@=�@=`B@=V@<��@<�@<��@<�@;�@;33@;o@:�@:�H@:~�@9��@9��@9��@9X@8�`@8Ĝ@8�9@8A�@7�w@7l�@7K�@7+@7
=@7
=@6�y@6ȴ@6ȴ@6��@65?@5@5�@5V@4��@4�/@4�j@4z�@4(�@3��@3��@3"�@2�H@2��@2��@2�!@2�\@2^5@2�@1��@1�@1��@1�^@1��@1x�@1X@1�@0��@0��@0�`@0Ĝ@0��@0Q�@01'@/�P@/�@.�y@.��@.�+@.v�@.V@.@-@-@-�-@-��@,��@,�@,��@,j@+�m@+��@+t�@+dZ@+dZ@+"�@*�!@*�!@*��@*�\@*�\@*�\@*�\@*�\@*~�@*~�@)��@)�@(�9@(��@(�@(r�@(bN@(1'@'�w@'l�@'+@'
=@&��@&��@&��@&E�@%@%��@%��@%��@%?}@$�/@$�D@$9X@$(�@$1@#�m@#��@#C�@#o@"��@"�\@"M�@"J@!�#@!G�@ ��@ ��@ �u@ �u@ �u@ �@ �@ �@ r�@ Q�@ b@�@��@|�@K�@+@ȴ@��@�+@v�@ff@E�@$�@@�h@O�@�@��@z�@(�@1@�m@�m@ƨ@��@�@33@�!@n�@=q@�#@�^@��@�7@x�@7L@&�@&�@�@�9@r�@r�@Q�@1'@b@  @�@
=@ȴ@��@��@�+@E�@$�@�@�-@O�@/@V@�@��@�j@�@��@��@�D@�D@�D@z�@z�@z�@z�@Z@I�@(�@��@�
@ƨ@�F@��@@�@x�@X@&�@&�@Ĝ@�@b@�@�@�P@|�@\)@K�@K�@K�@�@�y@�y@�@��@��@��@�+@ff@E�@5?@$�@�@@��@@�-@�h@�h@�h@�h@�@p�@`B@`B@O�@/@�j@j@I�@(�@(�@(�@(�@�
@t�@S�@33A�B[A�G�A�@OA�>�A�5�A���A�l�A��AŨ�A��ZA�@OA��UA��?A���A���A�-A�=qA���A��RA���A��FA���A�u%Aw�EAh��A_b�AV�rAS�eAK_AF}�A:OA7��A1�A.�
A**0A%jATaA�Aԕ@�/�@�$t@�2a@�˒@���@���@�G�@�c@���@�@�l�@���@�R�@wZ�@oخ@f��@[�w@R�y@H��@CE9@=�@6��@1��@-\�@(|�@#�@��@�~@�f@:�@��@\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Aɲ-A�"�A�"�A��A�1A˙�A���A�ƨA�oA�  A�z�Aǉ7A�n�A�VA��A��yAǛ�A��A�~�A�+A�  A�l�A�A���A���A�{A˗�A�bA�XA��
A�JA�VA��TAˡ�A���A�Aũ�A�Q�A�
=A�~�A��A˸RA��#AʑhA��A��TA�%A�~�A�1A�ĜA�`BAƏ\A���A�^5A�?}A��A�bA��A�$�A�A��A�ĜA��`A���A�bA�JA�ĜA�7LAʲ-A˕�A���A��A��#Aˡ�A�(�A�$�A�"�A��A�bA˴9A��/A�M�A�l�Aʝ�A��A���A���A���AˁA��A��A�{A��A��A��A�{A�{A��A�VA�AˋDAȧ�A���A�dZA�
=A��A�+A���A�
=A�jA�
=A��Aˏ\AˁA��
A��A˩�A˲-A�VAɍPA�dZA�A��mA��;A���A� �A��A��A�%A��HA�p�A��;A� �A��A���A�dZA��;A� �A��A�(�A�1'A�+A� �A�&�A��A��A�+A�+A�bA�O�A��A��
A� �A˶FAȴ9A�E�A�-A�1'A�$�A�"�A��A�Aɇ+A�1'A�+A�-A�{A�?}AɓuA�"�A�"�A�1'A�$�A�
=A���A�XA�+A�/A��A�`BA�%A�G�A�$�A��Aɲ-A���A���Aʗ�A�dZA�A�A��Aʉ7A�ƨA�ƨA�jA���A�1'A�VA˟�A��A�I�A�"�A�{A�O�A�ffA�{A�A��`A�A�A�Aƺ^A��HA�5?A�;dA�9XA�;dA�9XA�A�A�A�A�=qA�?}A�?}A�A�A�?}A�=qA�;dA�9XA�7LA�7LA�;dA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�7LA�9XA�5?A�5?A�33A�7LA�7LA�;dA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�;dA�?}A�A�A�?}A�?}A�?}A�?}A�?}A�=qA�=qA�?}A�?}A�A�A�A�A�?}A�A�A�A�A�?}A�A�A�?}A�A�A�E�A�C�A�A�A�A�A�C�A�A�A�A�A�?}A�A�A�A�A�E�A�C�A�A�A�C�A�E�A�A�A�A�A�E�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�A�A�C�A�C�A�A�A�C�A�E�A�C�A�A�A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�A�A�A�A�C�A�C�A�E�A�I�A�G�A�C�A�C�A�E�A�E�A�C�A�C�A�C�A�C�A�E�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�G�A�C�A�C�A�C�A�A�A�A�A�?}A�A�A�?}A�?}A�=qA�=qA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�C�A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�=qA�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�?}A�=qA�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�?}A�?}A�A�A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�A�A�A�A�?}A�?}A�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�=qA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�=qA�=qA�=qA�;dA�9XA�9XA�9XA�7LA�7LA�9XA�?}A�C�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�;dA�;dA�;dA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�;dA�;dA�;dA�;dA�9XA�;dA�;dA�7LA�7LA�9XA�9XA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�;dA�;dA�;dA�;dA�9XA�7LA�7LA�5?A�33A�7LA�9XA�;dA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�7LA�7LA�7LA�9XA�9XA�7LA�5?A�5?A�5?A�33A�-A��A��A��A�bA�
=A�JA�JA�VA�bA�oA�1A�A�A�A�%A�A�A�  A���A�  A�A�%A�A���A���A�  A���A���A���A���A���A���A�  A�  A�A�  A�  A�A�  A�  A�  A�A�
=A�1A�%A�A�A�A�  A�  A�A�A�  A�  A�  A�A�A�A���A���A���A���A���A���A���A���A���A�A�A�A���A���A���A���A���A���A��A��mA��;A��/A��/A��#A��A��
A��
A���A���A���A���A��
A��#A��#A��
A���A���A���A�ƨA�A˺^A˸RAˮA˴9AˮA˥�A˥�A˥�Aˣ�Aˣ�Aˣ�Aˣ�A˛�A˙�A˙�A˙�A˙�A˙�A˙�A˕�A˓uA˓uA˓uA˓uA˕�A˕�A˕�A˕�AˑhAˑhAˑhAˏ\A˅AˁAˁAˁAˁAˁAˁAˁAˁAˁAˁAˁAˁAˁA˅A˅Aˇ+A˅Aˇ+Aˇ+Aˉ7Aˇ+A˅AˁAˁAˁAˁA�~�A�~�A�~�A�~�A�~�A�~�A�|�A�v�A�p�A�jA�hsA�hsA�ffA�ffA�ffA�dZA�ffA�hsA�jA�jA�jA�jA�hsA�hsA�ffA�bNA�^5A�\)A�\)A�\)A�\)A�\)A�\)A�ZA�ZA�XA�VA�VA�S�A�Q�A�O�A�O�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    A�$�A�9XA�?}A�C�A�C�A�C�A�?}A�=qA�=qA�?}A�?}A�A�A�A�A�=qA�=qA�?}A�?}A�;dA�;dA�9XA�$�A�A�A�  A��yA˰!Aˉ7A�p�A�S�A�O�A�hsA�G�A�z�A�O�A��
A���AǶFA�bNA�S�A���Aŕ�AŁA��Aď\A�  A���A�ffA�Q�A��A�M�A�l�A�r�A��A�%A���A��A���A�x�A�33A��A��+A�G�A��A�^5A��mA�VA�=qA�ƨA��DA�^5A��+A���A�ĜA��-A�VA�{A�A�jA�VA���A��/A��A��HA���A��hA�dZA�~�A�v�A�S�A��;A��A�1A�^5A��/A��A��TA��
A�^5A��A��;A�VA�=qA��7A�p�A��A���A�S�A��A���A�ZA��/A���A��Az�yAwdZAtA�Aq�AlI�Aj��Ah�jAgVAe�Ab�HA`�RA_A\�+AY%AW��AW;dAV��AVȴAV^5AU�hAT1'AS`BARbNAPȴAL�`AK7LAJ�AI�AI`BAH�/AG�FAF�AE"�AAA?\)A<�!A:  A8�DA8(�A8bA8  A7�mA7A5G�A3XA2��A1�A0�`A0ȴA05?A/l�A.ĜA.�+A-&�A+�^A*��A)�A(M�A&ffA%�#A%��A$n�A#�A"ffA!�A �A33A��A��A�AbNA�HA�mA��A �A��AĜA��AAE�A��A�-A��A%A-A&�A��A5?A`BAA�9A�A�PA
��A
��A	�;A	��A	�hA	�A�#A�jA9XA��A�RA�AO�A�!A��A �HA $�@��@�E�@��@�C�@�Q�@���@�33@�=q@��@�%@�z�@� �@��@��;@�7@��@�@�(�@�ff@���@���@�(�@ާ�@�J@�&�@܃@�
=@�x�@��@�t�@�C�@��H@պ^@Ӿw@�|�@�dZ@�dZ@ҧ�@�~�@��@Ѻ^@ѡ�@�?}@�bN@ϕ�@�-@�r�@� �@�b@�1@ˮ@ʇ+@ɩ�@���@��/@Ȭ@�A�@�dZ@�ff@���@�7L@��/@��@öF@�K�@��@§�@�5?@��@�hs@�hs@�Ĝ@�ƨ@�t�@�o@�V@���@�7L@�V@���@���@��!@�?}@���@��u@�9X@��
@���@���@�^5@��T@�p�@��/@�dZ@�{@�=q@�5?@��^@�x�@�7L@�\)@���@�5?@��#@���@��^@��7@��@�&�@���@�9X@�ƨ@��@�+@��@�;d@��P@�;d@��@�&�@�7L@���@��`@��h@��T@��@��u@��@���@�t�@�ff@�J@��T@�7L@�%@��`@��`@�z�@�(�@�ƨ@�S�@���@�n�@���@���@�@��@���@��R@��@�@�;d@�33@��@�v�@�^5@�$�@�@���@���@��@��@���@���@���@��j@�z�@�j@�Z@���@�ȴ@�^5@�@�r�@��P@�|�@�|�@�\)@�ȴ@�p�@�G�@��@�Ĝ@�r�@�Z@�9X@��@�l�@�C�@��@��y@�ȴ@���@�5?@�J@��-@�7L@���@��@��/@���@��@�Z@��
@�|�@�C�@�@���@�{@��-@�X@��`@���@�O�@�n�@�-@��@���@�Q�@��
@�ƨ@�1'@�z�@��@���@�-@�{@�@���@��@�9X@� �@��@���@���@��@�t�@���@��!@�5?@�{@��-@���@���@��h@��@�O�@�j@��@�C�@�@�^5@���@�  @�l�@��@�33@���@���@�33@�ȴ@���@�ff@�-@��@��@�@�@���@���@���@��7@�p�@�x�@�x�@�`B@���@���@���@��@�j@� �@���@���@�|�@�dZ@�+@�
=@��H@�v�@�$�@���@���@���@���@���@���@���@��u@�z�@�bN@�Q�@�Q�@�Q�@� �@�  @|�@+@~�y@~v�@}�T@}�h@}�@}�@|z�@{dZ@z~�@z�@yG�@x�9@x�u@xr�@xbN@xQ�@x �@w�@v��@vV@vV@vv�@vV@u�h@uV@t��@tI�@t9X@s��@s�
@s�F@s��@sS�@s@r��@rM�@q��@qX@p��@pĜ@p�@pA�@p  @o��@n��@n��@nv�@n5?@m�T@m��@m�-@mV@l�@kƨ@kt�@kS�@k@j�\@i��@ix�@h�`@hbN@h  @g��@g
=@f��@f�y@f�@f��@fE�@f{@f@e�@e��@ep�@d��@c��@cƨ@ct�@c@bn�@a�^@aX@`�`@`�u@_�@_
=@^5?@^@]�@]V@\�@[�m@[ƨ@[�F@[�F@[��@[o@Z��@Z�@Y&�@X��@X��@Xr�@X1'@W�;@W��@Wl�@W;d@Vȴ@V�+@V5?@V@U�@T��@T(�@S��@S��@R��@R�\@Q��@Q�@PĜ@O�@Ol�@O
=@N�y@N�R@N��@Nff@N{@M�T@M�-@L��@LZ@L1@Kƨ@KdZ@J��@J-@J�@JJ@I��@H�`@H�u@HbN@HQ�@HA�@G�@GK�@G�@G�@F��@F��@FE�@F$�@F@E�T@E@E`B@D��@D�j@D�D@DZ@DZ@DZ@D�@CS�@C33@B��@B^5@B=q@A�@A��@A��@Ahs@Ahs@AX@A%@@�@@A�@@b@?�@?�;@?�;@?�;@?�;@?��@?+@>�+@=�T@=�@=`B@=V@<��@<�@<��@<�@;�@;33@;o@:�@:�H@:~�@9��@9��@9��@9X@8�`@8Ĝ@8�9@8A�@7�w@7l�@7K�@7+@7
=@7
=@6�y@6ȴ@6ȴ@6��@65?@5@5�@5V@4��@4�/@4�j@4z�@4(�@3��@3��@3"�@2�H@2��@2��@2�!@2�\@2^5@2�@1��@1�@1��@1�^@1��@1x�@1X@1�@0��@0��@0�`@0Ĝ@0��@0Q�@01'@/�P@/�@.�y@.��@.�+@.v�@.V@.@-@-@-�-@-��@,��@,�@,��@,j@+�m@+��@+t�@+dZ@+dZ@+"�@*�!@*�!@*��@*�\@*�\@*�\@*�\@*�\@*~�@*~�@)��@)�@(�9@(��@(�@(r�@(bN@(1'@'�w@'l�@'+@'
=@&��@&��@&��@&E�@%@%��@%��@%��@%?}@$�/@$�D@$9X@$(�@$1@#�m@#��@#C�@#o@"��@"�\@"M�@"J@!�#@!G�@ ��@ ��@ �u@ �u@ �u@ �@ �@ �@ r�@ Q�@ b@�@��@|�@K�@+@ȴ@��@�+@v�@ff@E�@$�@@�h@O�@�@��@z�@(�@1@�m@�m@ƨ@��@�@33@�!@n�@=q@�#@�^@��@�7@x�@7L@&�@&�@�@�9@r�@r�@Q�@1'@b@  @�@
=@ȴ@��@��@�+@E�@$�@�@�-@O�@/@V@�@��@�j@�@��@��@�D@�D@�D@z�@z�@z�@z�@Z@I�@(�@��@�
@ƨ@�F@��@@�@x�@X@&�@&�@Ĝ@�@b@�@�@�P@|�@\)@K�@K�@K�@�@�y@�y@�@��@��@��@�+@ff@E�@5?@$�@�@@��@@�-@�h@�h@�h@�h@�@p�@`B@`B@O�@/@�j@j@I�@(�@(�@(�@(�@�
@t�@S�@33A�B[A�G�A�@OA�>�A�5�A���A�l�A��AŨ�A��ZA�@OA��UA��?A���A���A�-A�=qA���A��RA���A��FA���A�u%Aw�EAh��A_b�AV�rAS�eAK_AF}�A:OA7��A1�A.�
A**0A%jATaA�Aԕ@�/�@�$t@�2a@�˒@���@���@�G�@�c@���@�@�l�@���@�R�@wZ�@oخ@f��@[�w@R�y@H��@CE9@=�@6��@1��@-\�@(|�@#�@��@�~@�f@:�@��@\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�A�A�A�A�=qA�?}A�?}A�A�A�?}A�=qA�;dA�9XA�7LA�7LA�;dA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�7LA�9XA�5?A�5?A�33A�7LA�7LA�;dA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�;dA�?}A�A�A�?}A�?}A�?}A�?}A�?}A�=qA�=qA�?}A�?}A�A�A�A�A�?}A�A�A�A�A�?}A�A�A�?}A�A�A�E�A�C�A�A�A�A�A�C�A�A�A�A�A�?}A�A�A�A�A�E�A�C�A�A�A�C�A�E�A�A�A�A�A�E�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�A�A�C�A�C�A�A�A�C�A�E�A�C�A�A�A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�A�A�A�A�C�A�C�A�E�A�I�A�G�A�C�A�C�A�E�A�E�A�C�A�C�A�C�A�C�A�E�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�G�A�C�A�C�A�C�A�A�A�A�A�?}A�A�A�?}A�?}A�=qA�=qA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�C�A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�=qA�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�?}A�=qA�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�?}A�?}A�A�A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�A�A�A�A�?}A�?}A�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�=qA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�=qA�;dA�;dA�=qA�=qA�=qA�;dA�9XA�9XA�9XA�7LA�7LA�9XA�?}A�C�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�;dA�;dA�;dA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�;dA�;dA�;dA�;dA�9XA�;dA�;dA�7LA�7LA�9XA�9XA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�;dA�;dA�;dA�;dA�9XA�7LA�7LA�5?A�33A�7LA�9XA�;dA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�7LA�7LA�7LA�9XA�9XA�7LA�5?A�5?A�5?A�33A�-A��A��A��A�bA�
=A�JA�JA�VA�bA�oA�1A�A�A�A�%A�A�A�  A���A�  A�A�%A�A���A���A�  A���A���A���A���A���A���A�  A�  A�A�  A�  A�A�  A�  A�  A�A�
=A�1A�%A�A�A�A�  A�  A�A�A�  A�  A�  A�A�A�A���A���A���A���A���A���A���A���A���A�A�A�A���A���A���A���A���A���A��A��mA��;A��/A��/A��#A��A��
A��
A���A���A���A���A��
A��#A��#A��
A���A���A���A�ƨA�A˺^A˸RAˮA˴9AˮA˥�A˥�A˥�Aˣ�Aˣ�Aˣ�Aˣ�A˛�A˙�A˙�A˙�A˙�A˙�A˙�A˕�A˓uA˓uA˓uA˓uA˕�A˕�A˕�A˕�AˑhAˑhAˑhAˏ\A˅AˁAˁAˁAˁAˁAˁAˁAˁAˁAˁAˁAˁAˁA˅A˅Aˇ+A˅Aˇ+Aˇ+Aˉ7Aˇ+A˅AˁAˁAˁAˁA�~�A�~�A�~�A�~�A�~�A�~�A�|�A�v�A�p�A�jA�hsA�hsA�ffA�ffA�ffA�dZA�ffA�hsA�jA�jA�jA�jA�hsA�hsA�ffA�bNA�^5A�\)A�\)A�\)A�\)A�\)A�\)A�ZA�ZA�XA�VA�VA�S�A�Q�A�O�A�O�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LB
T�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
T�B
T�B
T�B
T�B
S�B
Q�B
P�B
P�B
Q�B
S�B
VB
`BB
�1B
ȴB
��B%�BG�BhsBr�B}�Bv�BcTB_;BhsBR�BO�BK�BT�BG�BH�BF�BD�BP�BH�Be`BR�B`BBhsBhsBgmBv�B��B�9B�?B�'B�wB�B�;B�B��B��B��B��B�{B��B��B�+B�B�B�1B�%B�B�=B�B}�By�BffBW
BQ�BO�BI�B@�B;dB)�B#�B�B�B1B
��B
�sB
��B
�^B
�!B
��B
�PB
t�B
_;B
O�B
:^B
#�B
�B	��B	�sB	�NB	�B	��B	�XB	��B	�PB	~�B	l�B	VB	A�B	5?B	)�B	 �B	�B		7B	  B��B�B�B��B��B��B��BƨB��B�^B�3B�B��B��B�PB�DB�%B�B|�Bq�Bl�BcTB�oBM�BJ�BH�BE�BD�BB�BA�B=qB8RB49B33B1'B9XB9XB8RB8RB6FB49B1'B/B-B,B&�B�B�BuBbBJB	7B+BBBBB  B  B��BBBBB  B��B��B��B  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B��B�B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��BBB%BBB\B�B�B�BhBbBbB\BVBVBhB{B�B{BuB�B�B�B!�B#�B"�B"�B!�B"�B&�B)�B,B,B+B0!B5?B8RB;dB;dB?}BC�BH�BJ�BN�BS�BVBXBXB\)BaHBcTBdZBiyBp�Br�Bq�Bq�Br�Bq�Bp�Br�Bs�Bv�B|�B�B�+B�1B�DB�DB�=B�=B�oB��B��B��B��B��B��B��B��B�B�B�B�'B�?B�RB�LB�FB�LB�RB�dB�}BÖB��B��B��B��B��B��B��B�B�B�B�5B�NB�NB�BB�HB�NB�NB�TB�TB�`B�sB�B�B�B�B��B��B	  B	B	B	+B	+B		7B	PB	�B	�B	 �B	 �B	!�B	"�B	!�B	!�B	"�B	#�B	$�B	+B	.B	.B	0!B	1'B	1'B	1'B	1'B	33B	1'B	0!B	/B	-B	.B	/B	0!B	0!B	/B	/B	1'B	2-B	2-B	33B	49B	49B	6FB	8RB	:^B	;dB	=qB	=qB	=qB	A�B	C�B	E�B	H�B	I�B	I�B	J�B	J�B	J�B	K�B	M�B	O�B	P�B	P�B	Q�B	Q�B	Q�B	Q�B	R�B	T�B	W
B	]/B	^5B	[#B	ZB	ZB	[#B	\)B	aHB	dZB	ffB	l�B	n�B	o�B	o�B	n�B	o�B	r�B	q�B	q�B	r�B	r�B	r�B	r�B	s�B	t�B	u�B	u�B	v�B	v�B	u�B	u�B	t�B	t�B	t�B	t�B	t�B	t�B	s�B	r�B	p�B	p�B	q�B	t�B	x�B	y�B	y�B	z�B	z�B	{�B	}�B	}�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�+B	�1B	�7B	�=B	�=B	�=B	�=B	�=B	�7B	�=B	�DB	�DB	�JB	�JB	�DB	�JB	�DB	�JB	�PB	�PB	�JB	�PB	�PB	�PB	�PB	�JB	�PB	�VB	�VB	�\B	�\B	�bB	�bB	�hB	�uB	�uB	�{B	�{B	��B	��B	��B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�-B	�-B	�'B	�-B	�'B	�-B	�-B	�-B	�-B	�9B	�?B	�?B	�?B	�FB	�FB	�LB	�LB	�RB	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�XB	�^B	�^B	�dB	�dB	�dB	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�dB	�jB	�jB	�jB	�qB	�wB	�}B	�}B	��B	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	B	ÖB	B	ÖB	B	B	B	ÖB	ÖB	B	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B
S�B
R�B
U�B
UgB
R�B
U�B�B|�BPbBF�BN�Bf�B�5B�B��B�B�oBK�B�B
��B
�B
9�B	�dB	�OB	*0B��BΊB��B�BlBIlB?�B9$B5�B%FBB�B�wB�+B�-B%B BpB�!BݲB	jB	/OB	I�B	p;B	t�B	��B	��B	��B	�]B	�HB	��B	��B	��B	��B	�HB	��B	�B	ʦB	οB	�{B	�?B	��B	�B	�B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
n�B(�?`BB
F�B
T�B
P�B
�A�|�B
W
?���B
]/B
�\A�\)?%`B?7
=B
dZB	�qA��B@�B��?��@�B�A�;dB
bB
#�B
�9B	�A�9X?��B	��A�?}B
W
B
-B
� A;p�B!�?��!?��B	�B
s�?T�jB
u�A���?33B
S�B
k�BH�B	|�B
�F?dZB
��At�?r-B
��B�?�+B
�B
�B
%B
H�B
ffB
��@XB
��B�{@$(�B
q�@ɺ^?>v�B	�?��hB
�RA��T?���B{�B
R�B	�B
T�B
VB
t�B)�?'+B
�h?��B
�B
_;@�G�B
p�@�;dB
+B
T�B
T�B
R�B
R�B
T�B
W
B
W
B
R�B
W
B
O�B
�+B��?{B
p�?vB
`BB
�7@ �?�?�E�B
R�B
[#B��B
�7B
dZB
��?'�B
z�B~�?st�B	j?D��B
gmB
o�B
t�BƨB
W
B
=qB
ZA�|�B
��?h�9B��B
K�B
bNB
��?\B
B
49B
P�B
R�B
VB
W
B
D�B	,B
.A��B
S�B
X@�`BB� ?�O�A���B
y�B)�?~��B
?}B
VB
ZB
�BH�B
{�A�{AM�B
F�B	�B
aHB
��@'�B
K�B
J�B
P�B
ZB
]/A�&�BcT?��A���B
]/B
��B1'?^��?*��B
o�B?}?<�B
jB
�B�?�B	>wB)�?p�B
w�?�33BŢ>�?Z@}��B�A�JB
�B!�AK��A�?}B�?e��B
cTB
��?9�B
��>� �?CoB
I�B
S�B
T�B
T�B
S�B
P�B
Q�B
R�B
R�B
R�B
P�B
S�B
T�B
T�B
T�B
VB
T�B
T�B
VB
VB
T�B
T�B
T�B
VB
VB
W
B
T�B
T�B
T�B
T�B
VB
S�B
S�B
S�B
VB
S�B
W
B
VB
W
B
VB
VB
T�B
VB
VB
W
B
T�B
VB
T�B
VB
T�B
T�B
VB
VB
VB
T�B
W
B
VB
VB
T�B
R�B
T�B
VB
T�B
T�B
S�B
T�B
VB
T�B
T�B
T�B
S�B
T�B
S�B
S�B
T�B
T�B
S�B
S�B
P�B
S�B
T�B
S�B
S�B
T�B
S�B
T�B
S�B
S�B
R�B
T�B
S�B
S�B
R�B
T�B
S�B
R�B
S�B
S�B
T�B
S�B
R�B
S�B
R�B
T�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
T�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
T�B
S�B
S�B
S�B
S�B
R�B
S�B
S�B
VB
S�B
S�B
S�B
Q�B
R�B
S�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
T�B
R�B
S�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
S�B
T�B
T�B
S�B
S�B
T�B
S�B
T�B
S�B
S�B
S�B
T�B
S�B
S�B
R�B
R�B
S�B
S�B
R�B
S�B
S�B
S�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
T�B
T�B
T�B
VB
S�B
S�B
T�B
S�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
T�B
VB
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
T�B
T�B
VB
VB
VB
VB
VB
VB
T�B
VB
T�B
T�B
T�B
S�B
S�B
T�B
S�B
T�B
T�B
S�B
S�B
S�B
VB
S�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
T�B
T�B
VB
VB
T�B
VB
VB
VB
VB
T�B
T�B
VB
T�B
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
T�B
VB
VB
T�B
VB
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
T�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
T�B
VB
T�B
T�B
R�B
T�B
VB
T�B
VB
T�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
W
B
T�B
T�B
VB
T�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
VB
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
T�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
VB
VB
T�B
S�B
T�B
T�B
S�B
T�B
T�B
S�B
T�B
S�B
S�B
T�B
T�B
S�B
S�B
T�B
S�B
VB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
R�B
R�B
S�B
S�B
S�B
R�B
S�B
R�B
R�B
R�B
S�B
Q�B
R�B
Q�B
R�B
R�B
R�B
R�B
Q�B
R�B
R�B
S�B
N�B
N�B
P�B
T�B
P�B
Q�B
Q�B
O�B
P�B
N�B
O�B
P�B
O�B
N�B
T�B
Q�B
P�B
Q�B
O�B
N�B
O�B
S�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
O�B
P�B
P�B
O�B
P�B
Q�B
O�B
P�B
O�B
O�B
P�B
R�B
P�B
O�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
S�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
P�B
Q�B
P�B
R�B
VB
T�B
VB
W
B
W
B
VB
W
B
W
B
W
B
VB
VB
S�B
R�B
Q�B
P�B
T�B
T�B
T�B
VB
T�B
VB
W
B
YB
T�B
XB
W
B
VB
W
B
VB
W
B
XB
W
B
VB
VB
VB
VB
W
B
VB
XB
XB
XB
XB
YB
W
B
XB
YB
XB
XB
ZB
ZB
[#B
[#B
_;B
`BB
aHB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
aHB
bNB
aHB
bNB
bNB
ffB
ffB
ffB
gmB
gmB
hsB
iyB
k�B
m�B
o�B
o�B
o�B
s�B
t�B
t�B
t�B
u�B
v�B
x�B
y�B
~�B
�B
�B
�B
�B
�1B
�1B
�bB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�!B
�!B
�'B
�'B
�-B
�3B
�?B
�XB
�jB
�qB
�wB
�}B
B
B
ƨB
ǮB
ȴB
��B
��B
�B
�B
�NB
�`B
�fB
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
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  BBBB+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�G�A�G�A�/A�?}A�XA�5?A�ZA�9XA�=qA�=qA�M�A��A�JA� �A��A��A���Aˣ�A���A�E�A�^5A�K�A���AʮA�hsA�l�A�XA��A�dZA��Aȴ9A�bA�G�A�ZA�ĜA�~�A�bA�?}Aß�Aº^A���A��!A���A�33A���A�1'A�|�A��uA��jA�JA���A��A�`BA��FA��HA���A���A��+A���A�|�A���A���A���A��A���A��-A���A��HA�~�A��A��jA�`BA�{A�/A���A�E�A�+A���A�t�A�v�A��A�G�A�G�A���A�x�A���A��7A��7A���A���A��A��A��hA��A��A�|�A�|�A���A��A�z�A��A���A��7A�VAXA}C�A{
=Ax�`Av(�As+Ao�Ak��Ah5?Af1'Ad~�Aa��A`�A^ZA\��A[33AYAW�mAU��ATĜAR�jAR{APv�APE�AOt�AO"�AM��AL�+AKhsAJ1'AHZAGK�AE�AD��ADz�AC��AB=qA@�HA?XA<�HA:�/A8ȴA7p�A61'A4��A4��A3�PA3�A1�TA0�A0JA.(�A-33A,E�A+|�A*�HA*Q�A)S�A(�/A'��A&��A%�A$�+A#G�A"~�A!�7A �uAXA�A��A�A��A�`A�A�A5?A��A�\A�A�A��A�`A/A��A{AAz�A�A��AȴA{A��A5?A1A�PA
��A
r�A	33A~�At�A�`A�DA�A��A-A;dAv�A�A�hA�A r�@�@���@�33@�M�@���@��+@��@��@��T@���@�V@�hs@��@��@�x�@�I�@��y@�^@�|�@�5?@�-@�A�@�-@ݺ^@���@�33@�E�@���@��@׾w@֧�@ָR@ԋD@�1'@�/@���@�1'@�n�@��@��@�ff@щ7@���@с@�ƨ@�"�@�~�@�v�@���@�z�@�%@�bN@��@�o@�=q@�hs@�bN@��@�@�=q@��@�r�@� �@�1'@�ff@���@���@�E�@��@��@���@�\)@�  @��@��^@��T@�hs@�O�@�V@��P@�n�@�{@�@��m@��@�I�@�9X@�^5@���@���@��h@�1'@��F@�Ĝ@��+@�7L@�?}@�1'@�Q�@�1'@��@�"�@�^5@��@�hs@�j@�Z@��9@�l�@���@�ȴ@�t�@�"�@��@��@�bN@���@���@�5?@���@���@���@�$�@��@��@��@�A�@���@�
=@�Z@��w@��m@�bN@�C�@���@���@�V@�I�@��m@�;d@�"�@�t�@�o@�
=@�ƨ@�$�@��R@��R@��@��+@�+@��y@�M�@�ff@��#@��^@���@��#@��@���@�x�@��j@�|�@��/@�Z@���@��@�+@��T@�;d@�$�@�~�@�ff@���@�`B@�ƨ@�1'@�E�@���@�=q@��@��@�j@��;@�  @�\)@�5?@�b@�E�@��H@�p�@��#@���@�=q@�@��@��/@���@�(�@��@��9@�7L@��+@���@��w@�^5@��@�-@�V@��7@�7L@��@�O�@��@��w@�1'@�Z@�t�@��@� �@�33@�ƨ@�ȴ@�J@��@��@��y@��y@��@�\)@���@��@�  @�K�@���@���@�9X@�p�@��@�&�@�?}@�/@�E�@��`@��u@�|�@�;d@�9X@�Ĝ@�E�@�^5@�ff@���@�9X@��u@�b@��+@���@��@�E�@��T@�t�@�t�@���@��R@�V@���@�E�@�@��@�7L@���@��T@�x�@�1'@�r�@�&�@�`B@�7L@�J@��P@�r�@��@���@�;d@�|�@��P@�+@�@��H@�K�@�V@�M�@�=q@���@���@��^@�%@�1@��@K�@�A�@�A�@�@�b@}�-@\)@+@~��@|��@~�@~v�@|j@{"�@}O�@yX@z�@{��@yx�@w�@xQ�@wK�@vE�@x1'@t�D@u@uO�@vȴ@uV@u�h@t�@tZ@r��@q��@r^5@s33@qhs@q�7@q��@q&�@q�@pr�@pĜ@p1'@m��@q%@n��@m��@o��@nv�@m��@l�j@lI�@m/@lj@l�j@k33@ko@mO�@i��@m�T@i�#@i��@g\)@e@ep�@i��@f��@f�+@f�y@g�w@c�@d(�@hQ�@b�!@f�R@d�j@g�w@f�+@e?}@b��@dI�@a�@b�@b�@` �@aG�@_+@_
=@]��@^v�@^@_�@[t�@\��@\�j@\I�@\z�@Z�!@[�F@ZM�@XĜ@[@X��@U�T@Vff@[S�@X�`@W\)@T��@U��@Up�@UO�@U��@Tz�@U�@S�m@T9X@V�+@V��@T�j@S�F@VE�@U�@P��@R^5@Qx�@O�w@So@M�@O�w@Q&�@N�+@M��@Pb@Nȴ@M/@Nȴ@L1@N{@LZ@Jn�@L(�@N��@LZ@I��@K��@L�@H��@K"�@I%@G�@Hb@I��@G\)@H  @F5?@GK�@H�`@G+@E/@F5?@FE�@D��@C��@I�@F��@Dj@FV@C�
@B-@B��@Ct�@DI�@A��@CS�@B~�@B�\@B�\@@�u@?
=@A�^@>��@>��@@Q�@@��@@�@?K�@AG�@?��@>ȴ@A�^@?��@>�y@@Ĝ@=p�@;t�@?;d@?\)@=�@<��@=V@<z�@>ff@<��@<z�@<(�@<(�@8��@:��@:~�@6��@:-@7�;@:J@6ȴ@9G�@9hs@9&�@65?@:�\@8��@9hs@5/@6�@6��@41@1X@6ȴ@4�@3�m@4��@4I�@3��@5�T@5/@4�@4�@3@3��@5`B@2n�@1�@3o@4Z@333@3�m@0��@01'@/�@1&�@0��@1hs@1%@0 �@/K�@.v�@/|�@1�@0�@/�@0Ĝ@-�T@,�j@.�+@2J@.�y@.ȴ@-`B@+�F@.ȴ@,��@,�j@.$�@.@-?}@(r�@-p�@-�@+�
@*�!@,1@)�#@*�!@(��@)��@*��@)X@*M�@&E�@*�@*��@)��@'�;@%�T@)7L@)7L@'��@(��@'|�@(  @&E�@&ff@)&�@'�w@'|�@$(�@&�R@(A�@'K�@"�@$�@$��@$(�@$��@&�@%�@"=q@!X@#��@"�@!x�@�@ Q�@!�@V@ ��@l�@"��@��@   @{@;d@��@�@��@S�@{@?}@`B@ r�@$�@�@l�@��@�
@�
@z�@�@��@E�@�h@z�@33@ƨ@��@��@�F@��@&�@
=@�w@�@Q�@�;@Q�@�!@�T@�7@�@v�@�@��@;d@
=@��@��@
=@@�@5?@�-@V@��@�y@`B@9X@/@ȴ@+@�;@9X@��@�@�R@�@t�@5?@Z@ƨ@"�@�@�H@o@n�@+@�@o@��@��@�`@��@��@��@�@33@��@�@��@��@��@~�@��@��@��@"�@��@S�@�@O�@K�@Ĝ@  @33@K�@v�@�
@�;@\)@�h@�/@{@��@ƨ@|�@�w@|�@
��@@��@/@��@��@p�@{@`B@V@
��@��@
��@�j@`B@@��@5?@z�@
=q@
=q@5?@	X@
��@ƨ@��@5?@@��@
��@z�@
=q@�
@�
@(�@��@��A�oA̴nA�N<A���A���A�oAɫ�A��AÑhA� �A��A�6A��	A���A��5A��}A�	�A��{A��aA�	7A���A�h
At�Ae�9A[v`AUe�AOK^AKc AB�A>k�A5�#A0�A,�A&4�A"��A&�A�NA�A
͟@��@�"�@�e�@�^5@��L@���@�&�@�<�@��@���@�<�@���@}�S@uw2@mQ�@e&�@Q��@I��@D4n@+6z@3�:@0Ɇ@(l"@(l"@0Ɇ@kQ@�D@	��@kQ@U2@U2@خG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�=qA�x�A�x�A�ƨA̴9A��A�=qA�x�A�ƨA�A�+A�A�+A̴9A�=qA��A�A�=qA�=qA�=qA�A�=qA�=qA�x�A�A�A�=qA�=qA�=qA��A�A�=qA�A�=qA�x�A�+A�=qA�=qA̴9A̴9A�=qA�x�A��A�A��A̴9A�=qA�x�A̴9A�A̴9A�=qA�x�A�=qA��A�=qA̴9A�ƨA�ƨA̴9A�=qA�=qA�=qA̴9A�x�A̴9A�A�A�x�A�+A�A�A̴9A�x�A�=qA�=qA̴9A�=qA��A�A�A�A��A�=qA�A�x�A̴9A�A�=qA�=qA̴9A�x�A�=qA̴9A�A�ƨA�ƨA�+A�x�A�A�A�x�A��A��A�x�A�+A�A�=qA�=qA�A�=qA��A�x�A��A�=qA̴9A�=qA�x�A�=qA�=qA�=qA�A�=qA�=qA��A�A�A�=qA�=qA�=qA�=qA�ƨA�=qA�A̴9A�=qA̴9A�A��A�A�+A�=qA��A̴9A�A�A�x�A��A�A�=qA�=qA�=qA�A�=qA�+A��A�+A�x�A̴9A�=qA̴9A�A�A�A�=qA�ƨA�=qA�=qA�=qA�A�A��A�x�A��A�A�=qA��A�x�A�=qA�=qA�A��A�x�A�A�=qA�=qA�=qA�=qA��A̴9A�x�A�+A��A�A�A�x�A�=qA�A�+A�=qA�A�A�=qA�=qA�=qA�+A�=qA�x�A̴9A�+A�x�A̴9A̴9A�A��A�A�A�A�=qA�=qA��A��A�=qA��A̴9A�x�A�A�x�A�A̴9A�+A�A�A��A�A�=qA�A�A��A�A�x�A��A�=qA�A̴9A�ƨA�A�ƨA��Aˉ7A�=qA�A�A̴9A�+A�A̴9A�A��A�=qA�A�x�A�ƨA�A̴9A�ƨA�ƨA�ƨA�x�A�x�A�x�A�+A̴9A�=qA�=qA̴9A̴9A�ƨA�A��A�ƨA�x�A�=qA�ƨA�ƨA�+A�A�=qA�A�A̴9A�ƨA�A�x�A�A̴9A�A�A�=qA�ƨA�A�ƨA�x�A�A̴9A�=qA�=qA�A�x�A�A�A�A�ƨA�+A��A�A�A��A�ƨA�=qA̴9A�=qA�ƨA�x�A�ƨA�x�A�ƨA�=qA�x�A�ƨA�=qA�+Aˉ7A�A�A̴9A�A�A�A�+A�x�A��A��A�=qA�x�A�=qA�ƨA̴9A�ƨA�A�A̴9A��A�A̴9A�=qAˉ7A�=qA�+A�A�A�A�A�=qA�=qA�=qA�=qA̴9A�+A�Aˉ7A�ƨA�=qA�ƨA�=qA̴9A�x�A�ƨA�=qA�=qA�A�=qA�=qA�=qAˉ7A�=qA�=qAˉ7A�A�=qA�=qA�=qA�x�A�=qA�+A��A�=qA��A�x�A�x�A�=qA�=qA�=qA�x�A̴9A̴9A�A�A��A�A̴9A�=qA�ƨA�A�=qA�A̴9A�A�=qA�=qA��A�=qA�+A�=qA�A�ƨA�=qA̴9A�A�=qA̴9A�ƨAˉ7A�A̴9A�+A�ƨA�=qA�=qA�=qA�x�A�x�A�A�=qA�=qA�=qA�ƨA�+A�A�A�=qA�=qA�A̴9A�ƨA�=qA̴9A�=qA�ƨA�=qA�M�A�=qA��A̴9A�=qA�A̴9A�=qA�A�A�A�ƨA�x�A�A�ƨA�=qA�+A�A�x�A̴9A�x�A�=qA�A�A�=qA̴9A�x�A�=qA�ƨA�=qA�A�=qA̴9A�=qA�=qA�A��A̴9A̴9A�=qA�ƨA�=qA�=qA�A̴9A�ƨA�A�x�A�A̴9A�=qA��A�A�A�M�A�x�A�ƨA�=qAˉ7A�A�ƨA�=qAˉ7A̴9A�A�=qAˉ7A�A�=qA�ƨA�A�ƨA̴9A�ƨA�=qA̴9Aˉ7Aˉ7A�=qAˉ7A�=qA�+A�ƨA�A�x�A�+A̴9A�ƨA�ƨA�A�=qA̴9A�A�ƨA�ƨAˉ7A�A�ƨA�ƨA�A�A̴9A�ƨA��
A̴9A�x�A�ƨA�ƨA̴9Aˉ7A�A�A�ƨA�M�A�ƨA�=qA�=qA̴9A�ƨAˉ7A�ƨA�=qA�ƨA�ƨAˉ7Aˉ7A�ƨA�ƨA̴9Aˉ7Aˉ7A�Aˉ7Aˉ7A�ƨAˉ7Aˉ7A�ƨA�ƨA̴9Aˉ7A�ƨAˉ7A�M�A�ƨA�Aˉ7A�ƨA�ƨA�x�Aˉ7Aˉ7Aˉ7A�=qAˉ7Aˉ7A�ƨAˉ7Aʛ�Aˉ7A�oA�ƨA�M�A�M�A��Aˉ7A�ƨAˉ7A�oA�=qA�=qA̴9A�M�Aˉ7A�M�Aˉ7A�M�A�=qA�ƨA��A�=qA�ƨAˉ7Aˉ7A�M�A�=qA�M�A�=qA�ƨA�oA��
A�M�A�M�A�M�A��
A�M�Aˉ7A�oA�oA�oA�oA�A�oA�oAˉ7A�M�A�oA�=qA�oA�M�A�M�A�oA�A�oA��
A�oA�ƨA�M�Aˉ7A�=qA��
A�ƨA�x�Aʛ�A�=qA��
Aʛ�Aˉ7Aˉ7Aˉ7A�oA�M�A�oAˉ7Aˉ7A�ƨA�M�A��
Aˉ7A�oA��
A�M�A�ƨA��
A��
Aʛ�A��
A�M�Aʛ�A�oAʛ�A��
A��
A��
A�ƨAʛ�A�M�Aˉ7A��
A�`BA�"�A��
Aʛ�Aʛ�A�"�Aʛ�A�`BA��
Aʛ�Aʛ�A�M�A��
Aʛ�A�oA�ƨA��
Aʛ�A�oA�oA�`BA��mA�"�A�`BA�`BA�`BA�M�Aʛ�A�"�Aʛ�Aʛ�Aʛ�Aʛ�A��
A��mA��
A��
Aʛ�A��mA�oA�"�A�oA�`BA�"�A��
A��mA�"�A��mA�"�A��mA��
A��
A��
A�oA��mA�"�A�"�A��mAɬAʛ�AɬA�`BA��mA�"�A��
Aʛ�A�"�A�M�A�"�A�`BA�oA�oA��mA�"�A�oA��mA�"�Aʛ�AɬA��mA�`BAʛ�A�`BA�"�A�oA�oA��
A�M�Aʛ�AɬA�"�A�"�A�`BA�"�A��
Aʛ�A�"�A�"�A�"�A�oA��
A�`BA��mA�p�A�M�A�"�A��
A�"�A��mAɬA�"�A�p�A��
A��
A��
A�`BAɬA�"�A�"�A��mAʛ�A��
A��
A�`BA�"�A��mA�33A��mAʛ�Aʛ�A��mAʛ�A�p�A�`BA��mAɬA��mAȼjA��mA�`BAɬA�oAɬAɬA�`BA��mAɬAɬAʛ�A�"�AɬA�p�AɬAɬAɬA�p�A�p�A���AɬA�p�AɬA�p�A��mA�p�A���A��mA�p�A�33A���A�33A�p�A�"�A�"�AȼjA�33A���A�33AȼjA�33A�p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                                                                    PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES            TEMP            PSAL            TEMP_CNDC       PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + X2old*PRES + X3old*PRES^2 + X4old*PRES^3) / (1 + X2new*PRES_ADJUSTED + X3new*PRES_ADJUSTED^2 + X4new*PRES_ADJUSTED^3)                                                                                           Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            X2old = 1.8732e-6, X3old = -7.7689e-10, X4old = 1.4890e-13, X2new = 1.2485E-06, X3new = -3.9683E-10, X4new = 6.3692E-14                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Pre-April 2021 RBR CTD. Salinity re-computed by using new compressibility coefficients provided by RBR.                                                                                                                                                         Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Not applicable                                                                                                                                                                                                                                                  Bad PSAL.                                                                                                                                                                                                                                                       Not applicable                                                                                                                                                                                                                                                  202210041156222022100411562320221004115623202210041156232022100411562220221004115622202210041156232022100411562320221004115622202210041156222022100411562320221004115623202210041156222022100411562320221004115623202210041156232022100411562220221004115622202210041156232022100411562320221004115622202210041156222022100411562320221004115623202210041156222022100411562320221004115623202210041156232022100411562220221004115622202210041156232022100411562320221004115622202210041156222022100411562320221004115623AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            202208232247432022082322474320220823224743    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232247432022082322474320220823224743  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B83E            383E            383E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            202208232247432022082322474320220823224743  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�8800            800             800             UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202210041156232022100411562320221004115623  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                