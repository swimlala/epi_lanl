CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  G   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:40Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        'T  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ld   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'T  v<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'T  �h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T  μ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� '<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T 1   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'T Xh   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'T ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'T ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 	h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� :�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T Dl   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � k�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   l�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   x�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181640  20200831164744  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               $   $   $AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @ոA"�P�@ոA"�P�@ոA"�P�111 @ոA����@ոA����@ոA����@7I�^@7I�^@7I�^�cj=p��
�cj=p��
�cj=p��
111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    $   $   $ADA BDA  DA BDA @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� Dy{�D��D�=D��)D��qD��D�B�D�\D���D�qD�D�D�|)Dǳ�D��D�W\Dڏ
D�{D��D�0RD�uD��3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�;L�ͽ���    �L�;L�ͽ��;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�;L�ͽ���    �L�;L�ͽ���    �L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�;L�;L��        �L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L��        �L�;L��        �L�;L�;L�;L�ͽ��ͽ��;L�;L�ͽ��ͽ��;L�;L�;L��    ���;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��;L�;L�;L��        �L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�;L�;L�;L�ͽ��;L�;L�ͽ��ͽ��;L�;L�;L�ͽ��;L�ͽ��;L�;L�;L�;L��    ���ͽ��ͽ��;L�;L�ͽ���        ���;L�;L�ͽ���        ���ͽ��ͽ��ͽ��ͽ���    =���=���        ���ͽ���            =���    =���=���=���=���            =���                    =���=���                    =���=���=���=���=���=���>L��=���=���                        =���=���=���=���=���=���=���=���=���                        =���                =���=���=���=���=���=���=���                            =���=���=���=���=���=���    ���ͽ���            =���=���=���=���=���=���=���        ����        =���=���=���=���=���                =���=���=���                        =���=���                                =���=���        =���=���            ����=���=���    =���            ����                =���=���=���=���>L��=���=���>L��>L��>L��=���                =���=���=���    =���=���=���                            =���=���=���            ����    =���=���                        =���        ����        =���=���>L��>L��=���>L��>���>L��>L��>���>L��>L��>L��>���>L��        ���ͽ���                =���            =���=���=���                                ����            =���=���=���>L��=���=���=���            ����    =���=���>L��>L��>���>���>���>���?   ?��?333?333?L��?L��?L��?fff?�  ?���?���?���?���?�ff?�33?�33?�  ?���?���?ٙ�?�ff?�ff?�33?�33@   @ff@��@��@��@33@��@   @   @&ff@333@333@@  @Fff@Fff@S33@Y��@`  @fff@l��@s33@y��@�  @�ff@���@���@�  @�33@�ff@���@���@�33@�ff@���@���@�  @�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@���@�  @�33@�ff@陚@���@�33@�ff@���@���A��A33A��AffA	��A33A��A  A��A33A��A  A��A33A��A   A!��A#33A&ffA(  A)��A+33A.ffA0  A333A4��A6ffA8  A9��A<��A>ffA@  AC33AD��AFffAH  AK33AL��AP  AQ��AS33AT��AX  AY��A[33A^ffA`  Aa��Ac33AfffAh  Ai��Al��AnffAp  As33At��AvffAx  A{33A|��A~ffA�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  Ař�A�ffA�33A���Aə�A�ffA�  A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A���Aٙ�A�ffA�33A���Dp� Dp�fDp�3Dp��Dp� Dp��Dp�3Dp� Dp�fDp��DpٚDp� Dp�fDp�3Dp��DqfDq�Dq�Dq  Dq&fDq33Dq9�DqFfDqL�DqS3Dq` DqffDqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Dry�Dr� Dr��Dr�3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3Ds` DsffDsl�Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt33Dt9�DtFfDtL�DtS3Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt� Dt�fDt�3Dt��@&ff@333@333@@  @Fff@Fff@S33@Y��@`  @fff@l��@s33@y��@�  @�ff@���@���@�  @�33@�ff@���@���@�33@�ff@���@���@�  @�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@���@�  @�33@�ff@陚@���@�33@�ff@���@���A��A33A��AffA	��A33A��A  A��A33A��A  A��A33A��A   A!��A#33A&ffA(  A)��A+33A.ffA0  A333A4��A6ffA8  A9��A<��A>ffA@  AC33AD��AFffAH  AK33AL��AP  AQ��AS33AT��AX  AY��A[33A^ffA`  Aa��Ac33AfffAh  Ai��Al��AnffAp  As33At��AvffAx  A{33A|��A~ffA�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  Ař�A�ffA�33A���Aə�A�ffA�  A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A���Aٙ�A�ffA�33A���Dp� Dp�fDp�3Dp��Dp� Dp��Dp�3Dp� Dp�fDp��DpٚDp� Dp�fDp�3Dp��DqfDq�Dq�Dq  Dq&fDq33Dq9�DqFfDqL�DqS3Dq` DqffDqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Dry�Dr� Dr��Dr�3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3Ds` DsffDsl�Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt33Dt9�DtFfDtL�DtS3Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt� Dt�fDt�3Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @@  @�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�A�fgA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�33B�ffB�ffB�ffB�ffB�33B�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#3D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�gDq�Dq��Dr�Dr��DsgDs��Dt�Dt��Dy�RD�	GD�C�D���D���D�D�IGD���D��3D��D�K3D���Dǹ�D��D�]�DڕpD���D��
D�6�D�{�D�ٙG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL��=���>L�ͱL�ͱL��=��ͱL�ͱL�ͱL��=��ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=���=��ͱL��=���=��ͱL�ͱL��=��ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL��=���>L�ͱL�ͱL��=���>L�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=���=��ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��>L��>L�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��>L��>L�ͱL�ͱL��>L��>L�ͱL�ͱL�ͱL�ͱL��=���=��ͱL�ͱL��=���=��ͱL�ͱL�ͱL��>L��=��ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL�ͱL��>L��>L�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL�ͱL��=���=��ͱL�ͱL�ͱL�ͱL��=��ͱL�ͱL��=���=��ͱL�ͱL�ͱL��=��ͱL��=��ͱL�ͱL�ͱL�ͱL��>L��=���=���=��ͱL�ͱL��=���>L��>L��=��ͱL�ͱL��=���>L��>L��=���=���=���=���=���>L��>���>���>L��>L��=���=���>L��>L��>L��>���>L��>���>���>���>���>L��>L��>L��>���>L��>L��>L��>L��>L��>���>���>L��>L��>L��>L��>L��>���>���>���>���>���>���>���>���>���>L��>L��>L��>L��>L��>L��>���>���>���>���>���>���>���>���>���>L��>L��>L��>L��>L��>L��>���>L��>L��>L��>L��>���>���>���>���>���>���>���>L��>L��>L��>L��>L��>L��>L��>���>���>���>���>���>���>L��=���=���>L��>L��>L��>���>���>���>���>���>���>���>L��>L��=���>L��>L��>���>���>���>���>���>L��>L��>L��>L��>���>���>���>L��>L��>L��>L��>L��>L��>���>���>L��>L��>L��>L��>L��>L��>L��>L��>���>���>L��>L��>���>���>L��>L��>L��=���>���>���>L��>���>L��>L��>L��=���>L��>L��>L��>L��>���>���>���>���>���>���>���>���>���>���>���>L��>L��>L��>L��>���>���>���>L��>���>���>���>L��>L��>L��>L��>L��>L��>L��>���>���>���>L��>L��>L��=���>L��>���>���>L��>L��>L��>L��>L��>L��>���>L��>L��=���>L��>L��>���>���>���>���>���>���?   >���>���?   >���>���>���?   >���>L��>L��=���=���>L��>L��>L��>L��>���>L��>L��>L��>���>���>���>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>���>���>���>���>���>���>���>L��>L��>L��=���>L��>���>���>���>���?   ?   ?��?��?333?L��?fff?fff?�  ?�  ?�  ?���?���?�fg?�fg?�34?�34?�  ?���?���?ٙ�?�fg?�fg?�34@   @   @ff@ff@��@33@��@��@��@   @&fg@,��@,��@333@@  @@  @L��@S33@S33@`  @ffg@l��@s33@y��@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@�ff@���@�  @�33@�ff@ə�@�  @�33@�ff@ٙ�@���@�33@�ff@陙@���@�  @�33@���@���A   A��A��AffA  A	��A��AffA  A33A��AffA  A33A��AffA   A#33A$��A&ffA)��A+33A,��A.ffA1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AFffAH  AI��AK33ANffAP  AS33AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��AfffAi��Ak33Al��Ap  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A���A�34A�  A���A�fgA�34A�  A���A�fgA�34A�  A���A�fgA�34A���A���A�fgA�  A���A���A�fgA�  A���A���A�34A�  A���A�fgA�34A�  A���A�fgA�34A���A���A�fgA�34A���A���A�fgA�  A���A���A�fgA�  A���A���A�34A�  A���A�fgA�34A�  A���A�fgA�34A���A���A�fgA�34A���A���A�fgA�  A���Ař�A�34A�  A���A�fgA�34A�  A͙�A�fgA�34A�  Aљ�A�fgA�34A���Aՙ�A�fgA�  A���A�fgA�34A�  A���A�fgDp��Dp�3Dp� Dp�gDp��Dp��Dp� Dp��Dp�3DpٚDp�gDp��Dp�3Dq  DqgDq3Dq�Dq&gDq,�Dq33Dq@ DqFgDqS3DqY�Dq` Dql�Dqs3Dq� Dq�gDq�3Dq��Dq� Dq��Dq�3Dq� Dq�gDq��DqٚDq� Dq��Dq�3Dr  DrgDr�Dr�Dr  Dr,�Dr33Dr9�DrFgDrL�DrY�Dr` DrfgDrs3Dry�Dr�gDr��Dr��Dr� Dr��Dr�3Dr��Dr�gDr��DrٚDr� Dr�gDr�3Dr��DsgDs�Ds�Ds  Ds,�Ds33Ds9�DsFgDsL�DsY�Ds` Dsl�Dss3Dsy�Ds�gDs��Ds��Ds� Ds��Ds�3Ds� Ds�gDs��DsٚDs� Ds��Ds�3Dt  DtgDt�Dt�Dt  Dt,�Dt33Dt@ DtFgDtS3DtY�Dt` Dtl�Dts3Dt� Dt�gDt��Dt��Dt� Dt��Dt�3Dt� Dt�g@333@@  @@  @L��@S33@S33@`  @ffg@l��@s33@y��@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@�ff@���@�  @�33@�ff@ə�@�  @�33@�ff@ٙ�@���@�33@�ff@陙@���@�  @�33@���@���A   A��A��AffA  A	��A��AffA  A33A��AffA  A33A��AffA   A#33A$��A&ffA)��A+33A,��A.ffA1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AFffAH  AI��AK33ANffAP  AS33AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��AfffAi��Ak33Al��Ap  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A���A�34A�  A���A�fgA�34A�  A���A�fgA�34A�  A���A�fgA�34A���A���A�fgA�  A���A���A�fgA�  A���A���A�34A�  A���A�fgA�34A�  A���A�fgA�34A���A���A�fgA�34A���A���A�fgA�  A���A���A�fgA�  A���A���A�34A�  A���A�fgA�34A�  A���A�fgA�34A���A���A�fgA�34A���A���A�fgA�  A���Ař�A�34A�  A���A�fgA�34A�  A͙�A�fgA�34A�  Aљ�A�fgA�34A���Aՙ�A�fgA�  A���A�fgA�34A�  A���A�fgDp��Dp�3Dp� Dp�gDp��Dp��Dp� Dp��Dp�3DpٚDp�gDp��Dp�3Dq  DqgDq3Dq�Dq&gDq,�Dq33Dq@ DqFgDqS3DqY�Dq` Dql�Dqs3Dq� Dq�gDq�3Dq��Dq� Dq��Dq�3Dq� Dq�gDq��DqٚDq� Dq��Dq�3Dr  DrgDr�Dr�Dr  Dr,�Dr33Dr9�DrFgDrL�DrY�Dr` DrfgDrs3Dry�Dr�gDr��Dr��Dr� Dr��Dr�3Dr��Dr�gDr��DrٚDr� Dr�gDr�3Dr��DsgDs�Ds�Ds  Ds,�Ds33Ds9�DsFgDsL�DsY�Ds` Dsl�Dss3Dsy�Ds�gDs��Ds��Ds� Ds��Ds�3Ds� Ds�gDs��DsٚDs� Ds��Ds�3Dt  DtgDt�Dt�Dt  Dt,�Dt33Dt@ DtFgDtS3DtY�Dt` Dtl�Dts3Dt� Dt�gDt��Dt��Dt� Dt��Dt�3Dt� Dt�gG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���Aɡ�A�p�A�\)A�K�A�C�A�=qA�5?A�/A�+A�(�A�&�A�$�A�"�A�"�A� �A� �A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A�oA��A�$�A��AȑhA��TAǬA��AĲ-A���A�XA��FA��A���A�%A�bNA�
=A�7LA��-A��/A��
A��A�JA�r�A���A��A�^5A�JA��A�hsA���A�VA�Q�A�A�M�A��A�bA�|�A�JA� �A�p�A�  A��A��A�+A���A��A�v�A�VA���A���A���A�XA�A��DA���A��A���A��A���A�jA�-A�VA���A��#A���A��hA���A�O�A��`A�M�A�M�A�S�A�ffA���A��
A��A��A�JA�t�A���A��RA�33A�oA���A��7A�p�A�Q�A���A}�hA{�-Axz�Av��Ar-An1Aj�AiAg��AgoAf��Ae�Ab�yA`��A_�A^�`A^E�A]dZA[AZ5?AX9XAU33AT�AS|�AQƨAO�AN{ALv�AI��AGAFM�AD��ADr�ACp�AA�TA@Q�A?;dA>bNA<��A;�TA;|�A: �A8�\A8�A8r�A6��A4�A3�A3S�A2��A0�DA0ZA/��A.�+A.VA-��A-p�A+�#A*��A*VA);dA(1A'G�A&��A%ƨA%�7A%G�A$�`A$-A"�uA (�A;dA��A�A��A��A�-AjA^5AI�A�A"�A��A(�A7LA�RAA�AƨA
=A1'A��A�A��A/AĜA^5A1A+A  AhsA
�`A
~�A	�FA	A��A�A1'A�/A��A��A��AO�A ��@��m@���@�V@���@�I�@�~�@�hs@���@���@��7@�O�@��T@��\@�C�@�ƨ@��@���@�C�@��@�1@�+@�p�@��@��-@�S�@� �@�p�@ޗ�@�hs@ܬ@��@��@���@��@�+@�V@љ�@�?}@�r�@�"�@ΰ!@�{@�@�$�@��@��T@�=q@�C�@ϕ�@��@�A�@�"�@��H@ʇ+@�~�@�^5@ɡ�@�o@�hs@ċD@��^@��7@��@�j@�"�@��@�b@��@��@���@�v�@�ff@�hs@���@��j@��F@�^5@��@�$�@��^@��7@��u@�ƨ@��@��H@���@�&�@���@��@�n�@��
@�+@�I�@�1@��
@��\@�X@�1'@��@��@��P@�l�@�X@�Ĝ@�ƨ@�|�@�n�@��h@�p�@�p�@�Ĝ@��@��D@�b@�5?@��@�`B@��9@�b@��;@��F@��@�;d@�@�K�@�(�@�C�@�K�@���@�ȴ@�S�@���@� �@�b@���@��@�I�@�bN@�bN@�9X@�|�@��@��\@�n�@�v�@���@��@�&�@���@�  @��P@�|�@��@���@��\@�5?@��@�@��h@�X@�/@�V@��@��@��`@��`@��@�bN@��@��@�~�@��T@�x�@�O�@�G�@�&�@��@��j@�r�@���@��w@��P@�S�@�;d@�@��H@���@���@�v�@�E�@��T@��-@��@��`@��D@�bN@�A�@� �@�ƨ@�\)@�+@�
=@��y@�E�@�@��h@�hs@�G�@�V@��`@��D@� �@��m@���@�ƨ@��w@��@�dZ@�"�@���@���@�v�@�-@�@���@��@��D@���@�I�@��m@��;@��;@��
@�ƨ@���@�S�@�C�@���@�V@�@��7@���@���@�`B@�%@��j@�A�@� �@��w@���@��w@��w@���@��z@{�r@t��@j�h@b�@[��@T�.@I��@D-�@<�/@7o@0�@+o@'!-@ ی@x@��@&�@i�@
�@�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��jA��9A�
=A��DA��A���A�l�A���A�jA��+A�A�AÝ�A�1A�dZA�E�A�I�A��A��HA��A���A�K�A�bNA�%A¡�A�E�A�jA��
A��TA�Q�A�n�A���A�=qA���AĬAǇ+A��A�XA���A�S�A���A�
=A�ĜA�^5A�&�A�G�A�9XA��AÉ7AÃA��HAɋDA�(�A�oAʥ�A�I�A��A���A�ĜA̰!A��A��uA�$�A�Q�A�;dA��A��7A���A���A�M�A�  A�M�A�Q�A�XA��A�9XA�ȴA���A�|�A�{A�O�A�1'A�hsA���A�$�A���A�ffA�ƨA�v�A���A�O�AǁA��A���A���A��+A�ȴA�A�A��A��uA��/A�n�A�p�A���A���A���Aʟ�A�\)A�hsA�t�A�Q�A�^5A�r�A�=qA�\)A�G�A�^5A�ffAˣ�AÝ�A���A��A��uA�M�AēuA��`A�{A��-A���A�ĜA�&�A���A��A��hA���A��A���A�`BAɮA���A�K�A��/A�l�A���A��A���A��
A��FA��A�JA���A˓uA�  A�JAüjA��#A�ffA�A��#A�Q�Aǉ7A�&�A��A�jA� �A���A��A�~�A�v�A�v�A��wA���A�%A�v�A��/A��hA�A�A�M�A���AʸRA�$�A�l�A�A��A̓uA�C�A�|�A��A�ĜA�^5A�bNA�
=A̝�A���A�A�A˟�A̲-A�~�A�bNA�ffA�XA��A�ffA�1A�Q�A�"�A�t�A�`BA�bNA��A�  A�dZÁÁA�z�A�+A��A��A�VA�ffA�dZA�dZA�bNA�jA�jA�
=A�hsA�ffA�jA�l�A�jA�n�A�l�A�hsA�ffA�^5A�5?A�hsA�l�A�ffAͅÁÁA�v�A�jA�\)A�n�A�r�A�p�A�n�A�hsA�bNA�bNA�dZA�ffA�jA�hsA�p�A�ffA�bNA�XA�`BA�ffA�n�A�p�A�l�A�l�A�r�A�x�A�x�A�z�A�|�A�|�A�r�A�l�A�p�A�v�A�x�A�p�A�p�A�`BA�^5A�ffA�ffA�dZA�ffA�hsA�O�A�Q�A�\)A�bNA�bNA�hsA�r�A�z�A�jA�p�A�r�A�r�A�bNA�p�A�t�A͇+A�jA�I�A�I�A�bNA�`BA�`BA�\)A�XA�VA�ffA�Q�A�G�A�Q�A�M�A�?}A�VA�I�A�ffA�jA�bNA�XA�jA�bNA�^5A�ffA�hsA�l�A�p�A�r�A�t�A�dZA�bNA�bNA�jA�|�A�~�A�x�A�x�A�p�A�jA�z�A�Q�A�x�A�t�A�ffA�dZA�hsA�p�A�hsA�S�A�hsA�ffA�hsA�dZA�ffA�VA�^5A�ZA�Q�A�S�A�VA�hsA�\)A�bNA�ffA�jA�hsA�hsA�ZA�jA�ZA�bNA�\)A�v�A�E�A�;dA�S�A�Q�A�+A�(�A��A�JA�K�A�G�A�S�A�M�A�I�A�+A�G�A�?}A�G�A�E�A�O�A�A�A�I�A�;dA�K�A�;dA�(�A� �A�bA���A�$�A�VA�+A�S�A�M�A�r�A�-A�?}A�O�A�bNA�-ÁA�jA�l�A�S�A�p�A�jA�dZA�x�A�dZA�O�A�dZA�XA�S�A�^5A�dZA�1'A�9XA�S�A�1'A�^5A�XA�^5A�M�A�33A�G�A�S�A�x�A�|�A͉7A�p�A�jA�bNA�jA�r�A�jA�x�A͉7A�hsA�-A�bA�O�A�Q�A�l�A�bNA�`BA�S�A�A�A�ZA�jA�l�A�^5A�K�A�G�A�"�A�(�A�{A�"�A�S�A�XA�|�A�|�A�|�A�~�A�n�A�jA�bNA�^5A�\)A�XA�33A��A��;A�%A��;A̺^A̮A�ZA�-A��A��A���A�/A��A���A���Aʟ�A�E�A�7LA�-A�&�A��A�oA�
=A�A���A��A��A��A��A��mA��A��A��A��A��A��
A���A���A���A���A�A���AɶFAɰ!Aɰ!AɮAɥ�Aɡ�Aɟ�Aɝ�AɑhAɋDAɇ+A�~�A�|�A�z�A�x�A�x�A�x�A�v�A�t�A�v�A�r�A�r�A�r�A�t�A�r�A�p�A�n�A�p�A�n�A�p�A�r�A�x�A�l�A�hsA�jA�ffA�jA�ffA�dZA�dZA�ffA�bNA�^5A�^5A�`BA�`BA�^5A�bNA�ZA�XA�XA�ZA�ZA�S�A�S�A�S�A�VA�Q�A�Q�A�O�A�M�A�O�A�Q�A�K�A�I�A�K�A�K�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�E�A�G�A�G�A�E�A�C�A�E�A�C�A�E�A�C�A�A�A�A�A�=qA�=qA�=qA�;dA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�9XA�7LA�9XA�9XA�9XA�7LA�9XA�7LA�7LA�5?A�5?A�7LA�5?A�5?A�33A�33A�33A�5?A�1'A�/A�1'A�33A�1'A�1'A�1'A�1'A�/A�/A�/A�-A�-A�/A�/A�/A�-A�/A�-A�-A�-A�/A�-A�-A�+A�+A�-A�-A�+A�-A�+A�+A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�+A�+A�(�A�(�A�(�A�+A�(�A�(�A�(�A�+A�+A�(�A�+A�(�A�(�A�(�A�(�A�&�A�&�A�(�A�&�A�&�A�(�A�(�A�&�A�&�A�&�A�$�A�$�A�&�A�&�A�&�A�&�@���@���@��@���@���@��@��@��@��`@��`@���@���@���@�Ĝ@�Ĝ@��j@��9@��9@���@���@��u@�z�@�I�@�9X@�9X@�9X@�1'@�1'@�1'@�9X@�9X@�I�@�A�@�I�@�A�@�A�@�9X@�9X@�9X@�9X@�1'@�1'@� �@��@��@�b@���@��@��;@��;@��;@��
@���@�ƨ@�ƨ@�ƨ@��w@��F@��@��@��@���@���@���@���@���@���@���@���@���@���@���@��@��F@��@��@��F@��F@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@�ƨ@��w@��w@��w@��w@��w@��w@��w@�ƨ@�ƨ@��w@��w@�ƨ@��w@��w@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@��
@��
@��
@��
@��
A���A���A���A���A���A�ȴAɾwAɾwAɸRAɰ!AɮAɧ�Aɣ�Aɟ�Aɝ�Aɗ�AɓuAɇ+AɁA�|�A�z�A�v�A�t�A�v�A�t�A�t�A�r�A�p�A�n�A�p�A�p�A�p�A�n�A�l�A�l�A�l�A�hsA�hsA�ffA�ffA�bNA�`BA�`BA�^5A�\)A�\)A�\)A�ZA�ZA�ZA�XA�XA�XA�VA�VA�S�A�S�A�Q�A�Q�A�O�A�O�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�G�A�E�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�A�A�?}A�A�A�?}A�=qA�;dA�;dA�9XA�;dA�9XA�;dA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�5?A�5?A�5?A�5?A�33A�33A�1'A�33A�1'A�33A�1'A�/A�/A�/A�/A�1'A�/A�-A�-A�/A�/A�-A�-A�-A�-A�-A�-A�-A�+A�+A�+A�+A�+A�-A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�&�A�(�A�&�A�&�A�&�A�&�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�$�A�$�A�$�A�&�A�$�A�$�A�&�A�&�A�$�A�&�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A�"�A�"�@���@���@���@��@���@��@��`@��`@��@��@��/@���@���@���@���@��j@��9@��9@��@���@��u@��@�Z@�A�@�A�@�9X@�1'@�1'@�1'@�9X@�9X@�A�@�A�@�I�@�A�@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�(�@� �@��@��@�1@���@��m@��m@��;@��;@��
@���@�ƨ@�ƨ@��w@��w@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��@��F@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@�ƨ@��w@��w@��w@��w@��w@��w@�ƨ@��w@��w@��w@�ƨ@�ƨ@�ƨ@��w@�ƨ@��w@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@���@��
@��
@��
@��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A���Aɡ�A�p�A�\)A�K�A�C�A�=qA�5?A�/A�+A�(�A�&�A�$�A�"�A�"�A� �A� �A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A�oA��A�$�A��AȑhA��TAǬA��AĲ-A���A�XA��FA��A���A�%A�bNA�
=A�7LA��-A��/A��
A��A�JA�r�A���A��A�^5A�JA��A�hsA���A�VA�Q�A�A�M�A��A�bA�|�A�JA� �A�p�A�  A��A��A�+A���A��A�v�A�VA���A���A���A�XA�A��DA���A��A���A��A���A�jA�-A�VA���A��#A���A��hA���A�O�A��`A�M�A�M�A�S�A�ffA���A��
A��A��A�JA�t�A���A��RA�33A�oA���A��7A�p�A�Q�A���A}�hA{�-Axz�Av��Ar-An1Aj�AiAg��AgoAf��Ae�Ab�yA`��A_�A^�`A^E�A]dZA[AZ5?AX9XAU33AT�AS|�AQƨAO�AN{ALv�AI��AGAFM�AD��ADr�ACp�AA�TA@Q�A?;dA>bNA<��A;�TA;|�A: �A8�\A8�A8r�A6��A4�A3�A3S�A2��A0�DA0ZA/��A.�+A.VA-��A-p�A+�#A*��A*VA);dA(1A'G�A&��A%ƨA%�7A%G�A$�`A$-A"�uA (�A;dA��A�A��A��A�-AjA^5AI�A�A"�A��A(�A7LA�RAA�AƨA
=A1'A��A�A��A/AĜA^5A1A+A  AhsA
�`A
~�A	�FA	A��A�A1'A�/A��A��A��AO�A ��@��m@���@�V@���@�I�@�~�@�hs@���@���@��7@�O�@��T@��\@�C�@�ƨ@��@���@�C�@��@�1@�+@�p�@��@��-@�S�@� �@�p�@ޗ�@�hs@ܬ@��@��@���@��@�+@�V@љ�@�?}@�r�@�"�@ΰ!@�{@�@�$�@��@��T@�=q@�C�@ϕ�@��@�A�@�"�@��H@ʇ+@�~�@�^5@ɡ�@�o@�hs@ċD@��^@��7@��@�j@�"�@��@�b@��@��@���@�v�@�ff@�hs@���@��j@��F@�^5@��@�$�@��^@��7@��u@�ƨ@��@��H@���@�&�@���@��@�n�@��
@�+@�I�@�1@��
@��\@�X@�1'@��@��@��P@�l�@�X@�Ĝ@�ƨ@�|�@�n�@��h@�p�@�p�@�Ĝ@��@��D@�b@�5?@��@�`B@��9@�b@��;@��F@��@�;d@�@�K�@�(�@�C�@�K�@���@�ȴ@�S�@���@� �@�b@���@��@�I�@�bN@�bN@�9X@�|�@��@��\@�n�@�v�@���@��@�&�@���@�  @��P@�|�@��@���@��\@�5?@��@�@��h@�X@�/@�V@��@��@��`@��`@��@�bN@��@��@�~�@��T@�x�@�O�@�G�@�&�@��@��j@�r�@���@��w@��P@�S�@�;d@�@��H@���@���@�v�@�E�@��T@��-@��@��`@��D@�bN@�A�@� �@�ƨ@�\)@�+@�
=@��y@�E�@�@��h@�hs@�G�@�V@��`@��D@� �@��m@���@�ƨ@��w@��@�dZ@�"�@���@���@�v�@�-@�@���@��@��D@���@�I�@��m@��;@��;@��
@�ƨ@���@�S�@�C�@���@�V@�@��7@���@���@�`B@�%@��j@�A�@� �@��w@���@��w@��wG�O�@��z@{�r@t��@j�h@b�@[��@T�.@I��@D-�@<�/@7o@0�@+o@'!-@ ی@x@��@&�@i�@
�@�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��jA��9A�
=A��DA��A���A�l�A���A�jA��+A�A�AÝ�A�1A�dZA�E�A�I�A��A��HA��A���A�K�A�bNA�%A¡�A�E�A�jA��
A��TA�Q�A�n�A���A�=qA���AĬAǇ+A��A�XA���A�S�A���A�
=A�ĜA�^5A�&�A�G�A�9XA��AÉ7AÃA��HAɋDA�(�A�oAʥ�A�I�A��A���A�ĜA̰!A��A��uA�$�A�Q�A�;dA��A��7A���A���A�M�A�  A�M�A�Q�A�XA��A�9XA�ȴA���A�|�A�{A�O�A�1'A�hsA���A�$�A���A�ffA�ƨA�v�A���A�O�AǁA��A���A���A��+A�ȴA�A�A��A��uA��/A�n�A�p�A���A���A���Aʟ�A�\)A�hsA�t�A�Q�A�^5A�r�A�=qA�\)A�G�A�^5A�ffAˣ�AÝ�A���A��A��uA�M�AēuA��`A�{A��-A���A�ĜA�&�A���A��A��hA���A��A���A�`BAɮA���A�K�A��/A�l�A���A��A���A��
A��FA��A�JA���A˓uA�  A�JAüjA��#A�ffA�A��#A�Q�Aǉ7A�&�A��A�jA� �A���A��A�~�A�v�A�v�A��wA���A�%A�v�A��/A��hA�A�A�M�A���AʸRA�$�A�l�A�A��A̓uA�C�A�|�A��A�ĜA�^5A�bNA�
=A̝�A���A�A�A˟�A̲-A�~�A�bNA�ffA�XA��A�ffA�1A�Q�A�"�A�t�A�`BA�bNA��A�  A�dZÁÁA�z�A�+A��A��A�VA�ffA�dZA�dZA�bNA�jA�jA�
=A�hsA�ffA�jA�l�A�jA�n�A�l�A�hsA�ffA�^5A�5?A�hsA�l�A�ffAͅÁÁA�v�A�jA�\)A�n�A�r�A�p�A�n�A�hsA�bNA�bNA�dZA�ffA�jA�hsA�p�A�ffA�bNA�XA�`BA�ffA�n�A�p�A�l�A�l�A�r�A�x�A�x�A�z�A�|�A�|�A�r�A�l�A�p�A�v�A�x�A�p�A�p�A�`BA�^5A�ffA�ffA�dZA�ffA�hsA�O�A�Q�A�\)A�bNA�bNA�hsA�r�A�z�A�jA�p�A�r�A�r�A�bNA�p�A�t�A͇+A�jA�I�A�I�A�bNA�`BA�`BA�\)A�XA�VA�ffA�Q�A�G�A�Q�A�M�A�?}A�VA�I�A�ffA�jA�bNA�XA�jA�bNA�^5A�ffA�hsA�l�A�p�A�r�A�t�A�dZA�bNA�bNA�jA�|�A�~�A�x�A�x�A�p�A�jA�z�A�Q�A�x�A�t�A�ffA�dZA�hsA�p�A�hsA�S�A�hsA�ffA�hsA�dZA�ffA�VA�^5A�ZA�Q�A�S�A�VA�hsA�\)A�bNA�ffA�jA�hsA�hsA�ZA�jA�ZA�bNA�\)A�v�A�E�A�;dA�S�A�Q�A�+A�(�A��A�JA�K�A�G�A�S�A�M�A�I�A�+A�G�A�?}A�G�A�E�A�O�A�A�A�I�A�;dA�K�A�;dA�(�A� �A�bA���A�$�A�VA�+A�S�A�M�A�r�A�-A�?}A�O�A�bNA�-ÁA�jA�l�A�S�A�p�A�jA�dZA�x�A�dZA�O�A�dZA�XA�S�A�^5A�dZA�1'A�9XA�S�A�1'A�^5A�XA�^5A�M�A�33A�G�A�S�A�x�A�|�A͉7A�p�A�jA�bNA�jA�r�A�jA�x�A͉7A�hsA�-A�bA�O�A�Q�A�l�A�bNA�`BA�S�A�A�A�ZA�jA�l�A�^5A�K�A�G�A�"�A�(�A�{A�"�A�S�A�XA�|�A�|�A�|�A�~�A�n�A�jA�bNA�^5A�\)A�XA�33A��A��;A�%A��;A̺^A̮A�ZA�-A��A��A���A�/A��A���A���Aʟ�A�E�A�7LA�-A�&�A��A�oA�
=A�A���A��A��A��A��A��mA��A��A��A��A��A���A���A���A���A���A�ȴAɾwAɾwAɸRAɰ!AɮAɧ�Aɣ�Aɟ�Aɝ�Aɗ�AɓuAɇ+AɁA�|�A�z�A�v�A�t�A�v�A�t�A�t�A�r�A�p�A�n�A�p�A�p�A�p�A�n�A�l�A�l�A�l�A�hsA�hsA�ffA�ffA�bNA�`BA�`BA�^5A�\)A�\)A�\)A�ZA�ZA�ZA�XA�XA�XA�VA�VA�S�A�S�A�Q�A�Q�A�O�A�O�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�G�A�E�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�A�A�?}A�A�A�?}A�=qA�;dA�;dA�9XA�;dA�9XA�;dA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�5?A�5?A�5?A�5?A�33A�33A�1'A�33A�1'A�33A�1'A�/A�/A�/A�/A�1'A�/A�-A�-A�/A�/A�-A�-A�-A�-A�-A�-A�-A�+A�+A�+A�+A�+A�-A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�&�A�(�A�&�A�&�A�&�A�&�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�$�A�$�A�$�A�&�A�$�A�$�A�&�A�&�A�$�A�&�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A�"�A�"�@���@���@���@��@���@��@��`@��`@��@��@��/@���@���@���@���@��j@��9@��9@��@���@��u@��@�Z@�A�@�A�@�9X@�1'@�1'@�1'@�9X@�9X@�A�@�A�@�I�@�A�@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�(�@� �@��@��@�1@���@��m@��m@��;@��;@��
@���@�ƨ@�ƨ@��w@��w@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��@��F@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@�ƨ@��w@��w@��w@��w@��w@��w@�ƨ@��w@��w@��w@�ƨ@�ƨ@�ƨ@��w@�ƨ@��w@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@���@��
@��
@��
@��
A���A���A���A���A���A�ȴAɾwAɾwAɸRAɰ!AɮAɧ�Aɣ�Aɟ�Aɝ�Aɗ�AɓuAɇ+AɁA�|�A�z�A�v�A�t�A�v�A�t�A�t�A�r�A�p�A�n�A�p�A�p�A�p�A�n�A�l�A�l�A�l�A�hsA�hsA�ffA�ffA�bNA�`BA�`BA�^5A�\)A�\)A�\)A�ZA�ZA�ZA�XA�XA�XA�VA�VA�S�A�S�A�Q�A�Q�A�O�A�O�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�G�A�E�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�A�A�?}A�A�A�?}A�=qA�;dA�;dA�9XA�;dA�9XA�;dA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�5?A�5?A�5?A�5?A�33A�33A�1'A�33A�1'A�33A�1'A�/A�/A�/A�/A�1'A�/A�-A�-A�/A�/A�-A�-A�-A�-A�-A�-A�-A�+A�+A�+A�+A�+A�-A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�&�A�(�A�&�A�&�A�&�A�&�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�&�A�&�A�&�A�$�A�$�A�$�A�&�A�$�A�$�A�&�A�&�A�$�A�&�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A�"�A�"�@���@���@���@��@���@��@��`@��`@��@��@��/@���@���@���@���@��j@��9@��9@��@���@��u@��@�Z@�A�@�A�@�9X@�1'@�1'@�1'@�9X@�9X@�A�@�A�@�I�@�A�@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�(�@� �@��@��@�1@���@��m@��m@��;@��;@��
@���@�ƨ@�ƨ@��w@��w@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��@��F@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@�ƨ@��w@��w@��w@��w@��w@��w@�ƨ@��w@��w@��w@�ƨ@�ƨ@�ƨ@��w@�ƨ@��w@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@���@��
@��
@��
@��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=9�w=�/�=�.I=*:�=1ί=��=F��=V�S=y]O=���=�e�?��=��(=���>y�@i�V=��B=�$�?�4�=��<=��L>�@?K�=�x�=Ջ�>
�o@�p�=�ջ>#��@�m�@1$J>@�}�@+�I=�ә>$�f@�m�@���=C,�=F��=L%=r�!=��o=�{_>0�@�=jU=��=�;d?괷=��>la@���=���>ag@�k�=��5=쐂?տ�@�k�=^?�=vr=�E9=��=�P3?7Ŭ>�`�=���>V�@�p@�u�=�ȴ>e)5@�r�@�z=��x=��@q=��{> @�w\@�s=��3=Z�y=�2�=���=�Z=�+�>'g?��@�l@�q�=�� =g�=]�=��=!�=1P�=If{=VW�=U��=[-M=�Կ=���?Q�7=�v�>PB�@�o�@�g8>��@�Yu@�l?��#>�N@F�>��,>cV�@�pe@�r\=���>9��@Q��>��=��>_p?Ͼ#@�q7=�e�=�r�=���>*d�=Kf=��=�n�>��V=��P=�t�=���?�0j?�O>��@[��=.�2=4g=@��=},|=���>�=�QD?��=�n�>-l�@�y>=��n=�A=㷕=��=���>k�@+�[=��==���?#ߏ?��@�{�=���>�+?�R�@���@�*=u:?=�uy=�Ks>:�4=�M@=˒:>�xl?	2�>�M�?��>Gd�?^I�=��>W�?v�@�|�=�l�>N6�?�&@�zN=ג�?h �@EnY@1[W@���@��f@~">=�5>6�L@�w�@�sX@�x-@�w@�x�> {5?"f@�{ @�y�@�}�@�}k>)��@�DR@��I@��@�}V@�u@�{ @�c^@E�P@�w\@�sX@�vK@�z�@�|�@�|@S��@�|�@�{ @�zN@�{t@�y>@�x@�y>@�wp@�r�@�m@�j�@�t@�sX@�p�@�wp@�{�@�x�@�y>@�s�@�p�@�y�@�}A@�z�@�y�@�w@�u�@�m@�w�@�y�@�w@�z�@�y�@�vK@�s@�t�@�u�@�w�@�w@�t�@�y>@�y�@�{�@�{�@�{�@�w�@�{t@�y�@�t@�wp@�vK@�t�@�y)@�x-@�t~@�u�@�u�@�x-@�y)@�w�@�w\@�y>@�kQ@�m�@�u:@�t@�p�@�q�@�u�@�u:@�s@�rG@�j@�bx@�kf@�t~@�{t@�y�@�in@�in@�vK@�u�@�t�@�p�@�u:@�v�@�u�@�m�@�m�@�q"@�t�@�o�@�k�@�j@@�i�@�t�@�t@�x-@�x�@�v�@�w�@�x-@�u�@�y�@�zN@�~�@�}�@�{@�s@�`�@�m�@�f{@�zN@�zN@�|�@�|�@�w�@�q"@�pz@�nD@�wp@�s�@�wp@�t@�z�@�w�@�q�@�g8@�k�@�a(@�q�@�s�@�g�@�i�@�oT@�m�@�l�@�e�@�d�@�q�@�lv@�m�@�n�@�lv@�k�@�p�@�h�@�f�@�lv@�g�@�bx@�a(@�b�@�ek@�b9@�Z�@�RT@�Q/@�X�@�^@�j�@�n�@�j�@�m3@�k�@�i�@�i�@�f{@�c�@�g�@�e�@�a(@�`B@�a�@�_@�^@�[@�Q�@�Q/@�NQ@�T�@�Y�@�[W@�b@�s�@�|�@�i@�i@�f�@�l�@�q7@�p&@�p�@�rG@�f'@�q"@�p&@�nD@�nD@�j�@�i�@�g8@�_[@�c�@�m�@�e,@�dZ@�i�@�T�@�?�@�m�@�q�@�m�@�e,@�v�@�{�@�q7@�|1@��V@��<@�y�@�y�@�y�@�y�@�x�@�u�@�s@�q�@�f{@�g�@�U�@�j�@�nD@�t�@�q"@�i�@�e@�\�@�m3@�p�@�b�@�_�@�j@�_@�I{@�Wi@�A�@�`�@�e�@�kf@�nD@�nD@�n�@�t~@�`�@�VC@�Wi@�Dg@�G�@�I�@�8@��@� @��f@��@��@��]@��U@��)@���@��Y@�I@�5�@�=q@�M�@�2M@�@�~@��@�h@�@�	B@��	@��L@��@��@��<@��@��@��V@���@��k@���@��g@��@��g@���@��@��@��@��u@���@���@���@��z@��r@��r@��o@���@��@��I@��l@��@���@���@��}@��}@���@��@���@��@��X@��X@��@��m@��@��@��@���@���@���@��z@��&@�Ǐ@��~@��X@��@��H@�Ë@���@�Ë@���@�@��'@��;@��'@���@���@���@���@��Y@���@��]@���@���@��#@���@��f@���@��,@��,@��o@��E@���@���@��4@��4@���@��4@��^@��^@��4@��4@���@��4@���@���@���@��
@��
@���@���@���@���@��$@���@��g@���@���@���@���@��@���@���@��F@���@���@���@��F@���@���@���@��5@���@���@��5@���@��@���@���@��x@��x@���@��x@��x@��x@���@��@��@��$@��$@��9@���@��9@��9@���@��9@���@��@��@���@���@��9@��9@��9@��@���@��@��@��9@���@��}@��@��@���@��}@��S@��S@���@��}@���@��}@���@���@���@���@���@���@���@��9@��9@��9@���@��9@���@���@��$@��x@��9@��N@��N@��N@��N@���@��N@��}@��$@��$@��$@��N@���@��N@��N@��N@��$@���@���@��h@���@���@���@��N@��N@��_@P�m@P�@P��@P�"@P�&@P�&@P�+@P��@P��@P��@P�@@P��@P��@P�M@P��@P��@P�_@P�9@P�@P�@P�@P��@P��@P��@P��@P�/@P��@P݃@P�+@P��@P��@P�"@P�v@P�"@P��@P�z@P��@P�+@P�+@P�/@P�3@P��@Pڐ@Pپ@P�E@P�w@P�V@P�5@P�_@Pѷ@P�c@PБ@P�B@P�@P�F@P�@P�K@P�%@P�}@P�%@P��@P��@P��@P��@P�%@P�O@P�y@P�y@P�y@P��@P͟@P��@P��@P�@P��@Pл@Pҳ@P�[@PӮ@P�,@P�V@P��@P�V@P�,@P�V@P�,@P��@PԪ@PԀ@P�|@P�|@P��@P�w@P�w@P�$@P�w@P�w@P�s@P�@P�@P�s@P��@P�@P�o@P�o@P�o@P�@P�@P�@Pپ@Pپ@P�f@P�@P�]@P�@P�@P��@Pߤ@P��@P�H@��@��@�#@�f@�@�I@�Z@��@�
�@��@�@��@��@�P@���@��	@��@��'@��@���@��@��0@��@��o@��t@��_@���@��x@���@���@��@���@��@��>@��@��@��@��@��@���@��%@��@��m@���@��q@��G@��@���@��`@��6@��!@���@��z@��P@���@��~@��@���@��@��@��@���@��a@���@��z@��P@�޾@��@��@@���@�ݘ@��/@���@��@�ݘ@���@���@��@��@��@@��@@��j@��@��@@��@@��@��@���@���@��/@��D@��/@��H@���@��@���@���@�ں@�ڥ@��{@���@���@�پ@���@���@���@�٩@�ٔ@�٩@��U@��@@��@@��@��@���@�؄@��Z@��@���@��@�ײ@��4@��@��@��
@��4@��@���@�ֶ@���@���@�ֶ@�֌@��w@�֌@��9@�֌@�֡@��9@��@���@��@��$@�֡@��w@���@�ջ@�զ@�զ@��|@�զ@�ջ@�Ց@�Ց@���@���@���@��@��@���@��@���@��@��@��@��@��@��$@��9@��$@��M@��9@���@��@��$@��@��9@��M@��@��$@���@���@���@���@���@���@��@��@��9@���@�ջ@�Ց@��g@��|@�զ@�զ@�զ@�զ@Q�@Q�@Q@Qq@Q�@Q�@Q@Q�@Q�@Q�@QP@Q�@Q�@Q8@Qb@Q�@Q�@Q�@Q�@QZ@Q9@Q�@Q �@P�L@P�z@P��@P�+@P��@P��@P��@P��@P��@P�m@P�C@P��@P�m@P��@P�r@P��@P��@P�v@P�L@P��@P�U@P��@P��@P�f@P��@P�V@P�,@P�@P�Z@P�@P�@P�@P�l@P��@P��@P�O@P�%@P�O@P�y@P�@P�y@P�%@P�O@P�O@P�%@P��@P��@P��@P�y@P�@P��@P��@P�@P��@P�@P�@P��@P�V@P�@P�@P��@P�@P�@P�V@P��@P�@P�|@P��@P��@P�#@P�M@P�M@P�w@P�w@P�@P�@P�I@P��@P��@P�s@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�@@P��@P��@P��@P��@P�Y@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4444444444444443444444444434434434443344444444444444344344434444444443344334444433444444443344444444444444433433444443344344443444444444444443444444444434444444444434443344444444444444434443444433344333334433334333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@i�]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�p�G�O�G�O�@�m�G�O�G�O�@�}�G�O�G�O�G�O�@�m�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@�k�G�O�G�O�G�O�@�k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�p@�u�G�O�G�O�@�r�@�zG�O�G�O�G�O�G�O�G�O�@�wZ@�sG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�l@�q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�o�@�g:G�O�@�Yu@�lG�O�G�O�G�O�G�O�G�O�@�pd@�rXG�O�G�O�@Q��G�O�G�O�G�O�G�O�@�q3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@[��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�y=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�{�G�O�G�O�G�O�@���@�*G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�|�G�O�G�O�G�O�@�zPG�O�G�O�G�O�G�O�@���@��f@~";G�O�G�O�@�w�@�sW@�x2@�w
@�x�G�O�G�O�@�{#@�y�@�}�@�}lG�O�@�DW@��I@��@�}Z@�z@�{@�c_G�O�@�wZ@�sY@�vK@�z�@�|�@�|@S��@�|�@�{!@�zN@�{v@�y?@�x@�y=@�wr@�r�@�m@�j�@�t@�sW@�p�@�ws@�{�@�x�@�y<@�s�@�p�@�y�@�}A@�z�@�y�@�w@�u�@�m"@�w�@�y�@�w @�z�@�y�@�vF@�s@�t�@�u�@�w�@�w@�t�@�yF@�y�@�{�@�{�@�{�@�w�@�{v@�y�@�t@�ws@�vO@�t�@�y+@�x0@�t�@�u�@�u�@�x2@�y*@�w�@�wZ@�y@@�kR@�m�@�u@@�t@�p�@�q�@�u�@�u=@�s@�rD@�j�@�bx@�kf@�t�@�{s@�y�@�in@�iq@�vO@�u�@�t�@�p�@�u>@�v�@�u�@�m�@�m�@�q#@�t�@�o�@�k�@�jE@�i�@�t�@�t@�x-@�x�@�v�@�w�@�x.@�u�@�y�@�zK@�~�@�}�@�{@�s@�`�@�m�@�f|@�zQ@�zP@�|�@�|�@�w�@�q&@�p|@�nD@�wn@�s�@�wr@�t@�z�@�w�@�q�@�g8@�k�@�a*@�q�@�s�@�g�@�i�@�oV@�m�@�l�@�e�@�d�@�q�@�lu@�m�@�n�@�lv@�k�@�p�@�h�@�f�@�lw@�g�@�bz@�a)@�b�@�ek@�b;@�Z�@�RT@�Q1@�X�@�^@�j�@�n�@�k@�m2@�k�@�i�@�i�@�f@�c�@�g�@�e�@�a*@�`C@�a�@�_@�^@�[@�Q�@�Q0@�NP@�T�@�Y�@�[V@�b@�s�@�|�@�i@�i@�f�@�l�@�q6@�p&@�p�@�rE@�f&@�q@�p$@�nD@�nA@�j�@�i�@�g<@�_a@�c�@�m�@�e)@�dY@�i�@�T�@�?�@�m�@�q�@�m�@�e)@�v�@�{�@�q8@�|6@��T@��=@�y�@�y�@�y�@�y�@�x�@�u�@�s@�q�@�f�@�g�@�U�@�j�@�nD@�t�@�q!@�i�@�e@�\�@�m2@�p�@�b�@�_�@�j�@�_
@�Iz@�Wo@�A�@�`�@�e�@�ki@�nF@�nF@�n�@�t~@�`�@�VF@�Wf@�Dj@�G�@�I�@�8@��@� 
@��h@��@��@��Z@��Z@��)@���@��Z@�I@�5�@�=t@�M�@�2M@�@�}@��@�m@�@�	H@��@��J@��@��"@��<@��@��@��S@���@��k@���@��k@��@��k@��@��@�"@�j@�@�K@�Z@��@�
�@��@�@��@��@�O@���@��
@��@��)@��"@���@��@��.@��@��p@��r@��\@���@��y@��@���@��@���@��@��B@��@��@��@��@��@���@��$@��@��r@���@��p@��B@��!@���@��b@��<@��"@���@��|@��R@���@��@��@���@��@��@��@���@��c@���@��{@��R@�޾@�ހ@��@@���@�ݝ@��2@���@��@�ݘ@���@���@��@��@��@@��=@��j@��@��A@��@@��@���@���@���@��,@��E@��/@��L@���@��@���@���@�ں@�ڦ@��{@���@���@�پ@���@���@���@�٩@�ٕ@�٩@��V@��B@��B@��@��@���@�؇@��[@��@���@��@�״@��3@��@��$@��
@��3@��@���@�ֻ@���@���@�ֳ@�֌@��v@�֌@��8@�֎@�֠@��=@��@���@��@��!@�֢@��z@���@�ս@�լ@�է@��z@�ե@�պ@�Ւ@�Փ@���@���@���@��@��@���@��@���@��@��@��@��@��@��"@��8@��&@��M@��:@���@��@��'@��@��=@��P@��@��$@���@���@���@���@���@���@��@��@��7@���@�պ@�Փ@��n@��}@�ժ@�զ@�զ@�զ@Q�@Q�@Q
@Qr@Q�@Q�@Q@Q�@Q�@Q�@QR@Q�@Q�@Q6@Qb@Q�@Q�@Q�@Q�@Q[@Q;@Q�@Q �@P�J@P�x@P��@P�-@P��@P��@P��@P��@P��@P�m@P�B@P��@P�p@P��@P�v@P��@P��@P�u@P�J@P��@P�X@P��@P��@P�f@P��@P�X@P�-@P�@P�^@P�@P�@P�@P�k@P��@P��@P�R@P�"@P�R@P�z@P�@P�{@P�"@P�R@P�N@P�%@P��@P��@P��@P�z@P�@P��@P��@P�@P��@P�@P�@P��@P�X@P�@P�}@P��@P�@P�@P�X@P��@P�@P�z@P��@P��@P�&@P�N@P�M@P�v@P�u@P�@P�@P�K@P��@P��@P�r@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�C@P��@P��@P��@P��@P�X@P��@P��@��@��@�"@�j@�@�K@�Z@��@�
�@��@�@��@��@�O@���@��
@��@��)@��"@���@��@��.@��@��p@��r@��\@���@��y@��@���@��@���@��@��B@��@��@��@��@��@���@��$@��@��r@���@��p@��B@��!@���@��b@��<@��"@���@��|@��R@���@��@��@���@��@��@��@���@��c@���@��{@��R@�޾@�ހ@��@@���@�ݝ@��2@���@��@�ݘ@���@���@��@��@��@@��=@��j@��@��A@��@@��@���@���@���@��,@��E@��/@��L@���@��@���@���@�ں@�ڦ@��{@���@���@�پ@���@���@���@�٩@�ٕ@�٩@��V@��B@��B@��@��@���@�؇@��[@��@���@��@�״@��3@��@��$@��
@��3@��@���@�ֻ@���@���@�ֳ@�֌@��v@�֌@��8@�֎@�֠@��=@��@���@��@��!@�֢@��z@���@�ս@�լ@�է@��z@�ե@�պ@�Ւ@�Փ@���@���@���@��@��@���@��@���@��@��@��@��@��@��"@��8@��&@��M@��:@���@��@��'@��@��=@��P@��@��$@���@���@���@���@���@���@��@��@��7@���@�պ@�Փ@��n@��}@�ժ@�զ@�զ@�զ@Q�@Q�@Q
@Qr@Q�@Q�@Q@Q�@Q�@Q�@QR@Q�@Q�@Q6@Qb@Q�@Q�@Q�@Q�@Q[@Q;@Q�@Q �@P�J@P�x@P��@P�-@P��@P��@P��@P��@P��@P�m@P�B@P��@P�p@P��@P�v@P��@P��@P�u@P�J@P��@P�X@P��@P��@P�f@P��@P�X@P�-@P�@P�^@P�@P�@P�@P�k@P��@P��@P�R@P�"@P�R@P�z@P�@P�{@P�"@P�R@P�N@P�%@P��@P��@P��@P�z@P�@P��@P��@P�@P��@P�@P�@P��@P�X@P�@P�}@P��@P�@P�@P�X@P��@P�@P�z@P��@P��@P�&@P�N@P�M@P�v@P�u@P�@P�@P�K@P��@P��@P�r@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�C@P��@P��@P��@P��@P�X@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4444444444444443444444444434434434443344444444444444344344434444444443344334444433444444443344444444444444433433444443344344443444444444444443444444444434444444444434443344444444444444434443444433344333334433334333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��9��9�=9��9�E9��9�9��9��9��9�P9�E9�b9��9�A9��9�9�9�9ߚ9�w9�9ܐ9�U9�j9�V9��9ۄ9�9�9۔9� 9ڰ9�e9��9�T9��9�g9��9�39֗9՚9�9ԉ9�9��9��9Ӓ9�9��9��9ҩ9�J9�#9��9�`9�9��9Ћ9�9�-9��9΁9�9ͪ9̈́9��9��9̇9�)9��9ˎ9�)9�N9��9�9�9�_9�_9̇9̄9̮9�`9̈9̇9�N9�K9�+9�9ˈ9˟9ˋ9ʹ9Ɇ9ɗ9�[9�p9�F9�39�9ȕ9ȃ9�]9Ȉ9�s9ȃ9�J9�79�J9��9��9��9Ǳ9Ǳ9ǉ9�>9�9��9Ƹ9��9�{9�9��9��9��9�9��9ŧ9ŕ9ţ9��9ō9�i9�U9�i9�9�k9�|9� 9��9��9��9�9�~9�Y9��9Ī9Ě9Ė9�l9Ĕ9ħ9Ă9ă9ĺ9Ļ9ļ9��9��9��9��9ĺ9��9��9��9��9��9�9�9�9�/9�9Ĺ9��9�9��9� 9�29��9�	9��9ļ9��9��9��9��9��9��9�9��9ħ9ă9�a9�o9Ę9ĕ9ĕ9ĕ8�78�8�8�88�4�8�7�8�5�8�4|8�4�8�4�8�58�2�8�1-8�/Z8�/
8�/38�,�8�,8�+8�*8�'�8�%�8�#�8��8��8�.8��8��8��8��8�r8��8�g8��8��8�8��8�q8�8�A8��8�8��8�}8�#8��8�y8�~8��8��8��8�I8��8�`8�
j8�	�8�	Y8��8��8�|8�P8�|8��8��8��8�P8�|8�y8�S8�,8�8�,8��8��8�
�8�
�8�
�8�	�8�T8�I8�p8��8�.8�8�W8�8�.8��8�}8�8��8�h8�C8��8��8��8��8��8�8�u8��8�8��8��8�68�68�:8�:8��8� 8��8�8��8�q8��8��8��8��8�78��8��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B`BBaHBaHBaHBaHBbNBbNBbNBaHBaHBaHBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBaHBbNBbNBbNBbNBbNBbNBbNBbNBcTBdZBr�B�PBB�B��B�NB�qB��Bt�B]/B"�B\B\BBB%B��B�B�B�mB�B�B�fB�)B�#B�BB�yB�B�HB�
B�)B�mB�yB�fB�B�B�B��B�FB�B�LB�FB�!B��B��B��B�{B�bB�Bv�Bl�Bk�B]/B?}B,B#�B!�B�BhBJB1BB��B�B��B�dB��B��B��B�=Bt�B_;BK�B;dB+B�BbBB
��B
�NB
��B
ĜB
�B
��B
��B
��B
�\B
s�B
S�B
C�B
)�B
�B	��B	�#B	ĜB	�dB	�3B	�!B	�B	��B	�hB	�B	|�B	x�B	s�B	m�B	e`B	`BB	VB	I�B	H�B	B�B	8RB	0!B	&�B	 �B	�B	\B	+B��B��B��B�B�yB�mB�sB�BB�B�5B�B��B��B�)B�B��B��B��B��BÖBÖBÖBB��B��B��B�XB�?B�-B��B��B��B��B��B��B��B��B��B��B�PB�1B�B� Bv�Br�Bo�Bm�Bn�Bn�Bl�Bk�BjBgmBdZBbNBaHB^5B[#BXBVBS�BP�BN�BM�BL�BJ�BG�BD�BC�BB�B@�B>wB;dB9XB7LB33B0!B.B,B+B)�B(�B'�B'�B'�B(�B'�B-B5?BA�BJ�BZBo�B�B�VB��B��B��B��B��B��B��B��B��B�7Br�B]/BO�BI�BG�BH�BH�B@�B:^B:^B?}BA�BB�BE�BF�BI�BM�BO�BQ�BS�BZB`BBe`Bm�Bx�B}�B{�By�B�B�DB�JB�DB�=B�=B�7B�1B�1B�JB�PB�VB�\B�VB�{B��B��B��B��B��B��B��B��B�B�B�-B�9B�LB�XB�RB�RB�RB�dB�qB�dB��B��B��B�qB�FB�LBĜBȴBɺBɺBȴBƨB��B��B�B�/B�/B�BB�;B�mB�B�B�B�B��B��B��B	B	B	+B	1B	
=B	VB	bB	hB	uB	�B	�B	�B	%�B	)�B	+B	+B	,B	1'B	6FB	9XB	;dB	<jB	@�B	B�B	E�B	F�B	F�B	F�B	F�B	E�B	F�B	H�B	L�B	L�B	L�B	L�B	M�B	N�B	R�B	T�B	VB	W
B	W
B	XB	[#B	[#B	]/B	^5B	_;B	bNB	cTB	dZB	ffB	k�B	r�B	u�B	u�B	w�B	z�B	� B	�B	�B	�B	�B	�%B	�1B	�DB	�PB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�FB	�LB	�XB	�^B	�dB	�jB	�}B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�NB	�NB	�HB	�NB	�NB	�HB	�TB	�`B	�ZB	�TB	�NB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�B
�B
�B
1B
#TB
'�B
-]B
4�B
=VB
CGB
F�B
J#B
Q�B
TaB
Z�B
\�B
cnB
g�B
kkB
pB
shG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>g�v>��
?#�>R�n>\l>��X>u��>���>���>�ҿ>�U�A ��>���>��?7��A���>�yh?��@�%>�r>���?"�A�f�>ԍ�?��?,kjB�Y?g?LH|BBhA��?ES�B�0A�Yy?��?OõBYA���>s^1>w!O>~�>�?>�1>��(?$��Ak}^>��>��>��A/�<>�4�? ebB
<?�~?1�^BC�>�?�|A��Bt>��:>�'>�p�?�?	�h@��?���?י?�iQBFRBi�?�$?���BD�B�	>��H>���Ae'�>���?D��B	�B��?	+�>���>���>�{>�n>���?$g�@�*�BA�B�>��y>:�>4(N>:a>G��>[��>y�>��>>��>�`�>��/>�!B@� �?�?��zB@oB#p?C��B�GB<B@���?'�:A�S?Č�?��=B=B�%?�?i��A���@&�>�yp?*S�APjB]�>���>�A�>鰫?[;�>z�>�Կ>���?��->���>���?/<@�'R@?�?.��A�K�>[�>a��>prK>�f�>��?)F>�A	&�?Y�?X�}Bl�>��@? ��?!>㔖?
a?���A���>��r>��j@]�A8cBb�>��?5|�A��BE�A� >��>��n>�X??rr�>�ȣ? On?��@8 �@��@��^?|UC@���>��S?3�]@�YBV?'?�l�AJtBI]?O�@�߱A�FA�JA�ֻA�b�A���?��?e�SBC�BESBrNBC/Biq?F5@Z��BAcBG�BJ�B-4?TiB�BA\B@�BA A�1�Bu�Bg�A�}�BC|B@�BC=BHBF�BF9A��BG�BF�BD�BD�BC�B@�BB�BB�B?B=4BKB?�B=dB=iB7�B=EB:~B>�B>�BAaBBYBDBB�BBbBB\BC�B;�BD�BE�BA�BE�BA�BBiBABF�BD.BC�B@B=(BB�BCgBB�B@iB@VB;�B>sB=B;�BAB>zB:�B=�B@=B<�BD.BEYBD,BEBD�BCtBDcBABB�BEoBB B>�B=�B=bB9�B=�B:�B2�B+�B:B<�BA�B8�B5BA�BM�BCnBC�B?�BE�BHhBH�B:�BBWBI�BIBE�BG�B=�BB"BA B>�BE�BJBADBEbBGXBB%BD�BC�BFRBDzBAGB@DB0�B;�B2\B=nB<�BAkBAXB?�B<(B5.BCB<cB:�BC�BA>BE�B?�B=�B;�B7�B/B=SB@�B5B=_B?DB?�BA�B:SB8�B=KB=mB<KB;�B7�B7�B<�B:�B2�B>9B6�B4;B(�B=BBC�B7&B1#B8xB8.BS�BOBBBGQB?@BC�BC�BM�BB�BCB=SBA�B;�B=XB9`B@HB7tB<�BACB<BA�BG�B=B.B@�B7BI�BCuBL�BEmB<�B;YBTB2�B;�B<jB:�B9�B;AB;�B4B8�B?�B5jB2�B8YB=�B3�BF�BH~B*�B%9B=�BC�B=�B<;BV�BSmBD�B@�BD�B@�BA�BC�BG$BC�B?�B@�B8UB1B3'BKKBE�B@�BCB>�B?UB9�B9�B9�B>�B;�B.B0�BA�B9B3�B=�B1�BH�B:iB>B2dB2dB2�B7VB+�B#}B'�B�B�B$B/BB'�B�0B�]B�B�BB�B&BB�B��B��B{B#�B�B�B7B5�B/B1B8!B2�B4�B5:B;LB8�B7B3�B4�B8�B=B;�B9@B8�B98B:]B9 B5VB8wB7CB:�B6�B9)B;�B9$B9�B9ZB;LB=3B;[B8�B:�B=%B=�B=B=�B>�B>$B=�B>�B>�B>!B?[B?�B?KB>vB?:B>�B?�B>�B>OB=-B<�B9GB<�B><B<�B=�B<RB=�B=�B=TB<B=�B?7B>�B=�B<�B=�B;�B>GB>�B>AB=B<�B>�B>RB=�B<IB=�B=B=�B>�B=B;�B>+B?nB>B>0B?�B?�B?�B?�B?pB?�B?	B?�B?B?B?^B@B?EB@	B>�B?AB?�B?(B?�B?qB?hB@�B?cB@'B>�B?_B>�B?(B?�B@B@�B?�B?2B?wB@;B?B@"B?�B@CB@B>�B?�B@B@sB@kB@cB>�B@�BAyB@�B?�B@�B@@B@�B@�B@�BADB@�BA�BA�B@�B@�BABA�B@�BA�BA/BAuB@�BA�BABA�BAB@IB@�BA]B@aBA&BA~BB BAxBA"BA`BAWBAOBA>BA"BA-BA	BAuBB9BA\BABBBA�BBMBABB(BA�BA�BABABA�B@�BA�B@�BAvBAfBB*BBHBA�BB/BB'BARBABA�BA�BABB�BB?BAjBA�BA�BB�B	�7B	�4B	�DB	�dB	ɎB	˔B	��B	�9B	ʄB	�B	�B	ʞB	��B	��B	��B	ȶB	��B	��B	�sB	�bB	�WB	��B	�B	�vB	�iB	��B	˙B	��B	�hB	˸B	�dB	�sB	˧B	�LB	�B	ʺB	�BB	ʬB	ʟB	��B	�B	ɿB	��B	�EB	�B	��B	�MB	��B	��B	�]B	�B	�nB	�|B	�`B	̬B	̀B	��B	�B	͂B	��B	͘B	κB	ΎB	ΓB	ΤB	ΩB	κB	ϽB	ΓB	��B	�CB	��B	�rB	�sB	�KB	�B	�rB	��B	��B	�5B	�GB	��B	�B	��B	��B	��B	�KB	�B	��B	�sB	�XB	�xB	��B	��B	БB	еB	ШB	�FB	��B	ЖB	�B	�AB	�eB	ЅB	�{B	�mB	оB	бB	ФB	�B	��B	�XB	��B	үB	��B	њB	�8B	ҦB	��B	ӲB`�B`�B`BBaB`�B`�B`�B`SB`�BaOB`hB`�BabBa�B`�Ba�BaRBa�BaBa�Ba(Ba[Ba�Ba|BaXBa<Ba�Ba�Bb6BaaBa�Ba=Ba�Bb"Ba~BaBbBa�Ba�BaBbBa�Ba8Ba�Ba�Ba�BaqBa�BavBaGBa�Ba�BaCBa�BaoBa�BaiBa�Ba�Ba�BaBbJBa�BanBa�Ba�Ba�Ba�Ba>Ba�Bb#Ba�BbDBa�Ba�BbBbBaBawBa�Ba�Ba�BbBb/Bb'Ba�Ba�BbqBa�Ba�Bb�Ba�Ba�Ba Ba�Ba�Bb�Ba�Bb:Ba5Ba�Bb6Ba�BbBbBbBa�Ba�Ba�BaZBbBa�Ba�Ba�BbEBa�Bb�BaaBbBaPBa�BbBa�Ba�Ba�BaBa�Bb7BbBaYBapBa�Ba�Ba�Ba�Ba[Ba�Ba�BbBa�Ba�Ba�Ba�Ba`Ba�Bb:BbBa�Ba�Ba�Ba�Ba�B`�Ba�Ba�Ba�Ba�Ba�BaBa�Ba�BbDBa�BbeBb\BbKBbCBa�Ba�Bb5BbTBb8Ba�Ba�BbBaBa�BbBa�Bb�BbRBb.BalBb1Bb)BaKBaiBb.BawBa�Ba�Ba�Bb%Bb'BbFBb=Bb5Bb$B	�B	�B	�<B	�B	��B	�[B	�JB	�B	�nB	�B	�B	�jB	��B	�B	�B	��B	�9B	�fB	�B	��B	�?B	�B	�B	�xB	��B	�kB	��B	�]B	�oB	��B	�B	�CB	�\B	� B	�SB	�(B	�B	�XB	�B	��B	�{B	�BB	��B	��B	�RB	�nB	��B	��B	�B	�`B	�B	�B	�7B	�B	�DB	�B	�7B	�IB	�cB	�7B	�;B	�MB	�B	�6B	��B	��B	��B	�B	�B	�bB	�fB	�B	�B	�B	�B	�B	�B	��B	�9B	�>B	�B	�B	�B	�B	�]B	�nB	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�AB	�BB	�B	�XB	�0B	�nB	�dB	�GB	�,B	�B	�B	�B	�B	�jB	�B	��B	��B	�jB	�6B	�=B	�B	�rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444444444443444444444434434434443344444444444444344344434444444443344334444433444444443344444444444444433433444443344344443444444444444443444444444434444444444434443344444444444444434443444433344333334433334333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   B`0Ba6Ba7Ba6Ba6Bb=Bb?Bb?Ba7Ba5Ba5Bb=BbABb>Bb>Bb<Bb>Bb<Bb>Bb>Bb>BbABb>Bb<Ba7Bb>Bb>Bb<Bb>Bb>Bb<Bb>Bb>BcFBdKBr�B�>BB��B��B�<B�^B��Bt�B]B"�BMBHBB
BB��B�B�yB�\B�B�B�QB�B�B�1B�eB�pB�9B��B�B�\B�eB�UB�B�B��B��B�5B��B�:B�2B�B��B��B�mB�jB�MB�Bv�BlvBkqB]B?fB+�B#�B!�B�BTB7BBB��B�kB��B�QB��B��B�sB�(Bt�B_%BK�B;PB*�B�BLBB
��B
�:B
��B
ĆB
��B
��B
��B
��B
�HB
s�B
S�B
C�B
)�B
�B	��B	�B	ćB	�PB	�B	�B	��B	��B	�UB	�B	|�B	x�B	s�B	m}B	eKB	`-B	U�B	I�B	H�B	BwB	8=B	0B	&�B	 �B	�B	FB	B��B��B��B�B�eB�UB�]B�*B�B�B�B��B��B�B�B��BͽBͻB��BÀB�}B�~B�xB�lB�lB�nB�CB�&B�B��B��B��B��B��B��B��B��B��B�gB�9B�B�B�Bv�Br�Bo�Bm{Bn�Bn�BlrBknBjfBgVBdDBb7Ba1B^B[
BW�BU�BS�BP�BN�BM�BL�BJ�BG�BD�BC~BByB@jB>aB;KB9@B72B3B0B-�B+�B*�B)�B(�B'�B'�B'�B(�B'�B,�B5(BAqBJ�BZBo�B��B�=B�mB��B��B��B��B��B��B��B�pB�Br�B]BO�BI�BG�BH�BH�B@fB:DB:CB?cBArBBvBE�BF�BI�BM�BO�BQ�BS�BZB`*BeIBmyBx�B}�B{�By�B��B�)B�0B�)B�"B�#B�B�B�B�/B�6B�?B�AB�<B�aB�B��B��B��B��B��B��B��B��B�B�B�B�2B�>B�9B�6B�9B�JB�XB�KB�oB�oB�iB�WB�)B�4BăBțBɠBɡBșBƑB˭B��B��B�B�B�'B�!B�QB�pB�qB�pB�uB��B��B��B	B	B	B	B	
%B	=B	GB	OB	YB	lB	{B	�B	%�B	)�B	*�B	*�B	+�B	1B	6-B	9=B	;JB	<NB	@iB	BuB	E�B	F�B	F�B	F�B	F�B	E�B	F�B	H�B	L�B	L�B	L�B	L�B	M�B	N�B	R�B	T�B	U�B	V�B	V�B	W�B	[	B	[B	]B	^B	_ B	b3B	c<B	d?B	fNB	kjB	r�B	u�B	u�B	w�B	z�B	�B	��B	��B	��B	��B	�B	�B	�*B	�5B	�BB	�UB	�\B	�gB	�lB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�,B	�1B	�>B	�CB	�HB	�PB	�bB	�tB	ăB	ňB	ňB	ňB	ƍB	ȗB	̲B	͸B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�5B	�5B	�-B	�5B	�3B	�/B	�8B	�FB	�AB	�:B	�3B	�-B	�3B	�9B	�AB	�FB	�GG�O�B	��B
�B
rB
B
#9B
'�B
-CB
4�B
==B
C.B
FrB
J	B
Q�B
TFB
Z�B
\�B
cUB
g�B
kSB
o�B
sNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�HG�O�G�O�BBVG�O�G�O�B�!G�O�G�O�G�O�BX�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
*G�O�G�O�BC�G�O�G�O�G�O�BdG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BF>BioG�O�G�O�BD�B��G�O�G�O�G�O�G�O�G�O�B	��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BA�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B@eB#`G�O�B�8B<1G�O�G�O�G�O�G�O�G�O�B=B�G�O�G�O�A���G�O�G�O�G�O�G�O�B]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bl�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbvG�O�G�O�G�O�BEuA� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BU�G�O�G�O�G�O�BIMG�O�G�O�G�O�G�O�A�֞A�b~A��G�O�G�O�BC�BEBBr@BCBi^G�O�G�O�BASBG�BJ�B-"G�O�BvBAHB@�B@�A�1�Bu�Bg�G�O�BCiB@�BC,BG�BF�BF&A��BG�BF�BD�BD�BCB@�BB�BB�B>�B=%BKB?�B=SB=WB7�B=5B:oB>�B>~BAPBBKBDBB�BBUBBJBC�B;�BD�BEyBA�BE�BA�BBUB@�BF�BDBC�B@B=BB�BCYBB�B@WB@GB;�B>cB<�B;�BAB>mB:�B=�B@/B<�BDBEJBDBEBD�BCbBDUBABB�BEcBA�B>�B=�B=UB9�B=�B:�B2�B+zB:B<�BA�B8�B4�BA�BM~BC^BCtB?�BEpBHXBHtB:�BBGBI�BH�BE�BG�B=�BBBAB>�BE�BJ BA6BESBGHBBBD�BC�BF?BDkBA8B@4B0nB;�B2JB=^B<�BAZBAFB?�B<B5BB�B<QB:�BCxBA,BE�B?�B=�B;�B7�B/B=AB@�B5B=QB?5B?�BA�B:BB8�B=<B=\B<:B;�B7�B7�B<�B:�B2�B>*B6�B4,B(�B=0BC�B7B1B8hB8BS�BOBBBGAB?4BC�BC�BM�BB�BB�B=BBA�B;�B=GB9QB@7B7bB<�BA/B<BA�BG�B=	B.oB@�B7BI�BCgBL�BE`B<�B;KBS�B2zB;�B<XB:�B9�B;/B;�B3�B8�B?�B5[B2�B8IB=yB3xBF�BHnB*�B%$B=�BC�B=�B<'BV�BS\BD�B@�BD�B@�BA�BC�BGBC�B?�B@�B8CB0�B3BK;BE�B@�BB�B>�B?DB9qB9�B9zB>�B;�B.B0�BA�B8�B3uB=�B1�BH�B:ZB=�B2TB2TB2�B7FB+�B#oB'�B�B�BBB�B'�B�!B�OB�B�B�B�B&1B�B��B�uBmB#�B�B�B7B5�B.�B1B8B2qB4�B5*B;?B8�B7B3�B4�B8�B<�B;�B91B8�B9+B`�B`�B`0BaB`�B`�B`�B`BB`�Ba@B`VB`�BaTBaqB`�Ba�BaDBaxBaBavBaBaIBa|BakBaGBa*Ba�Ba�Bb*BaPBa�Ba0Ba�BbBanB`�Ba�Ba�Ba�BaBbBa�Ba+BamBa�Ba�BacBa�BaeBa8Ba�Ba�Ba2Ba�Ba^Ba�BaZBa�Ba�Ba�B`�Bb:Ba�Ba\Ba�BauBa�BaoBa-Ba�BbBa�Bb5Ba�Ba�Bb	Ba�BamBaeBa{BarBa�Bb	BbBbBa�Ba�BbcBawBa�Bb�Ba�Ba�BaBa�Ba�BbuBawBb)Ba$BauBb$Ba�BbBa�Ba�Ba�Ba�Ba�BaIBa�Ba�Ba�Ba�Bb5Ba�BbqBaNBbBa@Ba�Ba�Ba�Ba�Ba�BaBa�Bb)BbBaGBa_Ba�Ba�Ba�Ba�BaIBa�Ba�Ba�Ba�Ba�Ba�Ba�BaNBa�Bb+Ba�Ba�Ba�Ba�Ba�Ba�B`�Ba�Ba�Ba�Ba�Ba�B`�Ba�Ba�Bb1Ba�BbVBbKBb=Bb1BanBazBb&BbBBb(Ba�Ba�Ba�Ba Ba�Ba�Ba�Bb�BbABbBa[Bb#BbBa=BaYBb BadBa�Ba�BaxBbBbBb7Bb+Bb#BbB	�B	�B	�!B	�B	�B	�?B	�.B	�pB	�SB	�eB	�B	�QB	��B	�B	�B	�B	�B	�JB	�xB	�B	�%B	�kB	�B	�]B	�B	�PB	�B	�AB	�SB	��B	�B	�(B	�BB	�B	�7B	�B	�B	�?B	�B	��B	�`B	�%B	��B	�B	�8B	�TB	��B	��B	�~B	�EB	��B	�B	�B	�B	�(B	�B	�B	�-B	�JB	�B	�"B	�3B	�B	�B	��B	��B	��B	�B	�sB	�HB	�KB	�B	� B	�B	�wB	�B	�B	�B	�B	�"B	�rB	�B	�iB	�B	�BB	�QB	��B	�gB	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�&B	�*B	�gB	�=B	�B	�SB	�IB	�.B	�B	�B	�B	�wB	�B	�OB	�B	��B	�B	�OB	�B	�!B	�B	�VB`�B`�B`0BaB`�B`�B`�B`BB`�Ba@B`VB`�BaTBaqB`�Ba�BaDBaxBaBavBaBaIBa|BakBaGBa*Ba�Ba�Bb*BaPBa�Ba0Ba�BbBanB`�Ba�Ba�Ba�BaBbBa�Ba+BamBa�Ba�BacBa�BaeBa8Ba�Ba�Ba2Ba�Ba^Ba�BaZBa�Ba�Ba�B`�Bb:Ba�Ba\Ba�BauBa�BaoBa-Ba�BbBa�Bb5Ba�Ba�Bb	Ba�BamBaeBa{BarBa�Bb	BbBbBa�Ba�BbcBawBa�Bb�Ba�Ba�BaBa�Ba�BbuBawBb)Ba$BauBb$Ba�BbBa�Ba�Ba�Ba�Ba�BaIBa�Ba�Ba�Ba�Bb5Ba�BbqBaNBbBa@Ba�Ba�Ba�Ba�Ba�BaBa�Bb)BbBaGBa_Ba�Ba�Ba�Ba�BaIBa�Ba�Ba�Ba�Ba�Ba�Ba�BaNBa�Bb+Ba�Ba�Ba�Ba�Ba�Ba�B`�Ba�Ba�Ba�Ba�Ba�B`�Ba�Ba�Bb1Ba�BbVBbKBb=Bb1BanBazBb&BbBBb(Ba�Ba�Ba�Ba Ba�Ba�Ba�Bb�BbABbBa[Bb#BbBa=BaYBb BadBa�Ba�BaxBbBbBb7Bb+Bb#BbB	�B	�B	�!B	�B	�B	�?B	�.B	�pB	�SB	�eB	�B	�QB	��B	�B	�B	�B	�B	�JB	�xB	�B	�%B	�kB	�B	�]B	�B	�PB	�B	�AB	�SB	��B	�B	�(B	�BB	�B	�7B	�B	�B	�?B	�B	��B	�`B	�%B	��B	�B	�8B	�TB	��B	��B	�~B	�EB	��B	�B	�B	�B	�(B	�B	�B	�-B	�JB	�B	�"B	�3B	�B	�B	��B	��B	��B	�B	�sB	�HB	�KB	�B	� B	�B	�wB	�B	�B	�B	�B	�"B	�rB	�B	�iB	�B	�BB	�QB	��B	�gB	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�&B	�*B	�gB	�=B	�B	�SB	�IB	�.B	�B	�B	�B	�wB	�B	�OB	�B	��B	�B	�OB	�B	�!B	�B	�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444444444443444444444434434434443344444444444444344344434444444443344334444433444444443344444444444444433433444443344344443444444444444443444444444434444444444434443344444444444444434443444433344333334433334333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647442020083116474420200831164744202008311647442020083116474420200831164744202008311647442020083116474420200831164744202008311647442020083116474420200831164744AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816402019021918164020190219181640    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816402019021918164020190219181640  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816402019021918164020190219181640  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647442020083116474420200831164744  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                