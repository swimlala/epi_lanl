CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  B   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:45Z creation      
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
resolution        =���   axis      Z        '  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '  u�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� %�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ' /�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ' V�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� }�   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ' ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ' ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ' ߘ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ' x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 7�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ' AX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � hp   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   i0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   u0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �TArgo profile    3.1 1.2 19500101000000  20190219181645  20200831164756  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               3   3   3AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @��`U3@��`U3@��`U3111 @���Q��@���Q��@���Q��@7����l�@7����l�@7����l��c'�z�H�c'�z�H�c'�z�H111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    3   3   3ADA BDA  DA BDA @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�3D�G�D��\D���D��D�\)D���D���D��RD�@ D���D��RD��D�G
Dڃ3D�qD���D�*�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����ͽ��;L�ͽ��;L�ͽ��;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L��    =��;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ��;L��    �L�;L��        �L�;L�ͽ��;L�ͽ��;L�ͽ��ͽ��;L�;L�;L�;L�;L�;L�;L��    �L�;L��        ���;L�ͽ��;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�ͽ��ͽ��;L�;L�;L�ͽ��;L�ͽ���        �L�;L�;L�ͽ���=���    �L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�ͽ��;L�ͽ��;L�;L�;L�;L�;L�;L��        �L�;L�ͽ��;L�;L�ͽ��ͽ��;L�;L�;L�ͽ��;L�;L�;L�ͽ��;L�;L��    ���;L�;L�;L�ͽ���    ���;L�;L��    �L�;L�ͽ���    �L�;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ���            ���ͽ��ͽ���                =���=��ͽ���    ����                =���        >L��    =���    =���=���    =���=���=���                ����=���                =��ͽ���=���    =���                        ����                =���=���=���=��ͽ���=���=���    =���        =���    =���                                    =���    =���>L��            =���=���=���=���=���    =���    =���                    =���=���=���=���=���=���=���    ����    =���=���        =��ͽ���            =���=���=���        ����    =���    ����    =���=��ͽ���=���                    =���            =���            =���    =��ͽ���=���                        =���    ����    =���=���    =���        ����=��ͽ���    =���=���=���    =���=���                        =���=���    =���    =���    =���=���    ����        ����    =���=���    =���=���    ����        =���        =���    =���=���=���    =���    =���=���    =���    =���=���=���=���    ����                =���=���    =���=���=���=���>L��>���>L��>���>���>���>���>���?   ?   ?   ?   ?   ?   ?333?333?333?333?fff?L��?L��?fff?�  ?�  ?�  ?���?���?���?���?�ff?�ff?�33?�33?�  ?�33?�  ?���?ٙ�?ٙ�?ٙ�?�ff?�33?�33@   @   @ff@ff@��@33@33@��@��@   @   @&ff@&ff@,��@333@9��@@  @@  @Fff@L��@S33@Y��@Y��@`  @fff@l��@s33@s33@y��@�  @�33@�ff@�ff@���@���@�  @�33@�ff@���@���@�  @�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@�  @�33@�ff@���A   A   A��A33AffA  A	��A33A��AffA  A��A33A��A  A��A33A��AffA   A!��A$��A&ffA(  A+33A,��A.ffA1��A333A4��A8  A9��A;33A>ffA@  AA��AD��AFffAH  AK33AL��ANffAQ��AS33AVffAX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Ak33AnffAp  As33At��AvffAx  A{33A|��A~ffA���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A���A���A�ffA�  A���A�ffA�33A���Ař�A�33A�  Aə�A�ffA�  A���A�ffA�33A���Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�33A�  Dq�Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��DrٚDr� Dr�fDr�3Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt�3@,��@333@9��@@  @@  @Fff@L��@S33@Y��@Y��@`  @fff@l��@s33@s33@y��@�  @�33@�ff@�ff@���@���@�  @�33@�ff@���@���@�  @�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@�  @�33@�ff@���A   A   A��A33AffA  A	��A33A��AffA  A��A33A��A  A��A33A��AffA   A!��A$��A&ffA(  A+33A,��A.ffA1��A333A4��A8  A9��A;33A>ffA@  AA��AD��AFffAH  AK33AL��ANffAQ��AS33AVffAX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Ak33AnffAp  As33At��AvffAx  A{33A|��A~ffA���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A���A���A�ffA�  A���A�ffA�33A���Ař�A�33A�  Aə�A�ffA�  A���A�ffA�33A���Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�33A�  Dq�Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��DrٚDr� Dr�fDr�3Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @7�@�=q@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A�\)A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dp�Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Dy�D�pD�I�D���D���D�
D�^fD���D��)D���D�B=D���D�ڏD�D�IGDڅpD࿮D��3D�,�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���������������������������������������������������������������������������������������������������������������������������������=�\)>.{������������������������������������=�\)����=�\)=�\)������������������������������=�\)����=�\)=�\)��������������������������������������������������������������������=�\)=�\)��������>.{=�\)����������������������������������������=�\)=�\)����������������������������������=�\)����������=�\)������=�\)������=�\)��������������������=�\)=�\)=�\)������=�\)=�\)=�\)=�\)>.{>.{��=�\)��=�\)=�\)=�\)=�\)>.{=�\)=�\)>�=q=�\)>.{=�\)>.{>.{=�\)>.{>.{>.{=�\)=�\)=�\)=�\)��>.{=�\)=�\)=�\)=�\)>.{��>.{=�\)>.{=�\)=�\)=�\)=�\)=�\)=�\)��=�\)=�\)=�\)=�\)>.{>.{>.{>.{��>.{>.{=�\)>.{=�\)=�\)>.{=�\)>.{=�\)=�\)=�\)=�\)=�\)=�\)=�\)=�\)=�\)>.{=�\)>.{>�=q=�\)=�\)=�\)>.{>.{>.{>.{>.{=�\)>.{=�\)>.{=�\)=�\)=�\)=�\)=�\)>.{>.{>.{>.{>.{>.{>.{=�\)��=�\)>.{>.{=�\)=�\)>.{��=�\)=�\)=�\)>.{>.{>.{=�\)=�\)��=�\)>.{=�\)��=�\)>.{>.{��>.{=�\)=�\)=�\)=�\)=�\)>.{=�\)=�\)=�\)>.{=�\)=�\)=�\)>.{=�\)>.{��>.{=�\)=�\)=�\)=�\)=�\)=�\)>.{=�\)��=�\)>.{>.{=�\)>.{=�\)=�\)��>.{��=�\)>.{>.{>.{=�\)>.{>.{=�\)=�\)=�\)=�\)=�\)=�\)>.{>.{=�\)>.{=�\)>.{=�\)>.{>.{=�\)��=�\)=�\)��=�\)>.{>.{=�\)>.{>.{=�\)��=�\)=�\)>.{=�\)=�\)>.{=�\)>.{>.{>.{=�\)>.{=�\)>.{>.{=�\)>.{=�\)>.{>.{>.{>.{=�\)��=�\)=�\)=�\)=�\)>.{>.{=�\)>.{>.{>.{>.{>�=q>�p�>�=q>�p�>�p�>�p�>�p�>�p�?�?�?�?�?�?�?E�?E�?E�?E�?xQ�?^�R?^�R?xQ�?���?���?���?�?��]?��]?��]?�\)?�\)?�(�?�(�?���?�(�?���?�?�]?�]?�]?�\)?�(�?�(�@z�@z�@
�G@
�G@G�@�@�@{@{@$z�@$z�@*�G@*�G@1G�@7�@>{@Dz�@Dz�@J�G@QG�@W�@^{@^{@dz�@j�G@qG�@w�@w�@~{@�=q@�p�@���@���@�
>@�
>@�=q@�p�@���@��@��@�=q@�=q@�p�@���@��@�
>@�=q@�p�@���@��@�
>@�=q@�p�@��@�
>@�=q@�p�@أ�@��@�
>@�=q@�p�@��@��@�
>@�=q@�p�@���@��A�A�A�RAQ�A�A	�A
�RAQ�A�A�A�A�RAQ�A�A�A�RAQ�A�A�A!�A"�RA%�A'�A)�A,Q�A-�A/�A2�RA4Q�A5�A9�A:�RA<Q�A?�AA�AB�RAE�AG�AI�ALQ�AM�AO�AR�RATQ�AW�AY�AZ�RA]�A_�Aa�AdQ�Ae�Ai�Aj�RAlQ�Ao�Aq�AtQ�Au�Aw�Ay�A|Q�A}�A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A�(�A�A��\A�(�A���A��\A�\)A�(�A�A�\)A�(�A���A��\A�\)A���A�A�\)A�(�A�Aȏ\A�(�A���Ȁ\A�\)A���A�A�\)A�(�A���Aԏ\A�\)A���A�A�\)A�(�A�A܏\DqDq${Dq*�Dq7�Dq>DqD{DqQHDqW�Dq^Dqd{DqqHDqw�Dq~Dq�{Dq�HDq��Dq�{Dq��Dq�HDq��Dq�{Dq��Dq�HDq�Dq�{Dq��Dq�HDq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrQHDrW�Dr^Drj�DrqHDrw�Dr~Dr�{Dr�HDr��Dr�Dr��Dr�HDr��Dr�{Dr��Dr�HDr�Dr�{Dr��Dr��Dr�Ds{Ds
�Ds�DsDs${Ds1HDs7�Ds>DsD{DsQHDsW�Ds^Dsj�DsqHDsw�Ds~Ds��Ds�HDs��Ds�{Ds��Ds�HDs��Ds�{Ds��Ds�HDs�Ds�{Ds��Ds��Ds�Dt{Dt
�Dt�DtDt${Dt1HDt7�Dt>DtJ�DtQHDtW�Dt^Dtj�DtqHDtw�Dt�{Dt��Dt�HDt��Dt�{Dt��Dt�HDt�Dt�{Dt��Dt�HDt�Dt�{Dt��Dt��@1G�@7�@>{@Dz�@Dz�@J�G@QG�@W�@^{@^{@dz�@j�G@qG�@w�@w�@~{@�=q@�p�@���@���@�
>@�
>@�=q@�p�@���@��@��@�=q@�=q@�p�@���@��@�
>@�=q@�p�@���@��@�
>@�=q@�p�@��@�
>@�=q@�p�@أ�@��@�
>@�=q@�p�@��@��@�
>@�=q@�p�@���@��A�A�A�RAQ�A�A	�A
�RAQ�A�A�A�A�RAQ�A�A�A�RAQ�A�A�A!�A"�RA%�A'�A)�A,Q�A-�A/�A2�RA4Q�A5�A9�A:�RA<Q�A?�AA�AB�RAE�AG�AI�ALQ�AM�AO�AR�RATQ�AW�AY�AZ�RA]�A_�Aa�AdQ�Ae�Ai�Aj�RAlQ�Ao�Aq�AtQ�Au�Aw�Ay�A|Q�A}�A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A�(�A�A��\A�(�A���A��\A�\)A�(�A�A�\)A�(�A���A��\A�\)A���A�A�\)A�(�A�Aȏ\A�(�A���Ȁ\A�\)A���A�A�\)A�(�A���Aԏ\A�\)A���A�A�\)A�(�A�A܏\DqDq${Dq*�Dq7�Dq>DqD{DqQHDqW�Dq^Dqd{DqqHDqw�Dq~Dq�{Dq�HDq��Dq�{Dq��Dq�HDq��Dq�{Dq��Dq�HDq�Dq�{Dq��Dq�HDq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrQHDrW�Dr^Drj�DrqHDrw�Dr~Dr�{Dr�HDr��Dr�Dr��Dr�HDr��Dr�{Dr��Dr�HDr�Dr�{Dr��Dr��Dr�Ds{Ds
�Ds�DsDs${Ds1HDs7�Ds>DsD{DsQHDsW�Ds^Dsj�DsqHDsw�Ds~Ds��Ds�HDs��Ds�{Ds��Ds�HDs��Ds�{Ds��Ds�HDs�Ds�{Ds��Ds��Ds�Dt{Dt
�Dt�DtDt${Dt1HDt7�Dt>DtJ�DtQHDtW�Dt^Dtj�DtqHDtw�Dt�{Dt��Dt�HDt��Dt�{Dt��Dt�HDt�Dt�{Dt��Dt�HDt�Dt�{Dt��Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�1'A�33A�G�A�E�A�I�A�I�A�K�A�M�A�O�A�O�A�O�A�Q�A�M�A�I�A�M�A�M�A�C�A���A���A��9A��A���A���A��DA��A��A�t�A�`BA�Q�A�7LA��A��
A�JA���A���A��+A��-A�VA�A�ĜA�jA��A�ȴA��TA�O�A��TA��A��PA�=qA���A�I�A�{A�ȴA���A���A�`BA�bNA��A��A�K�A�7LA��-A�p�A���A��#A��PA�ffA���A��A�  A��wA�ffA�~�A�1'A�VA���A�x�A� �A��^A�l�A��A��A�
=A�9XA��/A�`BA��jA���A��/A���A�p�A��A��wA�7LA�M�A�S�A�VA�XA��^A�O�A�bA�p�A�-A���A�dZA�=qA~�`A|��Ay7LAu7LAp�HAp�DAlȴAj=qAg�Ae%Ac�;Ab��AaS�A`1'A^��A\ffA[XAW�TAUVATZAS\)AR=qAP{AN��AN$�AM�ALZAK|�AJĜAH5?AF�AEt�AE�AD�ADJAC�^AB��AAG�A@�HA@JA>�/A<�9A:n�A:$�A9��A9x�A8ZA6�A4z�A3\)A2��A3%A2�\A2JA1/A0��A/��A.�A.bA-�hA,�+A+�^A+O�A*�A)`BA(bNA'C�A&�A&n�A$�DA#A#oA"n�A!�A �HA��A/A�A��AS�A�hA��A�FA��A&�A1AȴA\)A5?AoA�DA$�A�AI�AVA�wA
�jA	A	%A{A�A�A��A�A\)A��A��AQ�AJA�AA"�A �!@�l�@��`@�o@�{@���@�  @�\)@��\@��T@�l�@�J@�`B@�l�@@��@�V@�t�@�v�@��@柾@��@�/@�bN@�\@�G�@�r�@�33@�V@�@�r�@թ�@ԣ�@��@��@�O�@�@�Z@�~�@ɉ7@�V@�K�@ź^@ļj@�I�@Å@�ff@�v�@�?}@��u@�(�@�|�@���@�p�@��@�Ĝ@���@��D@��j@��@�\)@�&�@���@���@��m@�`B@�S�@��\@��w@���@�O�@���@��@�-@�C�@�J@�G�@�\)@���@��@��@��`@��;@��@���@�M�@�O�@�r�@���@���@��!@�@��h@�O�@��@��j@��@��w@�
=@���@���@�&�@���@�Ĝ@�Ĝ@���@���@��D@�A�@���@�33@�o@�7L@��u@��@�E�@�-@���@�@��@��/@��j@�I�@��@�1@�hs@���@��@�ȴ@���@��@���@�n�@�5?@�J@��@��T@���@�X@�Ĝ@��@�Q�@� �@���@��m@���@�t�@�K�@�K�@��@��R@���@�ff@�M�@�V@��\@�$�@��T@�O�@���@�Ĝ@��D@��D@�r�@�Q�@�b@��F@�|�@�C�@���@��\@�M�@��@��@�@��^@��7@��@��`@���@��@��@�bN@�  @���@�+@�o@�
=@��y@��+@�M�@�$�@��T@��7@�p�@�&�@��`@��/@���@���@��@�r�@�z�@�bN@�(�@��@���@�t�@�;d@��@��y@��@�ȴ@��!@�~�@�=q@��@�{@�@��#@�p�@�O�@��@��@���@�r�@�Z@��@���@��F@��@�t�@�33@���@���@�V@�-@��@���@�`B@�&�@��@��@�z�@�  @���@�K�@��@��@��+@�{@��^@�X@��@��j@��D@�A�@��@���@���@�l�@�\)@�S�@�33@��@��y@��R@���@��+@�ff@��M@xѷ@r4@j�@c��@\�f@U��@M��@H:�@?��@9��@4��@,M@(,=@"�}@O@ff@v�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�{A�"�A�p�A�%A��mA���A�&�A�9XA��A�A�A�{A��-A�Q�A��A���A�?}A�K�A�{A��7A�
=A��A�ffA�M�A�1A��A��RA�{A���A���A�t�A�&�A��7A�S�A�~�A�%A��DA���A���A���A���A��HA�x�A�bNA��A���A�E�A��`A�?}A�VA��yA�VA���A��A��A���A��A�&�A�7LA��A�(�A�=qA�G�A�(�A�ƨA�{A���A�bNA�A�?}A��A��A�JA��A�(�A�&�A�$�A�JA��/A���A��A��-A�C�A��HA�O�A�bA��A�VA��A��A��FA��hA��TA�oA�oA�`BA��A��A�C�A�M�A�S�A�7LA��HA��DA��!A�{A�I�A�A�&�A��A�x�A���A�oA��\A�jA� �A��RA��\A��wA�A�A�hsA���A�hsA��#A�^5A��A��A�A��#A���A��wA��HA�bA�A���A�(�A�A��-A�&�A���A�1A�$�A�+A�/A��TA��A�|�A���A�-A�33A���A�~�A��A���A��`A���A��\A� �A�=qA�VA��RA�E�A�1'A�/A��A�VA�A�bNA�l�A�n�A�1'A�/A���A�-A�/A�bA�hsA���A�(�A�=qA�/A��/A�x�A� �A���A�33A�(�A�%A�ȴA�$�A�&�A��yA�1A��A���A�+A� �A���A���A�$�A��`A���A� �A�&�A�r�A�VA� �A��uA�(�A��A��A��A��A�ƨA�$�A�(�A�$�A�"�A�=qA��A�"�A�"�A�&�A�$�A�"�A�$�A�$�A�"�A�"�A�"�A� �A�+A�$�A�&�A�+A�+A�"�A�$�A��A��A��A��A� �A�"�A� �A� �A�"�A�VA� �A�"�A�VA� �A�"�A�"�A��A�
=A��A��A��A�"�A��A� �A��A��A�{A��A��A��A��A��A��A��A��A��A� �A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A��A�"�A� �A��A�"�A�"�A�"�A��A� �A� �A�"�A��A��A�
=A� �A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A� �A��A��A��A�{A��A��A� �A� �A�"�A�$�A� �A� �A��A��yA� �A� �A�"�A��A�
=A�"�A�$�A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA�{A��A��A��A��A��A�{A�bA�%A��A��A��A��A��A� �A��A��A��A�bA�bA��A��A��A��A��A��A�"�A�$�A�$�A� �A�$�A�$�A� �A�"�A�"�A� �A��A� �A�oA��A��A� �A�&�A� �A� �A�"�A�"�A� �A�$�A�$�A� �A� �A� �A��A� �A� �A�$�A�$�A�"�A� �A�(�A�&�A�"�A� �A�"�A�&�A� �A�"�A��A�oA��A��A��A�(�A��A�"�A�oA�{A� �A�"�A�$�A�$�A�$�A�"�A�&�A�"�A� �A��A�"�A�$�A�"�A�&�A�&�A�$�A�&�A�(�A�+A�-A�-A�/A�/A�-A�-A�/A�/A�/A�/A�1'A�1'A�1'A�33A�33A�5?A�9XA�7LA�9XA�9XA�;dA�9XA�7LA�7LA�33A�9XA�?}A�C�A�C�A�C�A�C�A�E�A�A�A�?}A�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�=qA�=qA�?}A�?}A�;dA�9XA�33A�/A�1'A�-A�/A�/A�/A�1'A�-A�/A�-A�-A�-A�-A�-A�-A�-A�-A�+A�1'A�-A�/A�1'A�33A�-A�/A�-A�-A�-A�+A�-A�/A�33A�5?A�9XA�=qA�;dA�?}A�A�A�C�A�A�A�A�A�C�A�C�A�E�A�C�A�E�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�I�A�E�A�A�A�?}A�E�A�A�A�A�A�A�A�A�A�?}A�I�A�I�A�I�A�K�A�K�A�I�A�K�A�G�A�G�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�G�A�G�A�I�A�I�A�K�A�I�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�O�A�M�A�M�A�M�A�O�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�Q�A�O�A�O�A�Q�A�O�A�O�A�S�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�M�A�O�A�M�A�O�A�O�A�M�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�M�A�O�A�O�A�K�A�I�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�;d@�33@�;d@�33@�33@�33@�+@��@�o@�o@�o@�
=@�
=@�@���@���@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��@��y@��@��y@��@��y@��y@��y@��y@��y@��H@��@�ȴ@���@�ȴ@���@���@��R@���@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@���@���@���@���@��\@��+@��\@��\@��+@��\@��\@��+@��+@��+@��+@��\@��+@��\@��+@��+@��+@��+@�~�@�~�@�v�@�~�@�v�@�v�@�n�@�n�@�n�@�ff@�ff@�ff@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�V@�V@�V@�M�@�M�@�V@�M�A�;dA�;dA�;dA�?}A�A�A�A�A�?}A�7LA�33A�5?A�/A�/A�/A�1'A�-A�/A�-A�/A�/A�-A�-A�/A�-A�-A�-A�-A�-A�+A�-A�+A�1'A�1'A�33A�/A�+A�-A�+A�-A�-A�1'A�33A�5?A�;dA�?}A�=qA�A�A�C�A�?}A�A�A�C�A�A�A�C�A�E�A�E�A�E�A�E�A�I�A�I�A�K�A�I�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�E�A�I�A�A�A�A�A�A�A�C�A�?}A�A�A�A�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�Q�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�S�A�Q�A�M�A�K�@�K�@�K�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�33@�+@�"�@��@�o@�
=@�
=@�
=@�
=@���@���@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��@��@��@��@��y@��@��y@��@��y@��y@��H@��@���@�ȴ@�ȴ@���@���@��R@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@���@���@���@��\@��\@��\@��\@��\@��\@��\@��\@��+@��+@��\@��\@��\@��+@��+@��+@��+@��+@��+@�~�@�~�@�~�@�v�@�v�@�n�@�n�@�n�@�n�@�ff@�ff@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�V@�V@�M�@�V@�V@�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�;dA�1'A�33A�G�A�E�A�I�A�I�A�K�A�M�A�O�A�O�A�O�A�Q�A�M�A�I�A�M�A�M�A�C�A���A���A��9A��A���A���A��DA��A��A�t�A�`BA�Q�A�7LA��A��
A�JA���A���A��+A��-A�VA�A�ĜA�jA��A�ȴA��TA�O�A��TA��A��PA�=qA���A�I�A�{A�ȴA���A���A�`BA�bNA��A��A�K�A�7LA��-A�p�A���A��#A��PA�ffA���A��A�  A��wA�ffA�~�A�1'A�VA���A�x�A� �A��^A�l�A��A��A�
=A�9XA��/A�`BA��jA���A��/A���A�p�A��A��wA�7LA�M�A�S�A�VA�XA��^A�O�A�bA�p�A�-A���A�dZA�=qA~�`A|��Ay7LAu7LAp�HAp�DAlȴAj=qAg�Ae%Ac�;Ab��AaS�A`1'A^��A\ffA[XAW�TAUVATZAS\)AR=qAP{AN��AN$�AM�ALZAK|�AJĜAH5?AF�AEt�AE�AD�ADJAC�^AB��AAG�A@�HA@JA>�/A<�9A:n�A:$�A9��A9x�A8ZA6�A4z�A3\)A2��A3%A2�\A2JA1/A0��A/��A.�A.bA-�hA,�+A+�^A+O�A*�A)`BA(bNA'C�A&�A&n�A$�DA#A#oA"n�A!�A �HA��A/A�A��AS�A�hA��A�FA��A&�A1AȴA\)A5?AoA�DA$�A�AI�AVA�wA
�jA	A	%A{A�A�A��A�A\)A��A��AQ�AJA�AA"�A �!@�l�@��`@�o@�{@���@�  @�\)@��\@��T@�l�@�J@�`B@�l�@@��@�V@�t�@�v�@��@柾@��@�/@�bN@�\@�G�@�r�@�33@�V@�@�r�@թ�@ԣ�@��@��@�O�@�@�Z@�~�@ɉ7@�V@�K�@ź^@ļj@�I�@Å@�ff@�v�@�?}@��u@�(�@�|�@���@�p�@��@�Ĝ@���@��D@��j@��@�\)@�&�@���@���@��m@�`B@�S�@��\@��w@���@�O�@���@��@�-@�C�@�J@�G�@�\)@���@��@��@��`@��;@��@���@�M�@�O�@�r�@���@���@��!@�@��h@�O�@��@��j@��@��w@�
=@���@���@�&�@���@�Ĝ@�Ĝ@���@���@��D@�A�@���@�33@�o@�7L@��u@��@�E�@�-@���@�@��@��/@��j@�I�@��@�1@�hs@���@��@�ȴ@���@��@���@�n�@�5?@�J@��@��T@���@�X@�Ĝ@��@�Q�@� �@���@��m@���@�t�@�K�@�K�@��@��R@���@�ff@�M�@�V@��\@�$�@��T@�O�@���@�Ĝ@��D@��D@�r�@�Q�@�b@��F@�|�@�C�@���@��\@�M�@��@��@�@��^@��7@��@��`@���@��@��@�bN@�  @���@�+@�o@�
=@��y@��+@�M�@�$�@��T@��7@�p�@�&�@��`@��/@���@���@��@�r�@�z�@�bN@�(�@��@���@�t�@�;d@��@��y@��@�ȴ@��!@�~�@�=q@��@�{@�@��#@�p�@�O�@��@��@���@�r�@�Z@��@���@��F@��@�t�@�33@���@���@�V@�-@��@���@�`B@�&�@��@��@�z�@�  @���@�K�@��@��@��+@�{@��^@�X@��@��j@��D@�A�@��@���@���@�l�@�\)@�S�@�33@��@��y@��R@���@��+G�O�@��M@xѷ@r4@j�@c��@\�f@U��@M��@H:�@?��@9��@4��@,M@(,=@"�}@O@ff@v�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�{A�"�A�p�A�%A��mA���A�&�A�9XA��A�A�A�{A��-A�Q�A��A���A�?}A�K�A�{A��7A�
=A��A�ffA�M�A�1A��A��RA�{A���A���A�t�A�&�A��7A�S�A�~�A�%A��DA���A���A���A���A��HA�x�A�bNA��A���A�E�A��`A�?}A�VA��yA�VA���A��A��A���A��A�&�A�7LA��A�(�A�=qA�G�A�(�A�ƨA�{A���A�bNA�A�?}A��A��A�JA��A�(�A�&�A�$�A�JA��/A���A��A��-A�C�A��HA�O�A�bA��A�VA��A��A��FA��hA��TA�oA�oA�`BA��A��A�C�A�M�A�S�A�7LA��HA��DA��!A�{A�I�A�A�&�A��A�x�A���A�oA��\A�jA� �A��RA��\A��wA�A�A�hsA���A�hsA��#A�^5A��A��A�A��#A���A��wA��HA�bA�A���A�(�A�A��-A�&�A���A�1A�$�A�+A�/A��TA��A�|�A���A�-A�33A���A�~�A��A���A��`A���A��\A� �A�=qA�VA��RA�E�A�1'A�/A��A�VA�A�bNA�l�A�n�A�1'A�/A���A�-A�/A�bA�hsA���A�(�A�=qA�/A��/A�x�A� �A���A�33A�(�A�%A�ȴA�$�A�&�A��yA�1A��A���A�+A� �A���A���A�$�A��`A���A� �A�&�A�r�A�VA� �A��uA�(�A��A��A��A��A�ƨA�$�A�(�A�$�A�"�A�=qA��A�"�A�"�A�&�A�$�A�"�A�$�A�$�A�"�A�"�A�"�A� �A�+A�$�A�&�A�+A�+A�"�A�$�A��A��A��A��A� �A�"�A� �A� �A�"�A�VA� �A�"�A�VA� �A�"�A�"�A��A�
=A��A��A��A�"�A��A� �A��A��A�{A��A��A��A��A��A��A��A��A��A� �A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A��A�"�A� �A��A�"�A�"�A�"�A��A� �A� �A�"�A��A��A�
=A� �A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A� �A��A��A��A�{A��A��A� �A� �A�"�A�$�A� �A� �A��A��yA� �A� �A�"�A��A�
=A�"�A�$�A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA�{A��A��A��A��A��A�{A�bA�%A��A��A��A��A��A� �A��A��A��A�bA�bA��A��A��A��A��A��A�"�A�$�A�$�A� �A�$�A�$�A� �A�"�A�"�A� �A��A� �A�oA��A��A� �A�&�A� �A� �A�"�A�"�A� �A�$�A�$�A� �A� �A� �A��A� �A� �A�$�A�$�A�"�A� �A�(�A�&�A�"�A� �A�"�A�&�A� �A�"�A��A�oA��A��A��A�(�A��A�"�A�oA�{A� �A�"�A�$�A�$�A�$�A�"�A�&�A�"�A� �A��A�"�A�$�A�"�A�&�A�&�A�$�A�&�A�(�A�+A�-A�-A�/A�/A�-A�-A�/A�/A�/A�/A�1'A�1'A�1'A�33A�33A�5?A�9XA�7LA�9XA�9XA�;dA�9XA�7LA�7LA�33A�9XA�?}A�C�A�C�A�C�A�C�A�E�A�A�A�?}A�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�?}A�A�A�A�A�?}A�7LA�33A�5?A�/A�/A�/A�1'A�-A�/A�-A�/A�/A�-A�-A�/A�-A�-A�-A�-A�-A�+A�-A�+A�1'A�1'A�33A�/A�+A�-A�+A�-A�-A�1'A�33A�5?A�;dA�?}A�=qA�A�A�C�A�?}A�A�A�C�A�A�A�C�A�E�A�E�A�E�A�E�A�I�A�I�A�K�A�I�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�E�A�I�A�A�A�A�A�A�A�C�A�?}A�A�A�A�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�Q�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�S�A�Q�A�M�A�K�@�K�@�K�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�33@�+@�"�@��@�o@�
=@�
=@�
=@�
=@���@���@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��@��@��@��@��y@��@��y@��@��y@��y@��H@��@���@�ȴ@�ȴ@���@���@��R@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@���@���@���@��\@��\@��\@��\@��\@��\@��\@��\@��+@��+@��\@��\@��\@��+@��+@��+@��+@��+@��+@�~�@�~�@�~�@�v�@�v�@�n�@�n�@�n�@�n�@�ff@�ff@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�V@�V@�M�@�V@�V@�M�A�;dA�;dA�;dA�?}A�A�A�A�A�?}A�7LA�33A�5?A�/A�/A�/A�1'A�-A�/A�-A�/A�/A�-A�-A�/A�-A�-A�-A�-A�-A�+A�-A�+A�1'A�1'A�33A�/A�+A�-A�+A�-A�-A�1'A�33A�5?A�;dA�?}A�=qA�A�A�C�A�?}A�A�A�C�A�A�A�C�A�E�A�E�A�E�A�E�A�I�A�I�A�K�A�I�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�E�A�I�A�A�A�A�A�A�A�C�A�?}A�A�A�A�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�Q�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�S�A�Q�A�M�A�K�@�K�@�K�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�33@�+@�"�@��@�o@�
=@�
=@�
=@�
=@���@���@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��@��@��@��@��y@��@��y@��@��y@��y@��H@��@���@�ȴ@�ȴ@���@���@��R@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@��!@���@���@���@��\@��\@��\@��\@��\@��\@��\@��\@��+@��+@��\@��\@��\@��+@��+@��+@��+@��+@��+@�~�@�~�@�~�@�v�@�v�@�n�@�n�@�n�@�n�@�ff@�ff@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�V@�V@�M�@�V@�V@�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@8�V?��0?Iu�@L?&"�@�0=���=���>%�~@�x?���=��W=��@'�>��x=͔>�3H@�e�=��>}Y@�4=���?+;=�=�>(�?�<�=Կ
>'G�?�l"?t|�@�Q�=��>Li/@�y�=��?I��=ߏG>�'�>	�>
�|@Mz�=���=��'=� �?�">@�2M=f�;=kJ=���>���=�x�?���>)��=�nn=�B1=��?�H=�C�=���?,�>��=�~?<��@�7�@�>->x�@�r=鈐?i�>X¤@�2>�9@�]>��>�?�@�9m=�J�=��=��>��@5+@\2#?� @�@�>��?�T@�e@�>��?Q��?!t�@�$�@ɰ>}Q/@�2�?=��=Ѭ�=��/=�Y�>m=�=Ԁ>	�@J��@�>->y@WM�@�=2@�=G>V�?�ؙ@���=�x->M��@�3H@�<K=��=��3>g�@��{=��X>x@?���>=�>���@9m3=�N�>t�@�Z�=�$><�@"��@�E�>;�D@7��@�=�=�1�=��=Ӯh>�b@�GE>2��@�?�@�>�@Cc^=�W >��>X�!@�1Q@�A�@�F_>�3?g��=��s?�t�=�&>;L�@�@:?)�A=�?�=�)>�JM?���>q�@@�>�>)�Q@�B�>�?���>	2@�E�@�A�@��!>'�@���@�FJ>�@�>X��@�FJ?���>AT�=�N�@'Y�@�E�>��>+F5@�EN@�EN>6�@�A�@�CW@CW?=��?�`�>Տq@�@�@�@�?�rG?W�@�D=@�B1?M�@�D�@�E�@ot�@	�^@�C�@5�@�D�@�B�@�m�@�C�?9�F@N�@�AJ@�A�@�D�@�D�@q��>��>@�C@�C�@�C�@�DR@�A�@�D=@�B�@�A�@�A�@�A�@�C@�CW@�C�@�C�@�E�@�C�@�B1@�C�@�B�@�@�@�@�@�A5@�A�@�A�@�A�@�A�@�BF@YSP@�BF@�B1@�B@�BF@�A�@�A�@�@�@�@�@�@�@�A�@�?�@�AJ@�@�@�@:@�@�@�A�@�?�@�?}@�?�@�@�@�>B@�?}@�@O@�?�@�@:@�A5@�A�@�>�@�<�@�?}@�?S@�?S@�@:@�?�@�>�@�@�@�?}@�>�@�@�@�AJ@�A5@�@�@�@�@�AJ@�@�@�@�@�@�@�A�@�A�@�@�@�AJ@�A�@�A�@�BF@�BF@�A�@�C-@�C@�CW@�A�@�@�@�?S@�>�@�>�@�?)@�?S@�@O@�@O@�?)@�?�@�@�@�?�@�?�@�@O@�@�@�@�@�@�@�?�@�?�@�A5@�@:@�@O@�>�@�A5@�@�@�C@�CW@�B�@�AJ@�@�@�AJ@�A�@�@�@�@�@�@�@�AJ@�@�@�A5@�B�@�A�@�?�@��@�@�@�?�@�?�@�@�@�AJ@�@�@�A�@�@�@�@O@�A5@�?S@�>B@�?)@�?)@�>�@�>�@�?)@�?)@�=�@�>�@�>�@�>�@�>@�>�@�=q@�>@�>@�=�@�>-@�>B@�>@�=�@�=q@�=G@�<�@�?}@�?�@�?�@�@O@�A5@�@�@�@�@�@�@�?)@�=G@�=�@�?S@�@�@�AJ@�?�@�AJ@�A�@�BF@�BF@�B�@�@�@�A�@�BF@�AJ@�A5@�>�@�CW@�B�@�B�@�BF@�@�@�AJ@�B�@�A�@�A�@�DR@�D=@�CW@�CW@�C�@�D=@�B�@�B�@�B�@�A�@�C�@�C�@�C�@�D=@�BF@�D�@�D�@�D�@�CW@�CW@�B1@�D=@�A�@�C�@�@:@�>�@�A�@�?�@�>�@�AJ@�BF@�A�@�@�@�AJ@�CW@�C@�C�@�CW@�B�@�CW@�C�@�BF@�A�@�A�@�B�@�C�@�DR@�C�@�C�@�CW@�EN@�F�@�F�@�GZ@�H@�H@�HA@�H@�HV@�HV@�HA@�H�@�H@�H�@�IR@�I�@�I�@�J�@�KI@�K�@�L�@�MU@�MU@�L�@�M@�L�@�L@�M@�O�@�P	@�P	@�P	@�Oa@�O�@�Oa@�MU@�MU@�M@�MU@�MU@�L�@�M@�M@�MU@�M@�M�@�NQ@�Nf@�N@�M�@�K^@�IR@�IR@�IR@�H�@�IR@�IR@�H�@�H�@�H�@�H�@�H�@�H�@�H�@�I�@�IR@�H�@�I@�H�@�I@�If@�J@�Jb@�Jb@�Jb@�I�@�J@�J@�J�@�Jb@�J�@�K
@�K
@�Ln@�M�@�Oa@�N�@�P�@�Q�@�Ri@�R@�R�@�Se@�S�@�Se@�T"@�S�@�T�@�Ta@�Ta@�T�@�Uq@�Uq@�U�@�U�@�U�@�U�@�U�@�Vm@�V@�VC@�U�@�U@�U�@�U�@�Uq@�T"@�S;@�S�@�T�@�S�@�T"@�Sz@�T"@�T7@�V�@�Vm@�V�@�W*@�Vm@�V�@�V�@�V�@�Vm@�Vm@�V�@�V�@�V�@�V�@�V�@�W*@�W*@�W*@�W~@�W�@�W�@�W�@�X%@�W�@�X%@�Xy@�X%@�X�@�XO@�X�@�X�@�X�@�Y6@�Y�@�Y`@�Y`@�Y`@�Y6@�Y�@�Y�@�Y�@�Z@�Z@�Z@�Z2@�Z�@�Z�@�[@�[@�Z�@�Z�@�[B@�[B@�[�@�[l@�[�@�[�@�[�@�[�@�[�@�[�@�[l@�[�@�[�@�\>@�\�@�\�@�\�@�\�@�\�@�]O@�]O@�]O@�]y@�]y@�]O@�]O@�]O@�]�@�]�@�]y@�]y@�]�@�]�@�]�@�]�@�]�@�]�@�^5@�^5@�^J@�^�@�^�@�^�@�^�@�_1@�^�@�_@�_[@�_[@�_[@�_1@�_�@�_�@�_@�`@�`@�`�@�`�@�`�@�`W@�`�@�`�@�`W@�_�@�`@�`W@�_@�`@P��@P�1@P�[@P�1@P��@P��@P��@P��@P�_@P�@P��@P�c@P�9@P�@P�h@P�l@P�p@P�O@P�%@P��@P��@P�)@P��@P��@P��@P��@P�2@P�@P��@P��@P��@P�6@P�@P��@P��@P��@P�@P�\@P��@P��@P��@P��@P��@P�\@P��@P��@P�\@P��@P�@P��@P��@P�H@P�H@P�H@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�Q@P�Q@P��@P��@P��@P��@P��@P�Y@P��@P��@P�
@P��@P�b@P��@P��@P��@P�
@P��@P�b@P�b@P�
@P�
@P�
@P�
@P��@P��@P�b@P��@P��@P��@P��@P�@P�@P�k@P�k@P�o@P�o@P�@P��@P��@P��@P��@P�@P�@P�@P�@P�@P��@P�@P��@P��@P��@P�x@P�x@P�$@P�x@P��@�r�@�r�@�r�@�t�@�ud@�t�@�t@�q�@�p&@�o�@�nD@�n�@�nD@�oi@�n@�n�@�m�@�n�@�n�@�n@�n@�o@�nY@�n@�n@�m�@�n@�m]@�n@�m�@�p@�o�@�qa@�oi@�m�@�n�@�n@�n�@�o@�pz@�q7@�q�@�t*@�v@�t @�v�@�w@�vu@�wG@�w�@�x@�w�@�xW@�x�@�x�@�y�@�y�@�z�@�z�@�z�@�z�@�{@�z�@�{5@�{J@�{J@�{5@�{J@�{�@�{J@�{5@�{5@�{5@�y�@�z�@�w�@�w�@�x@�y�@�w�@�x�@�x�@�|[@�{�@�{�@�{�@�{�@�{�@�{�@�{t@�{�@�{�@�{�@�{�@�{�@�|@�{�@�|@�|@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�}@�},@�}V@�}�@�}V@�},@�}�@�}�@�}�@�~g@�~(@�~=@�~(@�~(@�~=@�~g@�~�@�~�@�~�@�~�@�~�@�N@��@��@�N@�x@��@��@��@��@��@��I@��
@��I@��^@��I@��@��@��^@��@��Z@��o@��o@���@���@���@���@��o@��A@���@��,@��,@��@���@��A@���@��A@��,@��,@���@��,@��@��,@���@���@��k@���@���@���@���@��@��Q@��<@��<@��<@���@���@��Q@��{@��#@���@���@��@��b@��
@��b@��M@���@���@��b@��M@���@��Q@���@P�@P�@P�@P�@P�@P��@P֡@P֡@P�M@P�$@P��@P�R@P�|@P�|@P�R@P��@PӮ@P�_@P�h@P��@P�l@P�l@P�@PΚ@P�!@P��@P�O@P̣@P̣@P�O@P��@P˧@P�}@P�S@P�}@P��@P��@P�y@P̣@P�K@P�!@P�u@P�K@P�K@P�u@P�u@P͟@P͟@P�!@P�O@P��@Pȴ@P��@PǏ@PǏ@P��@P�?@P��@P�H@P�r@P�r@PĜ@P��@P�C@P�m@Pŗ@Pŗ@P�@PƓ@P�i@Pŗ@P�H@P�"@P��@P�P@P��@P�U@P�@P�@P�U@P�@P��@P�@P�U@P�@P��@P��@P�'@P�P@P��@P��@P��@P�+@P�@P��@P�Y@P�@P��@P��@P�
@P��@P�@P��@P�<@P��@P�@P��@P��@P��@P��@P��@P��@P��@P��@P�A@P��@P�o@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  444443444444444443443444444444434434444443444434444444444444444433434443444434444434344334443443444444433433344344334443444444443444344344443433344433344444434444443434443334334434444344334334444334433433343433334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�e�G�O�G�O�@�4G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Q�G�O�G�O�@�y�G�O�G�O�G�O�G�O�G�O�G�O�@Mz�G�O�G�O�G�O�G�O�@�2LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�7�@�>2G�O�@�qG�O�G�O�G�O�@�2G�O�G�O�G�O�G�O�@�9oG�O�G�O�G�O�G�O�G�O�@\2 G�O�@�@�G�O�G�O�@�e@�G�O�G�O�G�O�@�$�G�O�G�O�@�2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@J��@�>/G�O�@WM�@�=5@�=EG�O�G�O�@���G�O�G�O�@�3F@�<JG�O�G�O�G�O�@��zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Z�G�O�G�O�G�O�@�E�G�O�G�O�@�=�G�O�G�O�G�O�G�O�@�GEG�O�@�?�@�>�@Cc]G�O�G�O�G�O�@�1Q@�A�@�F`G�O�G�O�G�O�G�O�G�O�G�O�@�@;G�O�G�O�G�O�G�O�G�O�G�O�@�>�G�O�@�B�G�O�G�O�G�O�@�E�@�A�@�� G�O�@���@�FFG�O�G�O�@�FJG�O�G�O�G�O�G�O�@�E�G�O�G�O�@�EN@�EQG�O�@�A�@�CXG�O�G�O�G�O�G�O�@�@�@�@�G�O�G�O�@�D?@�B/G�O�@�D�@�E�@ot�G�O�@�C�G�O�@�D�@�B�@�m�@�C�G�O�@N�@�AG@�A�@�D�@�D�@q��G�O�@�C@�C�@�C�@�DT@�A�@�DA@�B�@�A�@�A�@�A�@�C@�CZ@�C�@�C�@�E�@�D@�B5@�C�@�B�@�@�@�@�@�A7@�A�@�A�@�A�@�A�@�BJ@YSP@�BC@�B5@�B	@�BF@�A�@�A�@�@�@�@�@�@�@�A�@�?�@�AM@�@�@�@>@�@�@�A�@�?�@�?~@�?�@�@�@�>A@�?|@�@Q@�?�@�@;@�A7@�A�@�>�@�<�@�?~@�?U@�?V@�@8@�?�@�>�@�@�@�?�@�>�@�@�@�AL@�A4@�@�@�@�@�AL@�@�@�@�@�@�@�A�@�A�@�@�@�AM@�A�@�A�@�BE@�BE@�A�@�C.@�C@�CZ@�A�@�@�@�?V@�>�@�>�@�?'@�?V@�@N@�@O@�?)@�?�@�@�@�?�@�?�@�@R@�@�@�@�@�@�@�?�@�?�@�A6@�@:@�@O@�? @�A:@�@�@�C@�CX@�B�@�AL@�@�@�AL@�A�@�@�@�@�@�@�@�AM@�@�@�A5@�B�@�A�@�?�@��@�@�@�?�@�?�@�@�@�AL@�@�@�A�@�@�@�@O@�A4@�?V@�>C@�?)@�?*@�?@�>�@�?&@�?)@�=�@�>�@�>�@�>�@�>@�?@�=u@�>@�>@�=�@�>+@�>D@�>@�=�@�=n@�=F@�<�@�?~@�?�@�?�@�@N@�A6@�@�@�@�@�@�@�?&@�=I@�=�@�?R@�@�@�AL@�?�@�AL@�A�@�BJ@�BC@�B�@�@�@�A�@�BC@�AL@�A:@�?@�CS@�B�@�B�@�BJ@�@�@�AM@�B�@�A�@�A�@�DT@�D>@�CV@�CX@�C�@�DA@�B�@�B�@�B�@�A�@�C�@�C�@�C�@�D=@�BJ@�D�@�D�@�D�@�CZ@�CZ@�B5@�D>@�A�@�C�@�@9@�>�@�A�@�?�@�>�@�AJ@�BE@�A�@�@�@�AJ@�CZ@�C@�C�@�CU@�B�@�CX@�C�@�BC@�A�@�A�@�B�@�D @�DQ@�C�@�C�@�CT@�EN@�F�@�F�@�G]@�H@�H@�HA@�H@�HX@�HZ@�HG@�H�@�H@�H�@�IR@�I�@�I�@�J�@�KG@�K�@�L�@�MW@�MT@�L�@�M@�L�@�L@�M@�O�@�P@�P
@�P@�Of@�O�@�Ob@�MR@�MY@�M @�MT@�MT@�L�@�M@�M@�MR@�r�@�r�@�r�@�t�@�ud@�t�@�t@�q�@�p(@�o�@�nC@�n�@�nF@�om@�n@�n�@�m�@�n�@�n�@�m�@�n@�o@�nZ@�n@�n@�m�@�n@�mZ@�n@�m�@�p@�o�@�qe@�oj@�m�@�n�@�n
@�n�@�o@�p|@�q8@�q�@�t*@�v@�t@�v�@�w@�vu@�wF@�w�@�x@�w�@�xY@�x�@�x�@�y�@�y�@�z�@�z�@�z�@�z�@�{@�z�@�{6@�{L@�{M@�{6@�{M@�{�@�{F@�{3@�{6@�{4@�y�@�z�@�w�@�w�@�x@�y�@�w�@�x�@�x�@�|\@�{�@�{�@�{�@�{�@�{�@�{�@�{s@�{�@�{�@�{�@�{�@�{�@�|@�{�@�|@�|@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�}@�}.@�}V@�}�@�}W@�}.@�}�@�}�@�}�@�~i@�~&@�~<@�~*@�~*@�~;@�~e@�~�@�~�@�~�@�~�@�~�@�N@��@��@�P@�z@��@��@��@��"@��@��J@��
@��F@��\@��M@��"@��@��\@��@��Z@��p@��n@���@���@���@���@��n@��@@���@��*@��-@��@���@��@@���@��B@��-@��+@���@��-@��@��-@��@���@��m@���@���@���@���@��@��R@��>@��:@��<@���@���@��R@��z@��"@���@���@��@��`@��
@��`@��O@���@���@��e@��K@���@��U@���@P� @P�@P�"@P�@P�@P��@P֣@P֣@P�N@P�#@P��@P�S@P�z@P�z@P�P@P��@PӮ@P�c@P�j@PϽ@P�j@P�n@P�@PΝ@P�"@P��@P�N@P̣@P̣@P�J@P��@P˨@P˂@P�V@P˂@P��@P��@P�x@P̢@P�F@P�#@P�u@P�K@P�M@P�u@P�v@P͝@P͝@P�"@P�P@P��@Pȵ@P��@PǍ@Pǈ@P��@P�>@P��@P�F@P�n@P�r@PĞ@P��@P�B@P�m@Pŕ@Pœ@P�@Pƕ@P�j@PŚ@P�H@P�"@P��@P�P@P��@P�R@P��@P��@P�R@P�~@P��@P��@P�S@P�{@P��@P��@P�(@P�N@P��@P��@P� @P�&@P�@P��@P�Z@P�@P��@P��@P�@P��@P�@P��@P�>@P��@P�@P��@P��@P��@P��@P��@P��@P��@P��@P�@@P��@P�p@P��@P��@P��@�r�@�r�@�r�@�t�@�ud@�t�@�t@�q�@�p(@�o�@�nC@�n�@�nF@�om@�n@�n�@�m�@�n�@�n�@�m�@�n@�o@�nZ@�n@�n@�m�@�n@�mZ@�n@�m�@�p@�o�@�qe@�oj@�m�@�n�@�n
@�n�@�o@�p|@�q8@�q�@�t*@�v@�t@�v�@�w@�vu@�wF@�w�@�x@�w�@�xY@�x�@�x�@�y�@�y�@�z�@�z�@�z�@�z�@�{@�z�@�{6@�{L@�{M@�{6@�{M@�{�@�{F@�{3@�{6@�{4@�y�@�z�@�w�@�w�@�x@�y�@�w�@�x�@�x�@�|\@�{�@�{�@�{�@�{�@�{�@�{�@�{s@�{�@�{�@�{�@�{�@�{�@�|@�{�@�|@�|@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�}@�}.@�}V@�}�@�}W@�}.@�}�@�}�@�}�@�~i@�~&@�~<@�~*@�~*@�~;@�~e@�~�@�~�@�~�@�~�@�~�@�N@��@��@�P@�z@��@��@��@��"@��@��J@��
@��F@��\@��M@��"@��@��\@��@��Z@��p@��n@���@���@���@���@��n@��@@���@��*@��-@��@���@��@@���@��B@��-@��+@���@��-@��@��-@��@���@��m@���@���@���@���@��@��R@��>@��:@��<@���@���@��R@��z@��"@���@���@��@��`@��
@��`@��O@���@���@��e@��K@���@��U@���@P� @P�@P�"@P�@P�@P��@P֣@P֣@P�N@P�#@P��@P�S@P�z@P�z@P�P@P��@PӮ@P�c@P�j@PϽ@P�j@P�n@P�@PΝ@P�"@P��@P�N@P̣@P̣@P�J@P��@P˨@P˂@P�V@P˂@P��@P��@P�x@P̢@P�F@P�#@P�u@P�K@P�M@P�u@P�v@P͝@P͝@P�"@P�P@P��@Pȵ@P��@PǍ@Pǈ@P��@P�>@P��@P�F@P�n@P�r@PĞ@P��@P�B@P�m@Pŕ@Pœ@P�@Pƕ@P�j@PŚ@P�H@P�"@P��@P�P@P��@P�R@P��@P��@P�R@P�~@P��@P��@P�S@P�{@P��@P��@P�(@P�N@P��@P��@P� @P�&@P�@P��@P�Z@P�@P��@P��@P�@P��@P�@P��@P�>@P��@P�@P��@P��@P��@P��@P��@P��@P��@P��@P�@@P��@P�p@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  444443444444444443443444444444434434444443444434444444444444444433434443444434444434344334443443444444433433344344334443444444443444344344443433344433344444434444443434443334334434444344334334444334433433343433334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9-8�9-99-949-;9-;�9-;q9-:�9-8+9-6s9-6F9-4t9-4�9-4w9-5�9-4H9-4�9-3�9-4�9-4�9-4*9-4H9-5U9-4�9-4H9-419-3�9-4G9-3~9-4H9-3�9-6[9-5�9-7�9-5�9-3�9-4�9-489-59-5Q9-6�9-7�9-8E9-:�9-<�9-:�9-=�9-=�9-=9-=�9->y9->�9->t9-?9-?<9-?�9-@r9-@�9-A�9-A�9-Ad9-A�9-A�9-A�9-B9-B/9-B09-B9-B09-Bp9-B(9-B9-B9-B9-@r9-A�9->�9->�9->�9-@v9->69-?�9-?R9-CM9-Bo9-B�9-B�9-B�9-B�9-B�9-BX9-Bp9-Bp9-B�9-B�9-B�9-B�9-B�9-B�9-C	9-Cy9-Cv9-C�9-C�9-C�9-C�9-C�9-D9-D*9-DU9-D�9-DV9-D*9-D�9-D�9-E9-Ev9-E09-EG9-E49-E49-EF9-Er9-E�9-E�9-F9-E�9-F9-Fg9-F�9-F�9-Fj9-F�9-F�9-F�9-F�9-GG9-GC9-Gq9-G-9-Gm9-G�9-Gt9-GG9-G9-G�9-HO9-H�9-H�9-H�9-H�9-H�9-H�9-H�9-H�9-I�9-I�9-Ik9-In9-IB9-I+9-I�9-I�9-I�9-In9-Il9-I)9-In9-I@9-In9-I�9-I�9-I�9-I�9-J9-I�9-J9-J]9-J�9-J�9-J�9-J�9-K9-J�9-J�9-J�9-K}9-J�9-K9-Kg9-K�9-Lr9-K�9-K�9-L9-LA9-K�9-K�9-L9-J�9-I�8���8���8���8���8���8���8��t8��t8��8���8���8��8��;8��;8��8���8��W8���8���8��08���8���8��8��8��q8��8��8���8���8���8��8���8��8��8��8��8��=8��8���8��8��r8���8��8��8���8���8���8���8��q8��8��8���8���8��8��8���8��/8��8��8��G8��K8��y8���8��&8��S8��}8��{8��8��8��^8��8��8���8���8��8��8�� 8��18��18�� 8��.8��8��8��8��+8��8��8���8��	8��]8��8��8���8��8��S8���8��8��H8��#8��8��?8��8��78��8��08���8��08��.8��8��.8��X8��08��]8��8�ߩ8��Q8���8��Q8��Q8��QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B  B  B��B��B��B�B�B��B��B��B�5B��B��B��B��B�B�B�)B��BĜB�-B�B}�Br�B[#BW
BQ�B<jB,B�B�BɺB�!B�RBƨB�3B��B��B�DB�1B�DB�By�BaHBG�B/B �B�B�B\BDBB��B�B�yB�;B��BÖB��B��B�PBu�BhsBbNB[#BS�BM�BC�B0!B�B
��B
�;B
��B
ƨB
�9B
��B
��B
�{B
�B
k�B
K�B
7LB
�B	��B	�B	��B	�?B	��B	�hB	�B	|�B	t�B	k�B	e`B	]/B	Q�B	I�B	8RB	)�B	&�B	$�B	 �B	�B	�B	�B	{B	hB	VB		7B��B��B��B��B	B	JB	DB	+B	B��B��B�B�/B��B��B�#B�/B�B��B�qB�LB�LBĜBƨBŢBǮBĜB��B�^B�^B�LB�3B�B�B��B��B��B��B��B��B�hB�\B�JB�1B�B� B|�Bz�By�Bv�Bo�BjBiyBgmBdZBaHB^5BZBVBQ�BM�BK�BH�BE�BB�B>wB<jB;dB8RB6FB49B33B1'B0!B.B+B)�B)�B)�B(�B(�B'�B&�B$�B#�B"�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B�B!�B&�B&�B�B�B�B�B �B!�B�B�B�B�B�B�B�B�BuB{B�B�B�B�B�B �B"�B#�B#�B#�B&�B2-B2-B7LB;dB;dB7LB0!B7LBXB]/BS�BL�BJ�BM�BI�BJ�BJ�BM�B^5Be`Be`BdZBaHB_;BbNBe`Be`Be`BgmBgmBffBcTBaHBcTBgmBiyBl�Bo�Bq�Bu�Bv�Bx�B}�B�B�B�B�1B�PB�bB�oB��B��B��B��B�B�B�B�B�!B�LB�qB��BÖBɺB��B��B��B�
B�B�)B�B��B��B	  B	B	B	B		7B	PB	bB	oB	uB	{B	�B	�B	�B	�B	#�B	)�B	2-B	5?B	:^B	:^B	<jB	?}B	A�B	D�B	G�B	I�B	L�B	N�B	P�B	T�B	W
B	YB	[#B	`BB	e`B	gmB	jB	l�B	p�B	q�B	q�B	s�B	t�B	v�B	{�B	|�B	~�B	�B	�B	�+B	�7B	�=B	�JB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�3B	�9B	�FB	�FB	�LB	�LB	�LB	�^B	�dB	�jB	�qB	�}B	��B	��B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B
�B
^B
B
�B
"�B
+B
2�B
8�B
@�B
G_B
L�B
Q�B
T{B
YB
^B
abB
ffB
i�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���@�Ku@�XkAe>-@oi�B��>���>�Zy?_� A~�d@��>�<K?o?Az[&@4�_?�H?���B =?c�?�kYB'�>�2/@��?��?l�?`�A?�F?G^?^��@ű�@���B��?{�?��B/�?�@��H?�_@�?8_)?:�A���>�ߏ?or?$@���B�@>�D�>��>�+�@:�?XC@�6/?fv2>��.>�с?��@H'�>��j>��Y@�	�??$�&@���BǛB&?AeMB�?|�@��J?�)�B��?>y�Al�@&TT@,K8B�e>���>��X>�|�?3/4Ai�A���@X�*B�?<FtA(�zB��B�1?6R�@�Nb@j1�B-�A�9?���B�M@�K�?D�>�^�?(q??�*E?��?6�?A�ʵBͧ?;�A�7�B�:B��?;�eAu�A�XL?��?�	[B�!B�!>�"�? �?H?B!>�V ?��i@�e?@��?��A���?H|?L�kB��>�e0?>1�A���Bւ?}�A���B�>̵5>��?�W?��Bۀ?o7�B�&B�A�&?"q�?3\�?���B�>B��B�b?B�J@���?'�TA/J�?&��?{N�Bʺ@x�1>��?�R?�Ġ@��B?���B��?b{-Bة?.�6AH��?6|JBɨB�qB�`?LB(+B�9@��?�@B�BA �?�g�?9A���B�C?J�?e�B�WB�l?;�JBʩB�1A��x?�%@�sR@�)B�.B�A��@�DB��B�@��B�B�tA�9�AY�`B��A��/B˫B��B�9BѬ@���A��
B�B��B�7B� A˝@Y�B̒B�B��B��B�kB��B�EBˉB�kB�tB�]BɈB̔B��B˹B�+B˽B�B�uB�,B� B�=B�LB�.B�7B��B��A�e�B̦B˽B��B̯B�%B�B��BԂB�	B�cB�B��B�4BʠB�4BͬB�B�oB�lB�5B�rB�DB�B��B��B�=B��B��B˄B��B�iB��B��B̏B��B̸B�fB��B��B�[B�FB�4B�	B�[B̣B�4BʌB��B�cB�B�}B�nB�kB��B�OB�LB�}B̀B�zBͣB�B��B�xB�B��B�B��B˒B�B��B��B�7B�B�2B�BΕB̣BΛB�B�iB�tB˒B�sB��B��B�]BͯB�B� B�WB˩B��B��B�UB�`B��B�4B��B�#BʢB̏B	�$B� B��B̘B͌B̆BΞB��B�[B˒B�FB��B�B�B��B��B��B�B�'BʒB�LB��B�LB��B͞B�B�JB�~B�MB��BɌB̡B�oB��B�pB��B�LB��B�uB�;B�iB�WB�,B��B�B�gB��B��B̣B�[B��B�[B�B��B��B�EB�LB�QB��B˩B��BȣBͷBΠB��B�B�4B�&B�@BɅB��BΚBͺB��BͯB�CB��B��B��B�@B��B�HB�HB�%B��B��B�FB��B̫B��BͦB˽B�B��B�B�B��B͵B�lB�OB�`B�OB�wB��BЭBͦB̀B�%B��B�=B̿BˮB˷B�:B�B�YB̎B͢B˧B�-B��B��B�^B��B�[B��B�)B�TB��B�FB�iB�LB̱B�B�%B�nB̿B��B��B̥B�YB�!B��B��B�oB̌B�B�dB�B�WB��B�=B�4BʑB��BɴB�WB�"B�zB��B��B�BʓB�_B˰B�VB�B˶B��BʛB��B�gB��B�B˪B��B�mB�dB�
B��B̄B˦B�rB̺B�iB�`B��B̠B̫B�=B�nB�BB�|B��B�B�>B�B̄B�PB��B͏BάB� B�CB��B�KB�0B��BͣB��B̬B�}B��BͅB��B̟B�yB�B��B��B��B�=B��B��B�B�NB�1B�(B�B˹B�^B�~B��B�$B̒B́B�RBˬB�lB͉B�4B��B� B�tB�B��B�B��B�'B˛B��B�B��B̉B�NB�qB��B˼B˳B�AB˭B��B̬B̚B�B�NB�HB�eB�|B�"B�jB��B�PB̸B�^B̒B̀B�wB�4B�|B�KB�BB�0B��B�GB�5B�,B��B̌B˯B��B�B�kB̂B�yB�GB�6B�RB�uB��B̃B̣B̑B˴B�wB̿B˰B�+B�jB�8B��B̏B˩B�LB�:B˅B̮B̥B��B̳B��B˛B�^B�UB̕B��B�QB�HB��B̨B�sB�<B�B�!B̧B�rB̠B��B��B��B�B�nB��B��B�>B�aB�$B�B�]B�RB��B͈B̢B�(B�B�!B��B�B��B̛B��B�B�LB͝B�]B	�B	͖B	θB	�~B	�B	�&B	��B	��B	ΦB	�KB	�B	��B	͌B	�qB	��B	�B	�RB	��B	κB	�pB	�UB	��B	�tB	ΒB	ϴB	ψB	�NB	�B	��B	ϽB	ДB	�IB	�B	�mB	�qB	ЃB	ДB	зB	�B	��B	СB	ϢB	ЦB	�KB	ЋB	АB	�&B	ϽB	�B	�B	��B	�QB	�TB	�7B	вB	��B	�6B	�=B	�B	�B	�B	�(B	�B	�KB	�1B	�aB	�TB	фB	�jB	�fB	�sB	��B	��B	�iB	�vB	��B	�B	��B	��B	�B	��B	ҋB	�qB	��B	��B	ѧB	ҪB	�NB	�EB	��B	�*B	�dB	�ZB	�MB	��B	ѫB	�3B	�%B	�cB	�UB	�B	��B	ҶB	�B	�B	�B	�	B	��B	��B	��B	ҊB	ҺB	�bB	�fB	�9B	�B	�B	ӹB	��B	��B��B��B��B��B��B�nB�nB�~B��B�B�SB��B�BB��B��B��B�DB�B�fB�B�B��B��B�B��B�:B��B�B�zB�B�B�$B�B�B��B�B��B��B�B�B��B�SB�B�(B��B�!B�XB�jB�XB��B��B��B�B�B�B�B�AB�!B�/B��B��B�\B�B�sB�B�vB�YB�dB�B�RB�B��B��B�B�B��B��B�B�B�;B��B�TB��B��B��B�B��B��B�B�hB�tB�kB�WB�B�B�B�SB�B�B��B��B��B�+B�B�<B�B�3B�>B�UB�B�oB�	B�fB�B�B�9B��B��B��B��B��B��B�&B�)B�B�WB�nB�B��B��B�B�B��B��B��B� B�B�.B��B�B�B�$B��B�xB��B��B��B�B��B�B��B�B��B�B�B�B�`B�#B��B��B�?B�jB��B��B��B�B�B�~B�B��B��B�B��B��B��B��B�>B�jB�LB�B�B�B�=B��B�B��B�3B�JB�B��B�lB��B�B�B�9B�B�B�B��B�(B	��B	��B	�B	�B	�B	�RB	�B	�B	��B	�B	�lB	��B	��B	��B	�B	�uB	�uB	�B	�B	�B	�UB	�HB	��B	�B	�B	�:B	��B	�B	��B	�B	�FB	�B	��B	��B	��B	��B	�	B	�KB	�\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�vB	�eB	��B	�4B	��B	��B	�WB	��B	��B	�WB	�hB	�[B	�`B	�B	��B	��B	��B	��B	�B	�ZB	�.B	�B	�B	��B	�B	� B	��B	�?B	�QB	�7B	�B	�B	�MB	�B	��B	��B	�B	�B	�1B	�6B	�B	�B	��B	�B	�B	�B	�^B	�$B	��B	�B	�GB	��B	�tB	�,B	��B	�jB	�B	�BB	�5B	�	B	�B	�B	��B	��B	�B	�sB	�(B	��B	� B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444443444444444443443444444444434434444443444434444444444444444433434443444434444434344334443443444444433433344344334443444444443444344344443433344433344444434444443434443334334434444344334334444334433433343433334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B�/B��B��B��B��B�B�B�"B��BėB�&B�B}�Br�B[BWBQ�B<bB, B�B�BɴB�B�NBƢB�-B��B��B�?B�-B�>B�By�Ba?BG�B/B �B�B�BSB>BB��B�B�tB�5B��BÑB��B��B�IBu�BhoBbIB[BS�BM�BC�B0B�B
��B
�5B
��B
ơB
�2B
��B
��B
�uB
��B
k}B
K�B
7DB
�B	��B	��B	��B	�:B	��B	�`B	�B	|�B	t�B	k}B	eWB	]'B	Q�B	I�B	8KB	)�B	&�B	$�B	 �B	�B	�B	�B	sB	aB	LB		/B��B��B��B��B		B	AB	=B	$B	B��B��B�B�(B��B��B�B�(B�B˽B�hB�DB�DBĔBƠBřBǦBĔB��B�XB�XB�DB�+B�B��B��B��B��B��B��B��B�`B�UB�AB�+B�B�B|�Bz�By�Bv�Bo�BjyBisBgeBdQBa@B^,BZBU�BQ�BM�BK�BH�BE�BB�B>nB<cB;[B8KB6?B41B3+B1B0B.B*�B)�B)�B)�B(�B(�B'�B&�B$�B#�B"�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�BB�B�B!�B!�B�B!�B&�B&�B�B�B�B�B �B!�B�B�B�B~BxBB�B|BmBrByByB�B�B�B �B"�B#�B#�B#�B&�B2%B2%B7AB;[B;[B7AB0B7CBXB]'BS�BL�BJ�BM�BI�BJ�BJ�BM�B^,BeXBeVBdPBa=B_2BbDBeWBeXBeXBgcBgdBf^BcMBaCBcNBgeBioBl�Bo�Bq�Bu�Bv�Bx�B}�B�B�B�B�'B�HB�YB�fB��B��B��B��B� B�B�B�B�B�CB�hB�~BÏBɰB��B��B��B��B�B� B�vB��B��B��B	 �B	B	B		-B	EB	ZB	fB	kB	qB	yB	�B	�B	�B	#�B	)�B	2#B	57B	:UB	:TB	<aB	?vB	AB	D�B	G�B	I�B	L�B	N�B	P�B	T�B	V�B	YB	[B	`=B	eUB	gdB	jvB	l�B	p�B	q�B	q�B	s�B	t�B	v�B	{�B	|�B	~�B	��B	�	B	� B	�-B	�4B	�CB	�OB	�LB	�TB	�UB	�YB	�XB	�XB	�`B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�$B	�%B	�*B	�/B	�=B	�;B	�DB	�DB	�CB	�WB	�YB	�`B	�iB	�tB	�yB	�{B	B	ĒB	ēB	řB	řB	ƞB	ȫB	ɰB	˾B	˽B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�+B	�4B	�7B	�8B	�BB	�IB	�PB	�QB	�UB	�VB	�]B	�]B	�aB	�kB	�kB	�pB	�pG�O�B	��B
�B
UB

B
�B
"�B
+B
2�B
8�B
@�B
GVB
L�B
Q�B
TqB
YB
^B
aYB
f^B
i�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B =G�O�G�O�B'�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B/�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�B�8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BǔB!G�O�B�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�B�bG�O�G�O�G�O�G�O�G�O�A�ŵG�O�B� G�O�G�O�B��B�,G�O�G�O�G�O�B-�G�O�G�O�B�IG�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ʮBͣG�O�A�7�B�6B��G�O�G�O�A�XAG�O�G�O�B�B�G�O�G�O�G�O�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B�{G�O�G�O�B�G�O�G�O�G�O�G�O�B�zG�O�B�!B�wA�G�O�G�O�G�O�B�9B��B�^G�O�G�O�G�O�G�O�G�O�G�O�BʴG�O�G�O�G�O�G�O�G�O�G�O�B��G�O�BؤG�O�G�O�G�O�BɣB�oB�ZG�O�B($B�0G�O�G�O�B�<G�O�G�O�G�O�G�O�B�9G�O�G�O�B�QB�gG�O�BʨB�,G�O�G�O�G�O�G�O�B�&B�G�O�G�O�B��B�G�O�B� B�iA�9�G�O�B��G�O�BˣBͺB�3BѥG�O�A���B�
B��B�4B�A˜�G�O�B̊B��B��B��B�dB��B�@B˃B�dB�oB�XBɄB̎B˹B˵B�%B˹B�B�nB�)B�B�9B�CB�%B�/B��B��A�e�B̝B˹B��B̨B�B�B��B�{B�B�_B�|B��B�/BʜB�/BͤB�B�kB�cB�,B�jB�=B�B˺B��B�9B��B̶BˀB��B�dB��B��B̋B��B̯B�bB��B��B�TB�=B�/B�B�TB̜B�/BʉB��B�_B�B�xB�jB�dB��B�IB�CB�wB�xB�wB͞B�BɻB�pB�BͿB�B��BˌB�B��B��B�0B�	B�/B��BΏB̜BΔB�B�dB�oBˌB�kB��B��B�XBͫB�B��B�TBˢB̻B��B�NB�XB��B�/B��B�BʜB̋B	�B��B��B̓B͇B́BΔB��B�YBˌB�=B��B�B�B��B��B��B�B�BʍB�GB��B�GB��B͙B�B�EB�yB�HB��BɆB̚B�hB��B�hB��B�GB��B�jB�4B�dB�TB�)B��B�B�aBιB��B̜B�TB��B�TB�B��B��B�@B�DB�MB��BˢBʼBȝBͮBΛB��B�zB�/B�#B�;B�}B��BΖB͵B��BͫB�?B��B��B��B�;B̻B�CB�CB�!B��B��B�?B��B̨B��B͢B˹B�B��B�B�B��BͰB�cB�HB�YB�IB�qBйBЦB͢B�xB�!B��B�8B̻B˥BˮB�2B�B�TB̊B͚BˡB�'B��B��B�WB��B�UB��B�#B�LB��B�AB�eB�GB̬B�B� B�gB̵B��B��B̝B�UB�B��B��B�mB̆B�B�]B��B�LB��B�7B�-BʌB��BɬB�OB�B�rB˻B˻B�BʎB�YB˧B�B��B��B��B��B�iB�iB�vB��B�B�MB��B�;B�|B��B�~B�=B��B�`B�B�B��B��B�B�yB�7B��B��B�rB�B�B� B�B�B��B�B��B��B�B�B��B�OB� B�%B��B�B�TB�cB�QB��B��B��B�B�B�B�B�9B�B�)B��B��B�UB�B�nB�{B�qB�SB�_B�B�IB��B��B��B�B�B��B��B� B�B�7B��B�MB��B�B��B� B��B�B�B�bB�lB�eB�RB�B�B�B�NB�B�B��B��B��B�#B�B�2B� B�*B�:B�PB�B�jB�B�aB�{B�B�5B��B��B��B��B��B��B�B�$B�B�SB�jB�B��B��B�B�B��B��B��B�B�B�(B��B�
B�B�B��B�qB��B��B��B�B��B�B��B�B��B�B�B�B�YB�B��B��B�5B�eB��B��B��B�}B�B�vB�B��B��B�B��B��B��B��B�5B�eB�GB�	B��B�B�8B��B�B��B�,B�BB�B��B�gB�B�B�B�1B�B�B�B��B�$B	��B	�B	�B	�B	�B	�KB	�B	�B	�B	�B	�cB	��B	��B	��B	�B	�kB	�mB	�{B	�B	�B	�KB	�@B	��B	�B	�|B	�1B	��B	��B	��B	�B	�>B	�B	��B	�B	��B	��B	��B	�@B	�QB	�B	�xB	�B	�{B	�B	�B	�vB	�vB	�zB	�B	�nB	�\B	��B	�*B	��B	��B	�NB	��B	�B	�MB	�]B	�RB	�VB	�B	�B	�B	��B	�B	�B	�PB	�$B	�B	�B	�B	�B	�B	��B	�4B	�IB	�.B	�B	�B	�CB	�B	��B	��B	�B	�B	�(B	�-B	�B	�B	��B	�B	��B	�B	�TB	�B	��B	�B	�?B	��B	�jB	�#B	�B	�_B	�B	�:B	�,B	��B	�B	�B	��B	��B	�B	�hB	�B	�B	��B	��B	��B�B��B��B��B��B�iB�iB�vB��B�B�MB��B�;B�|B��B�~B�=B��B�`B�B�B��B��B�B�yB�7B��B��B�rB�B�B� B�B�B��B�B��B��B�B�B��B�OB� B�%B��B�B�TB�cB�QB��B��B��B�B�B�B�B�9B�B�)B��B��B�UB�B�nB�{B�qB�SB�_B�B�IB��B��B��B�B�B��B��B� B�B�7B��B�MB��B�B��B� B��B�B�B�bB�lB�eB�RB�B�B�B�NB�B�B��B��B��B�#B�B�2B� B�*B�:B�PB�B�jB�B�aB�{B�B�5B��B��B��B��B��B��B�B�$B�B�SB�jB�B��B��B�B�B��B��B��B�B�B�(B��B�
B�B�B��B�qB��B��B��B�B��B�B��B�B��B�B�B�B�YB�B��B��B�5B�eB��B��B��B�}B�B�vB�B��B��B�B��B��B��B��B�5B�eB�GB�	B��B�B�8B��B�B��B�,B�BB�B��B�gB�B�B�B�1B�B�B�B��B�$B	��B	�B	�B	�B	�B	�KB	�B	�B	�B	�B	�cB	��B	��B	��B	�B	�kB	�mB	�{B	�B	�B	�KB	�@B	��B	�B	�|B	�1B	��B	��B	��B	�B	�>B	�B	��B	�B	��B	��B	��B	�@B	�QB	�B	�xB	�B	�{B	�B	�B	�vB	�vB	�zB	�B	�nB	�\B	��B	�*B	��B	��B	�NB	��B	�B	�MB	�]B	�RB	�VB	�B	�B	�B	��B	�B	�B	�PB	�$B	�B	�B	�B	�B	�B	��B	�4B	�IB	�.B	�B	�B	�CB	�B	��B	��B	�B	�B	�(B	�-B	�B	�B	��B	�B	��B	�B	�TB	�B	��B	�B	�?B	��B	�jB	�#B	�B	�_B	�B	�:B	�,B	��B	�B	�B	��B	��B	�B	�hB	�B	�B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444443444444444443443444444444434434444443444434444444444444444433434443444434444434344334443443444444433433344344334443444444443444344344443433344433344444434444443434443334334434444344334334444334433433343433334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.07 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.07 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.07 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647562020083116475620200831164756202008311647562020083116475620200831164756202008311647562020083116475620200831164756202008311647562020083116475620200831164756AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816452019021918164520190219181645    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816452019021918164520190219181645  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816452019021918164520190219181645  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647562020083116475620200831164756  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                