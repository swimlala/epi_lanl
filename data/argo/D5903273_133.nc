CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  Q   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:09Z creation      
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
resolution        =���   axis      Z        '�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  v�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�  �\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� )�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 3�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� [�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �t   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� �h   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �4   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� @�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� Jt   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � r@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   s    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                       SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �$Argo profile    3.1 1.2 19500101000000  20190219181709  20200831164907  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�8��D�@�8��D�@�8��D�111 @�8�s��<@�8�s��<@�8�s��<@6F�x���@6F�x���@6F�x����cM�^5?}�cM�^5?}�cM�^5?}111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�33A�33A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� DyxRD���D�>�D���D�� D��D�EqD��fD��HD�
=D�NfD��fD���D�{D�C�D�r�D��D��qD�7
D�}qD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O������L��=���    �L�;����L�;L�;��������L�;��������L�;L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�ͽ��ͽ��;L�;L�ͽ��ͽ��;L�;������������L�;L��    ���;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;����L�;L�;L�ͽ��;����L�;L�;L�;����������;L�;L�;����L��    �L�;���        �L�;L��    ���;L�;L�;��������L�;L�;L�;L�ͽ��;L�;L�;����L�ͽ��;L�;L�;L�ͽ��;L�;������ͽ��;L�;������;L�;L�;����L�;L�;����L�;L�;L�;����L�ͽ��;L�;��������L�ͽ��;������ͽ��;L�;L�ͽ��;�������    �L�;�����������    �L�;����L�;L�;L�;L�;����L�;L�ͽ��;L�;����L��=���=��;L�;L�;L�ͽ��;����L�;L�;L�;L�;����L��    =��ͽ��;������ͽ��;L�;L�;������ͽ��ͽ��;��������L�ͽ���    �L�;�������=���>L��=��;L�;L�;L�ͽ��ͽ��;����L�;L�;L�ͽ���    �L�;����L��    ���;L�;���    ���;�������=��ͽ��;����L�ͽ��ͽ��ͽ��ͽ��;L�;L�ͽ��ͽ��;L�ͽ���    ���ͽ��ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ��ͽ���=��ͽ��ͽ���=���>L��=���    ����                    =���    ����>L��=��ͽ���=��ͽ���    ���ͽ���=���    ����=���            =���                            ���ͽ��ͽ���        ���ͽ���                        =���            ���ͽ���        ����            ���ͽ���=���=���    ����            ���ͽ��ͽ���                    ����    ���ͽ���                            ���ͽ���    ���ͽ���                ���ͽ���=���    ����            ����=���    =���=��ͽ���    =���        ����            =��ͽ���            ���ͽ���            =���        =��ͽ��ͽ���    ����                    ���ͽ���                    ����    =���    =���                                =���=���    ����=���=���=���                ���ͽ���    ����    ����            ����=���    ����                    =���=���            =���=���=���                    ����=���=���            ����    ����        =���                    ����                ���ͽ���                >L��                    ����    ����                        =���    =���=���=���        ����    ���ͽ��ͽ���>L��        =���=���=���    ���;L��    ���ͽ���        =���>L��>L��>���>���?   ?��?��?333?L��?L��?fff?�  ?���?���?�ff?�ff?�  ?�  ?���?ٙ�?�ff?�33@   @ff@��@33@��@   @&ff@333@@  @L��@S33@Y��@fff@l��@y��@�  @�33@���@�  @�33@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@ə�@�  @�33@�ff@ٙ�@�  @�33@�ff@���@���@�33@���@���A   A��A33A��A  A	��A33AffA  A��A33AffA  A��A��AffA   A#33A$��A&ffA(  A)��A,��A.ffA0  A333A4��A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAP  AS33AT��AVffAX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��AfffAh  Ai��Ak33Al��AnffAp  Aq��As33At��AvffAx  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  Aə�A�ffA�33A���A͙�A�ffA�33A���Aљ�A�33A�  A���Aՙ�A�33A�  A���A�ffA�33A�  Dq3Dq�Dq  Dq,�Dq33Dq9�Dq@ DqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr3Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrS3Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Dr� Dr�fDr��DrٚDr� Dr�fDr�3Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt��Dt�fDt��Dt�3Dt� @   @&ff@333@@  @L��@S33@Y��@fff@l��@y��@�  @�33@���@�  @�33@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@ə�@�  @�33@�ff@ٙ�@�  @�33@�ff@���@���@�33@���@���A   A��A33A��A  A	��A33AffA  A��A33AffA  A��A��AffA   A#33A$��A&ffA(  A)��A,��A.ffA0  A333A4��A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAP  AS33AT��AVffAX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��AfffAh  Ai��Ak33Al��AnffAp  Aq��As33At��AvffAx  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  Aə�A�ffA�33A���A͙�A�ffA�33A���Aљ�A�33A�  A���Aՙ�A�33A�  A���A�ffA�33A�  Dq3Dq�Dq  Dq,�Dq33Dq9�Dq@ DqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr3Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrS3Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Dr� Dr�fDr��DrٚDr� Dr�fDr�3Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt��Dt�fDt��Dt�3Dt� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @:�H@�
=@�
=A�A#�AC�Ac�A�A�A�A���A���A�A�\A�B �HB�HB�HB�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBX�HB`�HBh�HBp�HBx�HB�p�B�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C 8RC8RC8RC8RC8RC
8RC8RC8RC8RC8RC8RCQ�C8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~8RC�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�Dq�Dq�DrDr�DsDs�DtDt�Dy�fD��D�E�D���D��
D��D�L{D��pD��RD�GD�UpD��pD�� D��D�J�D�y�D��D��{D�>D�{D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����<��	>��
>aG�<��	���<��	<��	������<��	������<��	<��	<��	<��	<��	<��	<��	=�=�<��	=�=�<��	<��	=�=�<��	���������<��	<��	>aG�=�<��	<��	<��	<��	<��	<��	=�<��	<��	���<��	<��	<��	=����<��	<��	<��	������=�<��	<��	���<��	>aG�<��	���>aG�>aG�<��	<��	>aG�=�<��	<��	������<��	<��	<��	<��	=�<��	<��	���<��	=�<��	<��	<��	=�<��	���=�=�<��	���=�<��	<��	���<��	<��	���<��	<��	<��	���<��	=�<��	������<��	=����=�=�<��	<��	=����=�>aG�<��	������=�>aG�<��	���<��	<��	<��	<��	���<��	<��	=�<��	���<��	>��
>��
<��	<��	<��	=����<��	<��	<��	<��	���<��	>aG�>��
=����=�=�<��	<��	���=�=�=�������<��	=�>aG�<��	���=�>��
>�
>>��
<��	<��	<��	=�=����<��	<��	<��	=�>aG�<��	���<��	>aG�=�<��	���>aG�=����=�>��
=����<��	=�=�=�=�<��	<��	=�=�<��	=�>aG�=�=�=�<��	=�<��	=�=�=�=�>��
=�=�>��
>�
>>��
>aG�=�>aG�>aG�>aG�>aG�>aG�>��
>aG�=�>�
>>��
=�>��
=�>aG�=�=�>��
>aG�=�>��
>aG�>aG�>aG�>��
>aG�>aG�>aG�>aG�>aG�>aG�>aG�=�=�=�>aG�>aG�=�=�>aG�>aG�>aG�>aG�>aG�>aG�>��
>aG�>aG�>aG�=�=�>aG�>aG�=�>aG�>aG�>aG�=�=�>��
>��
>aG�=�>aG�>aG�>aG�=�=�=�>aG�>aG�>aG�>aG�>aG�=�>aG�=�=�>aG�>aG�>aG�>aG�>aG�>aG�>aG�=�=�>aG�=�=�>aG�>aG�>aG�>aG�=�=�>��
>aG�=�>aG�>aG�>aG�=�>��
>aG�>��
>��
=�>aG�>��
>aG�>aG�=�>aG�>aG�>aG�>��
=�>aG�>aG�>aG�=�=�>aG�>aG�>aG�>��
>aG�>aG�>��
=�=�>aG�=�>aG�>aG�>aG�>aG�>aG�=�=�>aG�>aG�>aG�>aG�>aG�=�>aG�>��
>aG�>��
>aG�>aG�>aG�>aG�>aG�>aG�>aG�>aG�>��
>��
>aG�=�>��
>��
>��
>aG�>aG�>aG�>aG�=�=�>aG�=�>aG�=�>aG�>aG�>aG�=�>��
>aG�=�>aG�>aG�>aG�>aG�>aG�>��
>��
>aG�>aG�>aG�>��
>��
>��
>aG�>aG�>aG�>aG�>aG�=�>��
>��
>aG�>aG�>aG�=�>aG�=�>aG�>aG�>��
>aG�>aG�>aG�>aG�>aG�=�>aG�>aG�>aG�>aG�=�=�>aG�>aG�>aG�>aG�>�
>>aG�>aG�>aG�>aG�>aG�=�>aG�=�>aG�>aG�>aG�>aG�>aG�>aG�>��
>aG�>��
>��
>��
>aG�>aG�=�>aG�=�=�=�>�
>>aG�>aG�>��
>��
>��
>aG�=�<��	>aG�=�=�>aG�>aG�>��
>�
>>�
>?�?�R?8Q�?Q�?Q�?k�?��\?��\?�\)?�(�?���?�?\?\?�(�?�(�?���?�@G�@�@{@z�@�H@!G�@'�@.{@4z�@AG�@N{@Z�H@aG�@g�@tz�@z�H@��
@�
=@�=p@���@�
=@�=p@���@��
@�=p@�p�@���@�
=@�=p@���@��
@�
=@�p�@У�@�
=@�=p@�p�@��@�
=@�=p@�p�@��
@��
@�=pA Q�A�A�A�A�RAQ�A�A�A�RA�A�A�A�RA�A�A�A Q�A!�A#�A&�RA(Q�A)�A+�A-�A0Q�A1�A3�A6�RA8Q�A9�A;�A=�A@Q�AA�AC�AE�AF�RAI�AK�AM�AN�RAPQ�AQ�AS�AV�RAXQ�AY�A[�A]�A^�RA`Q�Aa�Ac�Ae�Af�RAhQ�Ai�Ak�Am�An�RApQ�Aq�As�Au�Av�RAxQ�Ay�A{�A}�A~�RA�(�A���A�A��\A�\)A�(�A���A�A��\A�\)A�(�A���A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A���A��\A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A���A\A�\)A�(�A�AƏ\A�(�A���A�A�\)A�(�A���AΏ\A�\)A�(�A���Aҏ\A�\)A���A�A֏\A�\)A���A�Aڏ\A�(�A���A�Dq!GDq'�Dq.Dq:�DqAGDqG�DqNDqZ�DqaGDqg�DqnDqz�Dq�GDq��Dq�Dq��Dq�GDq��Dq�Dq��Dq�GDqǮDq�zDq��Dq�GDq�Dq�zDq��DrGDrDrzDr�Dr!GDr.Dr4zDr:�DrAGDrNDrTzDrZ�DraGDrnDrtzDrz�Dr�GDr�Dr�zDr��Dr�GDr�Dr�zDr��DrǮDr�Dr�zDr��Dr�Dr�Dr�zDsGDs�DsDszDs!GDs'�Ds.Ds:�DsAGDsG�DsTzDsZ�DsaGDsg�DstzDsz�Ds�GDs��Ds�zDs��Ds�GDs��Ds�zDs��Ds�GDs�Ds�zDs��Ds�GDs�Ds�zDs��Dt�DtDtzDt�Dt'�Dt.Dt4zDtAGDtG�DtNDtTzDtaGDtg�DtnDtz�Dt�GDt��Dt�Dt��Dt�GDt��Dt�zDt��Dt�GDtǮDt�zDt��Dt�GDt�@.{@4z�@AG�@N{@Z�H@aG�@g�@tz�@z�H@��
@�
=@�=p@���@�
=@�=p@���@��
@�=p@�p�@���@�
=@�=p@���@��
@�
=@�p�@У�@�
=@�=p@�p�@��@�
=@�=p@�p�@��
@��
@�=pA Q�A�A�A�A�RAQ�A�A�A�RA�A�A�A�RA�A�A�A Q�A!�A#�A&�RA(Q�A)�A+�A-�A0Q�A1�A3�A6�RA8Q�A9�A;�A=�A@Q�AA�AC�AE�AF�RAI�AK�AM�AN�RAPQ�AQ�AS�AV�RAXQ�AY�A[�A]�A^�RA`Q�Aa�Ac�Ae�Af�RAhQ�Ai�Ak�Am�An�RApQ�Aq�As�Au�Av�RAxQ�Ay�A{�A}�A~�RA�(�A���A�A��\A�\)A�(�A���A�A��\A�\)A�(�A���A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A���A��\A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A���A\A�\)A�(�A�AƏ\A�(�A���A�A�\)A�(�A���AΏ\A�\)A�(�A���Aҏ\A�\)A���A�A֏\A�\)A���A�Aڏ\A�(�A���A�Dq!GDq'�Dq.Dq:�DqAGDqG�DqNDqZ�DqaGDqg�DqnDqz�Dq�GDq��Dq�Dq��Dq�GDq��Dq�Dq��Dq�GDqǮDq�zDq��Dq�GDq�Dq�zDq��DrGDrDrzDr�Dr!GDr.Dr4zDr:�DrAGDrNDrTzDrZ�DraGDrnDrtzDrz�Dr�GDr�Dr�zDr��Dr�GDr�Dr�zDr��DrǮDr�Dr�zDr��Dr�Dr�Dr�zDsGDs�DsDszDs!GDs'�Ds.Ds:�DsAGDsG�DsTzDsZ�DsaGDsg�DstzDsz�Ds�GDs��Ds�zDs��Ds�GDs��Ds�zDs��Ds�GDs�Ds�zDs��Ds�GDs�Ds�zDs��Dt�DtDtzDt�Dt'�Dt.Dt4zDtAGDtG�DtNDtTzDtaGDtg�DtnDtz�Dt�GDt��Dt�Dt��Dt�GDt��Dt�zDt��Dt�GDtǮDt�zDt��Dt�GDt�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��9A��!A���A��!A��9A���A��!A���A��PA�~�A�~�A�|�A�`BA�S�A�O�A�Q�A�S�A�S�A�S�A�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�S�A�=qA�
=A��A��A��A���A���A�G�A��wA��A�oA��A���A�`BA��^A��^A��PA��7A�=qA���A�A�S�A�bA���A��9A���A���A�JA�
=A�%A�ĜA�n�A�Q�A�5?A�JA���A���A�ffA���A���A�ffA��RA�1'A��A�S�A�VA�C�A��A�^5A�x�A�bNA�I�A�ZA��HA���A�I�A���A���A���A���A��A�K�A�-A��wA�+A�x�A��\A��wA���A��A��jA�^5A��A��^A��A���A�ffA�33A��
A��A���A��A���A�ȴA���A�Q�A�A�A�&�A�ĜA���A���At�A~VA|��A|�A{oAz��Ax��Ar��Ao�
AmAln�Ajn�AfZAd�A_O�A[��A[t�A[K�AY&�AU�^AT�!ATJAS��ARI�AN�ALJAKS�AH�jAEC�AB��A?�A<��A;�
A8�+A7�PA5��A3�;A3dZA2�A1\)A0VA0�A.VA-�7A,��A+
=A)"�A(�A(  A&�RA%�A$VA"�!A!A ĜA �A �uA jA�;A��A&�AZA9XAbA&�AJA�A��AM�A&�A��A�A��A1A&�A�HAr�A  A�A�A�wA�A
~�A	p�AA��A/A��AG�A�A$�A��A+A ��A z�A j@�5?@�K�@�~�@�r�@��+@���@�p�@��/@�@��@��/@@�33@��@�1@�P@�=q@�/@�j@�P@��@��D@ޗ�@ݩ�@�V@�Q�@�ȴ@�~�@�^5@�5?@���@�x�@؃@�o@�^5@��@���@�\)@��y@Ѻ^@�l�@���@���@ʰ!@��`@�Z@�1@Ǿw@�ȴ@��@Ł@�&�@ě�@�1@�l�@�;d@�"�@��@�v�@���@�7L@��u@�+@���@���@��7@�j@�I�@���@���@��+@�?}@��9@��@��u@�z�@�(�@�K�@��@�~�@��#@�x�@�hs@�Ĝ@�Z@�(�@�  @��@�o@��H@��@��7@���@�7L@��@���@�bN@�Z@�(�@��@�C�@��@��@�`B@�Ĝ@���@�=q@��@��@�7L@�z�@�z�@��@���@�?}@��@�9X@��@�C�@���@��\@���@�~�@��+@��!@�ȴ@�^5@�-@�$�@�{@�@�x�@��@�%@��@�Q�@�1'@��@��@�l�@�+@��y@���@�n�@�M�@���@�O�@�&�@�&�@��@�V@�V@��@�Ĝ@��j@��j@��9@��@��@���@���@��u@�z�@�9X@��@��m@�|�@��@���@���@���@���@���@���@�v�@�V@�=q@�-@��@��@���@���@��7@��@�&�@��@��@���@�Q�@�1'@�1'@� �@��;@��@���@��P@��P@�t�@�l�@�t�@�l�@�o@��!@���@���@��\@�5?@��T@�p�@�`B@��@�A�@��m@��;@���@�33@���@�?}@��j@�1@���@� �@�(�@�bN@���@��@���@��9@��@�j@�Q�@�b@��@�l�@�S�@�"�@��H@��\@�n�@�-@��@�/@��`@��u@�j@�9X@��;@�|�@��@��\@�n�@�{@���@�hs@�/@���@��/@��@�z�@�(�@�b@��@��;@��w@��P@�dZ@�"�@�@��R@��f@{K�@qB�@gv`@`�D@Vz@ORT@IIR@CiD@=��@86@0��@*&�@#�K@9�@��@��@�@�@��@	*0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��HA��\A�~�A�VA��TA�t�A���A�1A��A�ffA�x�A��hA�E�A��7A���A�"�A�=qA�5?A��A��yA��A�ZA��hA�  A���A���A��\A�oA���A��A�A���A��HA���A���A���A��HA��-A�9XA��A��A��
A��A���A��TA��/A�{A�$�A���A�1A���A�jA�VA�I�A��`A�;dA���A�/A�%A��#A���A�jA�~�A��A�ĜA�r�A��#A��
A�oA��;A��yA���A��A���A�5?A��PA��HA��HA��A�Q�A�bA���A���A��TA�M�A��+A�A�A��DA��jA���A��A��A��
A��-A��A�A�A��TA�dZA�A�E�A�$�A��A�t�A���A���A��A�1A�G�A��+A���A�v�A��A�ƨA�
=A�%A�"�A��wA���A�A��HA�1A��A�(�A��#A�ĜA�$�A�7LA���A���A��-A�/A�XA��wA�z�A��
A�&�A�z�A��
A���A�?}A��FA��FA�+A��A�VA�`BA�
=A��\A�+A��#A���A���A���A��A���A��^A��jA��A�ĜA��HA�oA��FA��DA��+A�{A���A���A�
=A��;A�|�A���A�ȴA�ȴA�"�A��RA�oA�;dA���A�7LA�z�A�&�A�9XA��7A���A��wA�{A�t�A��7A���A�r�A�x�A��A�ȴA�"�A��HA���A�A���A���A���A��
A�ȴA��jA�VA���A��HA�G�A���A�5?A���A�ȴA���A���A�33A��FA��RA���A�ĜA�ȴA���A�ȴA���A�ƨA�ȴA��^A���A�ĜA�n�A�A�ƨA�ȴA�ĜA���A��wA���A��wA��wA�A�ĜA�ƨA�A���A�ĜA�ƨA�A���A��^A�A�A�ĜA�ĜA�A�ƨA���A���A�A�ĜA�ĜA���A�ƨA�ĜA�ĜA��wA�ĜA���A���A�A���A�A�ȴA���A�ȴA�ĜA���A���A�ƨA�ȴA���A�ȴA�ĜA���A�ƨA���A���A���A���A�A�A���A��wA��wA�ĜA��jA��jA��wA�ĜA�A�ƨA�A�ƨA�ƨA�ĜA�ƨA�ĜA���A�ĜA�ȴA�ƨA�ȴA�ĜA�ƨA���A�A��jA�A��A��^A���A��RA���A��^A��jA��hA��^A���A��RA��RA��^A��RA��FA��FA��-A��!A��^A��!A��-A��RA��jA��FA�A��RA��FA��jA��RA��9A��uA��^A���A��jA�ƨA��^A��FA��jA��-A��!A��FA��9A��jA��A�Q�A��FA��9A��RA��!A��RA��FA��-A��^A��!A��FA��A��-A��9A���A��HA��FA��RA��-A��FA���A� �A��9A��-A��RA��-A��-A��^A��-A��FA��!A��A��A��!A��-A��9A��jA��-A��-A��9A��A��-A��A��A��A��-A��-A��A��-A��-A��RA��-A��!A��9A��A��9A��9A��FA��FA��FA��9A���A��9A��FA��^A��9A��9A��A��9A��!A��!A��9A���A��!A��-A���A��-A��!A��FA��!A��-A��FA��-A��!A��FA��9A��-A��!A��-A��!A��jA��A��A��-A��RA��FA��A��FA��-A��!A��!A��9A��^A��-A��-A��FA��A��RA��9A��FA��!A��FA��-A��A��-A��A��-A��!A��A��-A��FA��A��A��FA��RA��-A��FA��FA��^A��9A��9A��-A��A�VA��A���A�O�A��!A��RA��-A��-A��A��A��!A��A��A��A���A���A��A���A���A���A���A���A���A���A��!A��A���A��!A��A��A��-A��-A��^A��RA��RA�ĜA�A�ĜA��jA��^A��RA��wA��jA��jA��jA��FA��FA��!A��9A��-A��A��RA��9A��9A��jA��FA��-A��FA��!A��-A���A���A���A��FA��9A���A��-A��!A��9A��^A��9A��!A��FA��RA��-A��FA��jA��^A��FA��jA��^A��jA��jA��RA��^A��RA��^A��wA��^A��^A��^A��^A��^A���A��^A���A��^A��9A��FA��^A��RA��RA���A���A��!A��A���A���A���A���A���A���A���A��-A��-A��-A��9A��-A��-A��RA��A��RA��FA��9A��RA��RA��^A���A��wA��wA��wA��wA��jA��9A��9A��9A��-A���A��jA��^A��FA��A��!A���A��A���A���A��uA��hA��uA��uA��hA��hA��hA��hA��uA��uA��uA��\A��hA��\A��DA��PA��PA��\A��uA��PA��PA��hA��\A��7A��A��+A��A��A��A�~�A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A��A��A��A��+A��+A��A��A��+A��+A��+A��7A��+A��DA��7A��A�~�A�v�A�r�A�r�A�r�A�t�A�r�A�r�A�p�A�jA�ffA�p�A�ffA�dZA�bNA�^5A�\)A�^5A�\)A�\)A�XA�ZA�ZA�ZA�ZA�ZA�X@��m@��m@��m@��m@��@��m@��m@��m@��m@��;@��m@��m@��;@��;@��;@���@���@��
@��
@���@���@��
@���@���@���@�ƨ@�ƨ@��w@��w@��w@��F@��@��@��@���@���@���@���@���@���@���@���@���@���@��P@��P@��@��@��@��@��@��@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�dZ@�\)@�S�@�S�@�K�@�K�@�K�@�C�@�;d@�C�@�;d@�;d@�+@�+@�+@�+@�"�@�"�@�"�@��@��@��@��@�o@�o@�o@�
=@�
=@�
=@�@�
=@�
=@�@�@�@�@�
=@�@�@���@���@��@���@�ȴ@���@��R@��R@��R@��R@��!@��!@��!@��!@���@���@���@���@���@���@���@���A��A��9A��FA��FA��^A��FA��!A��-A��9A��9A���A��-A���A���A���A���A��!A���A���A��!A��!A��A��A��!A��-A��A���A���A���A���A���A���A���A���A���A���A��A��A��-A��!A��A��9A��-A��-A��-A��-A��9A��9A��FA��9A��9A��9A��FA��FA��FA��9A��9A��9A��9A��!A��9A��-A��9A��-A��!A��A��-A��-A��A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A��-A��9A���A��!A��A��9A��FA��FA��RA��9A��RA��RA��RA��9A��9A���A���A���A��FA��FA��A��A���A���A���A���A��\A��\A��PA��\A��\A��PA��PA��PA��\A��\A��\A��DA��PA��DA��DA��DA��DA��DA��PA��7A��DA��PA��DA��7A��A��A��A��A�|�A�~�A�~�A��A�~�A�|�A�~�A�~�A�~�A�|�A�z�A�~�A�|�A�~�A�~�A��A�~�A�~�A��A�~�A��A�~�A��A��A��A��A��A��A��A��A��A��A��A��A�z�A�t�A�p�A�p�A�l�A�n�A�n�A�n�A�l�A�`BA�dZA�ffA�`BA�`BA�\)A�\)A�\)A�XA�XA�VA�XA�VA�VA�VA�VA�V@��@��@��m@��m@��@��@��m@��m@��m@��m@��m@��m@��m@��;@��;@��
@���@��
@��
@��
@��
@��
@��
@���@���@��
@�ƨ@�ƨ@��w@��w@��F@��F@��F@��@��@���@���@���@���@���@���@���@���@���@��P@���@��P@��@��@��@��@��@��@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�dZ@�\)@�S�@�S�@�K�@�K�@�K�@�C�@�C�@�;d@�;d@�;d@�+@�33@�+@�"�@�"�@�"�@��@�o@��@��@�o@�o@�o@�o@�
=@�
=@�
=@�@�
=@�@�@�
=@�@�
=@�@�@���@���@��@��@�ȴ@���@���@��R@��R@��R@��R@��R@��!@��!@��!@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A��9A��!A���A��!A��9A���A��!A���A��PA�~�A�~�A�|�A�`BA�S�A�O�A�Q�A�S�A�S�A�S�A�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�S�A�=qA�
=A��A��A��A���A���A�G�A��wA��A�oA��A���A�`BA��^A��^A��PA��7A�=qA���A�A�S�A�bA���A��9A���A���A�JA�
=A�%A�ĜA�n�A�Q�A�5?A�JA���A���A�ffA���A���A�ffA��RA�1'A��A�S�A�VA�C�A��A�^5A�x�A�bNA�I�A�ZA��HA���A�I�A���A���A���A���A��A�K�A�-A��wA�+A�x�A��\A��wA���A��A��jA�^5A��A��^A��A���A�ffA�33A��
A��A���A��A���A�ȴA���A�Q�A�A�A�&�A�ĜA���A���At�A~VA|��A|�A{oAz��Ax��Ar��Ao�
AmAln�Ajn�AfZAd�A_O�A[��A[t�A[K�AY&�AU�^AT�!ATJAS��ARI�AN�ALJAKS�AH�jAEC�AB��A?�A<��A;�
A8�+A7�PA5��A3�;A3dZA2�A1\)A0VA0�A.VA-�7A,��A+
=A)"�A(�A(  A&�RA%�A$VA"�!A!A ĜA �A �uA jA�;A��A&�AZA9XAbA&�AJA�A��AM�A&�A��A�A��A1A&�A�HAr�A  A�A�A�wA�A
~�A	p�AA��A/A��AG�A�A$�A��A+A ��A z�A j@�5?@�K�@�~�@�r�@��+@���@�p�@��/@�@��@��/@@�33@��@�1@�P@�=q@�/@�j@�P@��@��D@ޗ�@ݩ�@�V@�Q�@�ȴ@�~�@�^5@�5?@���@�x�@؃@�o@�^5@��@���@�\)@��y@Ѻ^@�l�@���@���@ʰ!@��`@�Z@�1@Ǿw@�ȴ@��@Ł@�&�@ě�@�1@�l�@�;d@�"�@��@�v�@���@�7L@��u@�+@���@���@��7@�j@�I�@���@���@��+@�?}@��9@��@��u@�z�@�(�@�K�@��@�~�@��#@�x�@�hs@�Ĝ@�Z@�(�@�  @��@�o@��H@��@��7@���@�7L@��@���@�bN@�Z@�(�@��@�C�@��@��@�`B@�Ĝ@���@�=q@��@��@�7L@�z�@�z�@��@���@�?}@��@�9X@��@�C�@���@��\@���@�~�@��+@��!@�ȴ@�^5@�-@�$�@�{@�@�x�@��@�%@��@�Q�@�1'@��@��@�l�@�+@��y@���@�n�@�M�@���@�O�@�&�@�&�@��@�V@�V@��@�Ĝ@��j@��j@��9@��@��@���@���@��u@�z�@�9X@��@��m@�|�@��@���@���@���@���@���@���@�v�@�V@�=q@�-@��@��@���@���@��7@��@�&�@��@��@���@�Q�@�1'@�1'@� �@��;@��@���@��P@��P@�t�@�l�@�t�@�l�@�o@��!@���@���@��\@�5?@��T@�p�@�`B@��@�A�@��m@��;@���@�33@���@�?}@��j@�1@���@� �@�(�@�bN@���@��@���@��9@��@�j@�Q�@�b@��@�l�@�S�@�"�@��H@��\@�n�@�-@��@�/@��`@��u@�j@�9X@��;@�|�@��@��\@�n�@�{@���@�hs@�/@���@��/@��@�z�@�(�@�b@��@��;@��w@��P@�dZ@�"�@�G�O�@��f@{K�@qB�@gv`@`�D@Vz@ORT@IIR@CiD@=��@86@0��@*&�@#�K@9�@��@��@�@�@��@	*0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��HA��\A�~�A�VA��TA�t�A���A�1A��A�ffA�x�A��hA�E�A��7A���A�"�A�=qA�5?A��A��yA��A�ZA��hA�  A���A���A��\A�oA���A��A�A���A��HA���A���A���A��HA��-A�9XA��A��A��
A��A���A��TA��/A�{A�$�A���A�1A���A�jA�VA�I�A��`A�;dA���A�/A�%A��#A���A�jA�~�A��A�ĜA�r�A��#A��
A�oA��;A��yA���A��A���A�5?A��PA��HA��HA��A�Q�A�bA���A���A��TA�M�A��+A�A�A��DA��jA���A��A��A��
A��-A��A�A�A��TA�dZA�A�E�A�$�A��A�t�A���A���A��A�1A�G�A��+A���A�v�A��A�ƨA�
=A�%A�"�A��wA���A�A��HA�1A��A�(�A��#A�ĜA�$�A�7LA���A���A��-A�/A�XA��wA�z�A��
A�&�A�z�A��
A���A�?}A��FA��FA�+A��A�VA�`BA�
=A��\A�+A��#A���A���A���A��A���A��^A��jA��A�ĜA��HA�oA��FA��DA��+A�{A���A���A�
=A��;A�|�A���A�ȴA�ȴA�"�A��RA�oA�;dA���A�7LA�z�A�&�A�9XA��7A���A��wA�{A�t�A��7A���A�r�A�x�A��A�ȴA�"�A��HA���A�A���A���A���A��
A�ȴA��jA�VA���A��HA�G�A���A�5?A���A�ȴA���A���A�33A��FA��RA���A�ĜA�ȴA���A�ȴA���A�ƨA�ȴA��^A���A�ĜA�n�A�A�ƨA�ȴA�ĜA���A��wA���A��wA��wA�A�ĜA�ƨA�A���A�ĜA�ƨA�A���A��^A�A�A�ĜA�ĜA�A�ƨA���A���A�A�ĜA�ĜA���A�ƨA�ĜA�ĜA��wA�ĜA���A���A�A���A�A�ȴA���A�ȴA�ĜA���A���A�ƨA�ȴA���A�ȴA�ĜA���A�ƨA���A���A���A���A�A�A���A��wA��wA�ĜA��jA��jA��wA�ĜA�A�ƨA�A�ƨA�ƨA�ĜA�ƨA�ĜA���A�ĜA�ȴA�ƨA�ȴA�ĜA�ƨA���A�A��jA�A��A��^A���A��RA���A��^A��jA��hA��^A���A��RA��RA��^A��RA��FA��FA��-A��!A��^A��!A��-A��RA��jA��FA�A��RA��FA��jA��RA��9A��uA��^A���A��jA�ƨA��^A��FA��jA��-A��!A��FA��9A��jA��A�Q�A��FA��9A��RA��!A��RA��FA��-A��^A��!A��FA��A��-A��9A���A��HA��FA��RA��-A��FA���A� �A��9A��-A��RA��-A��-A��^A��-A��FA��!A��A��A��!A��-A��9A��jA��-A��-A��9A��A��-A��A��A��A��-A��-A��A��-A��-A��RA��-A��!A��9A��A��9A��9A��FA��FA��FA��9A���A��9A��FA��^A��9A��9A��A��9A��!A��!A��9A���A��!A��-A���A��-A��!A��FA��!A��-A��FA��-A��!A��FA��9A��-A��!A��-A��!A��jA��A��A��-A��RA��FA��A��FA��-A��!A��!A��9A��^A��-A��-A��FA��A��RA��9A��FA��!A��FA��-A��A��-A��A��-A��!A��A��-A��FA��A��A��FA��RA��-A��FA��FA��^A��9A��9A��-A��A�VA��A���A�O�A��!A��RA��-A��-A��A��A��!A��A��A��A���A���A��A���A���A���A���A���A���A���A��!A��A���A��!A��A��A��-A��-A��^A��RA��A��9A��FA��FA��^A��FA��!A��-A��9A��9A���A��-A���A���A���A���A��!A���A���A��!A��!A��A��A��!A��-A��A���A���A���A���A���A���A���A���A���A���A��A��A��-A��!A��A��9A��-A��-A��-A��-A��9A��9A��FA��9A��9A��9A��FA��FA��FA��9A��9A��9A��9A��!A��9A��-A��9A��-A��!A��A��-A��-A��A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A��-A��9A���A��!A��A��9A��FA��FA��RA��9A��RA��RA��RA��9A��9A���A���A���A��FA��FA��A��A���A���A���A���A��\A��\A��PA��\A��\A��PA��PA��PA��\A��\A��\A��DA��PA��DA��DA��DA��DA��DA��PA��7A��DA��PA��DA��7A��A��A��A��A�|�A�~�A�~�A��A�~�A�|�A�~�A�~�A�~�A�|�A�z�A�~�A�|�A�~�A�~�A��A�~�A�~�A��A�~�A��A�~�A��A��A��A��A��A��A��A��A��A��A��A��A�z�A�t�A�p�A�p�A�l�A�n�A�n�A�n�A�l�A�`BA�dZA�ffA�`BA�`BA�\)A�\)A�\)A�XA�XA�VA�XA�VA�VA�VA�VA�V@��@��@��m@��m@��@��@��m@��m@��m@��m@��m@��m@��m@��;@��;@��
@���@��
@��
@��
@��
@��
@��
@���@���@��
@�ƨ@�ƨ@��w@��w@��F@��F@��F@��@��@���@���@���@���@���@���@���@���@���@��P@���@��P@��@��@��@��@��@��@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�dZ@�\)@�S�@�S�@�K�@�K�@�K�@�C�@�C�@�;d@�;d@�;d@�+@�33@�+@�"�@�"�@�"�@��@�o@��@��@�o@�o@�o@�o@�
=@�
=@�
=@�@�
=@�@�@�
=@�@�
=@�@�@���@���@��@��@�ȴ@���@���@��R@��R@��R@��R@��R@��!@��!@��!@���@���@���@���@���@���@���A��A��9A��FA��FA��^A��FA��!A��-A��9A��9A���A��-A���A���A���A���A��!A���A���A��!A��!A��A��A��!A��-A��A���A���A���A���A���A���A���A���A���A���A��A��A��-A��!A��A��9A��-A��-A��-A��-A��9A��9A��FA��9A��9A��9A��FA��FA��FA��9A��9A��9A��9A��!A��9A��-A��9A��-A��!A��A��-A��-A��A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A��-A��9A���A��!A��A��9A��FA��FA��RA��9A��RA��RA��RA��9A��9A���A���A���A��FA��FA��A��A���A���A���A���A��\A��\A��PA��\A��\A��PA��PA��PA��\A��\A��\A��DA��PA��DA��DA��DA��DA��DA��PA��7A��DA��PA��DA��7A��A��A��A��A�|�A�~�A�~�A��A�~�A�|�A�~�A�~�A�~�A�|�A�z�A�~�A�|�A�~�A�~�A��A�~�A�~�A��A�~�A��A�~�A��A��A��A��A��A��A��A��A��A��A��A��A�z�A�t�A�p�A�p�A�l�A�n�A�n�A�n�A�l�A�`BA�dZA�ffA�`BA�`BA�\)A�\)A�\)A�XA�XA�VA�XA�VA�VA�VA�VA�V@��@��@��m@��m@��@��@��m@��m@��m@��m@��m@��m@��m@��;@��;@��
@���@��
@��
@��
@��
@��
@��
@���@���@��
@�ƨ@�ƨ@��w@��w@��F@��F@��F@��@��@���@���@���@���@���@���@���@���@���@��P@���@��P@��@��@��@��@��@��@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�dZ@�\)@�S�@�S�@�K�@�K�@�K�@�C�@�C�@�;d@�;d@�;d@�+@�33@�+@�"�@�"�@�"�@��@�o@��@��@�o@�o@�o@�o@�
=@�
=@�
=@�@�
=@�@�@�
=@�@�
=@�@�@���@���@��@��@�ȴ@���@���@��R@��R@��R@��R@��R@��!@��!@��!@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��@ON�@�k'@�o =ܦ�>K>W@VL�@��=Ё�>	/o?�Q=�u>6W�@\~R=�A�=��>>�=�T�=���?۶@�l�>��?@Ta?V��>�Z@�P3@�k�@i�q=�G@Pb�=��=�4n>!�t@�[-@�i�=�*=�=�>��@�$_=�q�>΅?��t@�g�@'4Y=�
>|�@�_?+^t>� G>���>��@�h�>
U=��>��@4{�@�j�?��=���>9�@�e�@�f{>�@S:@�f�@�g�>+>W@�ff@�g#=�=���=�Ĝ=�P�=�+?y�@"|1>x�?`wG@�q=�;?��>\@�<6>��=��=���>]��@�g�>@l�i@�a|@�`�=�>Zcs@�do@-��=�Ŭ=�z>ј�=��N=�@�>��@8�'=ǽ�=�T?��G@�b�=�0�=�:?=���>p��@T��>���@�f{?hs=Օ�?�C�?�P�>])@�gw@�=�X>*�@b�@�f�@�f�=�~�>/�'@A�K@Z�]=�1f=���=�Tv>�@j�y@��=���>M�D@�e�@�g#@�`>�@M��>��L@��>h^?)�?�r�@�e,=˧3>�@k@�e�@�e,?"a|@(��@�b�?��P@%��=��%>�Ec@���@�e�?0�>�6?���@�f�@�gw@�e�=���?k�@�f{@�d�@�f'@�d�>9|�@�T�>�/�@�do@�o=�Ԁ?Z��@m1�>DR?@�d�@�cs=�/?�@8��@�d�@$ ?,��>q�@�d?��M>�ی@�gb@�e�@urG>'�@�gw@�e�@�e�@�d�?��o>��?���@�d�@�ek@���@�e�@�d�@�f@5C@�ek?C��@�d�@�9�@�e,@�c�@�f@�ff@�c�@�c @�d@�d@�c @�f{@�e�@�e@�do@�d�@�b�@�c^@�do@�bx@�b$@�d@�c@�c�@�f'@�cs@�gb@�d@�c�@�bx@�c�@�d@�e�@�e,@�g�@�f@�e,@�bx@�c @�dZ@�do@�d�@�g#@�do@�do@�b�@�d@�d�@�ek@�e�@�d�@�hs@�f@�ek@�e@�e@�g#@�e@�d@�f{@�f�@�c�@�d@�d@�d�@�a�@�d�@�f'@�c @�`k@�b�@�d@�d�@�e,@�c@�cs@�d�@�e�@�c�@�aR@�a�@�cs@�c @�d@�e@�e@�d�@�d@�c�@�d@�b$@�c�@�d�@�c @�a�@�c�@�b@�a@�c�@�a|@F_[@�_�@�`W@�b$@�c @�b$@�^J@�b@�b�@�a�@�`@�`k@�`@�^t@�_�@�^�@�_�@�^J@�^�@�_p@�`�@�`W@�_p@�_@�^�@�`@�`�@�`@�a|@�`W@�_[@�^�@�_p@�`@�bN@�a|@�]�@�^ @�^�@�\h@�_p@�]%@�]O@�[�?I{@�]y@�_@�^�@�\h@�_p@�`W@�_@�_�@�_p@�_@�^�@�]�@�]y@�\@a4�@�\>@�\�@�]�@�\@�\�?�M�@�\h@�[�@�]�@�]�@�`@�_p@�]y@�_�@�]�@�\�@�[�@�]�@�^t@�]%@�]y@�\�@�Z�@�\h@�\�@�\h@�^J@�]y@�\@�]%@�\�@�]�@�^t@�\h@�\�@�^t@�^ @�\�@�\>@�[B@�^t@�^ @�\@�`@�^�@�^ @�^t@�\>@�]y@�]%@�^ @�^J@�\�@�]%@�]%@�Z�@�Y�@�Y!@�\�@�[�@�]�@�\@�\�@�^t@�\>@�^�@�]�@�]y@�]�@�^J@�]y@�]y@�]y@�\�@�]�@�]y@�\h@�]O@�\�@�]%@�\h@�]�@�^ @�]�@�\@�\�@�\�@�\h@�]y@�]y@�]O@�\@�\h@�\�@�]O@�\�@�]%@�]y@�\�@�[�@�\�@�\@�Z�@�]�@�\@�\�@�]y@�Z�@�\�@�\�@�\@�\�@�^�@�^ @�\h@�[�@�\>@���@�\�@�N@'@�]y@�\�@�]�@�\�@�\�@�\@�[@�\>@�Y�@�Zq@�Zq@�\}@�[B@�Zq@�[@�Z@�\S@�[l@�\�@�^t@�]�@�^_@�^�@�_@�a@�_�@�`-@�`�@�b�@�d@�d�@�d�@�e�@�d@�c�@�b�@�bx@�b�@�do@�bc@�b9@�b9@�bx@�`-@�^�@�_�@�`-@�b$@�b�@�b�@�b�@�b9@�b$@�a�@�a(@�_1@�[�@�Z�@�ag@�ag@�a@�c�@�b�@�b9@�c�@�b�@�b�@�c5@�d�@�c�@�dE@�e�@�e�@�e�@�e�@�f�@�g�@�gM@�f�@�f�@�f�@�fQ@�gM@�gM@�g�@�g�@�f�@�f�@�gM@�f�@�e�@�gM@�f�@�eA@�eA@�d�@�e�@�d0@�a�@�b�@�a�@�a�@�bN@�a�@�a�@�a�@�c5@�b�@�eV@�e�@�e�@�f<@�f�@�f�@�g�@�e�@�f<@�f�@�gM@�h�@�h^@�h�@�iD@�i�@�h^@�j@�i@�i@�g�@�f�@�e�@�e�@�b�@�f�@�g�@�f<@�b�@�b�@�b�@�`�@�_[@�\�@�Y�@�X�@�X�@�X�@�X�@�X�@�X�@�Xy@�Y6@�Y6@�X�@�XO@�W�@�W�@�W@�WT@�W�@�W�@�XO@�YK@�W�@�W�@�W�@�V�@�U�@�T�@�T�@�T�@�S�@�S�@�T@�T�@�TL@�TL@�S�@�Tv@�T�@�Ta@�T@�U@�T�@�U@�U\@�U�@�U@�U@�U\@�U@�T�@�U@�V�@�V�@�U�@�V�@�V�@�VX@�V�@�V�@�U\@�V@�T@�R�@�R*@�P@�Oa@�O@�N�@�N@�N@�M@�Ln@�J�@�M@�Jb@�If@�I@�H@�G�@�G�@�GZ@�G@�Go@�G�@�Go@�Go@�Go@�GZ@�GZ@�GZ@P�@P��@P�@P�@P�@P�@P��@P�@P��@P�[@P�[@P�[@P��@P��@P�@P��@P��@P��@P�c@P��@P��@P��@P��@P�h@P��@P�@P�l@P�B@P��@P��@P��@P�K@P�K@P�y@P��@P��@P�S@P��@P��@P�X@P��@P�X@P�.@P��@P�@P��@P�@P��@P��@P�@P��@P�6@P��@P��@P�e@P�;@P�@P��@P��@P��@P�L@P��@P�Q@P��@P�U@P��@P�Y@P�@P��@P�^@P�
@P�@P�b@P�@P�@P��@P��@P�@P��@P�k@P��@P�o@P�@P�@P��@P�s@P�@P�@P��@P�x@P�x@P�$@P�x@P�x@P�x@P�x@P�|@P��@P�V@P�[@P��@P�@P��@P��@P�!@P�K@P�!@P��@P��@P�y@P�O@P��@P�*@P�S@P�*@P��@P�X@P��@P�.@P�.@���@���@���@��`@��-@��!@��?@��*@���@��!@��7@���@��@��e@��a@���@��?@���@���@��h@���@��q@���@���@��@��:@��Y@���@���@���@��P@��u@��G@���@���@���@���@���@��@���@���@���@��p@���@���@��@��@��@��B@���@���@��@���@��S@���@���@��B@��B@���@��[@���@��F@���@��[@���@��K@��@��@��6@���@���@�� @��@��2@���@��u@���@��2@��C@���@���@���@���@��u@���@��C@���@��-@���@��6@���@��u@��}@���@��9@��t@��S@��t@���@��_@��c@���@���@��d@���@�� @��@��K@���@���@���@��2@���@��@���@��+@��+@��{@��@@���@���@��@��Q@��@��@@��@��@@���@��@���@��U@��Q@���@���@���@��j@���@���@���@���@��{@���@��,@��,@��<@��{@��k@���@���@���@���@���@��'@���@���@���@��8@��{@��f@���@��f@���@��f@���@��
@��@���@��Y@��^@��^@���@��8@��Y@��Y@��D@��V@��@�~R@�~g@�|p@�}k@�}�@�}�@�|�@�x�@�y@�z�@�x�@�w�@�v`@�u�@�v�@�u�@�uO@�u%@�u:@�uO@�u@�t�@�u@�t�@P�@P�@P�@P��@P��@P�@P��@P�@P��@P�@P�e@P�@P�;@P�;@P�@P�?@P�@P��@P�@P��@P��@P�@P��@P�@P�@P��@P�H@P��@P�L@P��@P�z@P��@Pީ@P�+@Pݭ@P݃@P�@P�3@P�8@Pی@Pی@Pی@P�b@P�8@Pں@P��@Pں@P��@P��@P��@P��@P��@P��@P�j@P��@P��@P��@P��@Pؙ@P��@Pզ@PԪ@P��@PӮ@P�@P��@P�_@P��@P��@P��@P�h@PБ@P��@P�B@P��@P��@P�K@P��@P��@P�%@P��@P̣@P��@P�O@P�)@P�S@P��@P��@Pʬ@Pʂ@P��@P�X@P�X@Pʬ@Pʂ@Pʬ@Pʬ@Pʂ@P�\@P�2@P��@Pŗ@P�L@P��@P�@P��@P��@P�Y@P�/@P�/@P�/@P��@P�]@P�
@P��@P��@P�8@P�@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 433344344444434444443444443334344433444344434443444434444344433433343344444444434443444434333443444444444443444434344444344433344434444344433343434443443334434444334443334433334343344343344434443443334333344433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333344333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ON�@�k&@�oG�O�G�O�@VL�G�O�G�O�G�O�G�O�G�O�G�O�@\~SG�O�G�O�G�O�G�O�G�O�G�O�@�l�G�O�G�O�G�O�G�O�G�O�@�P4@�k�@i�mG�O�@Pb�G�O�G�O�G�O�@�[0@�i�G�O�G�O�G�O�@�$bG�O�G�O�G�O�@�g�G�O�G�O�G�O�@�_G�O�G�O�G�O�G�O�@�h�G�O�G�O�G�O�G�O�@�j�G�O�G�O�G�O�@�e�@�fzG�O�@S8@�f�@�g�G�O�@�ff@�g#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�mG�O�G�O�G�O�@�<:G�O�G�O�G�O�G�O�@�g�G�O�@l�m@�a|@�`�G�O�G�O�@�dqG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�b�G�O�G�O�G�O�G�O�@T��G�O�@�f|G�O�G�O�G�O�G�O�G�O�@�gzG�O�G�O�G�O�@b�@�f�@�f�G�O�G�O�G�O�@Z�cG�O�G�O�G�O�G�O�@j�}G�O�G�O�G�O�@�e�@�g@�`G�O�@M��G�O�@��G�O�G�O�G�O�@�e.G�O�G�O�@k%@�e�@�e*G�O�G�O�@�b�G�O�G�O�G�O�G�O�@���@�e�G�O�G�O�G�O�@�f�@�gs@�e�G�O�G�O�@�f{@�d�@�f'@�d�G�O�@�T�G�O�@�dn@�oG�O�G�O�@m1�G�O�@�d�@�cuG�O�G�O�G�O�@�dG�O�G�O�G�O�@�dG�O�G�O�@�ge@�e�@urFG�O�@�gx@�e@�e�@�d�G�O�G�O�G�O�@�d�@�ej@���@�e�@�d�@�fG�O�@�ejG�O�@�d�@�9�@�e)@�c�@�f@�ff@�c�@�c@�d@�d
@�c@�f}@�e�@�e@�dl@�d�@�b�@�c`@�dq@�bw@�b&@�d@�c
@�c�@�f&@�cr@�ga@�d@�c�@�bx@�c�@�d	@�e@�e,@�g�@�f@�e)@�bv@�c"@�d\@�dk@�d�@�g%@�dn@�dl@�b�@�d@�d�@�el@�e�@�d�@�hs@�f@�ee@�e@�e@�g#@�e@�d@�f~@�f�@�c�@�d@�d@�d�@�a�@�d�@�f$@�c@�`j@�b�@�d@�d�@�e*@�c@�cr@�d�@�e�@�c�@�aR@�a�@�cr@�c!@�d@�e@�e@�d�@�d@�c�@�d@�b#@�c�@�d�@�c @�a�@�c�@�b@�a@�c�@�a|@F_^@�_�@�`U@�b)@�c@�b"@�^I@�b@�b�@�a�@�`@�`l@�_�@�^r@�_�@�^�@�_�@�^I@�^�@�_r@�`�@�`V@�_q@�_@�^�@�`@�`�@�`@�a~@�`S@�_Y@�^�@�_k@�`@�bN@�az@�]�@�^@�^�@�\f@�_n@�]$@�]N@�[�G�O�@�]v@�_@�^�@�\e@�_n@�`R@�_@�_�@�_r@�_@�^�@�]�@�]w@�\@a4�@�\>@�\�@�]�@�\@�\�G�O�@�\f@�[�@�]�@�]�@�`@�_q@�]s@�_�@�]�@�\�@�[�@�]�@�^r@�]$@�]u@�\�@�Z�@�\h@�\�@�\h@�^M@�]|@�\@�]!@�\�@�]�@�^u@�\h@�\�@�^u@�^@�\�@�\<@�[>@�^v@�^!@�\@�`@�^�@�^!@�^r@�\@@�]u@�]$@�^!@�^F@�\�@�]&@�]"@�Z�@�Y�@�Y@�\�@�[�@�]�@�\@�\�@�^s@�\;@�^�@�]�@�]s@�]�@�^F@�]z@�]v@�]z@�\�@�]�@�]y@�\g@�]M@�\�@�]%@�\g@�]�@�^!@�]�@�\@�\�@�\�@�\h@�]y@�]v@�]M@�\@�\f@�\�@�]N@�\�@�]!@�]w@�\�@�[�@�\�@�\@�Z�@�]�@�\@�\�@�]s@�Z�@�\�@�\�@�\@�\�@�^�@�^@�\f@�[�@�\@@���@�\�G�O�G�O�@�]s@�\�@�]�@�\�@�\�@�\@�[@�\9@�Y�@�Zn@�Zp@�\@�[C@�Zq@�[@�Z @�\S@�[r@�\�@�^v@�]�@�^_@�^�@�_@�a@�_�@�`.@�`�@�b�@�d@���@���@���@��a@��0@��#@��B@��.@���@��%@��:@���@��@��m@��b@���@��>@���@���@��j@���@��s@���@���@��@��;@��[@���@���@���@��R@��{@��J@���@���@���@���@���@��@���@���@���@��n@���@���@��@��@��@��F@���@���@��@���@��U@���@���@��C@��F@���@��]@���@��D@���@��]@���@��J@��@��
@��6@���@���@��@��@��3@���@��w@���@��2@��F@���@���@���@���@��u@���@��H@���@��/@���@��6@���@��r@��{@���@��;@��v@��U@��r@���@��`@��d@���@���@��i@���@��"@��@��N@���@���@���@��6@���@��@���@��.@��)@��x@��B@���@���@��@��R@��@��@@��@��E@���@��@���@��X@��P@���@���@���@��g@���@���@�� @���@��~@���@��.@��-@��=@��z@��j@���@���@���@���@���@��-@���@���@���@��<@��~@��h@���@��e@���@��h@���@��@��"@���@��X@��^@��_@���@��9@��[@��^@��F@��W@��@�~V@�~i@�|n@�}j@�}�@�}�@�|�@�x�@�y@�z�@�x�@�w�@�vb@�u�@�v�@�u�@�uQ@�u$@�u>@�uR@�u@�t�@�u@�t�@P�@P�@P�@P��@P��@P�@P��@P�@P��@P�@P�c@P�@P�;@P�@@P�@P�>@P�@P�@P�@P��@P��@P�@P��@P�@P�@P��@P�H@P��@P�H@P��@P�x@P��@Pި@P�-@Pݲ@P݀@P�@P�0@P�:@Pۋ@Pۋ@Pۋ@P�`@P�6@Pڸ@P��@Pڽ@P��@P��@P��@P��@P��@P��@P�k@P��@P��@P��@P��@Pؚ@P��@Pգ@PԪ@P��@PӮ@P�@P��@P�c@P��@P��@P��@P�f@PЎ@P��@P�C@P��@P��@P�M@P��@P��@P�#@P��@P̣@P��@P�N@P�0@P�M@P��@P�@Pʫ@Pʃ@P��@P�U@P�Z@Pʫ@P�}@Pʫ@Pʭ@Pʅ@P�]@P�5@P��@PŘ@P�M@P��@P�@P��@P��@P�]@P�2@P�2@P�.@P��@P�^@P�@P��@P��@P�5@P�@P��@P��@���@���@���@��a@��0@��#@��B@��.@���@��%@��:@���@��@��m@��b@���@��>@���@���@��j@���@��s@���@���@��@��;@��[@���@���@���@��R@��{@��J@���@���@���@���@���@��@���@���@���@��n@���@���@��@��@��@��F@���@���@��@���@��U@���@���@��C@��F@���@��]@���@��D@���@��]@���@��J@��@��
@��6@���@���@��@��@��3@���@��w@���@��2@��F@���@���@���@���@��u@���@��H@���@��/@���@��6@���@��r@��{@���@��;@��v@��U@��r@���@��`@��d@���@���@��i@���@��"@��@��N@���@���@���@��6@���@��@���@��.@��)@��x@��B@���@���@��@��R@��@��@@��@��E@���@��@���@��X@��P@���@���@���@��g@���@���@�� @���@��~@���@��.@��-@��=@��z@��j@���@���@���@���@���@��-@���@���@���@��<@��~@��h@���@��e@���@��h@���@��@��"@���@��X@��^@��_@���@��9@��[@��^@��F@��W@��@�~V@�~i@�|n@�}j@�}�@�}�@�|�@�x�@�y@�z�@�x�@�w�@�vb@�u�@�v�@�u�@�uQ@�u$@�u>@�uR@�u@�t�@�u@�t�@P�@P�@P�@P��@P��@P�@P��@P�@P��@P�@P�c@P�@P�;@P�@@P�@P�>@P�@P�@P�@P��@P��@P�@P��@P�@P�@P��@P�H@P��@P�H@P��@P�x@P��@Pި@P�-@Pݲ@P݀@P�@P�0@P�:@Pۋ@Pۋ@Pۋ@P�`@P�6@Pڸ@P��@Pڽ@P��@P��@P��@P��@P��@P��@P�k@P��@P��@P��@P��@Pؚ@P��@Pգ@PԪ@P��@PӮ@P�@P��@P�c@P��@P��@P��@P�f@PЎ@P��@P�C@P��@P��@P�M@P��@P��@P�#@P��@P̣@P��@P�N@P�0@P�M@P��@P�@Pʫ@Pʃ@P��@P�U@P�Z@Pʫ@P�}@Pʫ@Pʭ@Pʅ@P�]@P�5@P��@PŘ@P�M@P��@P�@P��@P��@P�]@P�2@P�2@P�.@P��@P�^@P�@P��@P��@P�5@P�@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 433344344444434444443444443334344433444344434443444434444344433433343344444444434443444434333443444444444443444434344444344433344434444344433343434443443334434444334443334433334343344343344434443443334333344433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333344333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�i�9�k�9�l�9�m'9�n�9�l�9�k�9�kp9�l�9�l�9�i"9�l9�i�9�h�9�iA9�h�9�k|9�jV9�h�9�k�9�l9�j9�j�9�k�9�l#9�lB9�f�9�h	9�e�9�d�9�hm9�iU9�i�9�j�9�k9�k;9�l9�l9�m�9�l�9�l�9�n9�m�9�l�9�m�9�m�9�n�9�n�9�n�9�n^9�n.9�n~9�o9�ou9�n�9�n�9�n�9�n�9�o9�m�9�nQ9�m�9�o9�m�9�m\9�m9�m�9�m�9�m9�i�9�jG9�kN9�i�9�i�9�i�9�iR9�i�9�i�9�j�9�k.9�l�9�l�9�l�9�m79�m�9�j�9�l9�n�9�o�9�m9�n9�m59�o�9�o�9�p)9�q9�ou9�q9�q?9�q9�pI9�p�9�l�9�lf9�k9�p�9�p�9�m9�mj9�k9�l9�i�9�g89�dS9�dB9�c�9�c�9�d�9�c�9�d9�c�9�dS9�d�9�dV9�c�9�c�9�c�9�cp9�c�9�c@9�c�9�d�9�c9�c9�d"9�c�9�bj9�`�9�`_9�`/9�`�9�_�9�_�9�_�9�`�9�`�9�_�9�`9�`L9�`9�`9�^�9�`�9�`.9�`N9�`,9�aV9�`�9�`�9�`�9�`�9�`�9�`�9�a�9�a�9�b	9�b�9�b�9�b89�b99�bz9�aS9�b�9�c 9�b�9�_�9�]�9�\�9�\�9�[?9�\9�\%9�\g9�[�9�Xb9�X�9�Z
9�X29�W�9�V�9�V9�V�9�U�9�U�9�U�9�U�9�U�9�U{9�Uq9�U�9�Uq9#v9#y9#z9#�9#�9#�9#�9#u9#�9#W9#69#Y9#9#9#�9#R9# �9# �9# �9# �9#9# �9#9# �9# l9#9"��9"�f9"�9"��9"�`9"��9"��9"�]9"��9"��9"�y9"��9"�9"�P9"�P9"�P9"�.9"�9"��9"��9"��9"�	9"�	9"�	9"�9"�	9"�9"��9"�E9"�C9"�A9"�%9"�9"��9"��9"��9"�Q9"�-9"��9"�9"�*9"��9"�9"�9"�9"�9"�x9"�9"�Y9"�9"�29"��9"��9"�J9"��9"�9"�9"�k9"�9"�9"�e9"�h9"�$9"�9"�E9"��9"��9"�$9"�9"�$9"�&9"�9"� 9"�9"�9"�/9"�e9"�^9"�9"�Y9"�79"�9"��9"��9"��9"�9"�S9"�9"��9"��9"�k9"�M9"�9"��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB�BB�BB�BC�BB�BB�BB�BB�BB�BB�BB�BB�BC�BC�BC�BD�BD�BD�BD�BE�BF�BF�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BH�BM�BXBjB� B�B�DB�hB��B��B��B��B�XB��B�}B�B�oBw�BT�BS�B`BB�1B��B�dB�B0!Bq�B�\B��B��B��B��B��B��B��B��B�oB�=B�BjB`BB^5BN�B"�BƨB�LB�VB��B�3B��B{�By�Be`BF�B>wB:^B8RB9XB>wBB�BF�BF�B=qB0!B,B#�B�BB�BB��B�9B��B�\B� Br�BiyBVBL�BH�BE�B6FB!�BoBVB
��B
�B
�ZB
��B
��B
hsB
%�B
DB	��B	�B	�yB	�NB	�B	�
B	ȴB	�B	��B	��B	��B	�VB	u�B	bNB	B�B	-B	)�B	%�B	�B	B��B��B��B�B�B��BȴB�XB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�DB�B}�B|�Bw�Bt�Bt�Bp�Bo�Bl�Bk�Bk�BjBiyBffBdZBbNBaHB`BB^5B[#BXBW
BT�BQ�BN�BJ�BE�BC�BC�BB�BA�B@�B?}B=qB<jB:^B8RB6FB33B1'B0!B/B/B/B/B0!B/B0!B/B-B-B/B0!B1'B2-B33B33B2-B2-B33B33B49B5?B49B5?B5?B7LB9XB9XB8RB7LB7LB8RB8RB8RB8RB:^B;dB;dB;dB<jB=qB?}BC�BE�BD�BD�BE�BD�BH�BJ�BK�BJ�BI�BI�BI�BI�BJ�BL�BN�BO�BP�BQ�BS�BS�BS�BW
BXBZB_;BdZBgmBk�Bn�Bn�Bn�Bn�Bm�Bn�Bp�Bo�Bn�Bp�Bp�Bp�Bp�Bp�Bv�Bx�B}�B�B�+B�PB�PB�PB�PB�PB�PB�PB�PB�bB�oB��B��B��B��B��B��B��B�B�B�B�!B�?B�RB�dB�}BBÖB��B��B��B�B�/B�NB�NB�`B�sB�B�B�B��B��B	B	B	
=B	\B	hB	oB	uB	�B	�B	�B	�B	�B	%�B	,B	/B	33B	49B	7LB	9XB	:^B	;dB	;dB	>wB	B�B	G�B	I�B	J�B	K�B	K�B	L�B	O�B	Q�B	Q�B	R�B	R�B	S�B	S�B	S�B	T�B	VB	XB	ZB	YB	\)B	dZB	gmB	jB	l�B	n�B	q�B	r�B	s�B	t�B	u�B	v�B	w�B	y�B	z�B	|�B	}�B	}�B	�B	�B	�B	�B	�1B	�7B	�7B	�7B	�JB	�VB	�\B	�\B	�\B	�bB	�bB	�bB	�bB	�oB	�{B	��B	��B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	�wB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�HB	�XB	�>B
�B
@B
qB
'B
-CB
2�B
8B
=�B
B�B
J�B
S�B
[=B
_�B
fB
h>B
j�B
oB
r�B
v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?<��A�B(Bc�?^J?�ֲA�a�Ao8�?
(g?5(�@��?m�?w%-A��?ˆ?%��?���>��n?�.@H�3B?.C@4b_AfcB@��?N��A��QB�A�Ҕ? ��A�Y>Š8>���?U��B6�B�?��?�_?3*B��?�?:�@�LIB�A���?(�?L^BZ@|P�@c?��?SaFB?;��?�9?5b�A�T�B�@ZO�?&�V?wMB	�B#]?C�*A�<BHBZ?b�B
%Bi>�4�>�&�>���>��?�@���A��?>�@���A��g?&�z@�O�?P �BN�@��>�M�?	wu?��RB��??��A�&�B�B�?"N?�!�B}A���>�a[?'�@��>��G?Y?=��A��?/�?��A�qBT>ǅQ>���?+}�?�;�A�z�?�:~BQ@<��?��A>��A
�?�nHB�Apf�?
D�?S�A��B
�Bi?��?h1A�vOA��W>��k>�?�>�4�?E�A��	AN��?�j?�LB
�B BA�?/cA���?���Ba�?DQ�@W��@��CB'?~�?LJ�A�5�B�Bo@n��A��B�A?��A�%�?�@ZB	pB2@�sw?0PH@¼!B&BB\�?#�@�B�B�BBQ�?u��B�k@#�SB�B U?�@��AA�wb?���B;B�?��@`��A���B�Ay��@}�?��B2@��?�B�B�Aʘ?\�=B;B
�B�B�@�?=+�@���BB�B�B�-B�B�B]A�Q�BK�@��B\BTLB�B�B�BmB	BB;B�BpBB4TB�BSB�B�B�B�B�BaBKB�ByBB�B�B�B�BB�B�BB�BZB�B�BhBqB�B�B�B�BBBSB�B�B�BBB�B�B�B�B#B�BB�BYBB�B�BkB;B7B,B�BXB�B�B&B�BOBoB>B�B7BhB�B�B�B
BB�B�B�B�B�BnB{B�B�B�BAB�B�B|B�B�B(A��B�B�B�B�BB�B#9B�B7B�B4B�BIBZBrB�BaB�BrBBB�B�B�B�B`B:B0B�B�B�B�B:BQBiBxBXB�B�B
B�B�B@C��B(B~B�B�B:B�BFB�B�B�B�BB�B�A�8!B�B�BB�B�Ac�B�B"B�B�BKBjB�B>B�B�B�B�B�B�B�BPBYB�B�B�B
B9B�BxB�B�B�B�B�B�B9BWBB�B�B�B�B�B9B�B�B�BB�B�B1BBHBPB�B<BoB'BDBIBCB�B�B�BaBB�B�B�B�B�B�B�BBhB<B�B�B�BcBPBXB�BCBB�B�B�B(BIBB�B�BpB�BxB`BPB�BB:B�B�B�B�BqB�B�BB�B_B�B�B�B^BA���BoA[eAT?�B�B�BB>B�B�B/BB@BMB�B�B�B�B;BB�BBB�BHB�B�B�B�B7B�B�B�B�B�B>B�BB�B�B�B�B�B"BB�BQB|B�B�BB&BBOB
BB�BJBPBBTBhB�B�B�BBxB>B�BDB3B~BrBNB�B�BaB�B�BSB{B*BB�B�B^B�B�B B$B/BB�BlB�BB�B�B�B�B�B�B�B�BKB|BxB�B�BB	BB�BB�B%B�B@BB�B�B)B�B�B�B B4B�B�BmB�B�B�B�B�B�B�B�B�B�B�B�B�B&B�B�B+BKBB�B	B BwB�B>B�BB|BBB`BsB>B�B�BuB�B�B�B�B�BBPB�B�B�BcB_BNB�B�B'B�BB�B BHB�B.BFBBOBBAB�B�B�B�B�B�BeB�B3B�BqBB"B�BIBB]B@B�B�BB�B�B�B�B�B�B�BdB:BdB,B�B�B�B�B�B{BrBLBDBB	�B	��B	��B	��B	��B	��B	�{B	��B	�rB	�B	��B	��B	�iB	�=B	��B	��B	��B	��B	�'B	�ZB	�MB	�/B	��B	�"B	�RB	��B	�EB	�)B	��B	��B	��B	��B	��B	��B	�|B	�PB	��B	�qB	�EB	�B	�hB	��B	��B	�{B	��B	�&B	��B	��B	��B	��B	�/B	��B	��B	�QB	�%B	��B	��B	�gB	��B	�tB	��B	��B	�9B	��B	�iB	��B	��B	�^B	�B	��B	�qB	��B	��B	��B	��B	��B	��B	�B	��B	�4B	��B	�TB	�B	�B	��B	�zB	�/B	�"B	��B	��B	�rB	�+B	�[B	�NB	�AB	�B	�aB	��B	�rB	��B	��B	��B	��B	��B	�MB	�RB	�&B	��B	��B	��B	�ZB	�B	�kB	�}B	�QB	��B	��B	�6B	�gB	�LBB�BB*BBaBB�BB�BBrBCBBBB�BCBBbBB�BC�BB>BC'BA�BB�BDQBBRBB�BCBB4BC<BB�BB#BC�BBBB�BB�BB�BCBC;BC+BCBC�BC�BCBCBCtBB�BC�BB�BC�BB'BCBCBC>BC6BB�BB�BB�BC
BB�BCMBB�BCCBB�BB�BCqBC�BBnBB�BC1BB�BB�BC%BB0BB(BC�BB�BB�BBVBB�BB�BB�BB�BBaBB BB�BB�BB�BC BB�BB�BC�BC�BCpBByBCFBC�BB�BC"BB�BB"BB�BB�BBtBB�BB�BB�BCWBC�BC�BCUB@�BCBB�BBoBB�BA�BA�BCBBC.BB�BB�BB�BA�BC!BB�BC+BB�BB�BB�BB�BCOBB9BC=BB�BB�BB�BC&BC6BCBB:BB�BCBBBCMBA�BBsBCBC1BB�BB�BB�BC�BCvBB�BCBB�BC~BBwBCBCoBB�BB�BCBC(BCBBfBB�BBTBB�BB�BB�BB�BB�BCBB�BB�BCBBhBB�BB�BCkBB�BB�BB�BC
BB�BB�BB�BC-BB�BC�BB{BC\BC~BB�BB�BBPBC0BC�BCjBD
BC<BDBC�BC�BC�BC�B	ڌB	�B	ۂB	ۆB	�iB	�zB	�_B	�&B	�7B	��B	��B	��B	�zB	�~B	��B	ۮB	�6B	�7B	�B	�B	�!B	��B	��B	ۿB	�VB	��B	ۥB	�;B	��B	ۊB	�1B	۩B	�}B	�B	ۭB	ܒB	�(B	܄B	��B	��B	��B	��B	ܬB	܀B	�'B	�B	��B	�iB	�[B	�AB	�4B	�&B	�B	ݳB	�JB	�<B	�"B	��B	��B	�EB	ݪB	��B	�^B	�%B	ޭB	ށB	�
B	ޱB	��B	��B	�xB	މB	�jB	�|B	�#B	�nB	�B	ެB	߰B	�B	ߕB	�=B	ߧB	��B	�B	�B	��B	��B	�vB	�[B	�{B	�B	�B	�'B	�B	��B	�B	��B	��B	��B	��B	�LB	�B	�B	��B	�B	�wB	�KB	�B	�B	�B	�B	�GB	�B	��B	�B	�>B	�"B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999433344344444434444443444443334344433444344434443444434444344433433343344444444434443444434333443444444444443444434344444344433344434444344433343434443443334434444334443334433334343344343344434443443334333344433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333344333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 BB{BB{BB{BC�BB{BB{BB{BB{BB{BB{BB{BB{BC�BC�BC�BD�BD�BD�BD�BE�BF�BF�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BH�BM�BW�BjnB�B�B�2B�RB��B��B��B��B�EB�uB�iB��B�\Bw�BT�BS�B`-B� B��B�QB�B0Bq�B�IB��B��B��B��B��B��B��B��B�_B�+B��BjlB`1B^!BN�B"�BƔB�=B�BB��B� B�vB{�By�BeLBF�B>dB:IB88B9?B>bBByBF�BF�B=]B0B+�B#�BpB �B�.B��B�&B�xB�FB�Br�BicBU�BL�BH�BE�B6.B!�BXBAB
��B
�iB
�FB
̹B
��B
h]B
%�B
-B	��B	�B	�cB	�8B	�B	��B	ȟB	��B	��B	�uB	�qB	�@B	u�B	b6B	BxB	,�B	)�B	%�B	pB	�B��B��B��B�oB�B��BțB�@B��B��B��B��B��B��B��B��B�yB�uB�uB�{B��B�sB�dB�WB�JB�+B��B}�B|�Bw�Bt�Bt�Bp�Bo�BlqBklBklBjeBi`BfLBdABb2Ba.B`*B^B[
BW�BV�BT�BQ�BN�BJ�BE�BC}BCzBBtBAnB@hB?aB=VB<OB:CB89B6+B3B1B0	B/B.�B/ B/B0B/B0B.�B,�B,�B/ B0B1B2B3B3B2B2B3B3B4B5#B4B5"B5#B70B9;B9=B87B71B7/B87B88B87B85B:BB;HB;IB;GB<NB=WB?`BC|BE�BD�BD�BE�BD�BH�BJ�BK�BJ�BI�BI�BI�BI�BJ�BL�BN�BO�BP�BQ�BS�BS�BS�BV�BW�BZ B_ Bd=BgOBkgBn{Bn|Bn~Bn}BmuBnzBp�Bo�Bn|Bp�Bp�Bp�Bp�Bp�Bv�Bx�B}�B��B�B�2B�2B�5B�2B�3B�1B�3B�3B�EB�SB�kB�uB��B��B��B��B��B��B��B��B�B�#B�6B�EB�aB�sB�zB̯B̲B��B��B�B�2B�1B�CB�UB�bB�tB��B��B��B	 �B	�B	
 B	?B	JB	RB	[B	eB	�B	�B	�B	�B	%�B	+�B	.�B	3B	4B	70B	9<B	:CB	;FB	;HB	>YB	BtB	G�B	I�B	J�B	K�B	K�B	L�B	O�B	Q�B	Q�B	R�B	R�B	S�B	S�B	S�B	T�B	U�B	W�B	Y�B	X�B	\B	d>B	gNB	jdB	lnB	n{B	q�B	r�B	s�B	t�B	u�B	v�B	w�B	y�B	z�B	|�B	}�B	}�B	��B	��B	��B	�B	�B	�B	�B	�B	�.B	�7B	�>B	�@B	�>B	�FB	�DB	�DB	�DB	�PB	�^B	�eB	�eB	�`B	�ZB	�^B	�cB	�eB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�.B	�/B	�6B	�9B	�AB	�FB	�IB	�MB	�ZB	�`B	�fB	�lB	�rB	�xB	�~B	ńB	ƋB	ȖB	ȗB	ɝB	ʦB	ʢB	ʢB	ʣB	˪B	̱B	̱B	͸B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�:B	�$B
�B
"B
UB
'B
-(B
2xB
8 B
=nB
B�B
JnB
S�B
[ B
_�B
e�B
h"B
j�B
o B
rzB
v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��BBc�G�O�G�O�A�a�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�A��/BwA��vG�O�A�BG�O�G�O�G�O�B6�B�G�O�G�O�G�O�B��G�O�G�O�G�O�B�G�O�G�O�G�O�BFG�O�G�O�G�O�G�O�B	G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B	tB#IG�O�A�;�B3BCG�O�B
BUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��?G�O�G�O�G�O�BN�G�O�G�O�G�O�G�O�B��G�O�A�&�B�BnG�O�G�O�BlG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBG�O�G�O�G�O�G�O�A�z�G�O�B>G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�A��B
oBSG�O�G�O�G�O�A��BG�O�G�O�G�O�G�O�A���G�O�G�O�G�O�B
�B�BA�G�O�A���G�O�Ba�G�O�G�O�G�O�B&�G�O�G�O�A�5�BpB\G�O�G�O�B�G�O�G�O�G�O�G�O�B	ZBG�O�G�O�G�O�BB�B\�G�O�G�O�B�B�BBQ�G�O�B�XG�O�B�B AG�O�G�O�A�wKG�O�B'B�G�O�G�O�G�O�B�G�O�G�O�G�O�BG�O�G�O�B�B�Aʗ�G�O�B)B
�B�B�G�O�G�O�G�O�BB�B�B�B�B�BKG�O�BK�G�O�BJBT<B�B�B�BZB�BB'B�B\B�B4@B�B<B�B|B�B�B�BPB5BzBdB�B�BzB�B�BB�B�B�B�BCB�B�BQB_B�B�BtB�B,B<B�B�B�BBB�B�B�B�BB�BB�BDB�B�B�BXB'B#BB�BBBmB�BB�B:B\B*B�B#BSB�B�B�B�B�BxB�B�BtB�BZBiB�B�B�B-B�B�BhB�B�BA��B�B�B�BmB�BlB#&B�B$B�B!B�B3BCB_B�BMB�BaB�BB~B�BrB�BJB$BB�B�B�B�B$B<BUBbBAB�BtB�B�BrB�G�O�BBiB�B|B%B�B0B�BrB�B�BB�B�A�8B�B�BB�B�G�O�B�BB�B�B:BXB�B*B�B�B�B�B�B�B�B;BFB�B�B�B�B&B�BaB�B�B�B�BzB�B%BBB�B�B�B�B�B�B#B�B�B�BiB�B�BBkB4B<BiB,B[BB4B4B0B|BtB�BKBB�B�B�B�B�B�B�B�BUB(B�B�B�BOB<BEB�B0BkB�B�B�BB5B�B�BsB[BsBaBJB;BuB
B#B�B�B�B�BZB�B�B�B�BKB�BzB�BKBA���B\G�O�G�O�B�B�B�B,ByB�BB�B+B:BrB�B�B�B(BBtB2B�B3ByBwB�BwB"B�B�B�B�B�BByBBBBMBB�BB�BB`BB�BBBB�BB�BBRBB�BC�BB/BCBA�BBvBD<BB@BB~BB�BB#BC*BB�BBBC�BB
BB�BB�BB~BB�BC*BCBCjBC�BC�BCBCBC`BB�BC�BB�BC�BBBB�BCBC,BC$BBqBB�BB�BB�BB�BC<BB�BC/BB�BB�BC[BC�BB^BB�BC BB�BB�BCBBBBBC�BB�BB�BBBBB�BB�BB�BB�BBOBA�BB�BB�BB�BB�BB�BB�BC�BCvBC^BBhBC4BC�BB~BCBB�BBBB�BB�BBcBB�BB�BB�BCEBC�BC�BCEB@�BB�BB�BB^BB�BA{BA�BC/BCBB�BB�BB�BA�BCBB�BCBB�BB�BB�BBoBC<BB'BC,BB�BB�BB{BCBC$BCBB%BB�BB�BA�BC<BA�BB`BCBC BB�BBoBB�BC�BCbBB�BB�BB�BClBBfBCBC[BB�BByBCBCBB�BBRBB�BBBBB�BB�BB�BB�BB�BB�BB�BB�BCBBVBB�BB�BCYBB�BB�BB�BB�BB�BB�BB�BCBB�BC�BBhBCGBClBB�BB�BB>BCBC�BCWBC�BC*BDBC�BC�BC�BC�B	�mB	�aB	�eB	�hB	�LB	�]B	�BB	�B	�B	��B	ڢB	ڨB	�^B	�dB	��B	ێB	�B	�B	��B	��B	�B	ڸB	��B	۠B	�7B	ڳB	ۈB	�B	��B	�oB	�B	ۋB	�_B	��B	ۑB	�sB	�B	�dB	ܲB	��B	��B	ܻB	܎B	�bB	�B	��B	��B	�KB	�>B	�$B	�B	�	B	��B	ݖB	�.B	�B	�B	��B	ܬB	�*B	݌B	��B	�CB	�B	ޑB	�eB	��B	ޒB	��B	��B	�[B	�kB	�LB	�_B	�B	�QB	��B	ލB	ߕB	��B	�wB	� B	ߊB	��B	��B	��B	߽B	߲B	�XB	�?B	�^B	��B	��B	�	B	��B	��B	��B	߻B	��B	߬B	�oB	�.B	�B	�B	��B	�B	�[B	�/B	�B	��B	��B	�B	�+B	��B	��B	�B	� B	�B	�B	�BByBBBBMBB�BB�BB`BB�BBBB�BB�BBRBB�BC�BB/BCBA�BBvBD<BB@BB~BB�BB#BC*BB�BBBC�BB
BB�BB�BB~BB�BC*BCBCjBC�BC�BCBCBC`BB�BC�BB�BC�BBBB�BCBC,BC$BBqBB�BB�BB�BB�BC<BB�BC/BB�BB�BC[BC�BB^BB�BC BB�BB�BCBBBBBC�BB�BB�BBBBB�BB�BB�BB�BBOBA�BB�BB�BB�BB�BB�BB�BC�BCvBC^BBhBC4BC�BB~BCBB�BBBB�BB�BBcBB�BB�BB�BCEBC�BC�BCEB@�BB�BB�BB^BB�BA{BA�BC/BCBB�BB�BB�BA�BCBB�BCBB�BB�BB�BBoBC<BB'BC,BB�BB�BB{BCBC$BCBB%BB�BB�BA�BC<BA�BB`BCBC BB�BBoBB�BC�BCbBB�BB�BB�BClBBfBCBC[BB�BByBCBCBB�BBRBB�BBBBB�BB�BB�BB�BB�BB�BB�BB�BCBBVBB�BB�BCYBB�BB�BB�BB�BB�BB�BB�BCBB�BC�BBhBCGBClBB�BB�BB>BCBC�BCWBC�BC*BDBC�BC�BC�BC�B	�mB	�aB	�eB	�hB	�LB	�]B	�BB	�B	�B	��B	ڢB	ڨB	�^B	�dB	��B	ێB	�B	�B	��B	��B	�B	ڸB	��B	۠B	�7B	ڳB	ۈB	�B	��B	�oB	�B	ۋB	�_B	��B	ۑB	�sB	�B	�dB	ܲB	��B	��B	ܻB	܎B	�bB	�B	��B	��B	�KB	�>B	�$B	�B	�	B	��B	ݖB	�.B	�B	�B	��B	ܬB	�*B	݌B	��B	�CB	�B	ޑB	�eB	��B	ޒB	��B	��B	�[B	�kB	�LB	�_B	�B	�QB	��B	ލB	ߕB	��B	�wB	� B	ߊB	��B	��B	��B	߽B	߲B	�XB	�?B	�^B	��B	��B	�	B	��B	��B	��B	߻B	��B	߬B	�oB	�.B	�B	�B	��B	�B	�[B	�/B	�B	��B	��B	�B	�+B	��B	��B	�B	� B	�B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999433344344444434444443444443334344433444344434443444434444344433433343344444444434443444434333443444444444443444434344444344433344434444344433343434443443334434444334443334433334343344343344434443443334333344433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333343333333333333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333344333333333333333333333111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.22 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.22 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.22 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649072020083116490720200831164907202008311649072020083116490720200831164907202008311649072020083116490720200831164907202008311649072020083116490720200831164907AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817092019021918170920190219181709    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817092019021918170920190219181709  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817092019021918170920190219181709  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649072020083116490720200831164907  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                