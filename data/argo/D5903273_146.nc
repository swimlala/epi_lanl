CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  R   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:13Z creation      
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
resolution        :�o     '�  Ј   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� *0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 4(   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� \    CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� A    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� K   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � r�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   s�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181713  20200831164918  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�I�B�u�@�I�B�u�@�I�B�u�111 @�I���"@�I���"@�I���"@5n��P@5n��P@5n��P�cM�$�/�cM�$�/�cM�$�/111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDy��D��D�R�D��{D��3D� D�H D��=D��RD�\D�FD���Dǻ�D���D�O
Dڈ D�� D� D�:=D�q�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    ���;�������    �L�;������������L�;L�;������������������;����L�;L�;���    �L�;������������L�;L�;�����������������������    �����L�;����������;L�;������������L�;����������������L�;������������L��=��;L�;����L�;L�ͽ��;L�;����L�;��������������������������;L�;����������;������������������;L�;��������L��        �����L�;��������L�;����L�ͽ��;�������    ���;����L�;��������������ͽ��;����L�;��������L�;��������L�;��������L��=���=��;����L��    ���;L�;����L�;L�ͽ��;����������;L�;�����������=���=��;L�;��������L�;��������L�;L�;��������������;�����������=���    �����L�;����L��    �L�;������������L�;����L�ͽ���=��ͽ��;��������L�;������ͽ��;������ͽ���    �L�;����L�ͽ���    =��ͽ��;����L�;L�;�������    =���=���    �������;L�;L�ͽ��ͽ��ͽ��ͽ��;L�;������ͽ��ͽ��;L�;����L�ͽ���    �L�;����L��    �L�ͽ��ͽ��ͽ��;L�;L��                ���ͽ��ͽ��ͽ���                        ���ͽ��ͽ���    =���        �L��=���    �L�ͽ��ͽ��ͽ���        �L��    ���ͽ��ͽ���    =��ͽ��ͽ��ͽ���                =��ͽ��ͽ���    ����    ���ͽ���    ����                ���ͽ���    ���ͽ��ͽ���        ����    ���ͽ��ͽ���    ���ͽ��ͽ���    ����            ���ͽ��ͽ��ͽ��ͽ���                =���    �L�ͽ��ͽ���        �L��                    ���ͽ��ͽ��ͽ��ͽ���    ����    �L��                ���ͽ��ͽ��ͽ���    >L��=��;L��=���        ���;L��=��ͽ���        ���ͽ��ͽ���=���    ����=���=��ͽ��ͽ��ͽ��ͽ��;L�ͽ���=���=��ͽ��ͽ��ͽ���=��;L�ͽ��ͽ���    ���;L�ͽ���        ����=���        ���ͽ��ͽ��ͽ���        ����    ���;L�ͽ���                �L��    ����    ���;L�ͽ���    ����                >L��>L��=���=���    ���ͽ��;L�ͽ��ͽ���        ����=���            ����    ���;L�ͽ��;L��        ����    ���ͽ���    =���        ���ͽ���    =���=���    ����    ����    ����                    ����        ����    =���>L��=���>L��>L��>���>���>���>���?   ?��?��?��?L��?L��?L��?fff?fff?�  ?�  ?�  ?���?���?�ff?�ff?�33?�  ?�  ?���?���?���?�ff?�33@   @   @   @   @ff@��@33@33@33@��@   @   @&ff@&ff@,��@333@9��@@  @@  @Fff@L��@S33@Y��@Y��@`  @`  @fff@l��@l��@s33@s33@y��@�33@�ff@�33@�ff@���@���@���@�  @�  @�33@�ff@�ff@���@���@�  @�  @�  @�33@�ff@���@���@���@�  @�  @�33@�ff@���@���@���@�  @�33@�ff@�ff@���@�  @�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@�  @�  @�33@�ff@���@���A   A��A33A33A��AffA  A	��A33A��AffA  A  A��A33A��A��AffA  A��A33A��A��AffA   A!��A#33A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A<��A>ffA@  AC33AD��AFffAH  AK33AL��AP  AQ��AT��AVffAY��A[33A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��At��AvffAy��A{33A|��A�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���Ař�A�33A�  A���A�ffA�33A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A�  Aٙ�A�ffA�  Dp� Dp�fDp��Dp��Dp� Dp��Dp�3DpٚDp�fDp��Dp�3Dq  DqfDq3Dq�Dq  Dq,�Dq33Dq@ DqFfDqL�DqY�Dq` DqffDqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Ds  DsfDs�Ds�Ds  Ds,�Ds33Ds@ DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds��Ds� Ds�fDs�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�DtffDtl�Dts3Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt� Dt�f@&ff@&ff@,��@333@9��@@  @@  @Fff@L��@S33@Y��@Y��@`  @`  @fff@l��@l��@s33@s33@y��@�33@�ff@�33@�ff@���@���@���@�  @�  @�33@�ff@�ff@���@���@�  @�  @�  @�33@�ff@���@���@���@�  @�  @�33@�ff@���@���@���@�  @�33@�ff@�ff@���@�  @�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@�  @�  @�33@�ff@���@���A   A��A33A33A��AffA  A	��A33A��AffA  A  A��A33A��A��AffA  A��A33A��A��AffA   A!��A#33A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A<��A>ffA@  AC33AD��AFffAH  AK33AL��AP  AQ��AT��AVffAY��A[33A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��At��AvffAy��A{33A|��A�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���Ař�A�33A�  A���A�ffA�33A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A�  Aٙ�A�ffA�  Dp� Dp�fDp��Dp��Dp� Dp��Dp�3DpٚDp�fDp��Dp�3Dq  DqfDq3Dq�Dq  Dq,�Dq33Dq@ DqFfDqL�DqY�Dq` DqffDqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Ds  DsfDs�Ds�Ds  Ds,�Ds33Ds@ DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds��Ds� Ds�fDs�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�DtffDtl�Dts3Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt� Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @<��@�  @�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A���A�  B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D D� D D� D D� D D� D D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D&fD&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp��Dq Dq� Dr Dr� Ds Ds� Dt Dt�fDy��D�
�D�Z�D��{D��3D� D�P D��=D��RD�\D�ND���D�ÅD��D�W
Dڐ D�� D� D�B=D�y�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�  >���L�нL��>�  =L�̽L�нL�нL��=L��=L�̽L�нL�нL�нL��>���L��=L��=L�̽L��>�  =L�̽L�нL�нL��=L��=L�̽L�нL�нL�нL�нL��>��>�  �L��=L�̽L�нL��>��=L�̽L�нL�нL��=L�̽L�нL�нL�нL��=L�̽L�нL�нL��=L��>�33=L�̽L��=L��=L��>��=L�̽L��=L�̽L�нL�нL�нL�нL�нL��>��=L�̽L�нL��>���L�нL�нL�нL��>��=L�̽L�нL��=L��>�  >�  �L��=L�̽L�нL��=L�̽L��=L��>���L�нL��>�  >���L��=L�̽L�нL�нL��>��>���L��=L�̽L�нL��=L�̽L�нL��=L�̽L�нL��=L��>�33>�33�L��=L��>�  >��=L�̽L��=L��=L��>���L�нL��>��=L�̽L�нL��>��>�33>�33=L�̽L�нL��=L�̽L�нL��=L��=L�̽L�нL�нL��>���L�нL��>��>�33>�  �L��=L�̽L��=L��>�  =L�̽L�нL�нL��=L�̽L��=L��>��>�33>���L�нL��=L�̽L��>��>���L��>��>��>�  =L�̽L��=L��>��>�  >�33>���L��=L��=L�̽L��>��>�  >�33>�33>�  �L��>��=L��=L��>��>��>��>��=L�̽L��>��>��>��=L�̽L��=L��>��>�  =L�̽L��=L��>�  =L��>��>��>��=L��=L��>�  >�  >�  >�  >��>��>��>��>�  >�  >�  >�  >�  >�  >��>��>��>�  >�33>�  >�  =L��>�33>�  =L��>��>��>��>�  >�  =L��>�  >��>��>��>�  >�33>��>��>��>�  >�  >�  >�  >�33>��>��>�  >��>�  >��>��>�  >��>�  >�  >�  >�  >��>��>�  >��>��>��>�  >�  >��>�  >��>��>��>�  >��>��>��>�  >��>�  >�  >�  >��>��>��>��>��>�  >�  >�  >�  >�33>�  =L��>��>��>�  >�  =L��>�  >�  >�  >�  >�  >��>��>��>��>��>�  >��>�  =L��>�  >�  >�  >�  >��>��>��>��>�  >�ff>�33=L��>�33>�  >�  >��=L��>�33>��>�  >�  >��>��>��>�33>�  >��>�33>�33>��>��>��>��=L��>��>�33>�33>��>��>��>�33=L��>��>��>�  >��=L��>��>�  >�  >��>�33>�  >�  >��>��>��>��>�  >�  >��>�  >��=L��>��>�  >�  >�  >�  =L��>�  >��>�  >��=L��>��>�  >��>�  >�  >�  >�  >�ff>�ff>�33>�33>�  >��>��=L��>��>��>�  >�  >��>�33>�  >�  >�  >��>�  >��=L��>��=L��>�  >�  >��>�  >��>��>�  >�33>�  >�  >��>��>�  >�33>�33>�  >��>�  >��>�  >��>�  >�  >�  >�  >�  >��>�  >�  >��>�  >�33>�ff>�33>�ff>�ff?��?&ff?&ff?&ff?@  ?Y��?Y��?Y��?�ff?�ff?�ff?�33?�33?�  ?�  ?�  ?���?���?�ff?�ff?�33?�  ?�  ?���?���?���@33@	��@  @  @  @  @ff@��@#33@#33@#33@)��@0  @0  @6ff@6ff@<��@C33@I��@P  @P  @Vff@\��@c33@i��@i��@p  @p  @vff@|��@|��@���@���@���@�33@�ff@�33@�ff@���@���@���@�  @�  @�33@�ff@�ff@���@���@�  @�  @�  @�33@�ff@���@���@���@�  @�  @�33@�ff@���@���@���@�  @�33@�ff@�ff@���@�  @�  @�33@�ff@ᙚ@���@�  @�33@�ff@�@���@�  @�  @�33@�ffA ��AffA  A��A33A33A��A
ffA  A��A33A��AffA  A  A��A33A��A��AffA  A��A33A ��A ��A"ffA$  A%��A'33A(��A*ffA,  A-��A/33A0��A2ffA4  A5��A733A8��A:ffA<  A=��A@��ABffAD  AG33AH��AJffAL  AO33AP��AT  AU��AX��AZffA]��A_33AbffAd  Ag33Ah��Al  Am��Ap��ArffAu��Ax��AzffA}��A33A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���AǙ�A�33A�  A���A�ffA�33A���Aϙ�A�ffA�  A���Aә�A�33A�  A���A�ffA�33A�  Aۙ�A�ffA�  Dp� Dp�fDp��DpɚDp� Dp��Dp�3Dp�Dp�fDp��Dq3Dq DqfDq#3Dq)�Dq0 Dq<�DqC3DqP DqVfDq\�Dqi�Dqp DqvfDq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq�3DqɚDq�fDq��Dq�3Dq� Dq�fDr3Dr	�Dr Dr�Dr#3Dr0 Dr6fDr<�DrI�DrP DrVfDrc3Dri�DrvfDr|�Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3Dr�Dr� Dr��Ds3Ds DsfDs�Ds)�Ds0 Ds<�DsC3DsP DsVfDs\�Dsi�Dsp Ds|�Ds�3Ds��Ds�fDs��Ds��Ds� Ds�fDs�3DsɚDs�fDs��Ds�3Ds� Ds�fDt3Dt	�Dt Dt�Dt#3Dt)�Dt6fDt<�DtI�DtP DtVfDtc3Dti�DtvfDt|�Dt�3Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt� Dt�f@6ff@6ff@<��@C33@I��@P  @P  @Vff@\��@c33@i��@i��@p  @p  @vff@|��@|��@���@���@���@�33@�ff@�33@�ff@���@���@���@�  @�  @�33@�ff@�ff@���@���@�  @�  @�  @�33@�ff@���@���@���@�  @�  @�33@�ff@���@���@���@�  @�33@�ff@�ff@���@�  @�  @�33@�ff@ᙚ@���@�  @�33@�ff@�@���@�  @�  @�33@�ffA ��AffA  A��A33A33A��A
ffA  A��A33A��AffA  A  A��A33A��A��AffA  A��A33A ��A ��A"ffA$  A%��A'33A(��A*ffA,  A-��A/33A0��A2ffA4  A5��A733A8��A:ffA<  A=��A@��ABffAD  AG33AH��AJffAL  AO33AP��AT  AU��AX��AZffA]��A_33AbffAd  Ag33Ah��Al  Am��Ap��ArffAu��Ax��AzffA}��A33A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���AǙ�A�33A�  A���A�ffA�33A���Aϙ�A�ffA�  A���Aә�A�33A�  A���A�ffA�33A�  Aۙ�A�ffA�  Dp� Dp�fDp��DpɚDp� Dp��Dp�3Dp�Dp�fDp��Dq3Dq DqfDq#3Dq)�Dq0 Dq<�DqC3DqP DqVfDq\�Dqi�Dqp DqvfDq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq�3DqɚDq�fDq��Dq�3Dq� Dq�fDr3Dr	�Dr Dr�Dr#3Dr0 Dr6fDr<�DrI�DrP DrVfDrc3Dri�DrvfDr|�Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3Dr�Dr� Dr��Ds3Ds DsfDs�Ds)�Ds0 Ds<�DsC3DsP DsVfDs\�Dsi�Dsp Ds|�Ds�3Ds��Ds�fDs��Ds��Ds� Ds�fDs�3DsɚDs�fDs��Ds�3Ds� Ds�fDt3Dt	�Dt Dt�Dt#3Dt)�Dt6fDt<�DtI�DtP DtVfDtc3Dti�DtvfDt|�Dt�3Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt� Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A×�A×�AÙ�AÕ�A×�AÕ�AÑhAÑhAÑhAÑhAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÙ�Aá�Aå�Aô9A×�A��`A���A£�A�dZA���A���A���A�t�A���A�?}A��`A�p�A�JA���A�hsA�O�A�ffA�1A��#A��wA��9A�5?A�G�A��A�5?A�VA��FA�1'A�bA���A�`BA�I�A�$�A��A��wA��A���A���A�n�A��HA��uA�Q�A�M�A� �A�33A���A�~�A�%A��TA�
=A���A�jA���A���A���A��A�^5A�A��HA���A�$�A��`A���A��A�`BA���A��+A��A���A��A�%A�E�A�ȴA�7LA�A�ƨA�~�A��
A��`A���A��#A���A��A���A�VA�jA�v�A��hA���A�l�A~�jAy��Awp�As�wAq+Ap1An5?Am�#Am�Ak��Aj$�Ah��Ah��AgXAc\)AaA`(�A_�A^ffA]C�A[AZ�AY�
AYp�AX��AXȴAX�uAW�hAV�AV5?AS��AP�AO�7AN�yAL�HAKl�AJ�+AI�AG�PAF�`AFJAD��AB�DA?��A?A>bA<ffA;�A:��A9O�A8��A8-A7%A3�wA2�DA0~�A.��A,�`A+|�A)�
A(�jA(~�A'��A'oA&�\A%"�A$JA#C�A"~�A!dZA ~�A �A�TA��A��A�jA�mA�9A�Av�A�A�A��A��A�A�wAhsA/A��A �A|�A�!A  A�AJA��A�7A
ffA	�
A�yAp�A�HA��A9XA�AA�PA��A��A�A��A �R@��`@��@�;d@��@�w@���@�G�@�r�@��@�33@�+@�7L@�F@�n�@�-@��m@�@⟾@�7@��@޸R@�M�@ܴ9@�S�@ڸR@ڰ!@ڰ!@�z�@�^5@ԓu@�9X@��H@�S�@˾w@���@�b@�ƨ@��@�5?@���@�x�@��H@�  @�@���@§�@�^5@��#@�/@�ȴ@��T@��7@��@��j@�Q�@�dZ@��\@���@���@� �@���@��@�^5@�-@���@�X@��9@�1@�C�@�"�@��@��y@��!@�5?@�hs@��@� �@��m@���@��y@���@�?}@��;@��@��@�V@��#@�G�@��@�;d@�o@���@�@�X@�E�@�{@��@���@��7@�?}@��`@��9@���@��u@��@�bN@�I�@�A�@�1'@� �@� �@��;@�S�@��+@�V@�ȴ@�K�@���@�n�@�?}@�%@���@��`@�z�@���@�"�@�^5@�=q@�=q@�=q@�5?@�J@�X@�7L@�Ĝ@��u@��u@��9@�Ĝ@�j@�"�@�v�@�5?@�@���@��^@�^5@�n�@�v�@�;d@�  @�b@�ƨ@��@�z�@���@�l�@��R@�33@�b@�A�@� �@��@��@�A�@��@�r�@�G�@�I�@���@�33@�S�@���@���@�v�@��T@�O�@���@��@��F@���@�\)@��;@�V@�ff@���@�^5@�5?@��@�Z@��F@��F@�l�@���@��@�v�@��T@�7L@��@���@�r�@���@�|�@�t�@�S�@�33@�
=@���@���@��R@�{@�-@��#@�?}@��@��D@�A�@�(�@�b@�  @���@��@���@��P@�\)@�C�@���@�v�@�M�@�=q@�-@�$�@��@��@���@���@��@�hs@�&�@���@��@���@�r�@�bN@�I�@��@�  @��;@��@�+@��@�@��y@��!@��\@�n�@�M�@�M�@�5?@�@���@���@�`B@�7L@~��@x��@pXy@h~@_]�@X,=@RQ@I��@C�@<$@5��@0"h@,��@&�x@!�"@Q�@��@��@Ta@	\�@($G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AÍPA���A���AËDAÏ\A��!A��uA�ffA��`A�r�A��-A�ĜA���A��FA�33A��RA�Q�A���A�t�A�&�AÍPA���A�ffA���A��A�A���A�+A�ȴA� �A��A�"�A×�A�1'A��;A��`A���A��-AÏ\A�?}A���A�E�A��-A�^5A���A���A�
=A��mA�ȴA��A�O�A���AÑhAÅA�VA�I�A�A�A�t�A�"�A�1'A�5?A�/A��A�+A�G�A��A�M�A�|�A�A�33A�VA�\)A�+A�`BA��mA��/A��A��#A���A��`Aå�AÓuA�K�A�p�A��A�-A�`BA���A� �AÍPA�1A���A®AÓuA��-A��A�K�A�ƨA�t�A��Aã�A���A�
=A��^A�I�AÍPA�1A��A�;dA��A�(�A��AìAß�A�t�A�/AÝ�AÙ�AÉ7A�+A�5?A��
A���A�
=A��A�5?AÃA�ƨA���A�=qAÝ�AÛ�AÃA�{A��FA���A��A��PA�hsA��A�z�A��hA��A��A�5?A��wA�ffAÝ�AÕ�A��jA��;A�x�A��yAÕ�AÕ�A��;A��A§�A�/APA�ZAç�AÕ�AÛ�A���A���A�\)A��A�9XAÝ�A�ffA���A×�AÕ�A�;dA�ȴA��+AÙ�AÝ�Aá�Aç�A�dZA�G�A�=qA�%A�(�Aß�Aá�Aã�Aå�A�bNA��AÙ�AÑhA�XAÓuA��AÝ�AÑhA�oA�ffAá�AÝ�A�n�A��DA��A�G�Aç�A�=qA+A�dZAß�A×�A��mA��Aç�Aú^Aé�A�ƨAô9Aé�AîAô9Að!AîAé�Aç�Aã�Aå�Aç�Aå�Aã�Aå�Aã�Aá�Aá�Aß�Aß�AÝ�AÙ�Aß�Aã�AÙ�A�|�Aß�A�dZAç�Aß�Aá�Aß�Að!A�x�Aô9Aå�AìAç�Aá�Aã�Aé�Aã�Aá�Aá�AîAìAò-AìAé�Aé�Aå�Aã�AÝ�Aß�AÝ�Aå�Aá�AÙ�A×�A�;dAá�Að!A�M�AÝ�AÝ�Aå�AÝ�Aã�Aá�Aá�Aß�AÝ�AøRAð!Aá�Aç�A�K�Aã�Að!AöFAô9AÍPAç�Aå�Aå�Aç�Aß�Aß�AìAîAìAîAç�Aá�Aá�AÝ�AìAç�Aç�AÝ�Aá�Aé�Aò-Aã�Aã�Aß�AøRAìAá�AÝ�Aã�Aá�AÝ�Aß�Að!Aç�AìAìAá�Aé�AÝ�AîAËDA���Aå�Að!AÝ�AÛ�AÕ�Aã�Aã�Aã�Aß�Aß�Aå�Aã�Aå�Aá�A��AÝ�Aß�AÛ�AÝ�AÝ�AÛ�Aá�Aß�AÙ�AÛ�Aá�Aç�Aá�AîAÙ�Aé�Aá�Aá�AÛ�AìAé�AÙ�Aã�Aã�Aã�Aß�Aã�Að!AîAå�Aå�Aé�Aã�A×�AÕ�AÙ�AÛ�Aá�Aå�Aß�AÝ�A�|�A×�AÅAÙ�A×�A�x�Aå�AÝ�Aß�AÛ�Aß�Aá�AÝ�AÝ�AÝ�Aß�AÝ�Aß�Aá�AÝ�A���AÙ�AÝ�AÙ�AÕ�A�$�AÙ�AÙ�Aß�AÝ�Aá�Aß�AÛ�AÏ\AÛ�A�33AÛ�AÙ�Aá�AÛ�AÝ�AÛ�Aá�Aß�Aß�Aç�Aã�Aã�Aå�Aç�AÛ�Aá�Aá�Aß�AÝ�Aá�AÛ�AÝ�AÛ�AÝ�AÙ�Aã�AÛ�AÛ�A×�A×�Aß�AÙ�AÕ�A×�AÕ�AÓuAÙ�AÙ�AÛ�AÕ�AÛ�A×�AÙ�AÙ�Aá�AÛ�AÛ�AÝ�Aß�Aß�Aá�Aá�AÙ�Aã�AÝ�AÝ�AÛ�AÝ�Aã�AÛ�A×�AÛ�AÛ�Aå�Aá�AÛ�AÛ�AÛ�AÝ�AÙ�AÛ�AÝ�AÛ�AÝ�AÙ�AÝ�AÝ�AÛ�AÛ�AÙ�A×�Aß�A×�AÛ�A×�AÕ�AÙ�AÙ�AÙ�A×�A×�Aß�Aå�AÝ�AÝ�Aé�AÙ�Aß�Aá�Aá�AÕ�Aá�Aå�Aç�Aá�AìAá�Aò-Að!Aò-Aç�Aå�Aé�AìAîAá�Aá�Aá�Aß�Aå�Aá�Aç�Aá�Aç�Aç�Aá�AìAÝ�Aé�Aç�Aå�Aå�Aé�Aé�Aã�AÝ�Aã�Aß�Aá�Aã�Aá�Aá�Aé�AìAá�Aå�Aã�Aç�Aç�Aã�Aá�Aç�Aá�Aå�Aß�Aå�Aå�Aá�Aá�Aá�AìAã�Aß�Aã�Aç�Aá�AÝ�Aá�Aå�Aå�Aã�Aé�AÝ�Aß�Aç�Aã�Aá�Aá�Aá�Aá�Aß�Aß�Aã�Aá�Aß�Aã�Aã�Aá�Aã�Aß�Aß�Aß�Aß�Aá�AÝ�Aß�A×�AÙ�AÛ�AÝ�Aå�AÝ�AÛ�Aá�AÕ�A×�A×�A×�AÕ�A×�AÙ�A×�Aß�AÕ�AÕ�AÙ�A×�A×�AÙ�AÙ�AÙ�AÛ�AÙ�A×�AÛ�A×�AÛ�AÙ�A×�A×�AÙ�A×�AÙ�A×�AÙ�AÕ�A×�AÙ�A×�AÙ�AÙ�A×�A×�AÙ�AÕ�AÙ�AÙ�A×�AÙ�A×�A×�A×�A×�AÕ�AÓuAÓuAÑhAÑhAÓuAÑhAÑhAÑhAÓuAÑhAÑhAÏ\AÏ\AÏ\AÑhAÏ\AÑhAÏ\AÏ\AÏ\AÑhAÏ\AÑhAÏ\AÏ\AÓuAÑhAÑhAÏ\AÓuAÑhAÑhAÑhAÑhAÑhAÓuAÑhAÓuAÑhAÑhAÑh@�^5@�^5@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�M�@�E�@�M�@�E�@�E�@�E�@�E�@�E�@�M�@�E�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�=q@�5?@�5?@�-@�-@�-@�-@�$�@��@��@��@�$�@�{@�{@�{@�J@�@���@��@��@��T@��T@��T@��#@��#@��#@��#@��#@��#@��#@��#@���@���@���@���@�@��^@��-@���@���@���@���@���@���@���@���@���@���@���@��@�x�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�X@�X@�O�@�O�@�G�@�O�@�G�@�G�@�?}@�?}@�7L@�7L@�/@�/@�/@�/@�/@�/@�&�@�&�@�/@�/A×�A×�AÕ�A×�A×�AÕ�AÙ�AÕ�AÕ�A×�A×�A×�AÕ�AÕ�A×�AÕ�A×�A×�A×�A×�AÕ�A×�A×�AÕ�AÛ�AÙ�AÙ�AÙ�AÙ�A×�AÙ�AÙ�A×�A×�AÙ�AÙ�A×�AÙ�AÙ�AÙ�AÝ�AÙ�AÙ�A×�A×�AÙ�AÛ�AÙ�AÙ�AÙ�AÙ�AÙ�AÙ�AÙ�A×�A×�A×�AÕ�AÕ�AÕ�AÕ�AÕ�A×�AÕ�AÕ�AÕ�A×�AÕ�AÕ�A×�A×�A×�A×�A×�AÕ�A×�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�A×�A×�AÕ�AÕ�AÕ�AÕ�A×�A×�A×�AÕ�AÕ�AÕ�A×�A×�A×�A×�A×�A×�A×�A×�A×�AÙ�A×�A×�A×�A×�AÕ�AÕ�AÕ�A×�A×�AÕ�AÑhAÑhAÑhAÓuAÓuAÓuAÓuAÓuAÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÑhAÓuAÑhAÓuAÓuAÑhAÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÏ\AÑhAÑhAÑhAÑhAÏ\AÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÍPAÑhAÏ\AÑhAÏ\AÏ\AÏ\AÑhAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÑh@�^5@�^5@�V@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�5?@�-@�-@�-@�$�@�$�@��@�$�@��@��@�{@�J@�J@�@���@��@��@��T@��T@��T@��#@��T@��#@��#@��#@��#@��#@���@���@���@���@�@�@��-@���@���@���@���@���@���@���@���@���@���@���@��7@��@�x�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�X@�X@�O�@�O�@�O�@�O�@�G�@�G�@�G�@�?}@�7L@�?}@�/@�7L@�/@�/@�/@�/@�/@�/@�/@�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A×�A×�AÙ�AÕ�A×�AÕ�AÑhAÑhAÑhAÑhAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÙ�Aá�Aå�Aô9A×�A��`A���A£�A�dZA���A���A���A�t�A���A�?}A��`A�p�A�JA���A�hsA�O�A�ffA�1A��#A��wA��9A�5?A�G�A��A�5?A�VA��FA�1'A�bA���A�`BA�I�A�$�A��A��wA��A���A���A�n�A��HA��uA�Q�A�M�A� �A�33A���A�~�A�%A��TA�
=A���A�jA���A���A���A��A�^5A�A��HA���A�$�A��`A���A��A�`BA���A��+A��A���A��A�%A�E�A�ȴA�7LA�A�ƨA�~�A��
A��`A���A��#A���A��A���A�VA�jA�v�A��hA���A�l�A~�jAy��Awp�As�wAq+Ap1An5?Am�#Am�Ak��Aj$�Ah��Ah��AgXAc\)AaA`(�A_�A^ffA]C�A[AZ�AY�
AYp�AX��AXȴAX�uAW�hAV�AV5?AS��AP�AO�7AN�yAL�HAKl�AJ�+AI�AG�PAF�`AFJAD��AB�DA?��A?A>bA<ffA;�A:��A9O�A8��A8-A7%A3�wA2�DA0~�A.��A,�`A+|�A)�
A(�jA(~�A'��A'oA&�\A%"�A$JA#C�A"~�A!dZA ~�A �A�TA��A��A�jA�mA�9A�Av�A�A�A��A��A�A�wAhsA/A��A �A|�A�!A  A�AJA��A�7A
ffA	�
A�yAp�A�HA��A9XA�AA�PA��A��A�A��A �R@��`@��@�;d@��@�w@���@�G�@�r�@��@�33@�+@�7L@�F@�n�@�-@��m@�@⟾@�7@��@޸R@�M�@ܴ9@�S�@ڸR@ڰ!@ڰ!@�z�@�^5@ԓu@�9X@��H@�S�@˾w@���@�b@�ƨ@��@�5?@���@�x�@��H@�  @�@���@§�@�^5@��#@�/@�ȴ@��T@��7@��@��j@�Q�@�dZ@��\@���@���@� �@���@��@�^5@�-@���@�X@��9@�1@�C�@�"�@��@��y@��!@�5?@�hs@��@� �@��m@���@��y@���@�?}@��;@��@��@�V@��#@�G�@��@�;d@�o@���@�@�X@�E�@�{@��@���@��7@�?}@��`@��9@���@��u@��@�bN@�I�@�A�@�1'@� �@� �@��;@�S�@��+@�V@�ȴ@�K�@���@�n�@�?}@�%@���@��`@�z�@���@�"�@�^5@�=q@�=q@�=q@�5?@�J@�X@�7L@�Ĝ@��u@��u@��9@�Ĝ@�j@�"�@�v�@�5?@�@���@��^@�^5@�n�@�v�@�;d@�  @�b@�ƨ@��@�z�@���@�l�@��R@�33@�b@�A�@� �@��@��@�A�@��@�r�@�G�@�I�@���@�33@�S�@���@���@�v�@��T@�O�@���@��@��F@���@�\)@��;@�V@�ff@���@�^5@�5?@��@�Z@��F@��F@�l�@���@��@�v�@��T@�7L@��@���@�r�@���@�|�@�t�@�S�@�33@�
=@���@���@��R@�{@�-@��#@�?}@��@��D@�A�@�(�@�b@�  @���@��@���@��P@�\)@�C�@���@�v�@�M�@�=q@�-@�$�@��@��@���@���@��@�hs@�&�@���@��@���@�r�@�bN@�I�@��@�  @��;@��@�+@��@�@��y@��!@��\@�n�@�M�@�M�@�5?@�@���@���@�`BG�O�@~��@x��@pXy@h~@_]�@X,=@RQ@I��@C�@<$@5��@0"h@,��@&�x@!�"@Q�@��@��@Ta@	\�@($G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AÍPA���A���AËDAÏ\A��!A��uA�ffA��`A�r�A��-A�ĜA���A��FA�33A��RA�Q�A���A�t�A�&�AÍPA���A�ffA���A��A�A���A�+A�ȴA� �A��A�"�A×�A�1'A��;A��`A���A��-AÏ\A�?}A���A�E�A��-A�^5A���A���A�
=A��mA�ȴA��A�O�A���AÑhAÅA�VA�I�A�A�A�t�A�"�A�1'A�5?A�/A��A�+A�G�A��A�M�A�|�A�A�33A�VA�\)A�+A�`BA��mA��/A��A��#A���A��`Aå�AÓuA�K�A�p�A��A�-A�`BA���A� �AÍPA�1A���A®AÓuA��-A��A�K�A�ƨA�t�A��Aã�A���A�
=A��^A�I�AÍPA�1A��A�;dA��A�(�A��AìAß�A�t�A�/AÝ�AÙ�AÉ7A�+A�5?A��
A���A�
=A��A�5?AÃA�ƨA���A�=qAÝ�AÛ�AÃA�{A��FA���A��A��PA�hsA��A�z�A��hA��A��A�5?A��wA�ffAÝ�AÕ�A��jA��;A�x�A��yAÕ�AÕ�A��;A��A§�A�/APA�ZAç�AÕ�AÛ�A���A���A�\)A��A�9XAÝ�A�ffA���A×�AÕ�A�;dA�ȴA��+AÙ�AÝ�Aá�Aç�A�dZA�G�A�=qA�%A�(�Aß�Aá�Aã�Aå�A�bNA��AÙ�AÑhA�XAÓuA��AÝ�AÑhA�oA�ffAá�AÝ�A�n�A��DA��A�G�Aç�A�=qA+A�dZAß�A×�A��mA��Aç�Aú^Aé�A�ƨAô9Aé�AîAô9Að!AîAé�Aç�Aã�Aå�Aç�Aå�Aã�Aå�Aã�Aá�Aá�Aß�Aß�AÝ�AÙ�Aß�Aã�AÙ�A�|�Aß�A�dZAç�Aß�Aá�Aß�Að!A�x�Aô9Aå�AìAç�Aá�Aã�Aé�Aã�Aá�Aá�AîAìAò-AìAé�Aé�Aå�Aã�AÝ�Aß�AÝ�Aå�Aá�AÙ�A×�A�;dAá�Að!A�M�AÝ�AÝ�Aå�AÝ�Aã�Aá�Aá�Aß�AÝ�AøRAð!Aá�Aç�A�K�Aã�Að!AöFAô9AÍPAç�Aå�Aå�Aç�Aß�Aß�AìAîAìAîAç�Aá�Aá�AÝ�AìAç�Aç�AÝ�Aá�Aé�Aò-Aã�Aã�Aß�AøRAìAá�AÝ�Aã�Aá�AÝ�Aß�Að!Aç�AìAìAá�Aé�AÝ�AîAËDA���Aå�Að!AÝ�AÛ�AÕ�Aã�Aã�Aã�Aß�Aß�Aå�Aã�Aå�Aá�A��AÝ�Aß�AÛ�AÝ�AÝ�AÛ�Aá�Aß�AÙ�AÛ�Aá�Aç�Aá�AîAÙ�Aé�Aá�Aá�AÛ�AìAé�AÙ�Aã�Aã�Aã�Aß�Aã�Að!AîAå�Aå�Aé�Aã�A×�AÕ�AÙ�AÛ�Aá�Aå�Aß�AÝ�A�|�A×�AÅAÙ�A×�A�x�Aå�AÝ�Aß�AÛ�Aß�Aá�AÝ�AÝ�AÝ�Aß�AÝ�Aß�Aá�AÝ�A���AÙ�AÝ�AÙ�AÕ�A�$�AÙ�AÙ�Aß�AÝ�Aá�Aß�AÛ�AÏ\AÛ�A�33AÛ�AÙ�Aá�AÛ�AÝ�AÛ�Aá�Aß�Aß�Aç�Aã�Aã�Aå�Aç�AÛ�Aá�Aá�Aß�AÝ�Aá�AÛ�AÝ�AÛ�AÝ�AÙ�Aã�AÛ�AÛ�A×�A×�Aß�AÙ�AÕ�A×�AÕ�AÓuAÙ�AÙ�AÛ�AÕ�AÛ�A×�AÙ�AÙ�Aá�AÛ�AÛ�AÝ�Aß�Aß�Aá�Aá�AÙ�Aã�AÝ�AÝ�AÛ�AÝ�Aã�AÛ�A×�AÛ�AÛ�Aå�Aá�AÛ�AÛ�AÛ�AÝ�AÙ�AÛ�AÝ�AÛ�AÝ�AÙ�AÝ�AÝ�A×�A×�AÕ�A×�A×�AÕ�AÙ�AÕ�AÕ�A×�A×�A×�AÕ�AÕ�A×�AÕ�A×�A×�A×�A×�AÕ�A×�A×�AÕ�AÛ�AÙ�AÙ�AÙ�AÙ�A×�AÙ�AÙ�A×�A×�AÙ�AÙ�A×�AÙ�AÙ�AÙ�AÝ�AÙ�AÙ�A×�A×�AÙ�AÛ�AÙ�AÙ�AÙ�AÙ�AÙ�AÙ�AÙ�A×�A×�A×�AÕ�AÕ�AÕ�AÕ�AÕ�A×�AÕ�AÕ�AÕ�A×�AÕ�AÕ�A×�A×�A×�A×�A×�AÕ�A×�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�A×�A×�AÕ�AÕ�AÕ�AÕ�A×�A×�A×�AÕ�AÕ�AÕ�A×�A×�A×�A×�A×�A×�A×�A×�A×�AÙ�A×�A×�A×�A×�AÕ�AÕ�AÕ�A×�A×�AÕ�AÑhAÑhAÑhAÓuAÓuAÓuAÓuAÓuAÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÑhAÓuAÑhAÓuAÓuAÑhAÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÏ\AÑhAÑhAÑhAÑhAÏ\AÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÍPAÑhAÏ\AÑhAÏ\AÏ\AÏ\AÑhAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÑh@�^5@�^5@�V@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�5?@�-@�-@�-@�$�@�$�@��@�$�@��@��@�{@�J@�J@�@���@��@��@��T@��T@��T@��#@��T@��#@��#@��#@��#@��#@���@���@���@���@�@�@��-@���@���@���@���@���@���@���@���@���@���@���@��7@��@�x�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�X@�X@�O�@�O�@�O�@�O�@�G�@�G�@�G�@�?}@�7L@�?}@�/@�7L@�/@�/@�/@�/@�/@�/@�/@�/A×�A×�AÕ�A×�A×�AÕ�AÙ�AÕ�AÕ�A×�A×�A×�AÕ�AÕ�A×�AÕ�A×�A×�A×�A×�AÕ�A×�A×�AÕ�AÛ�AÙ�AÙ�AÙ�AÙ�A×�AÙ�AÙ�A×�A×�AÙ�AÙ�A×�AÙ�AÙ�AÙ�AÝ�AÙ�AÙ�A×�A×�AÙ�AÛ�AÙ�AÙ�AÙ�AÙ�AÙ�AÙ�AÙ�A×�A×�A×�AÕ�AÕ�AÕ�AÕ�AÕ�A×�AÕ�AÕ�AÕ�A×�AÕ�AÕ�A×�A×�A×�A×�A×�AÕ�A×�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�AÕ�A×�A×�AÕ�AÕ�AÕ�AÕ�A×�A×�A×�AÕ�AÕ�AÕ�A×�A×�A×�A×�A×�A×�A×�A×�A×�AÙ�A×�A×�A×�A×�AÕ�AÕ�AÕ�A×�A×�AÕ�AÑhAÑhAÑhAÓuAÓuAÓuAÓuAÓuAÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÑhAÓuAÑhAÓuAÓuAÑhAÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÏ\AÑhAÑhAÑhAÑhAÏ\AÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÑhAÑhAÑhAÑhAÑhAÑhAÑhAÏ\AÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÍPAÏ\AÏ\AÏ\AÍPAÑhAÏ\AÑhAÏ\AÏ\AÏ\AÑhAÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÏ\AÑhAÑh@�^5@�^5@�V@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�5?@�-@�-@�-@�$�@�$�@��@�$�@��@��@�{@�J@�J@�@���@��@��@��T@��T@��T@��#@��T@��#@��#@��#@��#@��#@���@���@���@���@�@�@��-@���@���@���@���@���@���@���@���@���@���@���@��7@��@�x�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�X@�X@�O�@�O�@�O�@�O�@�G�@�G�@�G�@�?}@�7L@�?}@�/@�7L@�/@�/@�/@�/@�/@�/@�/@�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�7v>$��>'?�@��:@��F=��>j�>a�>poi@��@>#'g=�R�=��?E�?�R @��>�I?�xW>� �@kR�@���@Rt =���>?5?��2@���@���=���>�l=���=��	>a \@��@��>'|F@�/Z=��q>7�@���=ؓu>T��>�?��|=�{�=��=��=�:�>��o=��U?�֌=���>6��@��c@��)>��?��k@��s@���@���>�P@(�>�|?�{�@)F�>,*E@���>U\?&�b@�� ?�h>`-?��D@���=���?'`�=֡b?	�@��g=��#=���>�;�@��V@��F@���>5in@�(�=�t~>x��>�@�> n@���?��`>U�?�J@��c?��b>A��@��V>)�>�9>к@��@JI�>v�b@�*>�@���@5�=��>�@@9m�=�GE>gڥ@��o@��^@��c>7>�@��@���@���=�MU>���@B�@do@��$=�B�>u�@���=�f{>	Q�?�@���@��6@���>"�>?)?�a=�P�>Ɇ@{c@C�?l >&@���>�$_@��=�ؙ?W�	@���@���@>*o>T,=@���?�͊@��@��=ժ�>�h@V�>"�@���>R@��o@��@��,?�5+>��@��@��c>^�@���@]��?��@���@���@��^>1>gŬ@��'@��'@���@��
@�G�=�>W
=@���>Y��@��@��'@��'@��,@[t�?�3@���@���@���@���>HXy@��H@���?�A�>A-w@���@��M@��<>���?AJ?�W�@���@��]@�F�@�.@��M@���>W�]@���@���@��8@��$?Ф@��@��H@���@���@���@��D@��U@���@���@��D@���@���@���@���@���@���@��H@��D@��D@��H@��8@���@���@���?�L0@��H@��{@��D@���@��D@��@��@��@���@���@���@���@��D@��D@��D@��H@���@���@���@���@��@��H@���@���@��H@��H@��H@��H@��H@���@���@��8@���@z�K@���@���@���@��@��D@��D@���@��U@��H@��H@���@��U@��U@���@��D@���@��W@���@��e@�¹@��U@���@��@��@��H@��@���@���@���@���@���@���@��D@���@���@���@��D@��H@���@��H@���@���@���@��D@��H@���@���@���@���@���@���@���@���@���@���@���@��M@���@���@���@���@���@��V@R��@���@��H@��8@���@�N{@��M@���@���@���@���@��H@���@��8@���@T��@���@���@���@��8@���@��'@��'@���@���@���@��'@���@���@���@���@���@��@���@��H@���@���@���@��8@���@��D@��Y@��8@���@��H@���@���@���@���@���@���@��'@���@���@���@���@���@��<@��'@��<@���@���@��,@���@���@��H@���@��'@���@���@���@���@��H@���@���@���@���@���@��8@���@���@��'@��B@��8@���@���@��H@���@���@���@���@��o@�w@���@���@���@���@���@��8@��@��H@��H@��D@���@���@��Y@��8@��'@���@��H@���@���@���@���@��8@��8@���@���@��@��'@��'@��'@���@���@��'@��@��o@��,@��,@��o@��,@��@���@��'@���@��,@��'@��@���@��<@��@��@���@���@���@��<@���@��8@���@���@���@��<@��<@��,@���@��8@���@���@��M@���@��<@���@��M@���@���@���@���@���@��Y@��H@���@���@���@��@���@��<@���@���@��<@��M@���@���@��@��@��Y@���@���@��@��H@���@���@��H@��@��H@���@��@��j@���@��Y@��j@��v@��z@���@���@��@���@��z@���@���@��Y@���@���@��@��@���@��z@��z@��@��"@��"@��"@���@��"@��z@���@��z@���@��z@���@�Ë@���@���@���@��'@���@���@��"@��"@���@���@���@���@���@��"@�Ë@��3@�Ë@�ŗ@��3@���@�Ë@��"@��3@���@��"@��"@�Ĝ@�Ë@��z@�@���@��7@�Ĝ@��3@�Ë@�Ë@�Ĝ@���@���@���@���@���@���@�Ë@�Ĝ@���@�Ë@�Ë@�Ë@�Ë@�Ë@��7@��7@�à@�à@�à@�à@���@���@���@�@���@��7@��7@�@���@��@��n@��+@��+@��@��+@���@���@��;@��;@���@���@�@��;@��;@�¤@�à@���@���@�à@��	@��	@��L@���@��L@��L@�à@��L@��L@���@���@��L@��L@�à@��L@���@�õ@��L@�õ@��L@��L@��L@�õ@��L@��L@��@��@��L@��L@��L@��@�¹@��@��@��@��@�¹@��@��a@��a@��@��@��a@��@��a@���@���@��r@���@��@��r@��@��r@��r@��r@�ł@���@���@��.@��.@��.@��.@���@��?@��?@�Ɠ@���@���@���@�Ǥ@��;@���@PtT@Pt @Ps�@Ps�@Pr�@Pr�@Ps@PsX@Ps.@Ps@Ps�@Ps�@PsX@Ps�@Ps�@Ps�@Ps�@Ps�@Ps�@Ps�@Ps�@Ps�@Ps�@Pt @Ps�@Pt*@Pt*@Pt*@Pt*@Ps�@Ps�@Ps�@Ps�@Ps.@Ps.@Pr�@Pr�@Pr�@Pr2@Pq�@Pq7@Pp�@Pp�@Pp;@Po�@Po?@Po?@Pn�@Pn�@PnD@PmH@Pl�@Pl�@Pl�@Pk�@Pj�@PiY@Pi@Ph^@Ph
@Pg�@Pg�@Pg�@Pg�@Pg�@Pg�@Pg�@Pf�@Pf�@Pf�@Pe�@Pe@Pd�@Pd@Pb�@Pa�@Pa|@PaR@P`�@P`�@P`�@P`W@P`�@P`@P_[@P^�@P]�@P\@P[l@PZ�@PZ�@PY�@PY�@PY�@PY�@PYK@PX�@PW�@PW~@PW @PV�@PV�@PV@PU�@PU�@PU�@PU\@PT�@PT�@PT7@PT7@PT@PS�@PS�@PS�@PS�@PS�@PS�@PTa@PT7@��V@��A@��@��@��@���@��,@��A@���@���@��A@���@��@���@��=@���@��@��V@��@��k@���@���@��(@��@��@��8@��#@��@��@��@���@��@��@��@��8@��M@���@��b@��w@��b@��I@���@��@��8@��@���@��@��8@��@��8@��8@��@��@��M@���@���@��@��=@���@���@��@���@��#@��(@��|@��g@��g@��@��@���@��#@��8@���@��b@���@���@���@��@���@���@��8@��@��#@��w@���@��8@��#@���@��8@��8@��#@��b@��b@��w@��b@��b@��@��@���@���@���@��
@��@��s@���@���@���@��@���@��@���@��@���@��@���@��I@��s@��@���@��R@��R@��@��@��M@��b@��b@��@���@��@���@��k@��@���@��@��=@���@��@��@��@���@��g@��@���@���@���@���@���@���@��@���@��@��4@��s@��4@���@��@��^@��^@��^@��s@��^@���@���@���@���@���@���@��@���@��@��@��0@��@���@��o@��o@��o@��o@���@��0@��@���@��@��@���@���@��o@��0@��Z@��o@��Z@��o@���@���@���@���@���@���@��+@��@���@��@��j@��j@��@���@���@���@��@��<@��'@��f@��{@���@���@���@���@���@P��@P�+@P��@P�/@P��@P��@P��@P��@P��@P��@P��@P�@P�/@P�/@P�/@P�Y@P�/@P�Y@P�@P�Y@P�Y@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�Y@P��@P��@P�Y@P��@P�@P��@P�@P�^@P��@P�b@P�@P��@P��@P��@P��@P�A@P�@P�k@P��@P�@P��@P�I@P�I@P�x@P��@P��@P�@P��@P��@P��@P�c@P�c@P�9@P��@P�9@P�@P�9@P��@P�h@P��@P��@P��@P�F@P��@P��@P�}@P�}@P��@P��@P��@P��@P��@P��@P��@P�\@P�a@P��@P�D@P�@P��@P��@P��@P�L@P�L@P��@P�"@P��@P��@P��@P�Y@P�@P�^@P��@P�^@P��@P�b@P��@P��@P��@P�@P�@P�k@P�@P�k@P�k@P�A@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  34433444434444434443334443344444334344344444444444443344333444444344344434444344433343444434443443444334343444444333433344443443444333444443444343443344343344343433344334334333443333344343333343333433443334443334334333343333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�7uG�O�G�O�@��:@��FG�O�G�O�G�O�G�O�@��BG�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@kR�@���@Rs�G�O�G�O�G�O�@���@���G�O�G�O�G�O�G�O�G�O�@��@��G�O�@�/ZG�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��b@��*G�O�G�O�@��p@���@���G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@��G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�@��mG�O�G�O�G�O�@��W@��F@���G�O�@�(�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�@��fG�O�G�O�@��VG�O�G�O�G�O�@��@JI�G�O�@�*G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�@��o@��^@��bG�O�@��@���@���G�O�G�O�G�O�G�O�@��&G�O�G�O�@���G�O�G�O�G�O�@���@��7@���G�O�G�O�G�O�G�O�G�O�@{cG�O�G�O�G�O�@���G�O�@��G�O�G�O�@���@���G�O�G�O�@���G�O�@��@��G�O�G�O�@V�G�O�@���G�O�@��r@��@��-G�O�G�O�@��@��`G�O�@���@]��G�O�@���@���@��^G�O�G�O�@��)@��*@���@��@�G�G�O�G�O�@���G�O�@��@��'@��)@��*@[t�G�O�@���@���@���@���G�O�@��K@���G�O�G�O�@���@��M@��=G�O�G�O�G�O�@���@��b@�F�G�O�@��O@���G�O�@���@���@��:@��'G�O�@��@��I@���@���@���@��A@��V@���@���@��E@���@���@���@���@���@���@��K@��D@��D@��H@��9@���@���@���G�O�@��I@��@��C@���@��F@�� @��@��@���@���@���@���@��E@��G@��G@��G@���@���@���@���@��@��I@���@���@��F@��G@��K@��I@��K@���@���@��9@���@z�K@���@���@���@���@��E@��E@���@��V@��I@��K@���@��V@��S@���@��E@���@��[@���@��e@�»@��R@���@��@��@��K@��@���@���@���@���@���@���@��F@���@���@���@��E@��K@���@��H@���@���@���@��G@��G@���@���@���@���@���@���@���@���@���@���@���@��O@���@���@���@���@���@��W@R��@���@��J@��9@���@�Nz@��P@���@���@���@���@��K@���@��7@���@T��@���@���@���@��7@���@��)@��*@���@���@���@��(@���@���@���@���@���@���@���@��L@���@���@���@��<@���@��I@��Y@��<@���@��G@���@���@���@���@���@���@��*@���@���@���@���@���@��;@��(@��=@���@���@��+@���@���@��I@���@��)@���@���@���@���@��I@���@���@���@���@���@��:@���@���@��)@��B@��6@���@���@��H@���@���@���@���@��s@�y@���@���@���@���@���@��6@��@��I@��L@��C@���@���@��Z@��:@��'@���@��I@���@���@���@���@��7@��7@���@���@���@��(@��)@��'@���@���@��'@��~@��r@��.@��0@��n@��-@���@���@��(@���@��*@��)@��@���@��>@��~@��@���@���@���@��:@���@��7@���@���@���@��>@��A@��.@���@��=@���@���@��L@���@��:@���@��N@���@���@���@���@���@��W@��N@��S@��A@��@��@��@���@��.@��C@���@���@��A@���@��@���@��<@���@��@��Z@��@��m@���@���@��,@��@��@��;@��&@��@��@��@���@��@��@��@��8@��N@���@��b@��z@��e@��F@���@��@��9@��@���@��@��8@��@��:@��;@��@��@��K@���@���@��@��=@���@���@��@���@��#@��$@��{@��i@��k@��@��@���@�� @��8@���@��a@���@���@���@��@���@���@��:@��@��%@��v@���@��8@��(@���@��<@��:@��&@��g@��d@��z@��b@��b@��@��@���@���@���@��@��@��q@���@���@���@��@���@��@���@��@���@��@���@��J@��n@��@���@��P@��P@��@��@��N@��b@��e@��@���@��@���@��m@��@���@��@��A@���@��@��@��@���@��f@��@���@���@���@���@���@���@��@���@��@��1@��t@��4@���@��@��\@��`@��^@��t@��]@���@���@���@���@���@���@��@���@��@��@��.@��@���@��n@��i@��o@��l@���@��0@��@���@��@��@���@���@��n@��3@��V@��r@��Z@��q@���@���@���@���@���@���@��,@��@���@��@��k@��j@���@���@���@���@��@��=@��*@��f@��~@���@���@���@���@���@P��@P�(@P��@P�.@P��@P��@P��@P��@P��@P��@P��@P�@P�3@P�+@P�0@P�Z@P�3@P�Z@P�@P�]@P�U@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�[@P��@P��@P�[@P��@P�@P��@P�@P�^@P��@P�`@P�@P��@P��@P��@P��@P�C@P�@P�j@P��@P�@P��@P�J@P�K@P�{@P��@P��@P�@P��@P��@P��@P�f@P�c@P�:@P��@P�8@P�@P�;@P��@P�j@P��@P��@P��@P�F@P��@P��@P�~@P��@P��@P��@P��@P��@P��@P��@P��@P�`@P�^@P��@P�F@P�@P��@P��@P��@P�N@P�M@P��@P�"@P��@P��@P��@P�Z@P� @P�^@P��@P�`@P��@P�c@P��@P��@P��@P�@P�@P�j@P�@P�f@P�n@P�E@P��@P��@��S@��A@��@��@��@���@��.@��C@���@���@��A@���@��@���@��<@���@��@��Z@��@��m@���@���@��,@��@��@��;@��&@��@��@��@���@��@��@��@��8@��N@���@��b@��z@��e@��F@���@��@��9@��@���@��@��8@��@��:@��;@��@��@��K@���@���@��@��=@���@���@��@���@��#@��$@��{@��i@��k@��@��@���@�� @��8@���@��a@���@���@���@��@���@���@��:@��@��%@��v@���@��8@��(@���@��<@��:@��&@��g@��d@��z@��b@��b@��@��@���@���@���@��@��@��q@���@���@���@��@���@��@���@��@���@��@���@��J@��n@��@���@��P@��P@��@��@��N@��b@��e@��@���@��@���@��m@��@���@��@��A@���@��@��@��@���@��f@��@���@���@���@���@���@���@��@���@��@��1@��t@��4@���@��@��\@��`@��^@��t@��]@���@���@���@���@���@���@��@���@��@��@��.@��@���@��n@��i@��o@��l@���@��0@��@���@��@��@���@���@��n@��3@��V@��r@��Z@��q@���@���@���@���@���@���@��,@��@���@��@��k@��j@���@���@���@���@��@��=@��*@��f@��~@���@���@���@���@���@P��@P�(@P��@P�.@P��@P��@P��@P��@P��@P��@P��@P�@P�3@P�+@P�0@P�Z@P�3@P�Z@P�@P�]@P�U@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�[@P��@P��@P�[@P��@P�@P��@P�@P�^@P��@P�`@P�@P��@P��@P��@P��@P�C@P�@P�j@P��@P�@P��@P�J@P�K@P�{@P��@P��@P�@P��@P��@P��@P�f@P�c@P�:@P��@P�8@P�@P�;@P��@P�j@P��@P��@P��@P�F@P��@P��@P�~@P��@P��@P��@P��@P��@P��@P��@P��@P�`@P�^@P��@P�F@P�@P��@P��@P��@P�N@P�M@P��@P�"@P��@P��@P��@P�Z@P� @P�^@P��@P�`@P��@P�c@P��@P��@P��@P�@P�@P�j@P�@P�f@P�n@P�E@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  34433444434444434443334443344444334344344444444444443344333444444344344434444344433343444434443443444334343444444333433344443443444333444443444343443344343344343433344334334333443333344343333343333433443334443334334333343333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�2�9�2�9�2{9�29�39�2T9�2�9�2�9�29�3e9�2�9�3Q9�2�9�2A9�3�9�2d9�2+9�2�9�39�2�9�3R9�3c9�3�9�2�9�4�9�4�9�4{9�4�9�49�49�5=9�49�49�4e9�4�9�4�9�4=9�4�9�4�9�4�9�5�9�4T9�49�4�9�49�4Q9�59�4�9�4�9�4�9�4�9�59�4f9�4�9�4P9�4P9�4�9�3�9�3U9�3R9�3�9�3e9�4x9�3�9�3�9�3�9�3�9�49�49�4Q9�4u9�4�9�4N9�4�9�4@9�4Q9�4S9�49�4=9�4@9�4�9�49�4z9�4�9�4B9�4�9�4|9�59�4�9�4�9�4{9�4�9�4�9�4�9�4�9�4�9�4�9�4�9�59�5>9�5:9�5R9�5d9�5�9�5�9�5�9�5�9�5�9�5�9�5�9�69�5�9�5<9�59�5=9�5�9�5�9�5a9�5)9�3�9�3�9�49�4i9�4�9�4�9�4�9�4�9�3P9�39�3>9�2�9�39�3?9�3y9�3�9�4A9�3�9�3�9�49�4*9�3�9�49�5)9�5�9�5�9�5�9�5�9�69�6Q9�5<9�5a9�5t9�5�9�5w9�4Q9�5`9�5�9�5�9�5�9�5�9�5�9�5�9�5�9�5�9�69�69�69�6Q9�6$9�6N9�6O9�6a9�6Q9�6*9�6�9�6�9�6�9�6�9�6%9�6c9�6S9�69�6O9�6O9�6)9�6�9�6�9�6f9�6�9�6�9�6�9�6�9�6�9�6�9�6�9�6�9�6�9�6�9�7O9�7=9�79�779�7�9�7�9�7�9�7�9�7�9�8 9�8*9�8O9�8=9�8u9�8�9�8�9�8�9�8�9�8�9�8�9CK�9CK9CJ�9CJ!9CI�9CI�9CI�9CI�9CI�9CI�9CI�9CI�9CJ&9CJ9CJ#9CJK9CJ&9CJK9CI�9CJM9CJF9CJ�9CJ�9CJ�9CJ�9CJq9CJq9CJ�9CJ�9CJ�9CJ�9CJL9CJ�9CI�9CJL9CI�9CI�9CI�9CI�9CI_9CH�9CHq9CH!9CG�9CG�9CG�9CF�9CFw9CFP9CF�9CF9CEa9CD�9CD�9CD�9CC�9CB*9CA9C@�9C@U9C@*9C??9C?9C?9C>�9C?@9C>�9C>�9C>�9C>�9C>/9C=�9C=�9C<�9C<.9C:�9C9�9C9�9C9�9C8�9C8�9C8�9C8�9C8�9C8�9C89C7�9C6�9C4^9C3�9C2�9C2�9C2^9C299C1�9C1�9C1�9C1�9C0�9C/�9C/O9C/&9C.�9C.:9C.�9C.<9C-�9C-O9C,�9C,�9C+�9C,9C,9C+v9C+'9C+r9C+z9C+S9C+�9C+�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B  BB�BW
B��B��B��B�{B��B�B�9B�^BÖB��B�/B�B�B��B��B��B  B+BDBDB
=B\B�B�B�B�BVBPBbB�B�B�B�B �B+B1'B0!B0!B+B�B�B{B�BhB�mB�/BBÖB�FB��B��B��B�Bk�BR�BI�BE�B8RB�BB��BǮB��B�uB{�Bn�B_;B_;BffBZB%�B�B
=B
��B
�B
�B
�fB
�/B
��B
�dB
�'B
��B
�{B
y�B
VB
G�B
9XB
+B
!�B
�B
B	�HB	��B	�jB	�B	��B	��B	��B	��B	�\B	�B	{�B	x�B	m�B	ZB	P�B	G�B	C�B	>wB	7LB	-B	$�B	�B	�B	�B	�B	�B	�B	oB	JB	B�B�B�B�mB�mB�ZB�5B�B�B��B��BŢBǮBȴBƨB��B�wBĜB��B�wB�dB�3B��B��B��B��B��B�oB�\B�uB��B�uB�hB�bB�bB�\B�PB�=B�%B�B�B�B�B�B� B|�By�Bx�By�By�Bw�Bx�Bw�Bv�Bv�Bu�Bs�Bs�Br�Bq�Bp�Bn�Bl�Bk�BiyBdZBffBffBR�BO�BO�BO�BM�BP�BR�BS�B_;BjBy�Bv�BhsBN�BD�B=qB>wB>wBA�BB�BB�BD�BD�BB�B?}B=qB=qB>wBC�BJ�BJ�BI�BI�BJ�BI�BG�BF�BG�BH�BF�BD�BC�BD�B?}BC�BI�BK�BL�BM�BP�BZB_;B_;B^5BaHBiyBk�Bl�Bl�Bl�Bm�Bm�Bs�Bu�Bv�Bx�By�Bz�B�B�B�B�%B�%B�1B�=B�PB�\B�bB�{B��B��B��B��B�B�!B�LB�^B�^B�dB�jB�}BBÖBBĜBǮB��B��B��B�
B�B�B�#B�#B�/B�NB�fB�B��B��B��B��B��B	B	B	B	B	B	+B	JB	PB	PB	JB	\B	bB	hB	uB	�B	�B	�B	�B	"�B	"�B	"�B	#�B	#�B	#�B	$�B	%�B	)�B	,B	.B	/B	/B	1'B	49B	7LB	;dB	?}B	D�B	K�B	N�B	M�B	I�B	K�B	L�B	M�B	N�B	S�B	\)B	_;B	bNB	jB	q�B	r�B	r�B	m�B	hsB	gmB	gmB	iyB	n�B	v�B	x�B	y�B	}�B	�B	�B	�+B	�B	{�B	{�B	�1B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�B	�B	�B	�B	�'B	�-B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�9B	�LB	�XB	�jB	�wB	�wB	�}B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ȴB	ȴB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	��B	�DB
�B
B
B
&B
.B
5?B
<�B
C{B
K�B
O�B
V�B
X�B
\�B
aB
c�B
iDB
n}B
tTB
xRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
[8?UZK?W4�B�B��?>:?��s?' �?��B�G?Z��?Q�?�=@���@��uB�{?D��@�^�?�~�A�toB��A��?U ?}�)A5��B��B�>�)?3�M>���?#A�?�w�B��B��?[�B��?
��?n�nB�J?�2?��M?K�{@�a>�U�>�r>�\�?>?�5�?$f�Aa'?%�?m��B�mB��?=/^@�!>A��^A��B�	?*Z%A�"?-�{@�,�A���?c"[A��2?F3�@h	�B�d@N�m?'�UA15B֋?�@q�?	�U@R�B��>�>?#�?��FB�5B�TB��?plUB ;�?�?���@$h�?O��B�gA�?-�@A7�yB�?A"�+?|ԉB�[?av�?.�T@�B��A��?��1A�b�?I+NB��A��,?Q�?���A��I?�H?��lB��B�XB�2?m��B�,B�(BŨ?Iw?�iA�.Ak�B��?#}?�I�B�? �Y?/��@U� B�'B��B�v?U��?y�~A=%�?��?@�Aʟ�AO�;@�7.?:�[A���@%�B�?$�&@���B��B��A�c�?�]�B�A_�B	&
B�?	?G%A��?Q!�BH?:�!B�@B�B��@�&�@ �B:!B�?��(B��A���@�.:B��B��A��&?+S?��B�sB��B��B��A���? �j?�prB�T?�3	B�/B�0B�\B��A�O�@D�B�B�UA�o�Bĕ?�H�B��B�M@�W?z��B��B��B�D@�@P�HAa-B�pBM�AڗAG٩B�!B�	?�%�B]?B�!B��B��A@�B��B��B��B�NB��B�YB��B�pB��B��B�B��B±B�4B¹BB�8B��B��B��B�aBêB��Bİ@�e�B�B�~B��B�JB�BĩB�5B��B�BB�<B��B�_B�#B�WB��B�cB��B�~B�B�wB�qB�:B��B��B��B�kB��B��B��B��B��B�aBđA���B��B��B�B�4BļB��B�B�WB�/B�8B�RBżB�SB��B�#B�!B�B¹B��B�mB��B��B�oB�<B��B�oB�JB�JB��B��B�wB��B��BB��B�fB�B��B�,B��B�iB��B�B�WB�kB�SB��B��B��B�4B� BB�eB�JB�6B�hB�NB��B��B��B�zB��B	�A��RB�@B��B��B�;B��B�oB�	B��B��B��B��B��B��B��A���B�qB�WB��B��B� BB�AB�gB��B�>B�9B�!B��B��B�B�iB��B��BĝB��B��B��B�cB¹B�FB��B�cB�?B�mB�QB�QB��B�BęB�]B�jB�WB��B�HB�_B�{BΤB�?B�rBĸB��B�FB�EB�B��B� B��B��B�B�B�}B��B�]B�BB��B��B�B�rBB�B��BCB�jB�B��B��B��B��B��B�B�
B��B��B��B�~B�:B�+BÝB�MB��B��B��B±B¹B��B��BB��B�/B¢B�B��B�FB��BÕB�+B�B��BBB�.B��B��B�bB�UBB�B��B��B�]B��BB�tBĵB�CB�/B®BôB�fB��B�aB��B�)B��B�!B��B�cB�B��B�B��B�+B»B�qB�B��B�@B� B��B� BB��BB��B�6B�iB��B��B��B�kB�B��B¥B�jB�EB��B��B� B�_B¥BåBŷBŷB��B��B��B�8B��B�zB�B��B��BŗB�
B��B�VB�B��BªB�*B�B��B��B��B��B��B�B��B�pB¾BÂB�cB��B�?B�MB��B�}B��B��B�aB�?B�[B�yB¿B��B�B�,B��B�B��B�9B�eB�wB�B��B�TB�JB�nB�B�|B�tB�B�B�B�B��B�B�\B�B�GB��B��B��B��BÏB�PB��B�BùB�fB�B�YB��B��B�zBĥB��B�JB�B�B��B�	B�kB�aB�BB�IB��B��B�cB�@B��B�!B�B�B�3B�B�RB�_B�;B­B�&B��B�pB�zB��B�xB�TB�CBÊB��B��B��B�B��B�dB�\B�OB��BüB�AB�B�bBÅB��B�B�nB�NB�VB�`B�$B�aB�>B��B��BÒB�BĔB�B��B�NB�"B·B��B�kBB�B��B�cB�'B�B��B��B��B��B�mB��B� B��B��B��B�cBĩB��B�B�B�vBŀB�)B�gB��BŰB�xBŗB��B�B��B�B�,B��B��BčB�\BŢB�fBĽBŁB�B�gB�_BƥB�B��B�B�rB�BǧB	�B	��B	�zB	�pB	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�lB	��B	�_B	�tB	�WB	�yB	�?B	�pB	�UB	�HB	�+B	��B	��B	��B	�qB	�B	�B	��B	�yB	�lB	�B	��B	�QB	�	B	��B	�fB	�B	��B	��B	�OB	�BB	��B	��B	��B	�TB	�JB	��B	��B	��B	��B	�,B	��B	��B	�B	�rB	�8B	�+B	�B	�B	�\B	�#B	�&B	�B	��B	��B	�*B	�UB	�UB	�:B	�B	��B	��B	�aB	�B	�FB	��B	�9B	��B	��B	��B	�HB	��B	çB	�B	��B	��B	��B	�TB	�	B	�TB	��B	B	�IB	�B	÷B	�mB	�qB	�EB	��B	ÓB	ĊB	�B	�B	��B	ĀB	ąB	�:B	�KB	�aB	�SB	ąB	�YB�<B�(B��B�zB�qB�qB�&B��B�B��B��B�B�B�3B��B�IB�AB��B�%B��B�B�RB�B��B�B��B�B��B�B��B�3B��B��B�B�hB�|B��B�B��B�vB�B�B��B�B�B��B��B�%B�kB�B�B�yB��B�B�{B�{B��B�B�/B�'B��B�)B�gB�@B�B�jB�B��B��B�B�,B�8B��B�NB��B��B��B�dB�B�{B��B�BB��B��B�YB��B�B�TB��B�B�jB��B��B��B�B�B��B��B��B�B�B�!B�,B�rB��B��B��B�dB�B�SB�B�BB�B�1B�dB��B��B�B�&B�9B�0B�B��B�%B�0B�B�RB�>B��B��B�B�B��B��B�8B��B�eB�pB�_B�~B�B�=B�B�/B�eB�)B�LB�bB�bB�
B�TB�_B�B�FB�B�B�B��B�*B�,B�B�NB�
B�B�KB�CB�2B�dB�,B�CB�B�EB�!B��B�VB�EB�=B�,B�zB��B�B�<B�3B�^B�&B��B�B�?B�UB�aB�<B�GB�^B�UB�%B�PB�fB�6B�hB�B�PB�B��B��B��B��B��B��B�B�B� B�3B�6B�UB�sB�OB��B��B	��B	�4B	��B	�eB	�B	��B	��B	�B	�B	��B	�B	��B	��B	�B	�B	��B	��B	��B	�AB	�rB	�dB	�B	�zB	�mB	�SB	�'B	�B	�=B	�/B	�&B	�B	�B	��B	�.B	�}B	�B	�B	��B	��B	�yB	�B	��B	�MB	�%B	��B	��B	�)B	�B	�B	�B	�?B	�B	�PB	�	B	��B	�eB	�B	�EB	��B	�B	�hB	�B	�yB	�NB	�3B	�cB	�B	��B	��B	�B	�@B	�B	�B	�
B	�B	�sB	�B	�sB	�XB	��B	�B	�B	�B	�rB	�eB	��B	�XB	�B	��B	�dB	�B	�VB	�*B	�B	�B	�B	�RB	�cB	�B	�B	�tB	�:B	��B	�YB	�B	�PB	��B	�B	��B	��B	�XB	�KB	�OB	�B	�nB	�B	�B	�XB	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999934433444434444434443334443344444334344344444444444443344333444444344344434444344433343444434443443444334343444444333433344443443444333444443444343443344343344343433344334334333443333344343333343333433443334443334334333343333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��BB~BV�B�oB�|B�yB�eB�yB��B�$B�IBÀB��B�B�tB�B��B��B��B��BB1B-B
(BGBkBkBlBhB?B9BGByB�B�B�B �B*�B1B0B0B*�B�BkBfBlBSB�TB�B�|BÃB�/B��B��B��B��BkrBR�BI�BE�B89B�BB��BǙB��B�^B{�Bn~B_"B_!BfNBZB%�BiB
&B
��B
�{B
�gB
�NB
�B
��B
�LB
�B
��B
�dB
y�B
U�B
G�B
9?B
*�B
!�B
uB
�B	�-B	��B	�QB	��B	��B	��B	��B	�tB	�BB	��B	{�B	x�B	mtB	ZB	P�B	G�B	C{B	>\B	71B	,�B	$�B	�B	�B	�B	�B	�B	kB	TB	/B	 �B�B�B�iB�RB�PB�>B�B�B��B��BξBŇBǓBȚBƏB�eB�[BĄB�lB�\B�FB�B��B��B��B�~B�hB�QB�@B�YB�oB�YB�LB�DB�GB�?B�2B� B�
B��B��B��B��B��B�B|�By�Bx�By�By�Bw�Bx�Bw�Bv�Bv�Bu�Bs�Bs�Br�Bq�Bp�BnzBllBkgBiZBd>BfIBfGBR�BO�BO�BO�BM�BP�BR�BS�B_BjaBy�Bv�BhUBN�BD}B=QB>[B>WBAiBBpBBpBD|BD}BBqB?`B=QB=RB>ZBCzBJ�BJ�BI�BI�BJ�BI�BG�BF�BG�BH�BF�BD}BCvBDyB?^BCwBI�BK�BL�BM�BP�BY�B_B_B^Ba'BiZBkeBliBlmBllBmqBmsBs�Bu�Bv�Bx�By�Bz�B��B��B� B�B�B�B�B�/B�<B�DB�ZB�rB��B��B��B��B�B�,B�?B�<B�EB�JB�\B�pB�uB�nB�|BǌB̫BιB��B��B��B��B�B�B�B�-B�GB�B��B��B��B��B��B	�B	�B	�B	�B	�B	B	*B	/B	0B	+B	=B	CB	HB	SB	fB	B	�B	�B	"�B	"�B	"�B	#�B	#�B	#�B	$�B	%�B	)�B	+�B	-�B	.�B	.�B	1B	4B	7*B	;DB	?[B	DzB	K�B	N�B	M�B	I�B	K�B	L�B	M�B	N�B	S�B	\	B	_B	b.B	j]B	q�B	r�B	r�B	moB	hQB	gLB	gMB	iYB	nvB	v�B	x�B	y�B	}�B	��B	��B	�B	��B	{�B	{�B	�B	�`B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�
B	�B	�B	�B	�B	�*B	�8B	�IB	�WB	�UB	�\B	�aB	�hB	�hB	�nB	�mB	�sB	�zB	ŃB	łB	łB	ȓB	ȑB	ɘB	əB	ȒB	ɘB	ʠB	ʟB	˥B	̫B	̬B	̬B	ͲB	ϿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�B	�(B	�/B	�2B	�:B	�>G�O�B	�B	�#B
�B
�B
�B
%�B
-�B
5B
<�B
C]B
KsB
O�B
VhB
X�B
\�B
`�B
c�B
i$B
n\B
t6B
x0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
["G�O�G�O�ByB��G�O�G�O�G�O�G�O�B�4G�O�G�O�G�O�G�O�G�O�B�gG�O�G�O�G�O�A�tQB��A�fG�O�G�O�G�O�B��B��G�O�G�O�G�O�G�O�G�O�B��B��G�O�B�G�O�G�O�B�5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�YB��G�O�G�O�A��7A��B��G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�B�NG�O�G�O�G�O�B�xG�O�G�O�G�O�G�O�B�sG�O�G�O�G�O�B�!B�>B��G�O�B ;�G�O�G�O�G�O�G�O�B�QG�O�G�O�G�O�B�+G�O�G�O�B�GG�O�G�O�G�O�B��A���G�O�A�biG�O�B��G�O�G�O�G�O�G�O�G�O�G�O�B��B�BB�G�O�B�B�BŔG�O�G�O�G�O�G�O�B��G�O�G�O�B��G�O�G�O�G�O�B�B��B�aG�O�G�O�G�O�G�O�G�O�Aʟ�G�O�G�O�G�O�A��G�O�B�G�O�G�O�B��B��G�O�G�O�B��G�O�B	%�B��G�O�G�O�A��G�O�B4G�O�B�.B�B��G�O�G�O�B:B�G�O�B��A���G�O�B��B��A���G�O�G�O�B�_B��B��B��A���G�O�G�O�B�@G�O�B�B�B�IB��A�OhG�O�B�B�@A�o�BĀG�O�BýB�8G�O�G�O�B��B��B�.G�O�G�O�G�O�B�ZBM�Aږ�G�O�B�B��G�O�B],B�B��B��G�O�B��B��B��B�:B��B�@B��B�YB��B�lB�B��BB�B£B�tB�$B��B��BòB�KBÕB��BěG�O�B��B�mB��B�5B�BĔB� BӹB�/B�%B��B�LB�B�CB��B�KB��B�iB��B�bB�^B�'B��B��B�B�VBýB��BýB��B��B�KB�{A�®B��B��B�B� BĩB�lB� B�@B�B�$B�=BŪB�;B��B�B�B�B£B�mB�YB��B��B�[B�'B��B�[B�7B�5B�jB��B�bB��B��B�tB��B�OB�
B��B�BòB�SB��B�B�CB�VB�=B��B��B��B�B��B�zB�QB�5B�!B�TB�:B�qB��B��B�eB��B	�A��8B�-B��B³B�'B��B�[B��B��B��B��B��B��B�yB�}A���B�`B�BB��B¼B�BB�,B�SB��B�+B�$B�B��B��B��B�TBÿBºBĈB�xB��BíB�PB£B�4B��B�PB�,B�WB�=B�=B��B� BĆB�GB�WB�BB��B�3B�IB�eBΌB�+B�]BĢB��B�0B�1B�	B��B��B��B��B�B�B�hB��B�IB�-B�}B�oBB�_B�oB�B��BB�B�UB��B��BòB��B��B��B�B��B��B��B��B�jB�(B�BÆB�9B��B��B��BB£B��B��B�xB�uB�BB�B��B�0B¼BÀB�B��B��B�BB�B��B��B�KB�?B�mB��BķB��B�GB��BB�^BğB�-B�BBàB�PB��B�IB��B�B�yB�B��B�NB��B��B��B��B�B¨B�`B��B��B�+B��B��B��B�mBïB�xB��B�!B�RB��B��B��B�&B�B��B�bB�\B�\B�B��B��B�B��B�kB�lB�B��B�2B�-B��B�B��B��B�;B�mB��B�B�B�sB��B�
B��B�B��B��B�B�SB�dB��B�rB�~B�bB�B��B�B��B�uB��B�B�B�UB� B��B�bB��B��B�cB�cB��B�rB�B�B��B�B�QB�(B�qB�VB�B�B��B��B�B�!B��B�8B��B��B��B�QB�lB�fB��B�-B�B��B�FB�B�nB�@B�B�pB�UB�B�~B��B�B�B��B��B��B�B� B�B�B�ZB�B�B�B�PB�B�<B�sB�+B�B�B�NB�B��B�B�B�$B�B�mB��B�B�B�B�<B�&B��B��B�B�B��B��B�%B��B�PB�WB�JB�iB��B�(B�qB�B�PB�B�8B�LB�MB��B�=B�HB�sB�1B��B�B��B��B�B�B��B�;B��B��B�6B�/B�B�QB�B�.B��B�.B�B��B�AB�-B�(B�B�cB�B�mB�(B�B�HB�B��B�lB�-B�>B�LB�(B�2B�HB�AB�B�:B�PB�#B�SB�B�=B�B��B�B��B��B��B��B��B�	B��B�B�#B�AB�]B�;B��B�B	�}B	�B	�B	�CB	��B	�B	�B	��B	��B	�B	�~B	�B	�B	��B	��B	�B	�iB	�wB	� B	�QB	�BB	��B	�VB	�NB	�2B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�]B	��B	��B	�B	��B	�YB	��B	�vB	�,B	�B	��B	�B	�B	��B	�B	��B	� B	��B	�/B	��B	��B	�EB	��B	�#B	�B	�tB	�GB	�rB	�ZB	�-B	�B	�BB	��B	�B	��B	�B	�!B	�B	�B	��B	�^B	�QB	�B	�RB	�9B	�B	�hB	�xB	�kB	�RB	�GB	�B	�8B	�B	�B	�BB	�aB	�4B	�
B	��B	�B	�{B	�0B	�@B	�rB	�B	�SB	�B	��B	�7B	�yB	�.B	�B	�_B	�B	��B	�6B	�)B	�.B	�B	�NB	�{B	�fB	�9B	�yB	�B�&B�B��B�bB�\B�\B�B��B��B�B��B�kB�lB�B��B�2B�-B��B�B��B��B�;B�mB��B�B�B�sB��B�
B��B�B��B��B�B�SB�dB��B�rB�~B�bB�B��B�B��B�uB��B�B�B�UB� B��B�bB��B��B�cB�cB��B�rB�B�B��B�B�QB�(B�qB�VB�B�B��B��B�B�!B��B�8B��B��B��B�QB�lB�fB��B�-B�B��B�FB�B�nB�@B�B�pB�UB�B�~B��B�B�B��B��B��B�B� B�B�B�ZB�B�B�B�PB�B�<B�sB�+B�B�B�NB�B��B�B�B�$B�B�mB��B�B�B�B�<B�&B��B��B�B�B��B��B�%B��B�PB�WB�JB�iB��B�(B�qB�B�PB�B�8B�LB�MB��B�=B�HB�sB�1B��B�B��B��B�B�B��B�;B��B��B�6B�/B�B�QB�B�.B��B�.B�B��B�AB�-B�(B�B�cB�B�mB�(B�B�HB�B��B�lB�-B�>B�LB�(B�2B�HB�AB�B�:B�PB�#B�SB�B�=B�B��B�B��B��B��B��B��B�	B��B�B�#B�AB�]B�;B��B�B	�}B	�B	�B	�CB	��B	�B	�B	��B	��B	�B	�~B	�B	�B	��B	��B	�B	�iB	�wB	� B	�QB	�BB	��B	�VB	�NB	�2B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�]B	��B	��B	�B	��B	�YB	��B	�vB	�,B	�B	��B	�B	�B	��B	�B	��B	� B	��B	�/B	��B	��B	�EB	��B	�#B	�B	�tB	�GB	�rB	�ZB	�-B	�B	�BB	��B	�B	��B	�B	�!B	�B	�B	��B	�^B	�QB	�B	�RB	�9B	�B	�hB	�xB	�kB	�RB	�GB	�B	�8B	�B	�B	�BB	�aB	�4B	�
B	��B	�B	�{B	�0B	�@B	�rB	�B	�SB	�B	��B	�7B	�yB	�.B	�B	�_B	�B	��B	�6B	�)B	�.B	�B	�NB	�{B	�fB	�9B	�yB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999934433444434444434443334443344444334344344444444444443344333444444344344434444344433343444434443443444334343444444333433344443443444333444443444343443344343344343433344334334333443333344343333343333433443334443334334333343333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.25 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.25 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.25 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649182020083116491820200831164918202008311649182020083116491820200831164918202008311649182020083116491820200831164918202008311649182020083116491820200831164918AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817132019021918171320190219181713    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817132019021918171320190219181713  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817132019021918171320190219181713  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649182020083116491820200831164918  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                