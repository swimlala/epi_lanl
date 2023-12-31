CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  M   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:35Z creation      
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
_FillValue                 	�  �0   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�  ϴ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� (�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 2�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ZX   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �x   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� >   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � o�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   p`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   |`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181635  20200831164715  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @ա�)&N�@ա�)&N�@ա�)&N�111 @ա�`� @ա�`� @ա�`� @6�I�^5@6�I�^5@6�I�^5�cLA�7K��cLA�7K��cLA�7K�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Dsy�Dt  Dt� Dy�RD��D�>fD��3D���D��qD�@�D���D��qD��D�:=D�{3D�ٚD��\D�7�D�c3D��RD�D�7�D�nD��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    �L�;L�;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�ͽ��;L�;L��=��ͽ��;L�ͽ���    =��;L�;L�ͽ���=��;L�;L�;L�ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ��;L�;L�;L�;L��=���    �L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L��    =��;L�;L�ͽ��ͽ��;L�;L�;L�;L�ͽ��;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L��    �L�;L�;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�;L��    =��;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��;L�;L�ͽ��;L�;L�;L�ͽ��;L�;L�ͽ���    ���;L�;L��    =���    �L��    =���=��;L�;L��=���=���    �L�;L�;L��    >L��>L�;L�;L�;L�;L��=���    �L�;L�;L��    =��;L�;L�;L��        �L�;L�ͽ��;L�;L��    ���;L�;L�;L�ͽ��ͽ��ͽ��ͽ��;L�ͽ��ͽ���    ���;L�;L�ͽ���    ���;L�;L��        �L�;L�;L��    ����>L��=��ͽ��;L�ͽ��ͽ���    ���ͽ��ͽ��ͽ��;L��    ���;L��        ���ͽ��;L�ͽ���        =���    ���ͽ��ͽ���            ����        =���    =���=���=���    =���            =���    =���=���=���    =���=���>L��=���    =���        =���    =���=���=���=���    =���=���=���                    =���=���        >L��=���=���>L��=���        =���=���                >L��                =���=���=���        =��ͽ���        =���=���=���    =���=���    =���=���=���    =���    =���=���=���=���=���    =���        =���=���=���=���=���=���>L��>L��=���    =���        =���            ����=���        =���=���    =���    =���=���=���=���            =���=���    =���=���        =���        =���    =���            =���=���=���=���=���        =���=���=���                =���=���=���    =���>L��>���>L��=���=���>L��=���=���    ����                =���    =���=���>L��=���=���=���        =���=���        =���=���    =���=���=���    =��ͽ���=���    =���=���                =���        =���    =���    =���=���=���    =���                    =��ͽ���=���=���=���                =���=���=���=���    =���=���=���=���=���=���        =���=���=���=���=���=���>L��=���=���=���            =���    >L��>L��>L��>���>���>���?   ?   ?   ?��?��?333?L��?fff?fff?�  ?���?���?���?���?�ff?�33?�  ?�  ?�  ?���?ٙ�?�ff?�ff?�33@   @   @ff@��@��@33@��@   @   @&ff@,��@333@9��@@  @Fff@L��@S33@Y��@`  @l��@s33@s33@�  @�33@�ff@���@���@�  @�33@�ff@���@�  @�33@�33@���@���@�  @�33@���@���@�  @�33@ə�@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���A   A��A33A��A  A	��A33A��A  A��A33AffA  A��A33AffA   A!��A#33A&ffA(  A)��A+33A.ffA0  A1��A4��A6ffA8  A9��A<��A>ffA@  AC33AD��AFffAH  AI��AL��ANffAP  AS33AT��AVffAX  A[33A\��A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A~ffA���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  Ař�A�ffA�33A�  Aə�A�ffA�33A�  A͙�A�ffA�33A���Aљ�A�ffA�33A�  Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  Aݙ�Dq�Dq&fDq,�Dq33Dq@ DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Du  @333@9��@@  @Fff@L��@S33@Y��@`  @l��@s33@s33@�  @�33@�ff@���@���@�  @�33@�ff@���@�  @�33@�33@���@���@�  @�33@���@���@�  @�33@ə�@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���A   A��A33A��A  A	��A33A��A  A��A33AffA  A��A33AffA   A!��A#33A&ffA(  A)��A+33A.ffA0  A1��A4��A6ffA8  A9��A<��A>ffA@  AC33AD��AFffAH  AI��AL��ANffAP  AS33AT��AVffAX  A[33A\��A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A~ffA���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  Ař�A�ffA�33A�  Aə�A�ffA�33A�  A͙�A�ffA�33A���Aљ�A�ffA�33A�  Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  Aݙ�Dq�Dq&fDq,�Dq33Dq@ DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @?\)@��H@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�A�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BPBX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C
C
C

C
C
C
C
C
C
C0�C0�C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CN
CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1]D1�]D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dp�]Dq��Dr�Dr��Ds�Ds]Dt�Dt��Dy�D� �D�AGD��D��{D� RD�C�D���D��RD�� D�=D�~D��{D�=D�:�D�fD��3D� D�:�D�p�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�Q��G���G���G��#��#���G���G��#���G���G��#���G���G�>B�\�#���G��#�=�Q�>B�\��G���G��#�>B�\��G���G���G��#���G���G��#���G���G���G���G���G���G��#���G���G���G��#��#���G��#���G���G���G���G�>B�\=�Q��G��#���G���G���G���G���G���G���G���G���G���G�=�Q�>B�\��G���G��#��#���G���G���G���G��#���G��#���G���G���G���G���G���G���G���G�=�Q��G���G���G��#��#���G���G��#���G���G���G�=�Q�>B�\��G���G���G���G���G��#���G���G���G���G��#���G���G��#���G���G���G��#���G���G��#�=�Q�#���G���G�=�Q�>B�\=�Q��G�=�Q�>B�\>B�\��G���G�>B�\>B�\=�Q��G���G���G�=�Q�>�z�>�z��G���G���G���G�>B�\=�Q��G���G���G�=�Q�>B�\��G���G���G�=�Q�=�Q��G���G��#���G���G�=�Q�#���G���G���G��#��#��#��#���G��#��#�=�Q�#���G���G��#�=�Q�#���G���G�=�Q�=�Q��G���G���G�=�Q�#�>�z�>B�\�#���G��#��#�=�Q�#��#��#��#���G�=�Q�#���G�=�Q�=�Q�#��#���G��#�=�Q�=�Q�>B�\=�Q�#��#��#�=�Q�=�Q�=�Q�#�=�Q�=�Q�>B�\=�Q�>B�\>B�\>B�\=�Q�>B�\=�Q�=�Q�=�Q�>B�\=�Q�>B�\>B�\>B�\=�Q�>B�\>B�\>�z�>B�\=�Q�>B�\=�Q�=�Q�>B�\=�Q�>B�\>B�\>B�\>B�\=�Q�>B�\>B�\>B�\=�Q�=�Q�=�Q�=�Q�=�Q�>B�\>B�\=�Q�=�Q�>�z�>B�\>B�\>�z�>B�\=�Q�=�Q�>B�\>B�\=�Q�=�Q�=�Q�=�Q�>�z�=�Q�=�Q�=�Q�=�Q�>B�\>B�\>B�\=�Q�=�Q�>B�\�#�=�Q�=�Q�>B�\>B�\>B�\=�Q�>B�\>B�\=�Q�>B�\>B�\>B�\=�Q�>B�\=�Q�>B�\>B�\>B�\>B�\>B�\=�Q�>B�\=�Q�=�Q�>B�\>B�\>B�\>B�\>B�\>B�\>�z�>�z�>B�\=�Q�>B�\=�Q�=�Q�>B�\=�Q�=�Q�=�Q�#�>B�\=�Q�=�Q�>B�\>B�\=�Q�>B�\=�Q�>B�\>B�\>B�\>B�\=�Q�=�Q�=�Q�>B�\>B�\=�Q�>B�\>B�\=�Q�=�Q�>B�\=�Q�=�Q�>B�\=�Q�>B�\=�Q�=�Q�=�Q�>B�\>B�\>B�\>B�\>B�\=�Q�=�Q�>B�\>B�\>B�\=�Q�=�Q�=�Q�=�Q�>B�\>B�\>B�\=�Q�>B�\>�z�>Ǯ>�z�>B�\>B�\>�z�>B�\>B�\=�Q�#�=�Q�=�Q�=�Q�=�Q�>B�\=�Q�>B�\>B�\>�z�>B�\>B�\>B�\=�Q�=�Q�>B�\>B�\=�Q�=�Q�>B�\>B�\=�Q�>B�\>B�\>B�\=�Q�>B�\�#�>B�\=�Q�>B�\>B�\=�Q�=�Q�=�Q�=�Q�>B�\=�Q�=�Q�>B�\=�Q�>B�\=�Q�>B�\>B�\>B�\=�Q�>B�\=�Q�=�Q�=�Q�=�Q�=�Q�>B�\�#�>B�\>B�\>B�\=�Q�=�Q�=�Q�=�Q�>B�\>B�\>B�\>B�\=�Q�>B�\>B�\>B�\>B�\>B�\>B�\=�Q�=�Q�>B�\>B�\>B�\>B�\>B�\>B�\>�z�>B�\>B�\>B�\=�Q�=�Q�=�Q�>B�\=�Q�>�z�>�z�>�z�>Ǯ>Ǯ>Ǯ?
=?
=?
=?0��?0��?J=p?c�
?}p�?}p�?��?�Q�?�Q�?�Q�?��?��?��R?˅?˅?˅?�Q�?��?��?��?��R@@@(�@�\@�\@��@\)@%@%@,(�@2�\@8��@?\)@E@L(�@R�\@X��@_\)@e@r�\@x��@x��@��H@�{@�G�@�z�@��@��H@�{@�G�@�z�@��H@�{@�{@�z�@��@��H@�{@�z�@��@��H@�{@�z�@Ϯ@��H@�{@�G�@߮@��H@�{@�G�@�@��H@�{@�G�@��Ap�A
>A��A=qA	p�A
>A��A=qAp�A
>A��A�
Ap�A
>A��A�
A!p�A#
>A$��A'�
A)p�A+
>A,��A/�
A1p�A3
>A6=qA7�
A9p�A;
>A>=qA?�
AAp�AD��AF=qAG�
AIp�AK
>AN=qAO�
AQp�AT��AV=qAW�
AYp�A\��A^=qA_�
Aap�Ad��Af=qAg�
Aip�Al��An=qAo�
As
>At��Av=qAw�
A{
>A|��A~=qA�
A��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA��A�Q�A��A��RA��A��A��A��RA��A��A��A��RA��A��A��AĸRA�Q�A��A��AȸRA�Q�A��A��A̸RA�Q�A��A��AхA�Q�A��A��AԸRA�Q�A��A��AظRA�Q�A��A��AܸRA�Q�Dq]Dq,)Dq2�Dq8�DqE�DqL)DqR�DqX�Dqe�Dql)Dqr�Dq]Dq��Dq�)Dq��Dq�]Dq��Dq�)Dq��Dq�]Dq��DqҐDq��Dq�]Dq�)Dq�Dq��Dq�]Dr)Dr�Dr�Dr%�Dr,)Dr2�Dr8�DrE�DrL)DrR�Dr_]Dre�Drl)Drr�Dr]Dr��Dr�)Dr��Dr�]Dr��Dr��Dr��Dr�]Dr�)DrҐDr��Dr�]Dr�)Dr�Dr��Ds�Ds)Ds�Ds]Ds%�Ds,)Ds2�Ds?]DsE�DsL)DsX�Ds_]Dse�Dsr�Dsx�Ds]Ds�)Ds��Ds��Ds��Ds�)Ds��Ds�]Ds��Ds�)DsҐDs�]Ds��Ds�)Ds��Ds�]Dt�Dt�Dt�Dt]Dt,)Dt2�Dt8�DtE�DtL)DtR�Dt_]Dte�Dtl)Dtx�Dt]Dt��Dt�)Dt��Dt�]Dt��Dt��Dt��Dt�]Dt�)DtҐDt��Dt��Dt�)Dt�Dt�]Du�@8��@?\)@E@L(�@R�\@X��@_\)@e@r�\@x��@x��@��H@�{@�G�@�z�@��@��H@�{@�G�@�z�@��H@�{@�{@�z�@��@��H@�{@�z�@��@��H@�{@�z�@Ϯ@��H@�{@�G�@߮@��H@�{@�G�@�@��H@�{@�G�@��Ap�A
>A��A=qA	p�A
>A��A=qAp�A
>A��A�
Ap�A
>A��A�
A!p�A#
>A$��A'�
A)p�A+
>A,��A/�
A1p�A3
>A6=qA7�
A9p�A;
>A>=qA?�
AAp�AD��AF=qAG�
AIp�AK
>AN=qAO�
AQp�AT��AV=qAW�
AYp�A\��A^=qA_�
Aap�Ad��Af=qAg�
Aip�Al��An=qAo�
As
>At��Av=qAw�
A{
>A|��A~=qA�
A��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA��A�Q�A��A��RA��A��A��A��RA��A��A��A��RA��A��A��AĸRA�Q�A��A��AȸRA�Q�A��A��A̸RA�Q�A��A��AхA�Q�A��A��AԸRA�Q�A��A��AظRA�Q�A��A��AܸRA�Q�Dq]Dq,)Dq2�Dq8�DqE�DqL)DqR�DqX�Dqe�Dql)Dqr�Dq]Dq��Dq�)Dq��Dq�]Dq��Dq�)Dq��Dq�]Dq��DqҐDq��Dq�]Dq�)Dq�Dq��Dq�]Dr)Dr�Dr�Dr%�Dr,)Dr2�Dr8�DrE�DrL)DrR�Dr_]Dre�Drl)Drr�Dr]Dr��Dr�)Dr��Dr�]Dr��Dr��Dr��Dr�]Dr�)DrҐDr��Dr�]Dr�)Dr�Dr��Ds�Ds)Ds�Ds]Ds%�Ds,)Ds2�Ds?]DsE�DsL)DsX�Ds_]Dse�Dsr�Dsx�Ds]Ds�)Ds��Ds��Ds��Ds�)Ds��Ds�]Ds��Ds�)DsҐDs�]Ds��Ds�)Ds��Ds�]Dt�Dt�Dt�Dt]Dt,)Dt2�Dt8�DtE�DtL)DtR�Dt_]Dte�Dtl)Dtx�Dt]Dt��Dt�)Dt��Dt�]Dt��Dt��Dt��Dt�]Dt�)DtҐDt��Dt��Dt�)Dt�Dt�]Du�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA���A��
Ạ�ÁA�r�A�I�A�/A�{A�A���A��A��yA��`A��TA��TA��HA��;A��/A��A��A��
A��
A���A˾wAˮA˧�A�t�Aʛ�A�VAƍPA�;dA��mA��A�/A���A�z�A��mA�p�A�S�A�bNA�`BA���A���A�5?A�%A��jA���A�$�A��A�oA�ƨA�ffA���A��A��
A���A���A��A�G�A��\A��yA��7A��!A��A��A��\A���A��`A�9XA���A���A�~�A�A�A�A��A�bA�%A�=qA���A��\A���A�$�A�VA��7A��yA�7LA�JA���A��wA��TA��A��A�XA�{A�%A�ffA��-A��A�bA�wA|-Ax�/AvE�Av1'Av5?Au�
Av��Au�;At��As�#Ar�HAp��An5?Alv�Ai`BAfn�Ae?}Ab��AaVA`M�A]�AZZAW��AU�hAT��AS��ARn�APȴAP1AN��ANAL�!AK�TAJ�AH(�ADM�ABȴAA��AAXA@��A>bA<�+A:�+A7�PA5��A4��A2��A0Q�A.n�A,r�A,=qA,{A+�^A*�DA(��A(-A'l�A&�A&~�A%�
A$Q�A#�FA#"�A"�!A"�A!%A Q�A��A��A��A��A�
AO�A�A  A�!AA�A�wA;dAffA��AdZA�`A��A��A��A5?A�7AG�A
=A�\A�#A;dAz�A-A��A�AdZA�`A�TA��A�A�!A�A
z�A	�#A	�hA	;dA�yAȴA�+AQ�A5?A1AAhsA7LA"�A�!AI�A9XA�mA"�A�AdZA;dAȴA��A�+A �j@��@���@�S�@���@�bN@�l�@���@���@�j@�j@��u@�bN@�F@�@�@�\@�\)@�@�z�@�b@�=q@�ff@���@���@�t�@��@�5?@�&�@ۍP@���@؛�@���@�J@��T@�^5@Ցh@��@�M�@���@ՙ�@Ԭ@؛�@ڇ+@���@���@�
=@ڇ+@��#@���@�(�@��H@�j@���@�V@љ�@Ϯ@�X@˥�@�{@�bN@�z�@�E�@�&�@�dZ@��m@ʸR@�~�@ə�@���@�Ĝ@Ǯ@Ɵ�@���@�~�@��@�r�@��;@���@�O�@���@�l�@��R@���@�p�@��`@�j@�ƨ@���@���@���@�t�@��@���@�v�@�V@�$�@���@���@��7@��9@��@��F@��@���@�+@��#@�V@��/@�9X@�dZ@���@��7@��@�5?@�J@���@���@�Q�@��P@�l�@���@��T@���@�%@� �@��w@��P@��@���@�5?@�x�@��@���@���@�dZ@�l�@���@�E�@�7L@��D@��9@��`@���@���@�K�@�v�@��T@���@���@�`B@��@�V@��/@�z�@�A�@�  @��
@��F@��P@�dZ@�C�@�@��\@�^5@��@��#@��@���@���@�Q�@�1@��m@��w@�|�@�dZ@�;d@�+@�"�@�o@��!@�ff@�@��h@�/@��@��9@�z�@�Z@�A�@�b@�t�@�ȴ@�n�@�M�@�@���@�x�@�G�@�G�@�X@�`B@���@�j@��@�C�@��@��R@���@�~�@�E�@���@���@�@�@��^@���@���@�hs@�7L@�%@���@�r�@��;@���@�l�@�\)@�\)@�;d@�ȴ@���@��@���@��+@�^5@�V@�v�@�v�@�~�@�~�@�M�@��@�&�@���@���@��D@�t�@�C�@�;d@�K�@�l�@���@�\)@�C�@��@�@��@���@���@���@y��@s&@i�@b~�@[��@TD�@LFt@E�d@>v�@7�K@2��@-�@)j@%|@ C-@i�@��@�o@�@�IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33AËDAě�A���A�"�Að!A���Ȧ+A�%A��-A��
A���Aď\A�&�A��A�t�A�7LA�&�A�/A��A��AǇ+A�Q�A���A�E�A�n�A˓uA��/A��PA�A���A�%A�"�A��7A�-A��yAöFA���A�z�A�Aʲ-A�S�A��DA�%A�Q�A�JA��A�7LA���A� �A���A��A�JA�dZA�`BA��A�+A��wA��mA�JA��A���A��/A��A�O�A�t�A��A�t�A���A�v�A��HA�ffA��HA��AÛ�A�?}A��A��A�|�A���A�AȅA�M�A���A��A��A�VA˾wA��A��/A�S�A���A��#A�A��#A�{A�?}A�ƨA�\)A��A��!A��A�ȴA���A�;dA�VA��A�bNA��mA��A��9A��A���A��!A�%A�oA�bA��mA�=qA�=qA���A�?}A�z�A�M�A�C�A�|�A̺^A�M�A�I�A�hsA�A���A�C�A�A�A�VA�"�A�ZAƋDA�G�A�I�A��A��A�;dAţ�A��`A�I�Aò-A���A���A̟�A�C�A��A�/A�"�A�JA�I�AőhA�ĜA��A�hsA���A̡�A�C�A��
A���A��A��A¥�AŴ9A�?}A�oA���A�A�A��#A�(�Aá�AœuA�1'A�Q�A� �A�~�A�jA�\)A�Q�A��AƶFA��HA�7LA�A�A���A�I�A�G�AɬA�ȴA�S�A̋DAʼjA��A�z�A���A̝�A�z�A�;dA�dZA�E�A�7LA�{A̅A�=qÁA��A�9XA�?}A�=qA�9XA�^5A�E�A�$�A�XA�G�A�C�A�|�A�Q�A�G�A�O�A�VA�Q�A�C�A�S�A�G�A�Q�A�I�A�M�A�M�A�O�A�M�A�A�A�A�A�C�A�E�A�A�A�?}A�K�A�E�A�?}A�=qA�C�A�K�A�S�A�A�A�I�A�I�A�?}A�=qA�A�A�A�A�O�A�A�A�E�A�;dA�A�A�C�A�=qA�E�A�?}A�;dA�=qA�I�A�=qA�?}A�K�A�E�A�G�A�I�A�K�A�E�A�VA�I�A�O�A�E�A�C�A�M�A�K�A�K�A�O�A�\)A�C�A�K�A�\)A�I�A�E�A�C�A�G�A�I�A�O�A�G�A�S�A�M�A�G�A�O�A�G�A�G�A�I�A�E�A�E�A�M�A�G�A�C�A�K�A�G�A�I�A�M�A�?}A�?}A�VA�G�A�I�A�A�A�O�A�G�A�I�A�O�A�A�A�E�A�O�A�Q�A�G�A�O�A�E�A�G�A�C�A�O�A�=qA�?}A�C�A�C�A�K�A�?}A�E�A�G�A�G�A�A�A�=qA�E�A�A�A�K�A�K�A�K�A�K�A�7LA�Q�A�O�A�G�A�K�A�I�A�?}A�;dA�7LA�9XA�A�A�9XA�?}A�C�A�?}A�33A�;dA�A�A�33A�;dA�33A�9XA�A�A�G�A�Q�A�C�A�I�A�C�A�C�A�A�A�I�A�?}A�G�A�E�A�?}A�=qA�9XA�A�A�1'A�C�A�33A�33A�?}A�O�A���A�=qA�5?A�1'A�33A�?}A�33A�/A�=qA�7LA�33A�5?A�5?A�?}A�7LA�;dA�/A�=qA�C�A�(�A�&�A�(�A�5?A�1'A�/A�7LA� �A�/A�-A�/A�/A�/A�;dA�C�A�?}A�5?A�33A�7LA�ȴA�=qA�5?A�33A�5?A�+A�/A�=qA�=qA�=qA�33A�33A�33A�33A�/A�/A�5?A�33A�1'A�33A�33A�33A�/A�/A�-A�-A�+A�7LA�/A�1'A�5?A�1'A�1'A�5?A�5?A�1'A�1'A�1'A�33A�$�A�-A�1'A�-A�(�A�-A�+A�&�A�/A�(�A�$�A�(�A� �A� �A�"�A��A�"�A�$�A�&�A�1'A�$�A�-A�"�A��A��A�bA�{A�VA�JA��A�oA�{A�bA�{A��A��A��A��A��A��A��A��A�&�A�(�A��A��A�-A�5?A�-A�5?A�33A�"�A�(�A��A�"�A��A�"�A��A�&�A��A�VA�oA��A�JA�{A�JA�{A�bA�
=A�
=A�A�A�A�  A�1A�%A�A���A�%A���A���A��HA��A��
A���A���A�ȴA���A���A���A���A�ĜA�A̾wA���A�A�A�A̺^A���A̲-A̟�A̕�A̓uȂhA̍PȀ\Ȁ\ȂhȂhȀ\A̋DA̋DA̋DA̍PA̅Ả7Ȧ+Ȧ+ÃȦ+A̅ÁÃÁA�~�A�~�A�|�A�~�A�|�A�~�A�~�A�t�A�v�A�jA�hsA�p�A�dZA�ZA�XA�XA�`BA�dZA�`BA�VA�M�A�M�A�I�A�K�A�I�A�G�A�G�A�E�A�?}A�C�A�C�A�?}A�?}A�?}A�?}A�C�A�=qA�;dA�=qA�9XA�+A�(�A�-A�&�A�-A�&�A�(�A�$�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A�{A�{A�{A�bA�bA�bA�VA�JA�JA�JA�1A�1A�A�A�1A�%A�%A�%A�%A�1A�A�A�A�A���A���A���A���A���A���A��A��A���A��A���A��A��A��A��A��A��A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��yA��A��A��A��yA��yA��yA��@���@���@��P@��@�|�@�l�@�l�@�\)@�\)@�\)@�\)@�\)@�\)@�S�@�K�@�S�@�S�@�S�@�S�@�K�@�K�@�K�@�C�@�;d@�C�@�;d@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�33@�C�@�"�@��@��@�o@��@��@�o@�o@�o@�o@�o@�o@�
=@�o@�
=@�@�@�@���@���@���@���@�@�@�@�@���@��@��y@���@���@���@���@���@���@��y@���@��y@��@��y@��@��y@��y@��y@��y@��y@��H@��H@��y@��H@���@�ȴ@�ȴ@���@���@���@��R@��R@��!@��!@���@���@���@���@���@���@���@���@���@���@���@��\@��\@��\@��+@��\@��+@��+@��+@��+@��+@�~�@��+A�VA�bA�{A�oA�VA�VA�
=A�%A�  A���A���A���A��A���A���A���A���A���A��A��A��A���A��yA���A��A��HA��A��A��/A�ȴA���A���A̾wA���A���A���A̸RA̸RA̺^A̶FA̲-A̮A̼jA̺^A���A̼jA̲-A̡�A̍PȦ+A̅Ȧ+Ȧ+Ȧ+Ȧ+Ȧ+A̅ÃÃA̅A̅A̅A�~�A�|�A�|�A�z�A�~�A�z�A�z�A�z�A�|�A�z�A�v�A�t�A�x�A�v�A�v�A�z�A�x�A�t�A�p�A�dZA�jA�hsA�\)A�VA�S�A�S�A�VA�ZA�S�A�M�A�C�A�C�A�G�A�?}A�=qA�C�A�A�A�;dA�;dA�;dA�5?A�7LA�7LA�7LA�7LA�7LA�5?A�5?A�33A�+A� �A� �A� �A�"�A��A� �A��A��A��A��A�{A��A�{A�{A�oA�oA�JA�JA�JA�VA�JA�
=A�1A�%A�%A�%A�%A�A�A�A���A���A���A���A���A���A�  A�  A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��yA��yA��yA��yA��yA��mA��mA��mA��mA��mA��mA��`A��`A��mA��mA��m@���@���@��P@��P@�|�@�t�@�l�@�\)@�\)@�dZ@�\)@�dZ@�dZ@�\)@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�+@�;d@�"�@��@��@�o@�o@��@��@�o@�o@�o@�o@�o@�o@�
=@�
=@�@�@�@���@���@���@���@���@�@�@�@���@���@��@��H@���@���@�@���@�@��@��y@��y@��@��@��@��@��y@��y@��H@��y@��H@��H@��H@��H@��@��@���@�ȴ@���@���@���@��R@��!@��!@��!@���@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��\@��+@��+@��+@��+@��+@��+@��+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�bA���A��
Ạ�ÁA�r�A�I�A�/A�{A�A���A��A��yA��`A��TA��TA��HA��;A��/A��A��A��
A��
A���A˾wAˮA˧�A�t�Aʛ�A�VAƍPA�;dA��mA��A�/A���A�z�A��mA�p�A�S�A�bNA�`BA���A���A�5?A�%A��jA���A�$�A��A�oA�ƨA�ffA���A��A��
A���A���A��A�G�A��\A��yA��7A��!A��A��A��\A���A��`A�9XA���A���A�~�A�A�A�A��A�bA�%A�=qA���A��\A���A�$�A�VA��7A��yA�7LA�JA���A��wA��TA��A��A�XA�{A�%A�ffA��-A��A�bA�wA|-Ax�/AvE�Av1'Av5?Au�
Av��Au�;At��As�#Ar�HAp��An5?Alv�Ai`BAfn�Ae?}Ab��AaVA`M�A]�AZZAW��AU�hAT��AS��ARn�APȴAP1AN��ANAL�!AK�TAJ�AH(�ADM�ABȴAA��AAXA@��A>bA<�+A:�+A7�PA5��A4��A2��A0Q�A.n�A,r�A,=qA,{A+�^A*�DA(��A(-A'l�A&�A&~�A%�
A$Q�A#�FA#"�A"�!A"�A!%A Q�A��A��A��A��A�
AO�A�A  A�!AA�A�wA;dAffA��AdZA�`A��A��A��A5?A�7AG�A
=A�\A�#A;dAz�A-A��A�AdZA�`A�TA��A�A�!A�A
z�A	�#A	�hA	;dA�yAȴA�+AQ�A5?A1AAhsA7LA"�A�!AI�A9XA�mA"�A�AdZA;dAȴA��A�+A �j@��@���@�S�@���@�bN@�l�@���@���@�j@�j@��u@�bN@�F@�@�@�\@�\)@�@�z�@�b@�=q@�ff@���@���@�t�@��@�5?@�&�@ۍP@���@؛�@���@�J@��T@�^5@Ցh@��@�M�@���@ՙ�@Ԭ@؛�@ڇ+@���@���@�
=@ڇ+@��#@���@�(�@��H@�j@���@�V@љ�@Ϯ@�X@˥�@�{@�bN@�z�@�E�@�&�@�dZ@��m@ʸR@�~�@ə�@���@�Ĝ@Ǯ@Ɵ�@���@�~�@��@�r�@��;@���@�O�@���@�l�@��R@���@�p�@��`@�j@�ƨ@���@���@���@�t�@��@���@�v�@�V@�$�@���@���@��7@��9@��@��F@��@���@�+@��#@�V@��/@�9X@�dZ@���@��7@��@�5?@�J@���@���@�Q�@��P@�l�@���@��T@���@�%@� �@��w@��P@��@���@�5?@�x�@��@���@���@�dZ@�l�@���@�E�@�7L@��D@��9@��`@���@���@�K�@�v�@��T@���@���@�`B@��@�V@��/@�z�@�A�@�  @��
@��F@��P@�dZ@�C�@�@��\@�^5@��@��#@��@���@���@�Q�@�1@��m@��w@�|�@�dZ@�;d@�+@�"�@�o@��!@�ff@�@��h@�/@��@��9@�z�@�Z@�A�@�b@�t�@�ȴ@�n�@�M�@�@���@�x�@�G�@�G�@�X@�`B@���@�j@��@�C�@��@��R@���@�~�@�E�@���@���@�@�@��^@���@���@�hs@�7L@�%@���@�r�@��;@���@�l�@�\)@�\)@�;d@�ȴ@���@��@���@��+@�^5@�V@�v�@�v�@�~�@�~�@�M�@��@�&�@���@���@��D@�t�@�C�@�;d@�K�@�l�@���@�\)@�C�@��@�@��@���G�O�@���@y��@s&@i�@b~�@[��@TD�@LFt@E�d@>v�@7�K@2��@-�@)j@%|@ C-@i�@��@�o@�@�IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33AËDAě�A���A�"�Að!A���Ȧ+A�%A��-A��
A���Aď\A�&�A��A�t�A�7LA�&�A�/A��A��AǇ+A�Q�A���A�E�A�n�A˓uA��/A��PA�A���A�%A�"�A��7A�-A��yAöFA���A�z�A�Aʲ-A�S�A��DA�%A�Q�A�JA��A�7LA���A� �A���A��A�JA�dZA�`BA��A�+A��wA��mA�JA��A���A��/A��A�O�A�t�A��A�t�A���A�v�A��HA�ffA��HA��AÛ�A�?}A��A��A�|�A���A�AȅA�M�A���A��A��A�VA˾wA��A��/A�S�A���A��#A�A��#A�{A�?}A�ƨA�\)A��A��!A��A�ȴA���A�;dA�VA��A�bNA��mA��A��9A��A���A��!A�%A�oA�bA��mA�=qA�=qA���A�?}A�z�A�M�A�C�A�|�A̺^A�M�A�I�A�hsA�A���A�C�A�A�A�VA�"�A�ZAƋDA�G�A�I�A��A��A�;dAţ�A��`A�I�Aò-A���A���A̟�A�C�A��A�/A�"�A�JA�I�AőhA�ĜA��A�hsA���A̡�A�C�A��
A���A��A��A¥�AŴ9A�?}A�oA���A�A�A��#A�(�Aá�AœuA�1'A�Q�A� �A�~�A�jA�\)A�Q�A��AƶFA��HA�7LA�A�A���A�I�A�G�AɬA�ȴA�S�A̋DAʼjA��A�z�A���A̝�A�z�A�;dA�dZA�E�A�7LA�{A̅A�=qÁA��A�9XA�?}A�=qA�9XA�^5A�E�A�$�A�XA�G�A�C�A�|�A�Q�A�G�A�O�A�VA�Q�A�C�A�S�A�G�A�Q�A�I�A�M�A�M�A�O�A�M�A�A�A�A�A�C�A�E�A�A�A�?}A�K�A�E�A�?}A�=qA�C�A�K�A�S�A�A�A�I�A�I�A�?}A�=qA�A�A�A�A�O�A�A�A�E�A�;dA�A�A�C�A�=qA�E�A�?}A�;dA�=qA�I�A�=qA�?}A�K�A�E�A�G�A�I�A�K�A�E�A�VA�I�A�O�A�E�A�C�A�M�A�K�A�K�A�O�A�\)A�C�A�K�A�\)A�I�A�E�A�C�A�G�A�I�A�O�A�G�A�S�A�M�A�G�A�O�A�G�A�G�A�I�A�E�A�E�A�M�A�G�A�C�A�K�A�G�A�I�A�M�A�?}A�?}A�VA�G�A�I�A�A�A�O�A�G�A�I�A�O�A�A�A�E�A�O�A�Q�A�G�A�O�A�E�A�G�A�C�A�O�A�=qA�?}A�C�A�C�A�K�A�?}A�E�A�G�A�G�A�A�A�=qA�E�A�A�A�K�A�K�A�K�A�K�A�7LA�Q�A�O�A�G�A�K�A�I�A�?}A�;dA�7LA�9XA�A�A�9XA�?}A�C�A�?}A�33A�;dA�A�A�33A�;dA�33A�9XA�A�A�G�A�Q�A�C�A�I�A�C�A�C�A�A�A�I�A�?}A�G�A�E�A�?}A�=qA�9XA�A�A�1'A�C�A�33A�33A�?}A�O�A���A�=qA�5?A�1'A�33A�?}A�33A�/A�=qA�7LA�33A�5?A�5?A�?}A�7LA�;dA�/A�=qA�C�A�(�A�&�A�(�A�5?A�1'A�/A�7LA� �A�/A�-A�/A�/A�/A�;dA�C�A�?}A�5?A�33A�7LA�ȴA�=qA�5?A�33A�5?A�+A�/A�=qA�=qA�=qA�33A�33A�33A�33A�/A�/A�5?A�33A�1'A�33A�33A�33A�/A�/A�-A�-A�+A�7LA�/A�1'A�5?A�1'A�1'A�5?A�5?A�1'A�1'A�1'A�33A�$�A�-A�1'A�-A�(�A�-A�+A�&�A�/A�(�A�$�A�(�A� �A� �A�"�A��A�"�A�$�A�&�A�1'A�$�A�-A�"�A��A��A�bA�{A�VA�JA��A�oA�{A�bA�{A��A��A��A��A��A��A��A��A�&�A�(�A��A��A�-A�5?A�-A�5?A�33A�"�A�(�A��A�VA�bA�{A�oA�VA�VA�
=A�%A�  A���A���A���A��A���A���A���A���A���A��A��A��A���A��yA���A��A��HA��A��A��/A�ȴA���A���A̾wA���A���A���A̸RA̸RA̺^A̶FA̲-A̮A̼jA̺^A���A̼jA̲-A̡�A̍PȦ+A̅Ȧ+Ȧ+Ȧ+Ȧ+Ȧ+A̅ÃÃA̅A̅A̅A�~�A�|�A�|�A�z�A�~�A�z�A�z�A�z�A�|�A�z�A�v�A�t�A�x�A�v�A�v�A�z�A�x�A�t�A�p�A�dZA�jA�hsA�\)A�VA�S�A�S�A�VA�ZA�S�A�M�A�C�A�C�A�G�A�?}A�=qA�C�A�A�A�;dA�;dA�;dA�5?A�7LA�7LA�7LA�7LA�7LA�5?A�5?A�33A�+A� �A� �A� �A�"�A��A� �A��A��A��A��A�{A��A�{A�{A�oA�oA�JA�JA�JA�VA�JA�
=A�1A�%A�%A�%A�%A�A�A�A���A���A���A���A���A���A�  A�  A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��yA��yA��yA��yA��yA��mA��mA��mA��mA��mA��mA��`A��`A��mA��mA��m@���@���@��P@��P@�|�@�t�@�l�@�\)@�\)@�dZ@�\)@�dZ@�dZ@�\)@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�+@�;d@�"�@��@��@�o@�o@��@��@�o@�o@�o@�o@�o@�o@�
=@�
=@�@�@�@���@���@���@���@���@�@�@�@���@���@��@��H@���@���@�@���@�@��@��y@��y@��@��@��@��@��y@��y@��H@��y@��H@��H@��H@��H@��@��@���@�ȴ@���@���@���@��R@��!@��!@��!@���@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��\@��+@��+@��+@��+@��+@��+@��+A�VA�bA�{A�oA�VA�VA�
=A�%A�  A���A���A���A��A���A���A���A���A���A��A��A��A���A��yA���A��A��HA��A��A��/A�ȴA���A���A̾wA���A���A���A̸RA̸RA̺^A̶FA̲-A̮A̼jA̺^A���A̼jA̲-A̡�A̍PȦ+A̅Ȧ+Ȧ+Ȧ+Ȧ+Ȧ+A̅ÃÃA̅A̅A̅A�~�A�|�A�|�A�z�A�~�A�z�A�z�A�z�A�|�A�z�A�v�A�t�A�x�A�v�A�v�A�z�A�x�A�t�A�p�A�dZA�jA�hsA�\)A�VA�S�A�S�A�VA�ZA�S�A�M�A�C�A�C�A�G�A�?}A�=qA�C�A�A�A�;dA�;dA�;dA�5?A�7LA�7LA�7LA�7LA�7LA�5?A�5?A�33A�+A� �A� �A� �A�"�A��A� �A��A��A��A��A�{A��A�{A�{A�oA�oA�JA�JA�JA�VA�JA�
=A�1A�%A�%A�%A�%A�A�A�A���A���A���A���A���A���A�  A�  A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��yA��yA��yA��yA��yA��mA��mA��mA��mA��mA��mA��`A��`A��mA��mA��m@���@���@��P@��P@�|�@�t�@�l�@�\)@�\)@�dZ@�\)@�dZ@�dZ@�\)@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�+@�;d@�"�@��@��@�o@�o@��@��@�o@�o@�o@�o@�o@�o@�
=@�
=@�@�@�@���@���@���@���@���@�@�@�@���@���@��@��H@���@���@�@���@�@��@��y@��y@��@��@��@��@��y@��y@��H@��y@��H@��H@��H@��H@��@��@���@�ȴ@���@���@���@��R@��!@��!@��!@���@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��\@��+@��+@��+@��+@��+@��+@��+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=�:?=ٞ�>1��@�_�@7�>�6@�ә?\S=���?�r�=��>*gb@�n@�n�>C��>A�@�q@��]?��=�ܜ>ϳ�?G��@�xW=�ӄ=�o�?�p;@�nD=��I>�>��=|eA=�¤=�=��1>nN'>1�=ʦL?S�>	ԕ>�%F@�oi>�j�>�c�@�m	=�g�=�eV>��@O=2@�o�>��>U�C@�m	=f�3=|eA=t)�=�n�=��
> �B?�=G=�*�=靈>���@�l�@�H�>f�@cp&?uKI?P�O=�n�=��>�?�m=�8�>�?!K�={9=���=��=�J�>�6?�x�>�@S��@�q�=��>68�?�JM@�r=�3r>��@N�@���=��>��>s��@�zx@�~|=��>�	=�fQ=��=�I=>�@�=�~�=�K^>	�4@<K@�q�=�!=�3r>AJ=ɯ�>�y@��@!8G=��Q>\��@���@���=ʰ�=�@d>y0�@���@���=��?��?@��M@�l�@�X>��?E��@�w2@�w�?�L�>#�>���>Q6@���@��8@�[�=���=��@>-y�@O�z@���=���=ƒ�=��2?��x@��
@���=å<> ٔ@C;�@��
?n�>�@;I@��>>��?��@�ݘ>��R@\S=��{>T�\@[��>5y)@��/?���>0�@���@���@���>�~�>ٹ>At@���@��>K&�>��?�5?@��Y@���?��>�/@�">@���@�Y�@���@��k=��?�t�@�q"?X�K@���@��{?���>�ć@��M?�f@��<?f��@�&�@���?���@�n�@��@B�@>��@���@���@��I@��{@��3@��8@��
@��{@���@���@"�B@���@��
@���@��@���@��
@���@���@��k@���@��M@���@��@���@���@���@���@��M@��I@���@��Z@��M@��8@��8@��M@��M@���@��'@���@���@��s@���@��I@��I@��M@���@��M@���@���@��M@��<@���@��I@���@��s@���@���@��8@���@��@��<@��I@��{@���@��
@���@���@���@���@���@���@���@��k@���@��^@���@��@���@���@��<@��<@��@��M@��@��<@��
@���@���@��@��^@���@���@��'@��{@��@��^@���@��Z@��Z@���@���@��@��8@���@��@���@���@��Z@���@���@��s@���@��@���@��@���@���@��8@���@���@��@���@��4@��@���@���@���@���@��@���@��<@��k@��I@��Z@���@���@��@��@��I@���@��M@���@��'@��M@���@��<@���@��@��,@��I@��Z@��I@��@��I@���@���@��{@���@��@��,@���@��Z@��@��<@��@��Z@��k@���@��@��@��@���@���@��I@��,@��Z@��@��@��@��Z@��@�~g@��I@���@�~(@��k@��Z@���@��<@���@�~�@�~g@�~�@�}�@�{�@�~�@�|�@�z%@�zx@�{5@�yh@�w�@�xB@�z�@�|�@�z%@�z%@�zx@�{�@�xB@�y@�z%@�xB@�w�@�~|@�{�@�|�@�z�@�x�@k~�@�{�@�{�@�yh@�w2@�zx@�v�@�w�@�}V@�|�@�{�@�x�@�v�@�{�@�{�@�|�@�}�@�|F@�|F@�{�@�z�@�w�@�}V@�x�@�v!@�t?@�v�@�{5@�z%@�z%@�{5@�z%@�|F@�z%@�x�@�xB@�w�@�w�@�w2@�ud@�w2@�yh@�x�@�v�@�yh@�x�@�w�@�y@�xW@�w2@�w�@�t?@�ud@�u@�v�@�r�@�w�@�z�@�v�@�w�@�u�@�s.@�s�@�r�@�r�@�pP@�q@�r�@�tT@�s�@�u�@�u�@�ud@�v�@�u@�t�@�w�@�v�@�u�@�w�@�xB@�v�@�v�@�x�@�{5@�{5@�y�@�yh@�w�@�w�@�w2@�y@�tT@�tT@�s�@�ud@�ti@�r@�o�@�o?@�pP@�pP@�m�@�lv@�m�@�m	@�j+@�j�@�h�@�h@�h�@�g8@�g8@�h�@�g�@�c�@�f{@�aR@�Z�@�Z2@�Yu@�X�@�V�@�U@�SP@�W�@�Xd@�W�@�R�@�Q�@�P]@�O�@�OL@�Oa@�P]@�N�@�OL@�J�@�G�@�D�@�>�@�>�@�<u@�<K@�;:@�;O@�;�@�<K@�;O@�:�@�:?@�;O@�:�@�9�@�7�@�7L@�6�@�6;@�6�@�6P@�5�@�5�@�6;@�4�@�4n@�4n@�4n@�4@�3�@�3]@�1�@�.�@�.I@�+k@�*Z@�*o@�'�@�%�@�%p@�%�@�&-@�%�@� �@� q@��@��@��@��@�m@��@��@�a@��@��@�;@��@��@��@�~@��@�/@�r@��@��@��@��@�(@��@��@�o@�@�J@�9@�
�@�
=@�	�@�	l@�	l@��@��@��@��@�%@�%@�)@��@��@��@��@��@�q@�K@�6@��@�&@� �@� �@� �@�&@� i@� �@� �@� �@� i@��C@��C@���@��r@���@��"@���@��@��@��@���@���@���@��U@���@���@���@��D@��U@���@���@���@��U@��@��j@��@@��@@���@��@��D@���@���@��/@���@���@���@���@���@��/@���@���@��D@���@���@��/@���@���@���@���@��U@Pǹ@P�e@P�@P�@P��@P�z@P��@P�+@P�@P�+@P�@P�@P��@P�/@P��@P��@P��@P�/@P��@P�4@P��@P��@P��@P�<@P��@P��@P��@P�8@P��@P��@P��@P��@P�f@P��@P�j@P��@P�s@P��@P��@P�|@P�R@P�(@P�V@P��@P�(@P��@P��@P��@P��@P�@P��@P��@P�[@P��@P�_@P�@P�_@P�_@P�5@P��@P��@P��@P�_@P�5@P��@P�@P�_@P�_@P��@P��@P�5@P�@P��@P��@P�9@P��@P�@P��@P�h@P�>@P�>@P��@P��@P�@P�B@P��@P��@P��@P��@P�)@P��@P�.@P��@P�2@P��@P�6@P��@P�@P��@P��@P��@P��@P��@P�?@P��@P��@P��@P��@P��@P��@P�@P��@P�H@P�H@P��@P��@P�H@P�H@P�H@P�H@��*@���@���@��K@���@�� @��\@��P@��@��v@���@��Y@���@��	@���@���@���@��@���@���@��@���@���@��+@��+@�}�@��V@��@�}@�r\@�w2@�ud@�oi@�p�@�ud@�vu@�l�@�m�@�m�@�l�@�k'@�h�@�m�@�m@�p@�o @�k�@�b�@�Y�@�W�@�WT@�W�@�W?@�WT@�WT@�W�@�V�@�V.@�VX@�V�@�V�@�V�@�S�@�S&@�S�@�R~@�T�@�S&@�R�@�Ri@�Se@�S&@�Q�@�O�@�Q�@�Qn@�P�@�Q�@�Qn@�O�@�M�@�H�@�K�@�K4@�D�@�B1@�A�@�A@�BF@�D�@�Bp@�?}@�:�@�:�@�<�@�8q@�7�@�:i@�9�@�77@�7�@�7a@�4�@�5@�4�@�5T@�5+@�5�@�4�@�4n@�3r@�1�@�+�@�+�@�+�@�,(@�+@�,(@�*@�(�@�'�@�'g@�&�@�&�@�&�@�&�@�%�@�%�@�"}@�"}@�"�@�#�@�">@�!@�!@� @� �@��@� �@�@�K@�@�i@�~@��@�@��@��@��@��@��@�~@�@�@�X@�@�H@��@�@@��@�@�@�D@�P@�P@�@@��@�D@��@��@�@@��@��@��@��@�@�~@��@��@��@�j@��@��@��@��@��@�n@�n@��@��@�n@��@��@��@��@�n@��@��@��@��@��@P��@P��@P�C@P��@P��@P�+@P܇@P�@P�f@P�b@P��@P�8@P�8@P�@Pپ@Pٔ@Pٔ@P�@@Pپ@P�j@P�@@P�E@P�E@P��@P�@P�I@P�I@P��@P�s@P�s@Pם@P�s@Pם@P�s@P�@P�V@P�s@P�[@P�5@P�@Pэ@P��@P�c@P�5@PБ@P�@P��@Pл@P��@P�9@Pл@P�@P�B@P��@P�B@P��@P�@P�F@PΚ@P��@P��@P��@P�@P��@P�F@P�@P�.@P�F@P��@P�B@P�@P�l@P��@P��@P��@P�@P��@P�K@P��@P̣@P̣@P�%@P��@P��@P�S@P�%@P��@P��@P�.@P��@P�@Pƽ@Pƽ@P�i@P�m@Pŗ@P�H@P�@Pà@Pà@P�v@P�"@P��@P¤@P��@P�U@P��@P�U@P��@P�Y@P��@P��@P�Y@P��@P��@P�/@P�@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             4444344344444334433444434443444444444444434434444344344444444443343444444444444444433444344434443344444444443444443444334443344334443344443334444344443344434443443444434344333444334443344333334434334434343343344333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�_�G�O�G�O�@�ӗG�O�G�O�G�O�G�O�G�O�@�n@�n�G�O�G�O�@�q@��\G�O�G�O�G�O�G�O�@�xYG�O�G�O�G�O�@�nCG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ojG�O�G�O�@�m	G�O�G�O�G�O�G�O�@�o�G�O�G�O�@�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�l�@�H�G�O�@cp%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@S��@�q�G�O�G�O�G�O�@�rG�O�G�O�G�O�@���G�O�G�O�G�O�@�zz@�~~G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�q�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@���@���G�O�G�O�G�O�@���@���G�O�G�O�@��N@�l�G�O�G�O�G�O�@�w2@�w�G�O�G�O�G�O�G�O�@���@��6@�[�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@��@���G�O�G�O�G�O�@��
G�O�G�O�G�O�@��=G�O�G�O�@�ݚG�O�G�O�G�O�G�O�@[��G�O�@��/G�O�G�O�@���@���@���G�O�G�O�G�O�@���@��G�O�G�O�G�O�@��_@���G�O�G�O�@�"<@���@�Y�@���@��kG�O�G�O�@�q!G�O�@���@���G�O�G�O�@��NG�O�@��>G�O�@�&�@���G�O�@�n�@��G�O�G�O�@���@���@��K@��z@��6@��9@��
@��|@���@���G�O�@���@��
@���@��@���@��@���@���@��k@���@��N@���@��@���@���@���@���@��N@��J@���@��X@��N@��6@��8@��N@��R@���@��$@���@���@��v@���@��F@��J@��N@���@��N@���@���@��N@��>@���@��H@���@��w@���@���@��9@���@��@��>@��G@��@���@��@���@���@���@���@��~@���@���@��k@���@��`@���@��@���@���@��>@��>@��@��N@��@��>@��@���@���@��@��_@���@���@��*@��|@��@��`@���@��Y@��Y@���@���@��@��8@���@�� @���@���@��Y@���@���@��r@��~@��@���@��@���@���@��6@���@���@��@���@��7@��@���@���@���@���@��@���@��:@��k@��M@��Z@��@���@��@��@��J@���@��O@���@��(@��N@��~@��>@���@��@��,@��I@��[@��J@��@��J@��~@���@��y@���@��
@��)@���@��X@��	@��<@��@��[@��m@���@��@��@��@���@���@��I@��*@��X@��@��@��	@��Y@��@�~i@��F@���@�~#@��j@��Y@���@��B@���@�~�@�~h@�~�@�}�@�{�@�~�@�|�@�z%@�zx@�{4@�yj@�w�@�xD@�z�@�|�@�z%@�z$@�zz@�{�@�xA@�y@�z%@�xG@�w�@�~{@�{�@�|�@�z�@�x�@k~�@�{�@�{�@�yj@�w6@�zz@�v�@�w�@�}X@�|�@�{�@�x�@�v�@�{�@�{�@�|�@�}�@�|F@�|I@�{�@�z�@�w�@�}S@�x�@�v!@�t?@�v�@�{7@�z"@�z"@�{:@�z"@�|I@�z*@�y @�xB@�w�@�w�@�w2@�ud@�w0@�yf@�x�@�v�@�yh@�x�@�w�@�y@�x\@�w2@�w�@�t<@�ud@�u@�v�@�r�@�w�@�z�@�v�@�w�@�u�@�s.@�s�@�r�@�r�@�pR@�q@�r�@�tR@�s�@�u�@�u�@�uf@�v�@�u@�t�@�w�@�v�@�u�@�w�@�xE@�v�@�v�@�x�@�{6@�{9@�y�@�yh@�w�@�w�@�w6@�y@�tV@��(@���@���@��N@���@���@��[@��T@��@��}@���@��X@���@��@���@���@���@���@���@���@��@���@���@��,@��.@�~@��V@��@�}@�r]@�w2@�ud@�oj@�p�@�uf@�vv@�l�@�m�@�m�@�l�@�k*@�h�@�m�@�m@�p@�n�@�k�@�b�@�Y�@�W�@�WU@�W�@�W?@�WS@�WU@�W�@�V�@�V-@�VV@�V�@�V�@�V�@�S�@�S(@�S�@�R{@�T�@�S&@�R�@�Rg@�Sf@�S&@�Q�@�O�@�Q�@�Qp@�P�@�Q�@�Qp@�O�@�M�@�H�@�K�@�K7@�D�@�B6@�A�@�A@�BF@�D�@�Bp@�?~@�:�@�:�@�<�@�8t@�7�@�:f@�9�@�72@�7�@�7a@�4�@�5@�4�@�5U@�5,@�5�@�4�@�4m@�3q@�1�@�+�@�+�@�+�@�,'@�+@�,(@�*@�(�@�'�@�'g@�&�@�&�@�&�@�&�@�%�@�%�@�"~@�"�@�"�@�#�@�"B@� �@�!@� @� �@��@� �@�@�M@�@�k@�@��@�@��@��@��@��@��@�|@�@�@�Z@�@�E@��@�@@��@�@�@�H@�N@�R@�D@��@�F@��@��@�D@��@��@��@��@�@�@��@��@��@�j@��@��@��@��@��@�q@�n@��@��@�j@��@��@��@��@�m@��@��@��@��@��@P��@P��@P�C@P��@P��@P�-@P܊@P�@P�f@P�c@P��@P�6@P�:@P�@Pپ@Pٕ@P٘@P�B@Pپ@P�k@P�@@P�F@P�E@P��@P�@P�M@P�H@P��@P�s@P�s@Pכ@P�s@Pנ@P�s@P� @P�X@P�r@P�[@P�5@P�@Pэ@P��@P�e@P�6@PГ@P�@P��@Pк@P��@P�:@Pн@P�@P�F@P��@P�F@P��@P�@P�F@PΕ@P��@P��@P��@P�@P��@P�C@P�"@P�2@P�F@P��@P�B@P�@P�p@P��@P��@P��@P�@P��@P�K@P��@P̠@P̣@P�%@P��@P��@P�S@P�&@P��@P��@P�+@P��@P�@Pƻ@Pƾ@P�k@P�k@PŚ@P�K@P�@Pã@Pà@P�u@P�#@P��@P¦@P��@P�V@P��@P�V@P��@P�`@P��@P��@P�Z@P��@P��@P�0@P�@P��@P��@P��@��(@���@���@��N@���@���@��[@��T@��@��}@���@��X@���@��@���@���@���@���@���@���@��@���@���@��,@��.@�~@��V@��@�}@�r]@�w2@�ud@�oj@�p�@�uf@�vv@�l�@�m�@�m�@�l�@�k*@�h�@�m�@�m@�p@�n�@�k�@�b�@�Y�@�W�@�WU@�W�@�W?@�WS@�WU@�W�@�V�@�V-@�VV@�V�@�V�@�V�@�S�@�S(@�S�@�R{@�T�@�S&@�R�@�Rg@�Sf@�S&@�Q�@�O�@�Q�@�Qp@�P�@�Q�@�Qp@�O�@�M�@�H�@�K�@�K7@�D�@�B6@�A�@�A@�BF@�D�@�Bp@�?~@�:�@�:�@�<�@�8t@�7�@�:f@�9�@�72@�7�@�7a@�4�@�5@�4�@�5U@�5,@�5�@�4�@�4m@�3q@�1�@�+�@�+�@�+�@�,'@�+@�,(@�*@�(�@�'�@�'g@�&�@�&�@�&�@�&�@�%�@�%�@�"~@�"�@�"�@�#�@�"B@� �@�!@� @� �@��@� �@�@�M@�@�k@�@��@�@��@��@��@��@��@�|@�@�@�Z@�@�E@��@�@@��@�@�@�H@�N@�R@�D@��@�F@��@��@�D@��@��@��@��@�@�@��@��@��@�j@��@��@��@��@��@�q@�n@��@��@�j@��@��@��@��@�m@��@��@��@��@��@P��@P��@P�C@P��@P��@P�-@P܊@P�@P�f@P�c@P��@P�6@P�:@P�@Pپ@Pٕ@P٘@P�B@Pپ@P�k@P�@@P�F@P�E@P��@P�@P�M@P�H@P��@P�s@P�s@Pכ@P�s@Pנ@P�s@P� @P�X@P�r@P�[@P�5@P�@Pэ@P��@P�e@P�6@PГ@P�@P��@Pк@P��@P�:@Pн@P�@P�F@P��@P�F@P��@P�@P�F@PΕ@P��@P��@P��@P�@P��@P�C@P�"@P�2@P�F@P��@P�B@P�@P�p@P��@P��@P��@P�@P��@P�K@P��@P̠@P̣@P�%@P��@P��@P�S@P�&@P��@P��@P�+@P��@P�@Pƻ@Pƾ@P�k@P�k@PŚ@P�K@P�@Pã@Pà@P�u@P�#@P��@P¦@P��@P�V@P��@P�V@P��@P�`@P��@P��@P�Z@P��@P��@P�0@P�@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             4444344344444334433444434443444444444444434434444344344444444443343444444444444444433444344434443344444444443444443444334443344334443344443334444344443344434443443444434344333444334443344333334434334434343343344333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9 ��9 �]9 �%9 ��9 �`9 ��9 �&9 �C9 �.9 ��9 ��9 ��9 �9 �59 �59 �39 �9 ��9 �'9 �R9 ��9 ��9 ��9 ��9 ��9 ��9 �9 ��9 �$9 �(9 ��9 ��9 �i9 ��9 ��9 ��9 ��9 ��9 ��9 ��9 �t9 �Q9 ��9 �B9 �9 �9 �9 ��9 xE9 va9 u�9 v89 u�9 u�9 u�9 vH9 uK9 t�9 u9 u�9 ut9 ut9 r�9 r9 r�9 qy9 s�9 r9 q�9 qf9 rS9 r9 p�9 n�9 p�9 p�9 p9 p�9 p�9 o#9 mK9 hC9 k9 j�9 d�9 bS9 b9 a?9 bb9 d�9 b�9 _�9 [�9 [H9 ]39 Y=9 Xv9 [9 Z�9 X9 Xw9 X=9 U�9 V9 U�9 VU9 V/9 V�9 U�9 U}9 T�9 R�9 MB9 MQ9 M�9 M�9 L�9 M�9 K�9 J�9 I�9 I]9 H�9 H�9 H�9 H�9 G�9 G�9 D�9 D�9 E;9 F+9 D�9 Cf9 C�9 B�9 B�9 B9 CA9 A�9 A�9 A�9 @9 @$9 @19 ?�9 @�9 @�9 @s9 @�9 @r9 @!9 ?�9 ?�9 ?9 >�9 >9 =�9 ;A9 :�9 ;	9 :9 :Z9 <=9 <@9 ;E9 9�9 :X9 9�9 9�9 ;E9 ;�9 ;�9 ;�9 ;�9 ;9 ;|9 ;�9 ;�9 ;�9 ;h9 :�9 :�9 :�9 :�9 :�9 :�9 :~9 :�9 :�9 :z9 :�9 :�9 :�9 :�9 :}9 :�9 :�9 :�9 :�9 :�8�{�8�{�8�|
8�z�8�x�8�x<8�v�8�uX8�t�8�u�8�u.8�uz8�u}8�uT8�t8�s�8�s�8�s�8�t8�s�8�s�8�r�8�r�8�q�8�q�8�q�8�q�8�q�8�q�8�q�8�r8�q�8�r#8�q�8�q�8�o8�q�8�n)8�m8�l�8�l{8�k�8�lV8�m8�k�8�l8�k�8�k�8�k�8�l.8�k�8�l8�j\8�i�8�j\8�i 8�iE8�in8�i�8�i�8�i�8�j
8�j48�i�8�ik8�iM8�e�8�in8�i�8�jY8�j28�j�8�i�8�g!8�h8�iF8�h78�h�8�i#8�g�8�g�8�gs8�gK8�gM8�f�8�gt8�h8�f?8�e�8�di8�c�8�bh8�bk8�b8�a/8�a[8�`#8�_�8�_�8�_�8�_\8�_8�^�8�^�8�]�8�]b8�]�8�]b8�\�8�\}8�\�8�\�8�\x8�[�8�[�8�\Q8�\*8�\8�[�8�[�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�hB�hB�bB�\B�VB�VB�JB�JB�DB�=B�DB�DB�DB�JB�DB�PB�PB�PB�PB�PB�VB�VB�\B�{B��BB�
B�fB�B��B�BB��B.B?}BZBdZB&�BB�mB��B�B��B��B��B��B��B�5B�BŢBɺB��B��B��B+BB��B��B�`B��Bk�B]/B5?B&�B\B��B�`B��B��B�uB�B}�Bw�Bo�BiyBr�Bw�B� Br�B_;BVB^5BXBL�B>wB8RB:^B-B�BB
�B
�HB
�B
��B
ÖB
�B
�bB
s�B
T�B
33B
�B	�B	�
B	�}B	�3B	�9B	�?B	�dB	�NB	�yB	�B	�B	�mB	�HB	��B	ĜB	�B	��B	�=B	x�B	t�B	n�B	_;B	K�B	.B	�B	{B	DB	B��B	
=B	\B	oB	
=B	B��B�NB��BǮBȴBƨBÖB��B�wB�?B�B��B��B��B�VB�7B�1B�=B�7B�1B�%B�1B�1B�1B�+B�B�B�B�B�%B�B� B� B�B�7B�%B�VB�PB�B�B|�Bx�B�\B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�'B�B�-B�XB�XB�RB�XB�XB�XB�XB�^B�^B�^B�^B�^B�dB�jB�jB�wB��B��B��BBB��B�}BBŢBĜB��B�wB�qB�RB�jB�wB�wB�wBBȴB��B��B�B�B�B��BƨB�^BBÖB�-B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�LB�dB�'B�LB�^B�LB�wB��B�BB�B�B��B��B��B��B��B��B��B��B�B�B�B�yB�ZB�HB�/B�#B�HB�B�B	B	\B	VB	\B	bB	uB	�B	�B	hB		7B��B�B�B�B�mB�NB�5B�5B�)B�#B�HB�mB�B�B�B�B�B��B	B	1B	DB	\B	\B	\B	\B	hB	�B	�B	�B	�B	�B	�B	$�B	%�B	%�B	%�B	%�B	&�B	0!B	6FB	:^B	;dB	;dB	:^B	;dB	<jB	<jB	;dB	9XB	8RB	6FB	7LB	;dB	;dB	;dB	;dB	>wB	A�B	B�B	G�B	J�B	N�B	Q�B	R�B	R�B	Q�B	Q�B	W
B	ZB	YB	W
B	W
B	YB	]/B	]/B	^5B	`BB	bNB	cTB	dZB	iyB	m�B	o�B	p�B	r�B	s�B	t�B	t�B	u�B	x�B	x�B	z�B	{�B	�B	�B	�B	�%B	�1B	�7B	�=B	�DB	�JB	�JB	�PB	�PB	�VB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�FB	�RB	�RB	�XB	�XB	�XB	�XB	�dB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	�}B	B	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�)B	�/B	�/B	�5B	�;B	�BB	�TB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	��B	��B
�B
B
{B
!|B
(sB
0UB
7�B
BAB
G�B
L�B
N�B
S�B
V�B
]dB
abB
e�B
j�B
m�B
q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��j>�?��?`�MB]
A�Xz?5e^AΖ`@CZ�?!��Ah�?
��?X��Bh�Bn�?~4�?v�Bk�B�@���?�>@	~�@��uB��>��|?��A޲B��>�N�?7��?Ͷx>��>���?с>ө�?�S�?b�>�
B@��{?-ѳ@\B�Q?��@�wB�?��?L�@��A�|DBl�?�p�?���Br>���>��>�>>>�4�>��?!ދA&jy>���?`�?��aBj�B��?1�kA��@���@�j�>�ۥ?vy?$Ϟ@G�l>��?G`J@ȩ>���>ʺ8>��j?�G?(1�@��!?.��A��!B�@?��?i�(A7T'Bo�?	@�?;�A�{6B�<?��?�~c?�q�Bj�B��>�7?�<>�d>�7">���?�y�>ī�>�%?,�AFg�B��>��?p?D��?C?=*A�,A|ʖ?B�?�[�BuBt�>��?�?��*Bk�Br]?�A��Bn'BZNAD5�?2~�@�.BfBg8@���?D�?آ�?�}RBp�Bo�Bl�?�|?�M?\�A��Br>��'>��;?ОAYBr�B��>�Sz?�A�>�Bph@��?:�*A�_�A�/?4�A*�VB�a?�ŋAX�&?�?���A��?f��BuoA}�?]��Bt�A�E�B}�?��I@P3?D8mBp�B�?�;�?*�@�?dBnsB��@�z�?FU�AνBsA�5�Bn�Bn�?J,@�k�BZA@���B�%B��A6y�@JB� A4��BtG@���B
GBt�A�B��A��QA��AA�+Bu�Bs�BuUBuJAρ"BqDA���BiuBq�Bt At�>Bn�Bq2Bm�Bl�BlBr�BkaBp%Bj�Bo�Bn'Bl�BkNBoZBr-Br�BpjBqOBs�BrJBl-BqFBs�Bt[BrBn�BkaBq�BnFBoBt�Bs�Bs�Bs�BmUBr-BqOBvtBsBrBs}Bp�Bt�Bt�Bu�Bo�BsBs�Bn3Bo@Bo�Bp�Bn(Bo�Bk�BrBn�Bp�Bn�Bl�BpKBo+Bk�Bg�Bs
BmBBk/Bp�Br�Bq'Bo�Bm�BmUBm|Bj�Bn�Br�Bn�BrBqvBrBr�BpBBm_Br+Bs
Bo5Bm�Bl�Bm�Bs�Bp�Bj�Br�BqYBs'Bl�Bm�Bm_Bl�Bs�Bo�BkNBj�Bm|BlBq�BpiBqlBk	BpBsBnOBoBm�BpWBo�Bp�BnvBs0BstBo�Bo-Bl5BmJBk�Bl�Bt�Bh�Bl�Bp}Bm{Bn�Bs�Bs�Bu�Bt�Bp�BtBo�BoeBo�Bt[Bq�Bq3BuBtxBvBs
Bp�Bn)Bi�BoBn�BoBoeBq(BoRBp�Bl"BmABr7BrBrDBp�BvvBp	Bt]Bu_Bp�BiB�$Bp�BtQBs�Bv�Bp�Bw�Bx�Bs�Bq�Br�Br�Bq�Bk�Bq�Bn=Bp�BktBi�BrVBq�BqQBn�Br/Bp�Bm�BvxBr\Bo�Bo�Bp�Bn�Bi�Bl�Bk�Bp�Bo�Bl+A��BlyBo�BndBk�Br�BmWBiBnBmsBpsBnBlBpsBrBr�Bq�BqBq�BpkBo�Bl�Bs�BoSBm�BlBn�BnwBp�Bo�BoABo�Bq�BnGBm:Bn)Bm�Bm�Bl]Bp<Bn�Bo.BpBo�Bp�Bp�BqyBo�BqdBq�Bp�Bp�Bq�Bp�Bs�Bn�BrBtiBllBr"Bm@Bn�Bp�Bq\Bu�Bq�Bt�BwBs�Bu�Bv�Bx&Bv)BuBuBsBu�Bs�Bs@BvsBvCBp8BoeBuBy�Bq�BmMBpBkIBk�Bq�Bq;Bq�Bo-BpBpBp�BkqBn�Br8Bq�Bo�Bq�Bm+BqwBm�Bl�Bo�BmwBoGBp�Bo-Bo�Bn7Bm�Bk�Bo�Bg�BuBf�Bn�BqGBpbBpFBq�Bv�Bs'BrqBmVBt1Bq'BqOBr{Bq�Bq�BpBp�Bo�Bj^BmBn�Br�Bq[Bq�Br�Bq�BrBq�Bp�Bq7Br)BsBryBp�Br
BpBp�Bo�Bq�Bo�Bp(Bq_Bq'Bp�Bp�Bp�Bq�Bp�BqBo�BnlBo`Bn-Bp@BpBl�Bo/BqBq�Bq�BoBl�Bj8Bm�Bo3BnjBo�Bn5Bn�Bo�Bn�BnBpBnoBm�Bn�Bn�Bn�BnoBl8Bm�Bn	Bk�Bi�Bm�Bn�Bl�Bn�Bk�BmNBlBl�Bl�Bl�Bl�Bl#BmGBltBk�Bk�Bm(Bl%BlaBk�Bl4Bk�Bk�Bl9BlDBl4Bl�BlpBlUBk�Bl�BlGBm�Bm�Bl�Bl�Bl�Bl�Bl�Bk�BlDBmBl�Bl5Bm2Bl}BkBm�Bl�Bl�Bm�Bn�BnBnvBl-Bm�Bm�Bn�Bn<Bn�Bn�Bn]BmHBl�BnBm�Bn�Bm�BnVBncBmCBn�Bn7Bm�Bm�Bm�Bn�Bo�Bo�BpBo?Bo{Bo�Bo�BoOBo�BpuBpmBp�Bp#B	��B	ЕB	ТB	�4B	��B	�	B	сB	�B	�=B	��B	�"B	�B	ҀB	�B	��B	ҢB	ҕB	��B	�nB	��B	�nB	�SB	��B	�cB	ҴB	ӷB	ҙB	��B	�rB	�eB	�*B	�B	��B	�zB	�B	�&B	�gB	�hB	�?B	�B	��B	һB	�B	�HB	ӗB	� B	�2B	�CB	��B	�tB	�B	�uB	��B	�kB	�B	ӺB	��B	��B	ӤB	��B	��B	һB	�pB	�TB	�CB	ӾB	�>B	�1B	�sB	�GB	��B	��B	��B	�.B	��B	ҜB	�B	�6B	�B	��B	��B	�kB	�|B	��B	��B	яB	ѻB	�	B	�rB	��B	�dB	��B	ҰB	�+B	��B	�hB	�B	�3B	��B	ӯB	��B	�*B	��B	ӇB	�LB	�B	��B	�_B	�bB	�HB	�ZB	�>B	ӘB	ԛB	��B	ԱB	�fB	�YB	�OB	�2B��B�6B�ZB��B��B�2B�>B��B�0B�rB�B�B��B��B�:B�aB�B��B��B�^B��B�{B��B�B��B�pB�pB�BB�B�B�B��B�4B��B�B�?B��B��B��B��B��B�(B�-B�BB��B�B�B�BB��B�#B��B��B��B��B��B��B��B��B�B��B��B��B�B�TB��B�lB�B��B��B�/B�CB��B��B��B�AB��B�B�VB��B��B��B�_B��B�B�B��B�MB��B��B�dB��B�LB�B��B��B�3B�4B�\B��B�~B��B��B�gB��B��B�B��B�RB�gB��B��B�7B��B��B��B�-B��B��B��B��B��B�LB�NB��B�5B�B��B��B�rB�aB��B��B�B��B�}B�XB��B��B��B��B�(B��B��B��B��B�MB�B��B�B�.B��B�6B��B��B��B�ZB�\B��B��B��B��B�4B�^B��B��B�YB��B�,B��B��B�(B��B��B��B��B��B� B�xB�oB��B��B��B��B�!B��B��B�WB�FB�xB�pB��B�B�B�B�	B��B��B��B��B��B��B	�)B	�B	�OB	�LB	��B	�KB	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�vB	�+B	�mB	�#B	�B	�4B	�7B	�4B	�8B	�JB	�=B	�B	�4B	�'B	�8B	�B	�!B	��B	�B	�B	��B	��B	�B	��B	�}B	��B	�&B	�B	�B	��B	�B	�lB	�qB	�B	�HB	�kB	�B	�B	�B	�B	�B	�%B	�HB	�ZB	�<B	�@B	�RB	�B	�B	�B	�B	�yB	�B	��B	��B	��B	�B	�cB	�B	��B	��B	�B	�B	�B	�yB	� B	��B	��B	�QB	��B	�LB	��B	�HB	�VB	�B	�B	�B	�ZB	�B	��B	�B	�B	�1B	�B	��B	�B	�XB	�,B	�B	�,B	�nB	�B	�nB	�5B	�FB	�9B	� B	�jB	�\B	�B	�B	�eB	�,B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444344344444334433444434443444444444444434434444344344444444443343444444444444444433444344434443344444444443444443444334443344334443344443334444344443344434443443444434344333444334443344333334434334434343343344333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B�_B�_B�_B�OB�LB�QB�BB�BB�<B�4B�<B�>B�>B�AB�:B�JB�JB�LB�JB�LB�LB�OB�UB�qB��BB�B�[B�B��B�:B��B.B?yBZBdRB&�B �B�eB��B�B��B�B��B��B��B�.B�BŚBɳB��B��B��B%BB��B��B�XB��Bk~B](B54B&�BPB��B�[B��B��B�nB�B}�Bw�Bo�BisBr�Bw�B�Br�B_3BU�B^-BX	BL�B>oB8JB:WB-B�B �B
�B
�>B
�B
��B
ÍB
�B
�^B
s�B
T�B
3*B
�B	�B	�B	�uB	�+B	�0B	�8B	�\B	�DB	�rB	�B	�B	�dB	�@B	��B	ĒB	��B	�xB	�5B	x�B	t�B	n�B	_/B	K�B	.
B	�B	uB	9B	B��B	
3B	RB	eB	
4B	B��B�BBʶBǤBȨBƝBËB�yB�nB�6B�B��B��B��B�KB�.B�&B�3B�*B�(B�B�&B�(B�'B�"B�B��B��B�B�B�B�B�B�B�.B�B�KB�DB�B��B|�Bx�B�QB�QB�xB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�	B�B�B�B�B�!B�MB�NB�HB�JB�LB�MB�MB�RB�SB�TB�RB�SB�WB�`B�`B�kB�~B�yB�BBB�xB�tBBŘBĒB�}B�iB�eB�GB�aB�jB�kB�lBBȩB��B��B��B�B�B��BƚB�UBBËB�"B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�VB�B�@B�QB�@B�lB�{B�6B�B�B��B��B��B��B��B��B��B��B�B�B�B�nB�PB�>B�%B�B�;B�B�B	B	PB	JB	NB	YB	jB	�B	�B	]B		*B��B�B�B�~B�aB�AB�+B�(B�B�B�;B�`B�sB�B�B�B�B��B	 �B	&B	7B	RB	QB	QB	OB	\B	zB	�B	�B	�B	�B	�B	$�B	%�B	%�B	%�B	%�B	&�B	0B	6<B	:RB	;YB	;UB	:PB	;XB	<^B	<^B	;ZB	9LB	8EB	6:B	7>B	;YB	;YB	;YB	;ZB	>kB	AB	B�B	G�B	J�B	N�B	Q�B	R�B	R�B	Q�B	Q�B	V�B	ZB	Y	B	V�B	V�B	YB	]"B	]$B	^'B	`9B	bCB	cGB	dOB	imB	m�B	o�B	p�B	r�B	s�B	t�B	t�B	u�B	x�B	x�B	z�B	{�B	��B	�B	�B	�B	�(B	�+B	�4B	�9B	�>B	�@B	�DB	�EB	�KB	�PB	�ZB	�fB	�iB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�:B	�HB	�EB	�KB	�MB	�MB	�KB	�WB	�eB	�jB	�lB	�hB	�iB	�kB	�lB	�kB	�pB	�qB	B	œB	ƜB	ǣB	ǡB	ǠB	ǢB	˾B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�B	�"B	�!B	�*B	�/B	�8B	�FB	�SB	�SB	�TB	�TB	�SB	�UG�O�B	��B	��B
�B
�B
nB
!qB
(gB
0IB
7�B
B4B
G�B
L�B
N�B
S�B
V�B
]XB
aUB
e�B
j�B
m�B
q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B]G�O�G�O�AΖRG�O�G�O�G�O�G�O�G�O�Bh�Bn�G�O�G�O�Bk�B�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�JG�O�G�O�B�G�O�G�O�G�O�G�O�Bl�G�O�G�O�BrG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bj�B��G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��B�;G�O�G�O�G�O�Bo�G�O�G�O�G�O�B�3G�O�G�O�G�O�Bj�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�A�"G�O�G�O�G�O�BuBt�G�O�G�O�G�O�Bk�BrUG�O�G�O�Bn BZDG�O�G�O�G�O�BfBg2G�O�G�O�G�O�G�O�Bp�Bo�Bl�G�O�G�O�G�O�G�O�BrG�O�G�O�G�O�G�O�Br�B��G�O�G�O�G�O�Bp`G�O�G�O�G�O�A�!G�O�G�O�B�ZG�O�G�O�G�O�G�O�A��G�O�BuhG�O�G�O�Bt�A�E�B}�G�O�G�O�G�O�Bp�B�
G�O�G�O�G�O�BnnB��G�O�G�O�Aμ�BsA�5�Bn�Bn�G�O�G�O�BZ9G�O�B� B��G�O�G�O�B�G�O�Bt@G�O�B
GBtyG�O�B��A��DG�O�G�O�Bu�Bs�BuOBuCAρBq;A���BinBq�BtG�O�Bn�Bq*Bm�Bl�BlBr�Bk\BpBj�Bo�Bn Bl�BkGBoWBr&Br�BpcBqFBs�BrDBl$Bq=Bs�BtSBrBn�Bk\Bq�Bn?Bo Bt�Bs�Bs�Bs�BmNBr&BqFBvnBsBrBsvBp�Bt�Bt�Bu~Bo�BsBs�Bn,Bo8Bo�Bp�Bn$Bo�Bk�Br	Bn�Bp�Bn�BlwBp@Bo%Bk�Bg�BsBm<Bk'Bp�Br�Bq Bo�Bm�BmNBmuBj�Bn�Br�Bn�BrBqoBrBr�Bp;BmXBr$BsBo1Bm�Bl�Bm�Bs�Bp�Bj�Br�BqTBsBl�Bm�BmUBl�Bs�Bo�BkGBj�BmuBk�Bq�Bp`BqiBkBpBr�BnIBoBm�BpOBo�Bp�BnnBs)BslBo�Bo'Bl-BmCBk�Bl�Bt�Bh�Bl�BpvBmsBn�Bs�Bs�Bu�Bt�Bp�BtBo�Bo]Bo�BtSBq�Bq*BuBtoBvBsBp�Bn"Bi�Bo
Bn�BoBo]Bq BoLBp�BlBm8Br0BrBr=Bp�BvnBpBtSBuYBp�Bh�B�Bp�BtJBs�Bv�Bp�Bw�Bx�Bs�Bq�Br�Br�Bq�Bk�Bq�Bn6Bp�BkmBi�BrPBq�BqKBn�Br,Bp�Bm}BvrBrUBo�Bo�Bp�Bn�Bi�Bl�Bk�Bp�Bo�Bl$A��BlrBo�Bn_Bk�Br~BmPBiBnBmlBpmBnBlBpmBrBr�Bq�Bp�Bq�Bp`Bo�Bl�Bs�BoIBm�Bk�Bn�BnpBp�Bo�Bo=Bo�Bq�BnBBm2Bn"Bm�Bm�BlUBp5Bn�Bo'BpBo�Bp�Bp�BqqBo�Bq_Bq�Bp�Bp�Bq�Bp�Bs�Bn�BrxBt`BlcBrBm:Bn�Bp�BqSBu�Bq�Bt�BwBs�Bu�Bv�BxBv!BuBt�BsBu�Bs�Bs8BvhBv=Bp2Bo_BuBy�Bq�BmCBpBkEBk�Bq�Bq4Bq�B�xB�.B�VB��B��B�'B�6B��B�'B�oB�B�B��B��B�2B�XB��B��B��B�XB��B�xB��B��B��B�iB�iB�=B�B�B�B��B�-B��B�B�7B��B��B��B��B��B�"B�%B�6B��B�B�B�6B��B�B�~B��B��B��B��B��B��B��B�B��B��B��B�B�NB��B�bB�B��B��B�'B�>B��B��B��B�:B��B�B�MB��B��B��B�YB��B�B�B��B�HB�~B��B�aB��B�FB��B��B��B�/B�,B�QB��B�sB��B��B�`B��B��B�B��B�JB�`B��B��B�/B��B��B��B�$B��B��B��B��B��B�CB�EB��B�0B�B��B��B�hB�\B��B��B�
B��B�yB�PB��B��B��B��B�"B��B��B��B��B�HB�B��B�B�&B��B�-B��B��B��B�RB�RB��B��B�{B��B�-B�WB��B��B�UB��B�&B��B��B�$B��B��B��B��B��B�B�rB�iB��B��B��B��B�B��B��B�RB�>B�rB�kB��B��B�B�B��B��B��B��B��B��B��B	�B	�B	�BB	�BB	�B	�AB	�B	�B	�yB	�B	�B	��B	�B	�B	�B	�yB	�lB	� B	�`B	�B	��B	�)B	�+B	�(B	�+B	�@B	�0B	��B	�'B	�B	�,B	�B	�B	��B	�B	�B	�B	��B	��B	��B	�qB	��B	�B	�B	�xB	�B	�B	�_B	�hB	�B	�=B	�^B	�B	�B	��B	��B	�B	�B	�9B	�LB	�/B	�3B	�GB	�B	�B	�|B	�B	�mB	�B	��B	��B	��B	�B	�UB	��B	��B	��B	�B	�zB	�B	�mB	�B	��B	��B	�EB	��B	�@B	��B	�;B	�KB	�B	�B	�B	�OB	�B	�B	�B	�}B	�&B	�
B	��B	�B	�LB	� B	�B	�"B	�bB	��B	�bB	�.B	�:B	�.B	��B	�\B	�PB	�B	�B	�YB	� B	�B�xB�.B�VB��B��B�'B�6B��B�'B�oB�B�B��B��B�2B�XB��B��B��B�XB��B�xB��B��B��B�iB�iB�=B�B�B�B��B�-B��B�B�7B��B��B��B��B��B�"B�%B�6B��B�B�B�6B��B�B�~B��B��B��B��B��B��B��B�B��B��B��B�B�NB��B�bB�B��B��B�'B�>B��B��B��B�:B��B�B�MB��B��B��B�YB��B�B�B��B�HB�~B��B�aB��B�FB��B��B��B�/B�,B�QB��B�sB��B��B�`B��B��B�B��B�JB�`B��B��B�/B��B��B��B�$B��B��B��B��B��B�CB�EB��B�0B�B��B��B�hB�\B��B��B�
B��B�yB�PB��B��B��B��B�"B��B��B��B��B�HB�B��B�B�&B��B�-B��B��B��B�RB�RB��B��B�{B��B�-B�WB��B��B�UB��B�&B��B��B�$B��B��B��B��B��B�B�rB�iB��B��B��B��B�B��B��B�RB�>B�rB�kB��B��B�B�B��B��B��B��B��B��B��B	�B	�B	�BB	�BB	�B	�AB	�B	�B	�yB	�B	�B	��B	�B	�B	�B	�yB	�lB	� B	�`B	�B	��B	�)B	�+B	�(B	�+B	�@B	�0B	��B	�'B	�B	�,B	�B	�B	��B	�B	�B	�B	��B	��B	��B	�qB	��B	�B	�B	�xB	�B	�B	�_B	�hB	�B	�=B	�^B	�B	�B	��B	��B	�B	�B	�9B	�LB	�/B	�3B	�GB	�B	�B	�|B	�B	�mB	�B	��B	��B	��B	�B	�UB	��B	��B	��B	�B	�zB	�B	�mB	�B	��B	��B	�EB	��B	�@B	��B	�;B	�KB	�B	�B	�B	�OB	�B	�B	�B	�}B	�&B	�
B	��B	�B	�LB	� B	�B	�"B	�bB	��B	�bB	�.B	�:B	�.B	��B	�\B	�PB	�B	�B	�YB	� B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444344344444334433444434443444444444444434434444344344444444443343444444444444444433444344434443344444444443444443444334443344334443344443334444344443344434443443444434344333444334443344333334434334434343343344333333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647152020083116471520200831164715202008311647152020083116471520200831164715202008311647152020083116471520200831164715202008311647152020083116471520200831164715AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816352019021918163520190219181635    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816352019021918163520190219181635  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816352019021918163520190219181635  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647152020083116471520200831164715  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                