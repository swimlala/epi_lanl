CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:34Z creation      
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
resolution        =���   axis      Z        +  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  p$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     +  z�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     +  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     +  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     + �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� <�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     + G�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     + r�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     + ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� Ә   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     + �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     + 	t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� 4�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     + ?P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� jd   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     + u,   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   Ũ   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   Ŵ   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , Ƙ   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �$Argo profile    3.1 1.2 19500101000000  20190219181734  20200831165016  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @֨�Ȫ-@֨�Ȫ-@֨�Ȫ-111 @֨:�r@֨:�r@֨:�r@6:�G�{@6:�G�{@6:�G�{�b���l��b���l��b���l�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @,��@�  @�33A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�qD�E�D��fD���D��D�PRD��HD��RD��\D�G\D���D��fD��D�8�D�nfD��D�� D�<)D�[�D��3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O������L�;��������L�;L�;������������L�ͽ��ͽ��;������������������������������������L�;��������������������������ͽ��;����L�;L�;��������L�;����������;����������������L�;������������������������������������������;����L�;L�;����������������������������L�;������������������������L�;L�;����L�;L�;������������������������L�;L�;����������������������������L�ͽ��;����L�;L�;��������L�;��������L��    �������������L�;������������L��        ���������L�;����������������L��=���    �����L�;L�;����������;��������������������L�;����L�;L�;������������L�;��������L�;��������������;��������L�;�������������������    ���;������������������������L�;��������L�ͽ��;����������������L�;����L�;����������ͽ��;L�;����L�;L�;�������    ���;����L�ͽ��;L�;����L�ͽ��;��������L�;��������������;L�;����L�;��������������;��������������������L�;L�;��������L�;L�;������������L�;L�ͽ��;L�;������������L�;L�;L�ͽ��;����L�;����L�ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��;����L�;L�ͽ��;����L�ͽ��ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ��;������;L�ͽ��;L�ͽ���    ����            ���ͽ��ͽ��ͽ��ͽ��ͽ���    ���;L��    ����                    =��ͽ��;L��    ���ͽ���        =���=���    ����    �L�ͽ���    ���ͽ���        ���ͽ���    ���ͽ���        ���ͽ��ͽ��ͽ��ͽ��;L��                ���ͽ���        ���ͽ��ͽ���            ���ͽ���        ���ͽ���    ���ͽ��ͽ���        ���;L�ͽ���=��ͽ���    ����    �L�ͽ��ͽ��ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ���    ���;L��    ���ͽ��ͽ���        ����    ���ͽ��ͽ��ͽ��ͽ���    ���;L��        ���ͽ���    ����            ���ͽ��ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ����    =���=���        =���    ���ͽ��ͽ���        ���ͽ���    ����            ����                    ���ͽ���    ����                =���        =���=���=���    =���            ����    ���ͽ��ͽ���    ���ͽ���                ���ͽ��ͽ��ͽ��ͽ��ͽ���=���    ����        ����    ����=���    �L�ͽ��ͽ��ͽ��ͽ��;L��    ���ͽ���    ���ͽ��ͽ��ͽ��;L�ͽ��ͽ���        �L��    ����        ���ͽ��;L��    �L�ͽ��ͽ��ͽ��ͽ���    ����        ���;L��    ���ͽ���                =���            ���ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ��;L�ͽ��;L�;L��    ����    =���=���>���>���?   ?   ?   ?��?333?L��?L��?fff?fff?���?���?���?���?�33?�33?���?���?ٙ�?ٙ�?�33?�33@   @ff@ff@33@��@   @   @,��@9��@@  @Fff@L��@S33@`  @fff@s33@y��@�  @�ff@���@�  @�33@�ff@���@�  @�33@���@���@�33@���@���@�  @�ff@ə�@�  @�ff@ٙ�@�  @�33@陚@���@�33@�ff@���A   A33A��A  A	��A33AffA  A33A��A  A��A33AffA!��A#33A&ffA(  A)��A,��A.ffA1��A333A6ffA8  A;33A<��A>ffAA��AD��AFffAH  AK33AL��AP  AQ��AT��AVffAX  A[33A\��A`  Ac33Ad��AfffAi��Ak33AnffAp  As33At��AvffAy��A{33A|��A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�ffA�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�Aə�A�ffA�33A�  A���A͙�A�ffA�33A�  A���Aљ�Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�33A�  A���Aٙ�A�ffA�33A�  A�  DqFfDqL�DqY�Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt�3Dt��Du  @   @,��@9��@@  @Fff@L��@S33@`  @fff@s33@y��@�  @�ff@���@�  @�33@�ff@���@�  @�33@���@���@�33@���@���@�  @�ff@ə�@�  @�ff@ٙ�@�  @�33@陚@���@�33@�ff@���A   A33A��A  A	��A33AffA  A33A��A  A��A33AffA!��A#33A&ffA(  A)��A,��A.ffA1��A333A6ffA8  A;33A<��A>ffAA��AD��AFffAH  AK33AL��AP  AQ��AT��AVffAX  A[33A\��A`  Ac33Ad��AfffAi��Ak33AnffAp  As33At��AvffAy��A{33A|��A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�ffA�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�Aə�A�ffA�33A�  A���A͙�A�ffA�33A�  A���Aљ�Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�33A�  A���Aٙ�A�ffA�33A�  A�  DqFfDqL�DqY�Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt�3Dt��Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @;�@�\)@ʏ\A�A#�AC�AeG�A���A��
A��
A��
A��
A��
A��
A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�BqQ�Bx�B�u�B�u�B�u�B�u�B�u�B��)B�B�B�u�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy��D��D�MD���D��D�
=D�W�D���D�ǮD��D�N�D��)D���D�{D�@RD�u�D��)D��\D�C�D�b�D�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���\+<���\+��\+<�<���\+��\+��\+<�>�>���\+��\+��\+��\+��\+��\+��\+��\+��\+<���\+��\+��\+��\+��\+��\+>�>���\+<�<���\+��\+<���\+��\+>���\+��\+��\+��\+<���\+��\+��\+��\+��\+��\+��\+��\+��\+��\+>���\+<�<���\+��\+��\+��\+��\+��\+��\+<���\+��\+��\+��\+��\+��\+<�<���\+<�<���\+��\+��\+��\+��\+��\+<�<���\+��\+��\+��\+��\+��\+��\+<�>���\+<�<���\+��\+<���\+��\+<�>k���\+��\+��\+<���\+��\+��\+<�>k�>k���\+��\+<���\+��\+��\+��\+<�>���>k���\+<�<���\+��\+>���\+��\+��\+��\+��\+<���\+<�<���\+��\+��\+<���\+��\+<���\+��\+��\+>���\+��\+<���\+��\+��\+��\+>�>k�>���\+��\+��\+��\+��\+��\+<���\+��\+<�>���\+��\+��\+��\+<���\+<���\+��\+>�>�<���\+<�<���\+��\+>k�>���\+<�>�<���\+<�>���\+��\+<���\+��\+��\+>�<���\+<���\+��\+��\+>���\+��\+��\+��\+��\+<�<���\+��\+<�<���\+��\+��\+<�<�>�<���\+��\+��\+<�<�<�>���\+<���\+<�>�>�<�<�>�>�>���\+<�<�>���\+<�>�>�<�<�<�>�>�<�>���\+>�<�>�<�>�>k�>�>k�>k�>k�>�>�>�>�>�>�>k�>�<�>k�>�>k�>k�>k�>k�>k�>���>�<�>k�>�>�>k�>k�>���>���>k�>�>k�<�>�>k�>�>�>k�>k�>�>�>k�>�>�>k�>k�>�>�>�>�>�<�>k�>k�>k�>k�>�>�>k�>k�>�>�>�>k�>k�>k�>�>�>k�>k�>�>�>k�>�>�>�>k�>k�>�<�>�>���>�>k�>�>k�<�>�>�>�>k�>�>�>�>�>�>�>�>k�>�>�>�>k�>�<�>k�>�>�>�>k�>k�>�>k�>�>�>�>�>�>k�>�<�>k�>k�>�>�>k�>�>k�>k�>k�>�>�>�>k�>�>�>�>�>�>�>�>�>�>�>k�>�>k�>���>���>k�>k�>���>k�>�>�>�>k�>k�>�>�>k�>�>k�>k�>k�>�>k�>k�>k�>k�>k�>�>�>k�>�>k�>k�>k�>k�>���>k�>k�>���>���>���>k�>���>k�>k�>k�>�>k�>�>�>�>k�>�>�>k�>k�>k�>k�>�>�>�>�>�>�>���>k�>�>k�>k�>�>k�>�>���>k�<�>�>�>�>�<�>k�>�>�>k�>�>�>�>�<�>�>�>k�>k�<�>k�>�>k�>k�>�>�<�>k�<�>�>�>�>�>k�>�>k�>k�>�<�>k�>�>�>k�>k�>k�>k�>���>k�>k�>k�>�>�<�>�>�>�>�>�>k�>�>�<�>�<�<�>k�>�>k�>���>���?�?�?:�H?:�H?:�H?Tz�?n{?��
?��
?���?���?�=q?�
>?�
>?�
>?У�?У�?�=q?�=q?�
>?�
>@Q�@Q�@�R@�@�@!�@(Q�@.�R@.�R@;�@HQ�@N�R@U�@[�@a�@n�R@u�@���@�(�@�\)@�@���@�\)@��\@�@�(�@�\)@��\@���@�(�@��\@���@�(�@�\)@�@���@�\)@�@���@�\)@�\@���@�(�@��\@�A{A�A�GAz�A�AG�A�GAzA�A�GAz�A�AG�A�GA"zA%G�A&�GA*zA+�A-G�A0z�A2zA5G�A6�GA:zA;�A>�GA@z�ABzAEG�AHz�AJzAK�AN�GAPz�AS�AUG�AXz�AZzA[�A^�GA`z�Ac�Af�GAhz�AjzAmG�An�GArzAs�Av�GAxz�AzzA}G�A~�GA�=qA��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�=pA�
=A��
A£�A�p�A�=pA�
=A��
Aƣ�A�p�A�=pA�
=A��
Aʣ�A�p�A�p�A�=pA�
=A��
AΣ�A�p�A�=pA�
=A��
Aң�A�p�A�p�A�=pA�
=A��
A֣�A�p�A�=pA�
=A�
=A��
Aڣ�A�p�A�=pA�
=A��
A��
DqUDq[�DqhRDqn�DquDq{�Dq��Dq��Dq�Dq��Dq��Dq��Dq�Dq��Dq��DqθDq�DqۅDq��Dq�RDq�Dq��Dr�DrRDrDr�Dr!�Dr(RDr.�Dr;�DrA�DrHRDrN�Dr[�Dra�DrhRDrn�Dr{�Dr��Dr�RDr��Dr��Dr��Dr�RDr��Dr��Dr��Dr�RDrθDrۅDr��Dr�RDr�Dr��Ds�DsRDs�Ds�Ds!�Ds(RDs.�Ds;�DsA�DsHRDsN�Ds[�Dsa�DshRDsn�Ds{�Ds��Ds�RDs��Ds��Ds��Ds�RDs�Ds��Ds��Ds�RDs�DsۅDs��Ds�RDs�Ds��Dt�DtRDtDt�Dt!�Dt(RDt5Dt;�DtA�DtN�DtUDt[�Dta�Dtn�DtuDt{�Dt��Dt��Dt�Dt��Dt�RDt��Dt�Dt��Dt�RDtθDt�Dt��Dt�RDt�Dt�Du�DuRDu�@.�R@;�@HQ�@N�R@U�@[�@a�@n�R@u�@���@�(�@�\)@�@���@�\)@��\@�@�(�@�\)@��\@���@�(�@��\@���@�(�@�\)@�@���@�\)@�@���@�\)@�\@���@�(�@��\@�A{A�A�GAz�A�AG�A�GAzA�A�GAz�A�AG�A�GA"zA%G�A&�GA*zA+�A-G�A0z�A2zA5G�A6�GA:zA;�A>�GA@z�ABzAEG�AHz�AJzAK�AN�GAPz�AS�AUG�AXz�AZzA[�A^�GA`z�Ac�Af�GAhz�AjzAmG�An�GArzAs�Av�GAxz�AzzA}G�A~�GA�=qA��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�p�A�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�=pA�
=A��
A£�A�p�A�=pA�
=A��
Aƣ�A�p�A�=pA�
=A��
Aʣ�A�p�A�p�A�=pA�
=A��
AΣ�A�p�A�=pA�
=A��
Aң�A�p�A�p�A�=pA�
=A��
A֣�A�p�A�=pA�
=A�
=A��
Aڣ�A�p�A�=pA�
=A��
A��
DqUDq[�DqhRDqn�DquDq{�Dq��Dq��Dq�Dq��Dq��Dq��Dq�Dq��Dq��DqθDq�DqۅDq��Dq�RDq�Dq��Dr�DrRDrDr�Dr!�Dr(RDr.�Dr;�DrA�DrHRDrN�Dr[�Dra�DrhRDrn�Dr{�Dr��Dr�RDr��Dr��Dr��Dr�RDr��Dr��Dr��Dr�RDrθDrۅDr��Dr�RDr�Dr��Ds�DsRDs�Ds�Ds!�Ds(RDs.�Ds;�DsA�DsHRDsN�Ds[�Dsa�DshRDsn�Ds{�Ds��Ds�RDs��Ds��Ds��Ds�RDs�Ds��Ds��Ds�RDs�DsۅDs��Ds�RDs�Ds��Dt�DtRDtDt�Dt!�Dt(RDt5Dt;�DtA�DtN�DtUDt[�Dta�Dtn�DtuDt{�Dt��Dt��Dt�Dt��Dt�RDt��Dt�Dt��Dt�RDtθDt�Dt��Dt�RDt�Dt�Du�DuRDu�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�A�A�?}A�A�A�A�A�A�A�?}A�;dA�=qA�=qA�?}A�9XA�=qA�XA��A�bAǇ+A��AƋDA�|�A�7LA�%A�ƨA�|�A�VAě�A�ZA�{A��yAð!AÛ�A�C�A��A�A��FA�v�A���A�t�A���A���A�v�A�1A�ffA��-A��#A���A��-A���A�bNA�"�A��A��FA�ffA��RA�I�A��`A���A��HA��A�K�A�t�A��;A�"�A�Q�A�A��7A��A��A���A�
=A��
A��TA��A��
A�bNA�A���A�ffA��A���A���A��^A�A�A��;A��HA�%A�1'A�A�K�A�v�A�1'A�A�A��wA�n�A���A��FA��wA��A��A�ƨA���A�(�A���A��A�5?A�1'A�A�ĜA�1'A�=qA���A�oA�ffA��A�JA��mA�ffA��FA��A�7LA{�AwdZAv��Aup�Ar�9An�RAl-Ak��Ah��Af�`Ae?}Ac�AbVA`=qA\Q�AWoAQoAM�AJA�AH5?AF�jAF~�AD��AC�AB�yAAK�A?7LA>{A<�A<5?A;t�A;%A:�jA8�yA7�-A7VA6�+A6JA5K�A4ZA3hsA2�\A1�A0��A/�A/%A.�!A.�+A-�A-VA,^5A+?}A)��A(��A(1'A'\)A&�A$��A#VA"M�A!�A!7LA ^5A�A��A�uA�A�^A�yAbA33A�A��AE�At�AjAp�AȴAI�A�
A�A�yA9XA|�A�A1A�RA5?A��AS�A%A
-A	%A��A�wA/Ax�A"�A �A�A ĜA b@�O�@�j@��@�9X@��T@���@�K�@�+@�j@�V@��@�  @��@��@�A�@�E�@�@�`B@�%@�bN@��@�G�@�~�@��@�;d@�$�@պ^@Ӿw@���@�A�@�|�@�K�@Ο�@��#@�`B@˕�@ʟ�@�x�@�r�@�l�@�?}@�t�@�C�@�ȴ@�7L@�33@�ff@��T@�V@��m@�S�@��H@�-@���@�r�@���@�o@�V@��^@���@��7@�`B@�?}@�&�@�%@��/@���@��@��@��@��
@�|�@�K�@��@���@�$�@��#@��h@��7@�V@�j@��@�dZ@�"�@�$�@��h@��h@�x�@�&�@�%@��@�j@��@��^@���@��h@��9@� �@���@���@��@��@��7@��@�{@�O�@��h@��@�^5@�v�@�@�@��-@�`B@�G�@�&�@��`@�9X@�dZ@��;@��@��H@���@��;@�ƨ@���@��m@��
@�ƨ@��P@�
=@���@���@�^5@��@��h@�/@�K�@���@��@��#@��h@�V@��@�r�@��m@���@�A�@�j@�ƨ@��
@�I�@�A�@���@��@�A�@�Q�@�Z@�n�@���@��#@�?}@���@���@�O�@�p�@�V@�I�@�r�@�I�@��@�
=@��@��@�9X@�(�@��@�33@�+@�
=@�ȴ@��!@���@�~�@�~�@��R@��@��@��@��y@��y@�ȴ@�ff@�J@�@�&�@��9@�j@�1@�  @�I�@�1'@�1@��m@��@��m@��;@�dZ@�+@�o@�ȴ@�n�@�M�@�5?@���@��-@�?}@�V@�7L@�&�@�V@��@��@�Z@��F@�dZ@��@���@�V@��@���@��@���@��@��@��^@�O�@�&�@���@���@���@��u@�Z@��;@�  @�1@��
@�\)@�@��H@�ȴ@��+@�{@��T@���@�O�@��@���@���@��9@��@�Z@� �@��@��w@�t�@�;d@�33@��6@wZ�@q�@jZ�@c/�@Y�@P*�@I-w@C��@>�6@9�@3J#@-�@(`�@#�}@ ~@��@��@�@V@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VAʰ!A�p�A�(�A�{A�|�A��A�+A�l�A�bA�{A�+AƧ�A�/A�C�AǺ^A�%A���A�^5A�ffA���A�ƨA�n�A�A�Aġ�A��A�
=A��A�bA���A�l�A�
=A���A�Q�A�ZAƏ\A��AʶFA��A�O�A���A�9XA�r�A���A���A��FA��\A���A��/A���A��DA�%A�7LA�Q�AȰ!Aǝ�A��A���A�v�A��A�ĜA��9A���A�C�A�?}A��`A�^5A���A���A���A��AʑhA�A�-A�E�A�
=A�l�A��yA��7A��wAć+A���AľwAɕ�A��#A�VA��A���A��A�Q�A�1'A�~�A�{A��mA�9XA�oAŇ+A�A�Q�A�JA�;dA�{A� �A�JA�/A�E�A�\)A�A�O�A��A�  A��A��A�7LA�ffA��A�|�A���A�|�Aò-A���A�$�A�"�A�(�A� �A�oA� �A��/A�bA�r�A��DA�r�A�ȴA�A�A��HA��A���A��A��A�?}Aʡ�AȼjA��
A�VA��A�7LA�?}A�"�A��`A��A���A��A�
=A��HA¼jA�p�A�oA�{A�JA��RA���A�z�A��HA���A�jA�v�A��^A��uAʅA��A���A��A��/AÍPAʝ�A��A�oAİ!A��TAȋDA��A�VA���A���A�1A�A��HA�A��A�  A�n�A��A��A�Q�A�
=A��AĮA�S�AƝ�A�x�A�ȴA�;dA���A���AĲ-A���A��
A��A��Aɝ�AʅA�%A��yA��;A��
A�;dA�A�A�ƨA��;AĴ9A�^5A��wA���AļjA��#A�bA��A��A�hsA�+A�A�1A��A�
=A� �A�(�AÓuA��A�$�A��A��A�1A���Aȴ9A� �A��A� �A�t�A��A��A���A��A�VA��A��A�1A�1A�"�A��A��A��A�oA�ĜA� �A�"�A�oA�&�A��A�$�A��A� �A��A��A��A��A�(�A�&�A�"�A�(�A��A�JA�"�A�$�A�"�A�$�A� �A��A��A��A��A� �A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�$�A�{A�{A�$�A�"�A��A� �A�$�A� �A��A��A�"�A� �A��A�$�A� �A� �A�"�A��A��A�(�A�(�A�&�A�+A�(�A�(�A�+A�(�A�+A�+A�+A�+A�&�A�+A�(�A�&�A�$�A�(�A�&�A�+A�&�A�  A� �A�&�A�"�A�$�A�$�A�$�A�$�A�$�A�"�A�{A�&�A�&�A�&�A�&�A�$�A�$�A�&�A�"�A�$�A�&�A�"�A�(�A�&�A�$�A�"�A�$�A�$�A�"�A� �A�"�A� �A��A� �A��A��A�"�A�+A��A�bA�&�A�$�A�(�A�+A�&�A�&�A�-A�1'A�+A�33A�/A�+A�$�A�&�A�&�A�"�A�$�A�$�A� �A��A� �A� �A� �A�A��A�oA��A��A��A�$�A� �A��A� �A��A��A�oA��A��A��A��A��A��A��A�{A��A��A��A��A�{A�{A��A��A��A�VA��A��A��A��A��A� �A� �A� �A�"�A�"�A�"�A�"�A�$�A�"�A�"�A�$�A�"�A�$�A� �A�"�A�"�A� �A�"�A�$�A�"�A�$�A� �A��A�&�A�&�A�$�A�$�A�&�A�"�A�(�A�(�A�(�A�&�A�&�A�(�A�$�A�$�A�&�A�$�A�$�A��A���A�$�A�"�A� �A�$�A�"�A�$�A�&�A�"�A�$�A�&�A�$�A�&�A�(�A�$�A�&�A�"�A�"�A�&�A�"�A�"�A�(�A�"�A�+A�(�A�+A�(�A�A�(�A�-A�-A�-A�+A�-A�+A�-A�+A�$�A���A�+A�+A�&�A�+A�+A�+A�+A�+A�-A�1'A�$�A� �A� �A�"�A�&�A�&�A�-A�(�A�/A�-A�-A�+AʅA�-A�(�A�-A�-A�+A�-A�1'A�5?A�5?A�7LA�33A�7LA�7LA�;dA�=qA�=qA�;dA�=qA�?}A�?}A�?}A�?}A�=qA�?}A�?}A�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�A�A�A�A�?}A�?}A�=qA�=qA�A�A�A�A�C�A�C�A�A�A�A�A�A�A�?}A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�=qA�?}A�A�A�C�A�?}A�?}A�?}A�=qA�C�A�E�A�A�A�C�A�?}A�=qA�?}A�A�A�?}A�?}A�C�A�?}A�A�A�A�A�?}A�?}A�=qA�=qA�?}A�C�A�E�A�A�A�?}A�;dA�=qA�?}A�A�A�A�A�?}A�A�A�A�A�C�A�C�A�A�A�?}A�?}A�E�A�C�A�C�A�A�A�A�A�;dA�=qA�;dA�;dA�=qA�?}A�=qA�;dA�=qA�;dA�=qA�9XA�=qA�=qA�5?A�9XA�9XA�9XA�9XA�;dA�=qA�?}A�=qA�;dA�9XA�=qA�=qA�=qA�?}A�;dA�;dA�;dA�;dA�=qA�;dA�;dA�=qA�=qA�=qA�?}A�A�A�=qA�?}A�?}A�?}A�?}A�?}A�=qA�?}A�=qA�9XA�7LA�9XA�7LA�9XA�7LA�;dA�9XA�9XA�;dA�=qA�=qA�=qA�A�A�?}A�=qA�?}A�E�A�E�A�C�A�C�A�C�A�C�A�C�A�=qA�?}A�9XA�?}A�;dA�5?A�7LA�9XA�9XA�7LA�9XA�9XA�=qA�;dA�;dA�9XA�=qA�7LA�7LA�7LA�7LA�5?A�9XA�33A�9XA�9XA�9XA�;dA�;dA�9XA�9XA�5?A�5?A�7LA�9XA�;dA�;dA�;dA�=qA�?}A�=qA�?}A�;dA�?}A�A�A�C�A�C�A�=qA�?}A�C�A�=qA�9XA�9XA�5?A���A���A���@���@���@���@��u@���@��u@��u@��u@��u@��@�j@�bN@�bN@�bN@�bN@�bN@�bN@�Z@�Z@�bN@�bN@�Z@�Q�@�Z@�Q�@�Q�@�I�@�Q�@�Q�@�I�@�I�@�I�@�Q�@�A�@�A�@�1'@�1'@�1'@�(�@�(�@� �@�b@�1@�1@�1@�1@�  @�  @�  @�  @���@��@��@���@��@��@��m@��;@��;@��
@��
@���@�ƨ@���@�ƨ@�ƨ@�ƨ@�ƨ@��w@�ƨ@��w@��w@��w@��F@��F@��F@��F@��@���@���@���@���@���@���@��P@�t�@�dZ@�dZ@�dZ@�\)@�\)@�\)@�S�@�S�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�33@�;d@�;d@�33@�33@�33@�33@�33@�33@�+@�+A�A�A�?}A�;dA�;dA�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�A�A�C�A�A�A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�=qA�?}A�A�A�?}A�?}A�C�A�?}A�?}A�?}A�?}A�E�A�C�A�?}A�?}A�?}A�?}A�?}A�A�A�C�A�A�A�C�A�C�A�C�A�A�A�?}A�?}A�?}A�C�A�C�A�E�A�?}A�=qA�;dA�?}A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�C�A�A�A�A�A�E�A�A�A�?}A�A�A�?}A�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�9XA�9XA�9XA�7LA�9XA�;dA�=qA�=qA�?}A�=qA�9XA�;dA�=qA�=qA�=qA�?}A�=qA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�A�A�A�A�?}A�?}A�;dA�?}A�?}A�?}A�?}A�;dA�;dA�9XA�9XA�7LA�9XA�9XA�;dA�9XA�9XA�=qA�?}A�?}A�?}A�A�A�?}A�=qA�?}A�C�A�E�A�E�A�?}A�C�A�=qA�A�A�=qA�=qA�=qA�C�A�?}A�9XA�9XA�9XA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�?}A�=qA�;dA�7LA�9XA�9XA�9XA�5?A�5?A�9XA�9XA�;dA�;dA�;dA�9XA�9XA�7LA�7LA�7LA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�?}A�;dA�=qA�A�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�=qA�=qA�C�A�A�@���@���@���@���@���@���@���@���@��u@��D@�z�@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�Z@�Z@�Z@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�I�@�I�@�I�@�1'@�1'@�1'@�1'@�(�@�(�@�b@�1@�1@�1@�1@�1@�  @�  @�1@���@���@���@���@���@���@��@��@��m@��;@��
@��
@���@���@���@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@��w@��w@��F@��F@��F@��F@��@��@���@���@���@���@���@���@�|�@�l�@�l�@�dZ@�dZ@�dZ@�\)@�\)@�S�@�S�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�33@�33@�33@�33@�33@�33@�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�=qA�A�A�?}A�A�A�A�A�A�A�?}A�;dA�=qA�=qA�?}A�9XA�=qA�XA��A�bAǇ+A��AƋDA�|�A�7LA�%A�ƨA�|�A�VAě�A�ZA�{A��yAð!AÛ�A�C�A��A�A��FA�v�A���A�t�A���A���A�v�A�1A�ffA��-A��#A���A��-A���A�bNA�"�A��A��FA�ffA��RA�I�A��`A���A��HA��A�K�A�t�A��;A�"�A�Q�A�A��7A��A��A���A�
=A��
A��TA��A��
A�bNA�A���A�ffA��A���A���A��^A�A�A��;A��HA�%A�1'A�A�K�A�v�A�1'A�A�A��wA�n�A���A��FA��wA��A��A�ƨA���A�(�A���A��A�5?A�1'A�A�ĜA�1'A�=qA���A�oA�ffA��A�JA��mA�ffA��FA��A�7LA{�AwdZAv��Aup�Ar�9An�RAl-Ak��Ah��Af�`Ae?}Ac�AbVA`=qA\Q�AWoAQoAM�AJA�AH5?AF�jAF~�AD��AC�AB�yAAK�A?7LA>{A<�A<5?A;t�A;%A:�jA8�yA7�-A7VA6�+A6JA5K�A4ZA3hsA2�\A1�A0��A/�A/%A.�!A.�+A-�A-VA,^5A+?}A)��A(��A(1'A'\)A&�A$��A#VA"M�A!�A!7LA ^5A�A��A�uA�A�^A�yAbA33A�A��AE�At�AjAp�AȴAI�A�
A�A�yA9XA|�A�A1A�RA5?A��AS�A%A
-A	%A��A�wA/Ax�A"�A �A�A ĜA b@�O�@�j@��@�9X@��T@���@�K�@�+@�j@�V@��@�  @��@��@�A�@�E�@�@�`B@�%@�bN@��@�G�@�~�@��@�;d@�$�@պ^@Ӿw@���@�A�@�|�@�K�@Ο�@��#@�`B@˕�@ʟ�@�x�@�r�@�l�@�?}@�t�@�C�@�ȴ@�7L@�33@�ff@��T@�V@��m@�S�@��H@�-@���@�r�@���@�o@�V@��^@���@��7@�`B@�?}@�&�@�%@��/@���@��@��@��@��
@�|�@�K�@��@���@�$�@��#@��h@��7@�V@�j@��@�dZ@�"�@�$�@��h@��h@�x�@�&�@�%@��@�j@��@��^@���@��h@��9@� �@���@���@��@��@��7@��@�{@�O�@��h@��@�^5@�v�@�@�@��-@�`B@�G�@�&�@��`@�9X@�dZ@��;@��@��H@���@��;@�ƨ@���@��m@��
@�ƨ@��P@�
=@���@���@�^5@��@��h@�/@�K�@���@��@��#@��h@�V@��@�r�@��m@���@�A�@�j@�ƨ@��
@�I�@�A�@���@��@�A�@�Q�@�Z@�n�@���@��#@�?}@���@���@�O�@�p�@�V@�I�@�r�@�I�@��@�
=@��@��@�9X@�(�@��@�33@�+@�
=@�ȴ@��!@���@�~�@�~�@��R@��@��@��@��y@��y@�ȴ@�ff@�J@�@�&�@��9@�j@�1@�  @�I�@�1'@�1@��m@��@��m@��;@�dZ@�+@�o@�ȴ@�n�@�M�@�5?@���@��-@�?}@�V@�7L@�&�@�V@��@��@�Z@��F@�dZ@��@���@�V@��@���@��@���@��@��@��^@�O�@�&�@���@���@���@��u@�Z@��;@�  @�1@��
@�\)@�@��H@�ȴ@��+@�{@��T@���@�O�@��@���@���@��9@��@�Z@� �@��@��w@�t�@�;dG�O�@��6@wZ�@q�@jZ�@c/�@Y�@P*�@I-w@C��@>�6@9�@3J#@-�@(`�@#�}@ ~@��@��@�@V@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VAʰ!A�p�A�(�A�{A�|�A��A�+A�l�A�bA�{A�+AƧ�A�/A�C�AǺ^A�%A���A�^5A�ffA���A�ƨA�n�A�A�Aġ�A��A�
=A��A�bA���A�l�A�
=A���A�Q�A�ZAƏ\A��AʶFA��A�O�A���A�9XA�r�A���A���A��FA��\A���A��/A���A��DA�%A�7LA�Q�AȰ!Aǝ�A��A���A�v�A��A�ĜA��9A���A�C�A�?}A��`A�^5A���A���A���A��AʑhA�A�-A�E�A�
=A�l�A��yA��7A��wAć+A���AľwAɕ�A��#A�VA��A���A��A�Q�A�1'A�~�A�{A��mA�9XA�oAŇ+A�A�Q�A�JA�;dA�{A� �A�JA�/A�E�A�\)A�A�O�A��A�  A��A��A�7LA�ffA��A�|�A���A�|�Aò-A���A�$�A�"�A�(�A� �A�oA� �A��/A�bA�r�A��DA�r�A�ȴA�A�A��HA��A���A��A��A�?}Aʡ�AȼjA��
A�VA��A�7LA�?}A�"�A��`A��A���A��A�
=A��HA¼jA�p�A�oA�{A�JA��RA���A�z�A��HA���A�jA�v�A��^A��uAʅA��A���A��A��/AÍPAʝ�A��A�oAİ!A��TAȋDA��A�VA���A���A�1A�A��HA�A��A�  A�n�A��A��A�Q�A�
=A��AĮA�S�AƝ�A�x�A�ȴA�;dA���A���AĲ-A���A��
A��A��Aɝ�AʅA�%A��yA��;A��
A�;dA�A�A�ƨA��;AĴ9A�^5A��wA���AļjA��#A�bA��A��A�hsA�+A�A�1A��A�
=A� �A�(�AÓuA��A�$�A��A��A�1A���Aȴ9A� �A��A� �A�t�A��A��A���A��A�VA��A��A�1A�1A�"�A��A��A��A�oA�ĜA� �A�"�A�oA�&�A��A�$�A��A� �A��A��A��A��A�(�A�&�A�"�A�(�A��A�JA�"�A�$�A�"�A�$�A� �A��A��A��A��A� �A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�$�A�{A�{A�$�A�"�A��A� �A�$�A� �A��A��A�"�A� �A��A�$�A� �A� �A�"�A��A��A�(�A�(�A�&�A�+A�(�A�(�A�+A�(�A�+A�+A�+A�+A�&�A�+A�(�A�&�A�$�A�(�A�&�A�+A�&�A�  A� �A�&�A�"�A�$�A�$�A�$�A�$�A�$�A�"�A�{A�&�A�&�A�&�A�&�A�$�A�$�A�&�A�"�A�$�A�&�A�"�A�(�A�&�A�$�A�"�A�$�A�$�A�"�A� �A�"�A� �A��A� �A��A��A�"�A�+A��A�bA�&�A�$�A�(�A�+A�&�A�&�A�-A�1'A�+A�33A�/A�+A�$�A�&�A�&�A�"�A�$�A�$�A� �A��A� �A� �A� �A�A��A�oA��A��A��A�$�A� �A��A� �A��A��A�oA��A��A��A��A��A��A��A�{A��A��A��A��A�{A�{A��A��A��A�VA��A��A��A��A��A� �A� �A� �A�"�A�"�A�"�A�"�A�$�A�"�A�"�A�$�A�"�A�$�A� �A�"�A�"�A� �A�"�A�$�A�"�A�$�A� �A��A�&�A�&�A�$�A�$�A�&�A�"�A�(�A�(�A�(�A�&�A�&�A�(�A�$�A�$�A�&�A�$�A�$�A��A���A�$�A�"�A� �A�$�A�"�A�$�A�&�A�"�A�$�A�&�A�$�A�&�A�(�A�$�A�&�A�"�A�"�A�&�A�"�A�"�A�(�A�"�A�+A�(�A�+A�(�A�A�(�A�-A�-A�-A�+A�-A�+A�-A�+A�$�A���A�+A�+A�&�A�+A�+A�+A�+A�+A�-A�1'A�$�A� �A� �A�"�A�&�A�&�A�-A�(�A�/A�-A�-A�+AʅA�-A�(�A�-A�-A�+A�-A�1'A�5?A�5?A�7LA�33A�7LA�7LA�;dA�=qA�=qA�;dA�=qA�?}A�?}A�?}A�?}A�=qA�?}A�?}A�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�A�A�A�A�?}A�;dA�;dA�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�A�A�C�A�A�A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�=qA�?}A�A�A�?}A�?}A�C�A�?}A�?}A�?}A�?}A�E�A�C�A�?}A�?}A�?}A�?}A�?}A�A�A�C�A�A�A�C�A�C�A�C�A�A�A�?}A�?}A�?}A�C�A�C�A�E�A�?}A�=qA�;dA�?}A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�C�A�A�A�A�A�E�A�A�A�?}A�A�A�?}A�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�9XA�9XA�9XA�7LA�9XA�;dA�=qA�=qA�?}A�=qA�9XA�;dA�=qA�=qA�=qA�?}A�=qA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�A�A�A�A�?}A�?}A�;dA�?}A�?}A�?}A�?}A�;dA�;dA�9XA�9XA�7LA�9XA�9XA�;dA�9XA�9XA�=qA�?}A�?}A�?}A�A�A�?}A�=qA�?}A�C�A�E�A�E�A�?}A�C�A�=qA�A�A�=qA�=qA�=qA�C�A�?}A�9XA�9XA�9XA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�?}A�=qA�;dA�7LA�9XA�9XA�9XA�5?A�5?A�9XA�9XA�;dA�;dA�;dA�9XA�9XA�7LA�7LA�7LA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�?}A�;dA�=qA�A�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�=qA�=qA�C�A�A�@���@���@���@���@���@���@���@���@��u@��D@�z�@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�Z@�Z@�Z@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�I�@�I�@�I�@�1'@�1'@�1'@�1'@�(�@�(�@�b@�1@�1@�1@�1@�1@�  @�  @�1@���@���@���@���@���@���@��@��@��m@��;@��
@��
@���@���@���@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@��w@��w@��F@��F@��F@��F@��@��@���@���@���@���@���@���@�|�@�l�@�l�@�dZ@�dZ@�dZ@�\)@�\)@�S�@�S�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�33@�33@�33@�33@�33@�33@�33A�A�A�?}A�;dA�;dA�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�A�A�C�A�A�A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�=qA�?}A�A�A�?}A�?}A�C�A�?}A�?}A�?}A�?}A�E�A�C�A�?}A�?}A�?}A�?}A�?}A�A�A�C�A�A�A�C�A�C�A�C�A�A�A�?}A�?}A�?}A�C�A�C�A�E�A�?}A�=qA�;dA�?}A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�C�A�A�A�A�A�E�A�A�A�?}A�A�A�?}A�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�9XA�9XA�9XA�7LA�9XA�;dA�=qA�=qA�?}A�=qA�9XA�;dA�=qA�=qA�=qA�?}A�=qA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�A�A�A�A�?}A�?}A�;dA�?}A�?}A�?}A�?}A�;dA�;dA�9XA�9XA�7LA�9XA�9XA�;dA�9XA�9XA�=qA�?}A�?}A�?}A�A�A�?}A�=qA�?}A�C�A�E�A�E�A�?}A�C�A�=qA�A�A�=qA�=qA�=qA�C�A�?}A�9XA�9XA�9XA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�?}A�=qA�;dA�7LA�9XA�9XA�9XA�5?A�5?A�9XA�9XA�;dA�;dA�;dA�9XA�9XA�7LA�7LA�7LA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�?}A�;dA�=qA�A�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�=qA�=qA�C�A�A�@���@���@���@���@���@���@���@���@��u@��D@�z�@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�bN@�Z@�Z@�Z@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�I�@�I�@�I�@�1'@�1'@�1'@�1'@�(�@�(�@�b@�1@�1@�1@�1@�1@�  @�  @�1@���@���@���@���@���@���@��@��@��m@��;@��
@��
@���@���@���@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@��w@��w@��F@��F@��F@��F@��@��@���@���@���@���@���@���@�|�@�l�@�l�@�dZ@�dZ@�dZ@�\)@�\)@�S�@�S�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�;d@�33@�33@�33@�33@�33@�33@�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��E@�&l>s�T>��@�/E?^�=���=�VX>#ı@�.�@�5i>*>I�@��>�>,܇@�-�=̢�>#L@�g�>H�@���=�[> =�*�?��;>�^?��}@�*�?�Ц>S@O@�%[@�;�>��@�Bp@��>�[@I�@�"}>)��@G�=�`�?ws?2M>L?>=���=��M=�A_>%�q?.��>���=�u:>�z@V��@Z��>#��@���@"h=�q�>پ@��=���=��=�Q�>y�w@�0�=�_F=���=�}�=�]�>
��@$�o@���?��">4�w@�3�@�+k?��1=��=�28>(�>Ǘ�>��?�W @�3�=�~=��>6+k?���>�|=���>�
�@�2#@��#>&77@�5i?P�">+�,>�gw@�8�>�C?�ek@�1�@�7�=�C�?��i>��s?�f?���=�U�>9�@�2�@�6e?�	W>�@�1f?5+�=�t�>���?�\>5�@�5@�5�@�6�>i@�4�?�J�>!*�@�1�@�6�=�D�?	(9=��>|F@V��?��?jX�@�1{?�#�>8�@��x>�#y=�o�>�>�?�@�HA=�X�>��>��@�0�=�N>]�@�/E@��5=���=��I?�sm@�2#@�7v>���=���?���?��`?3�p>�?���@;�>�@�߹@�5i@ �+=��=�H�?�b9@]f'>�u@�,(@�i�>@?�1@�3H@��>��?���@�5i?�f<>�%@\��@�5@��_>@�@�7@�2#>��+>�>�@�.�@��=��?_A�@�'|=�	�><�@c�V?��j>L^�>n�@�4Y?��X>��#?h�@�-�?S��>|@���@�!�>��p@�%p?��I?��z@��@s�?!�F@��>t�@�8�@�9C@�:T@�:�=��?,�z>Un�@��@D_�>\8�@�82@�8�?�I=@�)�>]�d@�7�@�7�@�7�@�7�>v�}@�8q@�7v@�>�@*@�5�@�2�@�2?/>�F�@�8�@�: @�7�@X��@�9C@�82@�8�@�9�@�:*@.�O@�9�@�9C@�8�@�8�@�:T@�:�@�9@�8�@�7�@�7"@�7"@�9m@�:�@�9C@�:�@�<�@�7�@�6�@�9�@�:�@�:T@�:�@�:�@�;@�7"@�7"@�8�@�8�@�9�@�8�@�82@�8�@�8q@�9C@�: @�9�@�8�@�9m@�: @�7v@�7�@�8�@�:�@�:�@�:�@�8�@�:T@�:�@�:*@�:�@�;d@�;d@�:�@�:~@�: @�;d@�;@�;:@�<u@�=2@�;�@�<�@�<u@�;@�=�@�>�@�>�@�>�@�=2@�>B@�=�@�=�@�>l@�=�@�>�@�=�@�=�@�>@�=�@�=�@�<�@�=2@�<u@�<�@�<!@�:T@�;d@�<!@�;d@�<�@�;�@�;�@�<u@�<!@�<!@��	@�=2@�=�@�>�@�>�@�>@�<u@�<u@�<u@�=2@�<�@�;�@�=�@�<�@�>B@�;�@�;�@�;�@�;d@�:�@�;@�:�@�:�@�:T@�: @�=�@�=\@�;d@�;�@�<!@�=�@�<�@�=�@�?�@�?S@�@�@�@�@�BF@�?�@�@d@�?�@�A @�=�@�<�@�=2@�<!@�<!@�<!@�;�@�:�@�:�@�;d@�;@�8	@�7�@�:T@�;d@�:T@�9�@�<K@�;@�: @�9�@�7v@�7�@�6e@�7�@�7v@�7�@�7a@�7"@�7"@�6�@�82@�6�@�7@�7"@�6�@�6e@�7v@�8q@�82@�7v@�5i@�8�@�9�@�:T@�:T@�:T@�;@�;@�;:@�;d@�<!@�<!@�<!@�;�@�;�@�;�@�<!@�;�@�;�@�;d@�<u@�<!@�<u@�<u@�<�@�<�@�<!@�:T@�;d@�=\@�=�@�<�@�<�@�>B@�<K@�>@�>�@�>�@�>�@�>�@�?@�>B@�<u@�=2@�<u@�=�@�;�@�1�@�<u@�;d@�;�@�<!@�:~@�<�@�=�@�<u@�>B@�=�@�=�@�>B@�=�@�?S@�<u@�:�@�:T@�<u@�<!@�<!@�>@�>�@�?�@�?S@�>�@�>�@�<�@�>�@�?�@�?�@�?�@�?}@�?S@�?S@�?S@�>B@�>l@T�b@�>�@�>�@�>�@�?@�>�@�?�@�@d@�?�@�@�@�?�@�>�@�>B@�>B@�=�@�@%@�?�@�?S@�?�@�At@�@�@�?�@�@d@�ƨ@�?�@�?}@�@d@�?�@�?�@�CB@�CW@�D�@�Ec@�DR@�Ec@�F�@�F�@�G@�F�@�G0@�F�@�F�@�G0@�F�@�G0@�G0@�G0@�G0@�G�@�G�@�G�@�G�@�HA@�H�@�HA@�HA@�HA@�H�@�HA@�F�@�G�@�G�@�G�@�G�@�H�@�H�@�H�@�H�@�H�@�IR@�H�@�H�@�IR@�H�@�H�@�H�@�H�@�H�@�H�@�H�@�H�@�IR@�IR@�IR@�J@�I�@�J@�J@�Jb@�J@�J@�Jb@�J�@�J@�I�@�J�@�J�@�Jw@�Jw@�Jw@�J�@�L0@�K�@�K�@�K@�J�@�J�@�J�@�J�@�K4@�K�@�K�@�K�@�K�@�K4@�K4@�K�@�K4@�K4@�L�@�L�@�MU@�L�@�J�@�J�@�K�@�LD@�L�@�K�@�LD@�LD@�M@�L�@�MU@�MU@�M@�M�@�M�@�M@�MU@�M@�LD@�K4@�K�@�K�@�K4@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�KI@�K�@�I�@�J�@�K�@�K�@�K�@�K�@�L�@�M@�M@�M@�L�@�L�@�M@�M�@�M�@�M@�Mj@�Mj@�M�@�Mj@�M�@�M�@�N'@�M�@�N'@�N�@�O7@�N�@�N'@�M�@�N�@�N�@�N�@�N'@�N'@�M@�M@�L�@�L�@�LY@�LY@�L�@�Mj@�M@�Mj@�N'@�O�@�O�@�O�@�O�@�O�@�O�@�O�@�P�@�Q�@�R@�Q�@�Qn@�Q@�O�@�P]@�O7@�O7@�O7@�P]@�M@�M�@�N'@�M@�M@�N<@�N<@�N<@�N�@�N�@�N�@�N<@�M�@�L�@�L�@�L�@�Ln@�L@�K�@�Ln@�M+@�Ln@�M@�M+@�Ln@�K
@�JM@�K^@�I�@�JM@�I�@�I�@�H�@�H�@�H�@�H,@�F@�DR@�A5@�?�@�>�@�<u@�6�@�1�@�0U@�*E@��@��@��@��=@���@���@�Б@P��@P�t@P�@P�!@P�K@P�!@P��@P�%@P�@P�`@P��@P�@P�@P�@P�@P�@P�@P�@P��@P�@P�@P�i@P��@P�m@P�C@P�@P�C@P�@P�C@P�@P�@P�H@P��@P�"@P��@P�Y@P�@P�@P�]@P�8@P�@@P�E@P�@P��@Pם@P��@P��@P�@P��@P��@P�R@P�R@P�R@PԪ@PԀ@P�@Pӄ@Pҳ@Pѷ@P�@P��@P�h@P��@P��@P�l@P�B@P�@PΚ@P�p@P�@P��@P�u@P̣@P��@P̣@P�%@P��@Pʂ@P�.@P��@P�2@P�2@P��@P�?@P��@P��@P�U@P�@P��@P�Y@P��@P�]@P��@P�b@P�b@P��@P�f@P��@P��@P�j@P��@P�j@P�j@P�j@P�j@P�j@P�@P�j@P�j@P�j@P�@P��@P�o@P�o@P�o@P�o@P�o@P�@P��@P��@��D@��^@���@���@��/@���@��@��@@��+@���@��@@��@��n@���@��@���@��Y@��@��/@��/@��n@���@���@���@��@��+@���@��@@��j@���@��j@���@���@��@���@��j@���@���@��j@��'@��<@��f@��3@���@���@���@���@���@���@��f@��a@���@��a@���@��7@���@��'@��<@��'@���@���@��]@��a@��'@���@���@���@���@��v@��v@���@���@��	@��r@��@���@���@���@���@���@��"@���@��@��'@���@���@���@��@��Q@���@���@��f@��'@��f@���@��@���@��j@��Q@���@���@���@��3@��@���@��a@���@���@��@��a@��Y@���@���@���@��H@��H@��]@��H@���@���@���@��*@��@���@��/@���@���@���@��n@��n@���@���@��L@���@��Q@���@��@��"@��"@��v@��D@��n@��T@��@��~@���@��@���@���@���@���@��i@��L@��T@��P@���@��@���@���@��*@���@��	@���@���@���@��@���@���@���@��Y@��&@��@���@��L@���@��	@��@��"@��"@���@��]@���@���@���@��]@���@��a@��"@���@���@��	@��Y@��@���@���@��@���@���@��@���@���@���@���@���@���@��T@�� @���@��Q@���@��H@���@Q$�@Q$�@Q$ @Q$J@Q$J@Q$t@Q$t@Q$t@Q$ @Q"�@Q �@Q�@Qq@Q@Q�@Q�@Q�@Q�@Q@Q@Q?@Q�@Q�@Q�@Q�@Qq@Q�@Q�@Q�@Q�@Q�@Q�@Q�@QH@Q�@Q�@Q�@Q�@Q/@Q	@Q�@Q�@QI@Qs@Qs@Qs@Qs@Qw@QM@Q@Q(@Q�@QV@Q�@Q�@Q�@Q@Q0@Q�@Q�@Q
�@Q
=@Q	�@Q	@Q�@Q�@Q�@Qp@QF@Q@Q�@Q�@Qt@Qy@QO@QO@Qy@Q)@Q�@Q.@Q�@Q�@Q\@Q�@Q@P��@P�+@P�Y@P�@P��@P��@P�	@P�8@P��@P�@P��@P�@@P��@P�o@P�E@P�@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�s@P�@P�I@P�s@P�I@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       43443444433443443443434444443443343344344444444444444334344434444344444434433444444434444444334344434433444444433443444443334344334444344344344443444344334443344444444433444434334433443443334334433443443444344434433434433444333344434433434333343334333443333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&oG�O�G�O�@�/JG�O�G�O�G�O�G�O�@�.�@�5gG�O�G�O�@��G�O�G�O�@�-�G�O�G�O�@�g�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�@�*�G�O�G�O�@�%]@�;�G�O�@�Bs@��G�O�G�O�@�"zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@V��@Z��G�O�@���G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�@�0�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@�3�@�+oG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�2%@��!G�O�@�5kG�O�G�O�G�O�@�8�G�O�G�O�@�1�@�7�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�2�@�6gG�O�G�O�@�1fG�O�G�O�G�O�G�O�G�O�@�5@�5�@�6�G�O�@�4�G�O�G�O�@�1�@�6�G�O�G�O�G�O�G�O�@V��G�O�G�O�@�1}G�O�G�O�@��xG�O�G�O�G�O�G�O�@�HBG�O�G�O�G�O�@�0�G�O�G�O�@�/G@��8G�O�G�O�G�O�@�2"@�7wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�߻@�5lG�O�G�O�G�O�G�O�@]f*G�O�@�,&@�i�G�O�G�O�@�3F@��G�O�G�O�@�5gG�O�G�O�@\��@�4�@��]G�O�@�7@�2G�O�G�O�@�.�@��G�O�G�O�@�'�G�O�G�O�@c�ZG�O�G�O�G�O�@�4ZG�O�G�O�G�O�@�-�G�O�G�O�@���@�!�G�O�@�%rG�O�G�O�@��@s�G�O�G�O�G�O�@�8�@�9D@�:V@�:�G�O�G�O�G�O�@��G�O�G�O�@�82@�8�G�O�@�)�G�O�@�7�@�7�@�7�@�7�G�O�@�8o@�7t@�>�G�O�@�5�@�2�@�2G�O�G�O�@�8�@�9�@�7�@X��@�9F@�82@�8�@�9�@�:*G�O�@�9�@�9F@�8�@�8�@�:W@�:�@�9@�8�@�7�@�7"@�7"@�9k@�:�@�9?@�:�@�<�@�7�@�6�@�9�@�:�@�:W@�:�@�:�@�;@�7"@�7"@�8�@�8�@�9�@�8�@�82@�8�@�8v@�9D@�9�@�9�@�8�@�9k@�:@�7w@�7�@�8�@�:�@�:�@�:�@�8�@�:R@�:�@�:(@�:�@�;d@�;f@�:�@�:|@�9�@�;e@�;@�;9@�<z@�=0@�;�@�<�@�<u@�;@�=�@�>�@�?@�>�@�=6@�>C@�=�@�=�@�>o@�=�@�>�@�=�@�=�@�> @�=�@�=�@�<�@�=6@�<p@�<�@�<@�:T@�;i@�<@�;e@�<�@�;�@�;�@�<w@�<"@�<!@��
@�=0@�=�@�>�@�>�@�>@�<s@�<p@�<s@�=2@�<�@�;�@�=�@�<�@�>@@�;�@�;�@�;�@�;e@�:�@�;@�:�@�:�@�:V@�:@�=�@�=Z@�;b@�;�@�<!@�=�@�<�@�=�@�?�@�?S@�@�@�@�@�BG@�?�@�@f@�?�@�A @�=�@�<�@�=0@�<"@�<"@�<"@�;�@�:�@�:�@�;d@�;@�8@�7�@�:Q@�;e@�:V@�9�@�<H@�;@�:@�9�@�7w@�7�@�6f@�7�@�7q@�7�@�7b@�7@�7"@�6�@�80@�6�@�7@�7$@�6�@�6e@�7v@�8r@�86@�7t@�5k@�8�@�9�@�:W@�:S@�:V@�;@�;@�;=@�;e@�<"@�<"@�<"@�;�@�;�@�;�@�<@�;�@�;�@�;d@�<u@�<!@�<s@�<s@�<�@�<�@�<"@�:V@�;e@�=Z@�=�@�<�@�<�@�>=@�<K@�> @�>�@�?@�>�@�>�@�?@�>@@�<s@�=.@�<s@�=�@�;�@�1�@�<s@�;e@�;�@�<"@�:~@�<�@�=�@�<s@�>@@�=�@�=�@�>=@�=�@�?S@�<p@�:�@�:T@�<v@�<"@�<"@�> @�>�@�?�@�?Q@�>�@�>�@�<�@�>�@�?�@�?�@�?�@�?z@�?S@�?N@�?S@�>B@�>l@T�h@�>�@�>�@�>�@�?@�>�@�?�@�@e@�?�@�@�@�?�@�?@�>@@�>@@�=�@�@$@�?�@�?N@�?�@�Aq@�@�@�?�@�@f@�ƪ@�?�@�?~@�@c@�?�@�?�@�CB@�CX@�D�@�Ee@�DR@�E`@�F�@�F�@�G@�F�@�G2@�F�@�F�@�G0@�F�@�G.@�G.@�G.@�G2@�G�@�G�@�G�@�G�@�H@@�H�@�HB@�HE@�HE@�H�@�HC@�F�@�G�@��F@��Z@���@���@��0@���@��@��@@��,@���@��E@��@��n@���@��@���@��Z@��@��0@��2@��p@���@���@���@��{@��,@���@��C@��n@���@��k@���@���@���@���@��l@���@���@��h@��(@��>@��i@��2@���@���@���@���@���@���@��f@��b@���@��e@���@��9@���@��$@��8@��'@���@���@��^@��c@��&@���@���@���@���@��x@��x@���@���@��
@��n@��@���@���@���@���@���@��!@���@��@��'@���@���@���@��@��S@���@���@��g@��&@��d@���@��@���@��i@��N@���@���@���@��4@��@���@��b@���@���@��@��c@��Z@���@���@���@��J@��J@��`@��K@���@���@���@��*@��@���@��0@���@���@���@��n@��m@���@���@��M@���@��Q@���@��@��"@��$@��y@��C@��m@��U@��@��@���@��@���@���@���@���@��l@��N@��X@��R@���@��@���@���@��&@���@��	@���@���@���@��@���@���@���@��\@��%@��@���@��J@���@��@��"@��"@��"@���@��[@���@���@���@��^@���@��`@��#@���@���@��
@��Z@��@���@���@��@���@���@��@���@���@���@���@���@���@��V@��@���@��R@���@��L@���@Q$�@Q$�@Q$#@Q$J@Q$M@Q$u@Q$u@Q$x@Q$#@Q"�@Q �@Q�@Qp@Q@Q�@Q�@Q�@Q�@Q@Q@Q>@Q�@Q�@Q�@Q�@Qs@Q�@Q�@Q�@Q�@Q�@Q�@Q�@QK@Q�@Q�@Q�@Q�@Q0@Q@Q�@Q�@QK@Qs@Qp@Qv@Qs@Qx@QN@Q @Q*@Q�@Q]@Q�@Q�@Q�@Q@Q0@Q�@Q�@Q
�@Q
@@Q	�@Q	@Q�@Q�@Q�@Qr@QF@Q@Q�@Q�@Qs@Qz@QP@QN@Q~@Q*@Q�@Q.@Q�@Q�@Q[@Q�@Q@P��@P�+@P�V@P�@P��@P��@P�@P�8@P��@P�@P��@P�>@P��@P�r@P�F@P�@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�s@P�@P�H@P�s@P�H@P��@P��@��F@��Z@���@���@��0@���@��@��@@��,@���@��E@��@��n@���@��@���@��Z@��@��0@��2@��p@���@���@���@��{@��,@���@��C@��n@���@��k@���@���@���@���@��l@���@���@��h@��(@��>@��i@��2@���@���@���@���@���@���@��f@��b@���@��e@���@��9@���@��$@��8@��'@���@���@��^@��c@��&@���@���@���@���@��x@��x@���@���@��
@��n@��@���@���@���@���@���@��!@���@��@��'@���@���@���@��@��S@���@���@��g@��&@��d@���@��@���@��i@��N@���@���@���@��4@��@���@��b@���@���@��@��c@��Z@���@���@���@��J@��J@��`@��K@���@���@���@��*@��@���@��0@���@���@���@��n@��m@���@���@��M@���@��Q@���@��@��"@��$@��y@��C@��m@��U@��@��@���@��@���@���@���@���@��l@��N@��X@��R@���@��@���@���@��&@���@��	@���@���@���@��@���@���@���@��\@��%@��@���@��J@���@��@��"@��"@��"@���@��[@���@���@���@��^@���@��`@��#@���@���@��
@��Z@��@���@���@��@���@���@��@���@���@���@���@���@���@��V@��@���@��R@���@��L@���@Q$�@Q$�@Q$#@Q$J@Q$M@Q$u@Q$u@Q$x@Q$#@Q"�@Q �@Q�@Qp@Q@Q�@Q�@Q�@Q�@Q@Q@Q>@Q�@Q�@Q�@Q�@Qs@Q�@Q�@Q�@Q�@Q�@Q�@Q�@QK@Q�@Q�@Q�@Q�@Q0@Q@Q�@Q�@QK@Qs@Qp@Qv@Qs@Qx@QN@Q @Q*@Q�@Q]@Q�@Q�@Q�@Q@Q0@Q�@Q�@Q
�@Q
@@Q	�@Q	@Q�@Q�@Q�@Qr@QF@Q@Q�@Q�@Qs@Qz@QP@QN@Q~@Q*@Q�@Q.@Q�@Q�@Q[@Q�@Q@P��@P�+@P�V@P�@P��@P��@P�@P�8@P��@P�@P��@P�>@P��@P�r@P�F@P�@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�s@P�@P�H@P�s@P�H@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       43443444433443443443434444443443343344344444444444444334344434444344444434433444444434444444334344434433444444433443444443334344334444344344344443444344334443344444444433444434334433443443334334433443443444344434433434433444333344434433434333343334333443333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�;�9�;9�9�9�9s9�;�9�;:9�;�9�<�9�<�9�<R9�<�9�;�9�<9�<?9�<�9�<S9�;�9�;�9�;�9�;�9�<	9�<.9�<D9�<R9�<�9�<�9�<z9�<�9�<�9�<@9�<�9�=o9�<A9�<�9�= 9�<�9�<U9�>Q9�<�9�=�9�=�9�=�9�?i9�>�9�=o9�=o9�=:9�=l9�=#9�=�9�>�9�>:9�>�9�>O9�>�9�>9�=�9�=�9�=�9�>�9�?19�?�9�>�9�=�9�=Y9�>9�>�9�>�9�>�9�>�9�>T9�? 9�?E9�?�9�?V9�>�9�@�9�?�9�?19�@9�>t9�>9�>c9�=�9�>9�>9�>9�>c9�=�9�>:9�>9�=�9�=�9�=�9�>;9�=�9�<e9�<�9�=�9�=[9�=�9�>>9�?k9�?V9�>�9�>�9�>>9�>�9�@$9�>�9�@s9�?9�?9�?39�?9�?9�?�9�?�9�?�9�?�9�@�9�A.9�A9�@�9�@M9�>�9�@�9�@�9�@�9�@�9�?�9�>�9�>�9�>'9�=�9�=�9�>e9�>u9�>w9�>�9�@_9�@�9�AU9�A9�Az9�A�9�@:9�@�9�B�9�Cf9�CN9�Ai9�C9�AW9�B89�?�9�A9�@�9�Bj9�A*9�>�9�?D9�?	9�?�9�?�9�?V9�@9�@9�?�9�@u9�B9�A 9�A�9�>�9�>�9�?G9�?[9�>u9�>u9�>�9�?�9�?�9�?�9�?�9�?�9�?9�>�9�>v9�>�9�?9�?E9�@s9�@(9�?�9�@�9�A 9�@�9�@�9�?W9�@9�A�9�Bo9�A�9�A�9�A�9�AV9�A	9�@�9�=�9�=79�?�9�=�9;��9;��9;�\9;�9;�9;�9;�9;�9;�\9;�9;�B9;� 9;�9;�#9;��9;�9;�9;�9;�9;�9;�*9;��9;��9;�9;�9;�9;�9;�9;��9;��9;�9;�9;�9;�i9;��9;�N9;�"9;�n9;�9;�9;�9;�z9;�m9;ޑ9;ގ9;ޔ9;ޑ9;ݯ9;݊9;�F9;܃9;�9;��9;��9;�9;��9;�|9;ڽ9;�'9;�E9;خ9;�9;�9;�9;��9;֟9;֜9;�z9;�R9;�-9;�9;ջ9;Օ9;Ե9;ԏ9;ԍ9;Ը9;Ӈ9;�9;Ҥ9;�Y9;�9;��9;�,9;��9;��9;ʋ9;��9;Ʉ9;�[9;�9;Ȣ9;��9;�N9;��9;ƹ9;�9;ņ9;�g9;�?9;�9;��9;��9;Ħ9;ħ9;��9;��9;��9;��9;��9;��9;��9;Ĩ9;Ă9;�59;�[9;Ă9;�[9;��9;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbNBbNBbNBcTBbNBbNBbNBbNBcTBbNBbNBbNBaHBK�B>wBXB\)B]/B[#B^5B]/BZBT�BN�BJ�BJ�BM�BJ�BI�BH�BG�BI�BG�B^5B��B��B��B�BuB��B�B�B��B��B}�B`BB`BBt�B|�B�B�DB�hB��B��B��B��B�jB�}B�jB�jB�FB�RB�BĜBƨBB�}B��B�B�fB�B�B��B��B��B��B��BǮBB�dB�B��B�{B�JB~�Bs�Bk�BXB9XB2-B9XB'�B�B{B+B�B�)B�dB��Br�BB�B$�B\B
�B
�HB
��B
B
�'B
��B
�PB
w�B
l�B
`BB
ZB
XB
R�B
G�B
'�B
�B
B	�B	�9B	��B	��B	�B	e`B	Q�B	I�B	;dB	2-B	&�B	�B	uB	B�B�BÖB��BƨB�}BĜB��BɺBƨBĜB�}B�jB�XB�RB�LB�FB�?B�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�JB�=B�+B�B� B|�B{�By�Bx�Bv�Bu�Bt�Br�Bp�Bo�Bm�Bl�BjBgmBgmBffBiyBjB}�B�B�B�B� Bz�B|�B{�B|�Bz�By�By�Bw�Bt�Bv�Bt�Bo�Bk�BffB`BBS�BC�B;dB9XB8RB7LB7LB7LB5?B33B33B2-B0!B/B.B.B0!B/B.B-B/B33B49B6FB5?B5?B5?B5?B7LB7LB9XB=qBA�BD�BD�BF�BG�BG�BI�BJ�BJ�BM�BP�BQ�BQ�BQ�BW
B[#B[#BZB\)BaHBbNBbNBcTBe`BgmBhsBhsBk�Bm�Bo�Bq�Bu�By�Bz�B{�B�B�1B�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�RB�^B�qB�qB��BŢB��B��B��B��B��B��B��B��B�B�B�B�
B��B�#B�HB�TB�mB�B�B�B��B��B	B	JB	PB	bB	hB	�B	�B	�B	�B	�B	 �B	'�B	+B	/B	7LB	;dB	<jB	?}B	E�B	J�B	J�B	L�B	O�B	Q�B	Q�B	XB	[#B	\)B	\)B	ZB	]/B	[#B	ZB	ZB	YB	YB	YB	[#B	^5B	cTB	ffB	ffB	hsB	o�B	r�B	v�B	x�B	z�B	|�B	|�B	}�B	~�B	� B	�B	�B	�B	�1B	�DB	�DB	�PB	�hB	�{B	��B	��B	�oB	�VB	�VB	�PB	�JB	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�B	�!B	�!B	�!B	�'B	�3B	�FB	�XB	�XB	�XB	�XB	�^B	�jB	�wB	�wB	�wB	��B	B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�/B	�5B	�BB	�TB	�TB	�TB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
YB
�B
�B
�B
*KB
5�B
<jB
@�B
E�B
I�B
QNB
W$B
\]B
aB
dtB
hsB
lB
pUB
s3B
x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AϋBC?���@��B$�@Q��>�C?	�?OY�B%�B*g?)a=?/�BA��o?DZ?Z�B)?R?P+�A��:?�0B@.>�ӷ?%�?��@��;?6��@��B"A7}�?��6B�B�?3�xA�7B�!?D#SA���B%�?W_�A�DT?�@��j@X'�?�o~>�2v>��~?
E�?S,b@v9?�A�?M�?I�A��A�7�?M�BϸAH0�>ѮO?*Q�B	E�>��?'?2f?�-�B�r>�f�?�?�D?��?0?�Ay�B:?A�?dqB-Ba�@�=>�#?��?VFZ@+,?H�A&rB>�>��>��?l�D@��,?L��?l@+�B'lB
��?Q��B+9@��Q?Y��@�|B0�?I�@���B"jB/�?uA%B@)��@3pH@�X�?^�?j��B&�B($A*_?@+�B5�@|�>�:C@ �bA(��?fVB#�B%`B�?��B*�A&��?KB(�B�j?E�@;��>�X?*�'A���@�ed@��B&	A!*?&½A���@��>�?#2@�A��>�~�?"k�?��OB2�?
h?��B(�B�>ৃ?̟@�Y<B'dB/e?��?A
)�@��@u�??<k�@�.A�hv?,��A�uB(Avt_>㕁?��@ĭ8A��:?9�B"�AЍi?#?A�JB&�B?2�"A�B/%A��?2[�A���B(mB
��?v��B(B&�?���?�� B#fA�c:?&W@�sBYP>��?>H�A���A8�?�d?�v}B�gA��?���@��+BZs@�Y�?=�2B
�kB�E?��Bl�A�A9x�A֡A�y�@`�AgP�?2>�BCcB/�B+�B=�?
@o�?�I�A�މA�j�?�v!B(KB�@إ�A�L�?�=�B(�B(�B1`BL�?��;B(|B*�B��A�x(B(OB7B0�@-��@"�B*�B+|B1A�6�B(|B)�B*�B+8B/�A��|B)�B(|B.B&�B+�B)%B)�B(�B)B*cB(�B+�B'bB&�B)�B)WB)XB.�B(�B(�B)oB)%B*}B,oB*cB++B)HB(�B(�B*yB)�B)XB)=B)�B*�B*VB+BB,�B+tB+�B+�B,sB-�B,QB(�B-�B.�B(�B)@B,B+3B)�B*}B+'B+|B*jB*�B=�B*�B,�B+�B+�B,�B.	B*eB*�B,!B*]B)�B*�B)�B)�B*B)�B*7B)4B+&B)�B*+B*�B*�B)�B)�B(`B)~B6�B+;B)�B*YB*�B)�B*B*�B*WB+A��B*�B*�B+�B+�B,
B*�B)�B+eB+IB)�B*�B*B* B,<B*�B)�B)�B*jB*�B*B*�B+FB*8B*�B.�B,9B'GB.�B2'B+&B*�B*3B+)B,nB-�B+cB+8B+B(�B)�B,�B+�B)�B*�B+B*NB*NB+YB+NB*�B+3B*�B3B+B/�B+�B,�B+'B*eB*�B*�B)�B(MB*�B,B+B(^B+B*�B*kB+4B*B,�B*�B+B)�B*�B+GB,BB+�B,&B*�B,�B,B+NB+�B*�B+�B*�B*�B*�B*bB+B+B+B*B*�B*�B*FB*�B*B+3B+\B+B,-B+eB*�B+�B*NB*0B+�B*�B*�B*�B*�B+{B+>B*xB*�B+OB+�B+�B+kB,<B*�B*xB*�B+�B-A�B*�B*jB+�B*NB)�B*�B+.B+eB,<B*�B+�B+{B*eB-GB)�B)�B)gB)�B+ B+B*xB-RB+)B+�B*�B+BPjB+B*iB*�B*�B+B*B*�B*B)�B,jA�ލB*�B*�B,)B*�B*�B+)B+�B+!B+5B(�B,�B-�B-�B,�B-7B,�B*B+�B+HB+5B*iB+�A�AB*iB+�B+B*�B+B-�B,*B+�B,jB*�B-#B,�B,�B+�B*nB*�B+/B*qB)�B)�B)�B)�B*�B)�B*B+B*	B*B*�B*"B)�B)�B)�B*B*}B(QB(�B)CB)�B)�B+mB+�B*B)�B(�B){B)�B)�B*#B*�B)]B*vB*B*B*UB*MB*EB*�B*yB*hB*=B*�B*�B*�B+*B*�B)�B*8B*�B+dB*=B*gB)�B*�B*�B*�B+�B*�B)(B*�B)�B*�B+uB*�B)�B*�B+uB)�B+B*�B)�B*�B*�B+RB+AB+�B**B)�B*�B)�B+sB+=B+B*�B)�B*�B*B*�B)�B*(B*�B+TB+�B)�B)�B)�B*QB)�B*�B*�B+�B*�B*�B)�B*8B*�B*(B+.B*B+LB)�B(ZB,B+�B+_B+WB+�B+�B*�B*$B*�B+cB,B*�B+iB+`B)�B+�B+�B, B+�B+'B+�B,@B+B+fB,B+�B*jB+FB*B*�B*�B+
B*TB+B)IB*	B+FB,B*�B+�B+%B,�B*�B+�B,TB,�B,B,B,JB*�B+#B,1B,GB*�B*�B+^B+	B*�B)�B)�B+:B*iB,�B+fB*MB,�B,hB*�B*�B,cB+�B+�B*=B*�B*�B+qB)wB*�B*�B*�B*hB*�B(�B+�B*,B)wB*iB)KB(�B(B'`B)�B(�B(B&�B%�B%B%B#�B"�B!�B5B�B�BBHB�B�B�BxB
� B
�B
�*B
�SB
�!B
�B
�B	�]B	��B	��B	ȟB	ǠB	ȅB	�YB	ǥB	ƃB	��B	�B	�RB	�EB	ɳB	�+B	�mB	�_B	ʿB	ʓB	�VB	�B	�B	ʨB	�/B	�B	�6B	��B	��B	��B	�LB	�?B	��B	�{B	�B	�JB	�B	ɶB	ɜB	�$B	�?B	��B	�B	�B	ʸB	ʌB	ʑB	��B	�B	��B	��B	ʕB	˘B	ˋB	��B	��B	�`B	�B	�cB	ʝB	�%B	��B	˓B	�B	��B	��B	ˊB	�^B	��B	��B	�qB	�6B	��B	�DB	�YB	�-B	��B	��B	�yB	�>B	��B	�^B	�QB	�^B	�-B	�tB	�'B	̿B	�tB	�B	��B	�ZB	�B	̊B	�?B	�CB	̭B	�sB	��B	��B	̅B	̵B	�{B	�nB	�TB	�FB	�9B	��B	�B	�B	�B	̟B	�UB	�B	� B	��B	��B	��B	́B	�eB	�9Bb�Bb�Bb�Bb`BbeBa�BbBBbvBbZBa�BbUBbBbEBbvBa�BbqBbBbtBa�Bb�Bb�Bb�BbBbBb�Bc'Bb�Bc"Bc8BbzBbVBb�Bc"Bc	Bb_Bb�Bb;Bb�Bb�BcYBcdBc{Bb�BcBb�Bb�Bb�Bb�Bb{BbWBbmBb�BbLBa�BbBbZBb�Bb�Bb{BbGBbyBb BckBcBc�Bb�Bb�Bb�BbkBbcBa�Bb�Ba�BbPBb�BbSBbtBc"Bc5BcCBbSBa�Bb�BbBb�BboBbzBb�BbBb�Bb6Ba�Ba�Bb�BbFBcBa�Bb]Bc�Bb�Bb�BbBc1BbDBb�Bc�Bb�Bb�Bc�Bb&Bc#Bb�BcJBb�Bb�Bb�Bb�Bb�Bb�BcBcBb�Bb�Bc@Bb�Bb�Bb�Bb�Bb�Bb�BcdBbtBb�BblBb�Bb"Bb�Ba�Bb�Bb�Bb�Bb@BcBb�BbYBc-Bb�Bb"Bb�Bb�BbxBb�Bb�Bc{Bb�Ba�Bc(Bb�BbBbZBbWBb�BbmBb]BbTBb�BcGBc?BbGBb�Bb�Bb�Bc�BbpBa�BbDBbOBb�Bb�Ba�BbhBa�Ba�Ba�BbHBa�Bb!Ba�Bb7Ba�BaBb5Ba�BacBa�Ba�Ba�B`�B`�B`�B`�B`�B`-B_�B_B_�B_@B_$B^'B]�B]�B\�B	�B	�B	�"B	�3B	�&B	�8B	�*B	�B	��B	��B	�gB	�B	�B	�B	�B	�<B	�.B	�@B	�pB	�cB	�gB	�-B	� B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�UB	��B	��B	��B	�B	�6B	�bB	�6B	��B	��B	�B	��B	��B	��B	�B	��B	�^B	�B	��B	�MB	�QB	�cB	�7B	��B	�*B	��B	��B	��B	��B	�xB	�B	��B	��B	��B	�bB	�6B	��B	��B	��B	�kB	��B	�|B	�oB	�tB	��B	�B	��B	�gB	�B	��B	�YB	�;B	��B	�B	�xB	�1B	�B	��B	�aB	��B	�5B	��B	��B	�B	��B	�wB	�>B	�B	��B	��B	��B	��B	��B	�|B	�oB	�aB	�TB	�:B	�-B	�B	��B	��B	��B	��B	�wB	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943443444433443443443434444443443343344344444444444444334344434444344444434433444444434444444334344434433444444433443444443334344334444344344344443444344334443344444444433444434334433443443334334433443443444344434433434433444333344434433434333343334333443333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   Bb;Bb;Bb;Bc?Bb9Bb9Bb9Bb9BcBBb;Bb;Bb9Ba6BK�B>cBW�B\B]B[B^#B]BZ
BT�BN�BJ�BJ�BM�BJ�BI�BH�BG�BI�BG�B^#B�xB�B��B�xB_B��B�
B��B�zB��B}�B`/B`,Bt�B|�B��B�0B�TB�mB�~B��B��B�WB�iB�WB�WB�2B�?B��BĆBƕB�zB�jB�rB�
B�QB�vB�B��B��B��B˰BʫBǝB�|B�QB��B��B�hB�4B~�Bs�BksBW�B9>B2B9@B'�B�BgBB�B�B�MB�kBr�BBxB$�BHB
�B
�2B
��B
�yB
�B
��B
�7B
w�B
luB
`+B
ZB
W�B
R�B
G�B
'�B
�B
�B	�B	�!B	��B	��B	�B	eHB	Q�B	I�B	;LB	2B	&�B	�B	]B	 B��B�B�|B��BƍB�eBąB��BɡBƌBĄB�bB�PB�>B�8B�3B�,B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�yB�nB�dB�MB�BB�1B�"B�B� B�B|�B{�By�Bx�Bv�Bu�Bt�Br�Bp�Bo�BmtBlpBjfBgQBgSBfJBi_BjcB}�B��B��B��B�Bz�B|�B{�B|�Bz�By�By�Bw�Bt�Bv�Bt�Bo�BkhBfIB`'BS�BCyB;JB9:B86B7.B70B71B5$B3B3B2B0B.�B-�B-�B0B.�B-�B,�B.�B3B4B6)B5"B5$B5"B5"B7/B70B9;B=UBAjBD~BDBF�BG�BG�BI�BJ�BJ�BM�BP�BQ�BQ�BQ�BV�B[B[BY�B\Ba*Bb/Bb/Bc5BeABgOBhTBhTBklBmqBoBq�Bu�By�Bz�B{�B��B�B�0B�SB�cB�vB��B��B��B��B��B��B��B��B��B��B��B�B�)B�3B�@B�TB�UB�gBŃBͶBϿB��B��B��B��B��B��B��B��B��B��B��B�B�+B�7B�OB�yB��B�B��B��B	�B	+B	2B	DB	JB	bB	tB	|B	�B	�B	 �B	'�B	*�B	.�B	7.B	;DB	<MB	?]B	E�B	J�B	J�B	L�B	O�B	Q�B	Q�B	W�B	[B	\B	\	B	Y�B	]B	[B	Y�B	Y�B	X�B	X�B	X�B	[B	^B	c4B	fHB	fHB	hVB	oB	r�B	v�B	x�B	z�B	|�B	|�B	}�B	~�B	�B	��B	��B	�B	�B	�&B	�(B	�4B	�JB	�ZB	�bB	�eB	�SB	�9B	�9B	�2B	�-B	�9B	�IB	�SB	�UB	�_B	�vB	�~B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�
B	�B	�)B	�:B	�;B	�8B	�9B	�AB	�NB	�XB	�ZB	�YB	�eB	�rB	�~B	ńB	ǏB	ɛB	˦B	˨B	κB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�$B	�6B	�5B	�7B	�=B	�OB	�aB	�lB	�iB	�fB	�lB	�lB	�nB	�qB	�{B	�zB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��G�O�B	��B
<B
�B
�B
hB
*-B
5pB
<NB
@�B
E�B
I�B
Q0B
WB
\>B
`�B
dUB
hTB
k�B
p9B
sB
xlG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BC
G�O�G�O�B$�G�O�G�O�G�O�G�O�B%�B*TG�O�G�O�A��NG�O�G�O�B(�G�O�G�O�A��G�O�B@G�O�G�O�G�O�G�O�G�O�G�O�B"G�O�G�O�B�B��G�O�A�6�B�G�O�G�O�B%yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�7|G�O�BϦG�O�G�O�G�O�B	E�G�O�G�O�G�O�G�O�B�_G�O�G�O�G�O�G�O�G�O�G�O�B:-G�O�G�O�B,�Ba�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B'XB
��G�O�B+&G�O�G�O�G�O�B0�G�O�G�O�B"XB/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B&wB(G�O�G�O�B5�G�O�G�O�G�O�G�O�G�O�B#�B%NB��G�O�B*wG�O�G�O�B(�B�XG�O�G�O�G�O�G�O�A���G�O�G�O�B%�G�O�G�O�A��fG�O�G�O�G�O�G�O�A��G�O�G�O�G�O�B2�G�O�G�O�B(�B�G�O�G�O�G�O�B'OB/TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�UB(G�O�G�O�G�O�G�O�A��G�O�B"�AЍMG�O�G�O�B&�B	G�O�G�O�B/G�O�G�O�A�ʿB(ZB
��G�O�B'�B&�G�O�G�O�B#SA�cG�O�G�O�BY@G�O�G�O�A���G�O�G�O�G�O�B�UG�O�G�O�G�O�BZcG�O�G�O�B
�XB�2G�O�Bl�G�O�G�O�A֠�A�y�G�O�G�O�G�O�BCQB/yB+�B=�G�O�G�O�G�O�A��lG�O�G�O�B(9B��G�O�A�LtG�O�B(�B(�B1LBL�G�O�B(hB*�B��G�O�B(=B7lB0�G�O�G�O�B*�B+hB1A�6�B(lB)�B*�B+$B/|G�O�B)�B(lB-�B&�B+�B)B)�B(�B)lB*QB(�B+�B'NB&�B)�B)BB)EB.�B(�B(�B)^B)B*kB,\B*QB+B)7B(�B(�B*dB)�B)EB),B)�B*�B*BB+.B,tB+cB+wB+�B,aB-�B,<B(�B-�B.�B(�B),B,B+ B)�B*kB+B+hB*WB*�B=�B*�B,�B+yB+�B,�B-�B*QB*�B,B*HB)�B*�B)�B)�B)�B)�B*%B)!B+B)�B*B*�B*�B)�B)�B(MB)kB6�B+*B)qB*GB*�B)�B)�B*�B*EB+A��B*nB*�B+�B+�B+�B*�B)�B+QB+4B)�B*�B)�B*B,)B*�B)�B)�B*WB*qB*
B*�B+3B*&B*�B.�B,#B'2B.�B2B+B*�B*B+B,ZB-�B+QB+&B+jB(�B)�B,qB+�B)�B*nB*�B*;B*;B+GB+<B*qB+ B*�B3B+B/�B+�B,�B+B*RB*�B*�B)�B(;B*�B,B+B(HB*�B*�B*VB+"B*B,�B*�B+B)�B*�B+3B,-B+�B,B*�B,�B, B+;B+�B*�B+�B*�B*�B*�B*MB*�B*�B*�B)�B*�B*�B*1B*�B)�B+ B+IB+B,B+QB*�B+�B*;B*B+�B*�B*�B*�B*�B+eB++B*dB*�B+>B+�B+�B+ZB,)B*�B*dB*�B+�B-A��B*�B*WB+�B*;B)�B*�B+B+QB,)B*�B+�B+eB*QB-4B)�B)�B)TB)�B+B*�B*dB-@B+B+�B*�B*�BPWB*�B*VB*�B*�B*�B* B*�B* B)�B,WA��zB*}B*�B,B*�B*}B+B+�B+B+"B(�B,�B-�B-�B,�B-#B,�B*B+�B+3B+"B*VB+�A�#B*VB+�B+B*�B+jB-�B,B+�B,YB*�B-B,�B,�B+yB*WB*�B+B*]B)�B)rB)�B)�B*�B)�B*B+B)�B)�B*�B*B)�B)�B)�B)�B*jB(<B(�Bb�BbyBb{BbLBbRBa�Bb/BbbBbIBa�BbDBa�Bb0BbbBa�Bb^Ba�Bb`Ba�BbyBb�Bb�Ba�Ba�Bb�BcBb�BcBc'BbgBbDBb�BcBb�BbGBb�Bb)Bb�Bb�BcFBcQBcjBb�Bb�Bb�Bb�Bb�Bb�BbiBbDBbXBb�Bb9Ba�Ba�BbDBbBb�BbiBb5BbeBa�BcZBb�BcwBb�Bb�BbpBbXBbOBa�Bb�Ba�Bb;Bb�Bb?BbbBcBc"Bc0Bb?Ba�Bb�BbBb�Bb[BbiBb�Ba�BbkBb"Ba�Ba�Bb�Bb2Bb�Ba�BbIBc�Bb�BbvBa�BcBb0Bb�Bc�Bb�BbyBc�BbBcBbvBc4BbyBb�Bb�Bb�Bb�Bb�Bb�Bb�Bb�Bb�Bc/BbyBbBb�Bb�Bb�Bb�BcQBbbBb�BbWBb�BbBb�Ba�Bb�Bb�Bb�Bb,Bb�Bb�BbFBcBb�BbBb�Bb�BbaBb�Bb�BcjBb�Ba�BcBb�BbBbDBbDBb�BbWBbGBbBBb�Bc4Bc/Bb4Bb�Bb�Bb�Bc�Bb[Ba�Bb4Bb?Bb�Bb�Ba�BbUBa�Ba�Ba�Bb5Ba�BbBa�Bb$Ba�B`�Bb#Ba�BaQBa�Ba�BasB`�B`�B`�B`�B`�B`B_�B^�B_�B_-B_B^B]�B]{B\�B	�B	�zB	�B	�B	�
B	�B	�B	��B	�B	�B	�HB	��B	�yB	��B	�sB	�B	�B	� B	�RB	�BB	�HB	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�8B	�B	��B	��B	��B	�B	�BB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�@B	��B	�{B	�1B	�2B	�CB	�B	��B	�B	��B	��B	�hB	��B	�XB	��B	��B	�}B	�oB	�EB	�B	��B	��B	�xB	�LB	��B	�^B	�QB	�YB	�cB	��B	��B	�IB	��B	��B	�;B	�B	��B	�B	�XB	�B	��B	��B	�CB	��B	�B	��B	��B	��B	��B	�YB	� B	��B	��B	��B	�aB	�fB	�wB	�\B	�RB	�DB	�7B	�B	�B	��B	��B	�nB	��B	��B	�WB	��B	��Bb�BbyBb{BbLBbRBa�Bb/BbbBbIBa�BbDBa�Bb0BbbBa�Bb^Ba�Bb`Ba�BbyBb�Bb�Ba�Ba�Bb�BcBb�BcBc'BbgBbDBb�BcBb�BbGBb�Bb)Bb�Bb�BcFBcQBcjBb�Bb�Bb�Bb�Bb�Bb�BbiBbDBbXBb�Bb9Ba�Ba�BbDBbBb�BbiBb5BbeBa�BcZBb�BcwBb�Bb�BbpBbXBbOBa�Bb�Ba�Bb;Bb�Bb?BbbBcBc"Bc0Bb?Ba�Bb�BbBb�Bb[BbiBb�Ba�BbkBb"Ba�Ba�Bb�Bb2Bb�Ba�BbIBc�Bb�BbvBa�BcBb0Bb�Bc�Bb�BbyBc�BbBcBbvBc4BbyBb�Bb�Bb�Bb�Bb�Bb�Bb�Bb�Bb�Bc/BbyBbBb�Bb�Bb�Bb�BcQBbbBb�BbWBb�BbBb�Ba�Bb�Bb�Bb�Bb,Bb�Bb�BbFBcBb�BbBb�Bb�BbaBb�Bb�BcjBb�Ba�BcBb�BbBbDBbDBb�BbWBbGBbBBb�Bc4Bc/Bb4Bb�Bb�Bb�Bc�Bb[Ba�Bb4Bb?Bb�Bb�Ba�BbUBa�Ba�Ba�Bb5Ba�BbBa�Bb$Ba�B`�Bb#Ba�BaQBa�Ba�BasB`�B`�B`�B`�B`�B`B_�B^�B_�B_-B_B^B]�B]{B\�B	�B	�zB	�B	�B	�
B	�B	�B	��B	�B	�B	�HB	��B	�yB	��B	�sB	�B	�B	� B	�RB	�BB	�HB	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�8B	�B	��B	��B	��B	�B	�BB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�@B	��B	�{B	�1B	�2B	�CB	�B	��B	�B	��B	��B	�hB	��B	�XB	��B	��B	�}B	�oB	�EB	�B	��B	��B	�xB	�LB	��B	�^B	�QB	�YB	�cB	��B	��B	�IB	��B	��B	�;B	�B	��B	�B	�XB	�B	��B	��B	�CB	��B	�B	��B	��B	��B	��B	�YB	� B	��B	��B	��B	�aB	�fB	�wB	�\B	�RB	�DB	�7B	�B	�B	��B	��B	�nB	��B	��B	�WB	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943443444433443443443434444443443343344344444444444444334344434444344444434433444444434444444334344434433444444433443444443334344334444344344344443444344334443344444444433444434334433443443334334433443443444344434433434433444333344434433434333343334333443333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311650172020083116501720200831165017202008311650172020083116501720200831165017202008311650172020083116501720200831165017202008311650172020083116501720200831165017AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817342019021918173420190219181734    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817342019021918173420190219181734  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817342019021918173420190219181734  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311650172020083116501720200831165017  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                