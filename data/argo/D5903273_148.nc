CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ]   N_CALIB       	N_HISTORY             
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
resolution        =���   axis      Z        (\  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ml   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (\  w�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (\  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (\  �T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (\ �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 -$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (\ 7<   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (\ _�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (\ �   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �h   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (\ Ā   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (\ ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (\ P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 G�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (\ Q�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � z    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   z�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �Argo profile    3.1 1.2 19500101000000  20190219181713  20200831164919  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�L�0�*[@�L�0�*[@�L�0�*[111 @�L�}'��@�L�}'��@�L�}'��@5F$�/�@5F$�/�@5F$�/��cYV��cYV��cYV�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  BffB  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy~�D�D�M�D��{D��\D��D�QHD��D�ɚD��D�R�D���D��
D�	HD�1�D�r�D�{D���D�,�D�D��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����;�������������������������������������������>L��    �����L�;��������L�;�����������    �L�;���������������    �����L�;L�;�����������=��ͽ��;��������L��    �����L�;L�ͽ��;�����������    =���    �����L�;��������L�;������������L�;����������������L�;L�;��������L�ͽ��;����L�;L�;������������L�;��������������;����L�;�������    �����L�;����L�ͽ��ͽ��;��������L�;L�;������ͽ��;L�;����L�ͽ��ͽ��;����L�;L�;L�;L�;����������ͽ��;����L�;L�;���������������    �L�;����������;L�;����������������������������L�ͽ��;��������L�;�������������������=���    �����������;L�;�������    =��;L�;L�;L�;L�;L�;����L�;��������������;��������������;L�;����L�;L�ͽ��;��������L�ͽ��;����������;L�;��������L�;����L�;L�;L�;L�;L�;����L�;���        ���;�����������    �L�;L�;L�;L�ͽ��ͽ���    �����L�;L��    ���;L�;L�ͽ��;L�;L�ͽ��;���    ���ͽ���    ���ͽ���    =���    �L��    =��ͽ��ͽ��ͽ���        =���    ����        =���    >L��    �L�ͽ��ͽ���=��ͽ���                ����        >L��=��ͽ��ͽ��ͽ���        ����        =���        =���    ����            ���ͽ���    ����    ���ͽ��;L��    ���ͽ��ͽ��ͽ��ͽ���    =���    ����    ����    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ����    =���=���                ����        >L��=��ͽ��ͽ��ͽ���        ����=���=���                            ����    =��ͽ��ͽ���=���=���    �L��=���=���=���    ����    ����    ���ͽ���        �L�ͽ���        ���;L�ͽ��ͽ��ͽ���    �L�ͽ��ͽ���    =���=��ͽ��;L��    ����    =��ͽ��ͽ��ͽ��ͽ���        ���ͽ���    =���>L�ͽ���    >L��=���                    ���ͽ���=��ͽ���            ���ͽ���            ���ͽ���        �L��    ����=���=��ͽ��ͽ��ͽ���            =���                �������;L��            =��ͽ��ͽ���=���        =���    ���ͽ��ͽ���=���=��ͽ���        =���    ���;L��=��ͽ��ͽ��ͽ���=��;L�ͽ��ͽ���=��ͽ��ͽ���=���    ����            �L�;L�ͽ���    ���ͽ���=���    ����=���                ���;L��        >L�ͽ���    =���=���                        ���;L�ͽ���    �L��        =���=���>L��>���>���>���?   ?��?��?333?L��?�  ?fff?�  ?���?���?�ff?�  ?�33?�  ?�  ?ٙ�?ٙ�?�ff?�33@   @   @ff@��@33@��@   @&ff@333@9��@Fff@L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@ə�@�  @�ff@ٙ�@���@�33@�ff@���@�  @�ff@���@���A��A33AffA  A33A��A  A��A33AffA��A33A��AffA!��A#33A&ffA)��A+33A,��A0  A1��A333A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAQ��AS33AVffAX  AY��A[33A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���Ař�A�ffA�33A���Aə�A�ffA�33A�  A���A�ffA�33A�  A���Aљ�A�ffA�  A���Aՙ�A�ffA�33A�  A���A�ffA�33A�  Dq��Dq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt��Dt��Du  DufDu�Du3Du  @   @&ff@333@9��@Fff@L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@ə�@�  @�ff@ٙ�@���@�33@�ff@���@�  @�ff@���@���A��A33AffA  A33A��A  A��A33AffA��A33A��AffA!��A#33A&ffA)��A+33A,��A0  A1��A333A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAQ��AS33AVffAX  AY��A[33A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���Ař�A�ffA�33A���Aə�A�ffA�33A�  A���A�ffA�33A�  A���Aљ�A�ffA�  A���Aՙ�A�ffA�33A�  A���A�ffA�33A�  Dq��Dq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt��Dt��Du  DufDu�Du3Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  BffB  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy~�D�D�M�D��{D��\D��D�QHD��D�ɚD��D�R�D���D��
D�	HD�1�D�r�D�{D���D�,�D�D��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����;�������������������������������������������>L��    �����L�;��������L�;�����������    �L�;���������������    �����L�;L�;�����������=��ͽ��;��������L��    �����L�;L�ͽ��;�����������    =���    �����L�;��������L�;������������L�;����������������L�;L�;��������L�ͽ��;����L�;L�;������������L�;��������������;����L�;�������    �����L�;����L�ͽ��ͽ��;��������L�;L�;������ͽ��;L�;����L�ͽ��ͽ��;����L�;L�;L�;L�;����������ͽ��;����L�;L�;���������������    �L�;����������;L�;����������������������������L�ͽ��;��������L�;�������������������=���    �����������;L�;�������    =��;L�;L�;L�;L�;L�;����L�;��������������;��������������;L�;����L�;L�ͽ��;��������L�ͽ��;����������;L�;��������L�;����L�;L�;L�;L�;L�;����L�;���        ���;�����������    �L�;L�;L�;L�ͽ��ͽ���    �����L�;L��    ���;L�;L�ͽ��;L�;L�ͽ��;���    ���ͽ���    ���ͽ���    =���    �L��    =��ͽ��ͽ��ͽ���        =���    ����        =���    >L��    �L�ͽ��ͽ���=��ͽ���                ����        >L��=��ͽ��ͽ��ͽ���        ����        =���        =���    ����            ���ͽ���    ����    ���ͽ��;L��    ���ͽ��ͽ��ͽ��ͽ���    =���    ����    ����    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ����    =���=���                ����        >L��=��ͽ��ͽ��ͽ���        ����=���=���                            ����    =��ͽ��ͽ���=���=���    �L��=���=���=���    ����    ����    ���ͽ���        �L�ͽ���        ���;L�ͽ��ͽ��ͽ���    �L�ͽ��ͽ���    =���=��ͽ��;L��    ����    =��ͽ��ͽ��ͽ��ͽ���        ���ͽ���    =���>L�ͽ���    >L��=���                    ���ͽ���=��ͽ���            ���ͽ���            ���ͽ���        �L��    ����=���=��ͽ��ͽ��ͽ���            =���                �������;L��            =��ͽ��ͽ���=���        =���    ���ͽ��ͽ���=���=��ͽ���        =���    ���;L��=��ͽ��ͽ��ͽ���=��;L�ͽ��ͽ���=��ͽ��ͽ���=���    ����            �L�;L�ͽ���    ���ͽ���=���    ����=���                ���;L��        >L�ͽ���    =���=���                        ���;L�ͽ���    �L��        =���=���>L��>���>���>���?   ?��?��?333?L��?�  ?fff?�  ?���?���?�ff?�  ?�33?�  ?�  ?ٙ�?ٙ�?�ff?�33@   @   @ff@��@33@��@   @&ff@333@9��@Fff@L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@ə�@�  @�ff@ٙ�@���@�33@�ff@���@�  @�ff@���@���A��A33AffA  A33A��A  A��A33AffA��A33A��AffA!��A#33A&ffA)��A+33A,��A0  A1��A333A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAQ��AS33AVffAX  AY��A[33A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���Ař�A�ffA�33A���Aə�A�ffA�33A�  A���A�ffA�33A�  A���Aљ�A�ffA�  A���Aՙ�A�ffA�33A�  A���A�ffA�33A�  Dq��Dq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt��Dt��Du  DufDu�Du3Du  @   @&ff@333@9��@Fff@L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@ə�@�  @�ff@ٙ�@���@�33@�ff@���@�  @�ff@���@���A��A33AffA  A33A��A  A��A33AffA��A33A��AffA!��A#33A&ffA)��A+33A,��A0  A1��A333A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAQ��AS33AVffAX  AY��A[33A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAq��As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���Ař�A�ffA�33A���Aə�A�ffA�33A�  A���A�ffA�33A�  A���Aљ�A�ffA�  A���Aՙ�A�ffA�33A�  A���A�ffA�33A�  Dq��Dq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt��Dt��Du  DufDu�Du3Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AŸRAŸRAź^AŸRAŸRAź^AŸRAżjA���AžwAžwA�A�A�A�A�ĜAżjA�|�A��Aĉ7A��/A���A��jA�G�A��HA���A�-A��RA�$�A�~�A�A�Q�A��A��jA�|�A��RA���A��PA�jA���A�z�A�9XA���A��A�;dA���A�A�A��A�ffA�S�A�A�bNA��A���A�A�A��TA���A�\)A�5?A��A�{A�VA��A�33A�ȴA��7A��TA��A��7A��wA�K�A�JA���A��A��TA��RA� �A��PA�5?A��A�ƨA��FA�v�A���A�  A�oA��`A��PA�l�A�5?A��A���A��uA��A�  A�|�A�jA�1A��jA��-A���A�hsA��wA��A�$�A��FA��A�+A�%A���A��\A�7LA�~�A�?}A���A�1A���A���A��A}t�A|I�Azn�Au|�An^5AlffAk"�AjjAhjAfjAc�wAa�;Aa��Aa�A`E�A]��A[�mA[|�AZ��AZ=qAY�mAX~�AU��AS��AP��AN�HAM�wAL�`AJ�DAJE�AJ1AI`BAH��AH~�AF�AD�/ACO�AB�!AB$�A@�A>�A>-A=x�A<n�A:�A9p�A8��A8�!A77LA6VA4VA1�A0�A0�A/S�A-�A*�A)XA(n�A'�A&ĜA%��A%G�A$~�A"�9A��AM�A1A�hA�AȴA�A{A|�AbA��A1'A`BA�\A�-AȴA�FA�A��A �Ap�A�jA��A��A\)A�`A�A��Al�AoA�jAVAbA��A
�A	�A�+AZA7LA��A�+At�A^5A%A A�@�;d@���@�"�@��u@�@�/@���@�`B@�bN@���@�33@�v�@���@���@�@��@��/@��@�hs@�A�@�S�@݉7@��@��@�E�@��@��@�r�@��m@��y@Ձ@�C�@���@���@͑h@ʏ\@�7L@�Ĝ@�Q�@��;@���@��@�@��@��y@��H@�@�=q@���@��7@��@��j@�j@�
=@���@�/@�V@���@� �@�+@��R@��@���@�x�@�%@�9X@��@��@��@�7L@��@���@���@�Z@�9X@��@��w@�dZ@�o@��@���@�n�@�M�@��@��9@�b@�;d@���@��7@��w@��@�ff@���@�7L@��9@�  @�l�@�33@�
=@��@���@�^5@�=q@���@�p�@�G�@�&�@���@���@��@�"�@���@�M�@�`B@���@��@�bN@� �@�1@�dZ@��@�ȴ@�^5@�@�p�@���@�M�@�dZ@�S�@�n�@��u@� �@��@�b@��@��w@�|�@�l�@�l�@�S�@�C�@���@��H@�33@�\)@��R@�~�@�M�@��@�-@�$�@�$�@�@���@��D@�r�@�bN@���@�/@���@�hs@�7L@�&�@��@���@�z�@�j@���@���@��u@��!@�$�@�5?@��T@��9@��9@��#@���@��j@��@�@���@�ff@�5?@���@��h@�?}@�V@��@��@��@��9@��@�r�@��@�z�@�1'@��
@�\)@���@���@��!@��\@�ff@�V@�^5@���@��!@�~�@�=q@�E�@�$�@��@�{@��@���@�/@��j@���@���@�33@�
=@���@���@�~�@�V@���@�X@���@��7@��`@���@�S�@�C�@��@���@��+@�V@�5?@���@�@��^@�@��-@��h@��h@��h@��@�j@��u@��D@��@���@��@�33@�o@��@��H@��H@���@���@���@x��@p�j@h,=@_g�@W�Q@P֡@J�h@C(@<�@7��@1��@,��@%��@ ?�@a|@��@�@{J@e�@Z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�A�r�A�
=A�;dA��A��/A�VA�S�A�?}A�ĜA�ZAŁAŅA��A�A�A��
A�t�A�I�A�G�A���A���A�x�A�ffA�9XA��`A��/A���A�|�A��A���A�`BA��
A�+A��yA�x�A�jA�v�A��A�I�A�ZA���A�ƨA�S�A�n�A�bA��!A��AĲ-AŋDAŅA��#A��A�=qA�$�A��A�  A�ZA��^A�M�Ać+A�5?A���A��;A��A�^5A�jA��A¾wA�;dA�&�A���A���A�=qA��Aĝ�A�ffAüjA��A��RA���A�jA¥�A��!A��A���Aĥ�A��
A�;dA�dZA�`BA�n�A���A��A���A�A���A¡�A�v�A�;dA�^5A�(�A���A�z�A�bA��A��mA��PA��PA�dZA�"�A�hsA�l�A��A�E�A���A���A�hsA�{A�ffA�z�A�=qA��wA��`A�M�A�~�A�~�A�A�z�A�7LA���A�z�A�K�A���A�r�A�ƨA�\)A®A�VA��A��TA�7LA��;Aŕ�Aŕ�A��#A��A��;AŅA�v�A��Aò-Aŕ�Aŉ7A��AœuAŅA�XAę�A�M�A�oA��9A��TAÍPA�Q�A��;A�XA�ffA�z�A��uA���Aġ�AŋDA�A�A��/A�XA�XA��A��A�ȴA�p�A���A���A�%A��AŅA���A�&�A�&�A�M�AÃA�&�A���AāAōPAŋDA�bNA�;dA�G�AŋDAŉ7A��;A�hsA�`BAŋDA�t�Aŉ7A�VA��DA�{Aŉ7AōPAŋDA��
AōPAÙ�A�ȴA�XA�r�AŇ+AŋDA�+Aŉ7Aŏ\A�XAōPAœuAōPA�|�A�z�Aŉ7AŁA�t�AŅA�z�AŋDAŇ+AŋDAŋDAŉ7Aŏ\AōPAŏ\Aŏ\Aŏ\AÇ+A��TA�~�A�v�AŋDAŃAŋDAŇ+Aŉ7AŅA�x�Aŉ7AőhAŗ�Aŉ7AŅAŋDAŏ\AōPAŋDAŇ+AŇ+Aŉ7AœuAœuAš�Aŗ�Aŏ\A�z�AŁAőhA�~�A�~�AŋDAœuAœuAŏ\Aŉ7AōPA��HAŇ+A�~�AŇ+AŇ+A�dZA�z�Aŝ�AőhAŗ�Aŝ�Aŉ7AœuAŗ�AŃAōPAŁAŏ\Aŉ7AŅAőhAŋDAŁAōPAš�Aş�AőhAŝ�Aś�AōPAŅAř�Aţ�Aş�Aş�Aŝ�Aş�Aš�Aš�Aş�Aţ�Aš�Aš�Aš�Aţ�Aţ�Aş�Aţ�Ať�Aţ�Aš�Ať�Aš�Aş�Aŧ�Aŧ�Ať�Aŝ�A�VAũ�Aţ�Ať�Aş�Aŗ�Aš�Ať�Aţ�Aţ�Aţ�Ať�Ať�Ať�Aš�Ať�Ať�Aş�A��`Aũ�Aş�Ať�Aś�A�\)Aŕ�Aş�AŬAũ�AŬAś�AŬAŮAŬAţ�Aũ�Aß�AŰ!AŰ!Aũ�AŲ-AŰ!AŬAŬAŮAŬAũ�Ať�AŮAŰ!Aŧ�Aţ�Aũ�Ať�Aş�Aŝ�Aš�Aŧ�Ať�Aţ�Aţ�Aũ�Aş�Ať�AŬAũ�Ať�Aŧ�Ať�Ať�Aŧ�Aš�Aš�Ať�Aţ�Aũ�AŬAŧ�AŬAŰ!AŰ!Aŧ�AŰ!AŮAť�Aũ�Ať�A�O�A�"�Aũ�AŲ-AŬAŬAŬAũ�Ať�AŬAŬAŧ�Ať�Aŧ�Aŧ�Ať�Ať�AŃAŬAŧ�Ať�Aŧ�Aţ�AŮAũ�Aś�A�K�Aũ�A�^5AŴ9AŮAŬAŬAŧ�AŬAŴ9AŬAŮAũ�AŰ!AŮAţ�Aŝ�Aš�Aş�A�bNAŗ�Aš�Aś�Aũ�Aş�Aŗ�Ař�Aţ�Aś�Aŗ�Ař�Aŕ�Aŗ�Aŝ�Aŝ�Aś�Aŕ�Aŏ\AőhAš�Aş�Aş�Aŝ�Aš�Aš�Aţ�Ař�Aŉ7AŇ+A�1'Aş�Aŝ�Aş�Aş�Aŝ�Aš�AŴ9Aš�AŸRAżjAź^Aź^AŶFAŶFAź^Aź^AŶFAŶFAŶFAź^AŸRAŴ9AŴ9AŸRAŴ9AŴ9AŲ-AŶFAŴ9AŴ9AŸRAŸRAŴ9AŸRAŶFAŶFAŸRAŶFAŸRAŶFAŸRAŸRAŸRAź^Aź^AżjAŸRAżjAź^Aź^AŸRAŸRAź^Aź^Aź^Aź^AżjAź^Aź^Aź^AżjAżjAź^AŸRAź^Aź^AżjAŸRAź^AżjAżjAżjAŶFAŶFAŶFAŸRAŸRAŶFAŶFAŶFAŶFAŶFAŶFAŶFAŶFAŶFAŸRAŶFAź^AŶFAżjAź^Aź^AżjAź^Aź^Aź^AŸRAŸRAź^Aź^AŸRAŸRAź^Aź^Aź^AŸRAŸRAŸRAŴ9AŴ9AŰ!AŶFAŶFAŶFAŸRAŸRAŶFAŸRAŸRAź^Aź^Aź^Aź^Aź^Aź^AŸRAź^AŸRAź^AŸRAź^AżjAżjAżjAžwAžwAžwAžwA���AžwA���A���AžwAžwAžwAžwA���AžwAžwAžwA���AžwAžwA���AžwAžwA���A���A���AžwAžwAżjAžwAžwAżjAžwAżjAżjAżjAžwAžwAżjAżjAź^AżjAżjAžwAžwAžwAżjAżjAżjAžwA���AžwAžwA���AżjAžwAžwAžwAžwAżjAžwAžwA���AžwA���A�AžwA���A���A�ĜA�A���A�A�A�A���A���A�A�A�A���A�A���A�A���A�A�A���A�ĜA�A�A�A�ĜA�A�ĜA�A�A�A�A�A�A�A�A�@�
=@�
=@�
=@���@��@��H@��H@��H@��y@�
=@��@�33@�33@�+@�C�@�K�@�\)@�\)@�S�@�;d@�;d@�+@�+@�"�@�"�@�"�@�"�@��@�o@�o@�o@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�@�@�@�@�@�@�@�@���@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��@��H@��H@��y@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��y@��y@��y@��H@��y@��y@��y@��y@��y@��H@��H@��H@��H@��@���@���@���@���@���@�ȴ@���@���@���@���@���@���@��R@��R@��R@��R@���@���@���@��R@��R@��R@��R@���@���@���@���@��+@�~�@�n�@�n�AŸRAŶFAŸRAź^AŸRAŸRAŸRAŸRAŸRAżjAź^AŸRAŸRAŸRAŸRAź^Aź^Aź^AŸRAź^AżjAź^Aź^AżjAżjAżjAżjAź^Aź^Aź^Aź^Aź^AŸRAź^AżjAżjAŸRAŶFAŶFAŶFAŶFAŸRAź^AŸRAŶFAŸRAŶFAŶFAŶFAŶFAŶFAŸRAŶFAŸRAŶFAź^AżjAżjAżjAżjAŸRAŸRAź^AŸRAŸRAź^Aź^Aź^Aź^Aź^Aź^Aź^Aź^AŸRAŸRAŶFAŶFAŶFAŶFAŶFAŸRAŸRAŸRAŸRAŸRAź^Aź^AżjAź^AŸRAź^AżjAź^Aź^Aź^AżjAź^Aź^AżjA���AžwA���AžwAżjA���A���A���AžwA���A���AžwAžwA���A���A���A���A���AžwA���AžwA���AžwA���A���A���A���A���A���AžwAžwAżjAżjAżjAżjAžwAžwAżjAżjAžwAżjAżjAžwAżjAžwAžwAžwAżjAžwAžwA���A���AžwAžwA���AžwAžwA���A���AžwA���A���A���A���A�A���A�A�A���A���A�A�A�A�A�A�A���A�A�A�A���A�A�A���A�A�A�A�ĜA�A�A�A�A�A�A�ĜA�ĜA�A�A�A�ĜA�ĜA�A�A�Ĝ@�
=@�
=@�
=@�@��@��y@��H@��@��y@�@�o@�;d@�;d@�33@�C�@�K�@�\)@�\)@�S�@�K�@�;d@�;d@�+@�+@�+@�"�@�"�@��@�o@�o@�o@�o@�o@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�@�@�@�
=@�@�@�@�@�@��y@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��y@��H@��H@��H@��H@��H@��y@��H@��H@��H@��H@��H@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��H@��H@��H@��H@���@���@��@���@���@���@�ȴ@���@���@���@���@���@���@���@���@��R@��R@���@���@���@��R@��R@��R@��R@���@���@���@��\@�~�@�v�@�v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 AŸRAŸRAź^AŸRAŸRAź^AŸRAżjA���AžwAžwA�A�A�A�A�ĜAżjA�|�A��Aĉ7A��/A���A��jA�G�A��HA���A�-A��RA�$�A�~�A�A�Q�A��A��jA�|�A��RA���A��PA�jA���A�z�A�9XA���A��A�;dA���A�A�A��A�ffA�S�A�A�bNA��A���A�A�A��TA���A�\)A�5?A��A�{A�VA��A�33A�ȴA��7A��TA��A��7A��wA�K�A�JA���A��A��TA��RA� �A��PA�5?A��A�ƨA��FA�v�A���A�  A�oA��`A��PA�l�A�5?A��A���A��uA��A�  A�|�A�jA�1A��jA��-A���A�hsA��wA��A�$�A��FA��A�+A�%A���A��\A�7LA�~�A�?}A���A�1A���A���A��A}t�A|I�Azn�Au|�An^5AlffAk"�AjjAhjAfjAc�wAa�;Aa��Aa�A`E�A]��A[�mA[|�AZ��AZ=qAY�mAX~�AU��AS��AP��AN�HAM�wAL�`AJ�DAJE�AJ1AI`BAH��AH~�AF�AD�/ACO�AB�!AB$�A@�A>�A>-A=x�A<n�A:�A9p�A8��A8�!A77LA6VA4VA1�A0�A0�A/S�A-�A*�A)XA(n�A'�A&ĜA%��A%G�A$~�A"�9A��AM�A1A�hA�AȴA�A{A|�AbA��A1'A`BA�\A�-AȴA�FA�A��A �Ap�A�jA��A��A\)A�`A�A��Al�AoA�jAVAbA��A
�A	�A�+AZA7LA��A�+At�A^5A%A A�@�;d@���@�"�@��u@�@�/@���@�`B@�bN@���@�33@�v�@���@���@�@��@��/@��@�hs@�A�@�S�@݉7@��@��@�E�@��@��@�r�@��m@��y@Ձ@�C�@���@���@͑h@ʏ\@�7L@�Ĝ@�Q�@��;@���@��@�@��@��y@��H@�@�=q@���@��7@��@��j@�j@�
=@���@�/@�V@���@� �@�+@��R@��@���@�x�@�%@�9X@��@��@��@�7L@��@���@���@�Z@�9X@��@��w@�dZ@�o@��@���@�n�@�M�@��@��9@�b@�;d@���@��7@��w@��@�ff@���@�7L@��9@�  @�l�@�33@�
=@��@���@�^5@�=q@���@�p�@�G�@�&�@���@���@��@�"�@���@�M�@�`B@���@��@�bN@� �@�1@�dZ@��@�ȴ@�^5@�@�p�@���@�M�@�dZ@�S�@�n�@��u@� �@��@�b@��@��w@�|�@�l�@�l�@�S�@�C�@���@��H@�33@�\)@��R@�~�@�M�@��@�-@�$�@�$�@�@���@��D@�r�@�bN@���@�/@���@�hs@�7L@�&�@��@���@�z�@�j@���@���@��u@��!@�$�@�5?@��T@��9@��9@��#@���@��j@��@�@���@�ff@�5?@���@��h@�?}@�V@��@��@��@��9@��@�r�@��@�z�@�1'@��
@�\)@���@���@��!@��\@�ff@�V@�^5@���@��!@�~�@�=q@�E�@�$�@��@�{@��@���@�/@��j@���@���@�33@�
=@���@���@�~�@�V@���@�X@���@��7@��`@���@�S�@�C�@��@���@��+@�V@�5?@���@�@��^@�@��-@��h@��h@��h@��@�j@��u@��D@��@���@��@�33@�o@��@��H@��H@���G�O�@���@x��@p�j@h,=@_g�@W�Q@P֡@J�h@C(@<�@7��@1��@,��@%��@ ?�@a|@��@�@{J@e�@Z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�A�r�A�
=A�;dA��A��/A�VA�S�A�?}A�ĜA�ZAŁAŅA��A�A�A��
A�t�A�I�A�G�A���A���A�x�A�ffA�9XA��`A��/A���A�|�A��A���A�`BA��
A�+A��yA�x�A�jA�v�A��A�I�A�ZA���A�ƨA�S�A�n�A�bA��!A��AĲ-AŋDAŅA��#A��A�=qA�$�A��A�  A�ZA��^A�M�Ać+A�5?A���A��;A��A�^5A�jA��A¾wA�;dA�&�A���A���A�=qA��Aĝ�A�ffAüjA��A��RA���A�jA¥�A��!A��A���Aĥ�A��
A�;dA�dZA�`BA�n�A���A��A���A�A���A¡�A�v�A�;dA�^5A�(�A���A�z�A�bA��A��mA��PA��PA�dZA�"�A�hsA�l�A��A�E�A���A���A�hsA�{A�ffA�z�A�=qA��wA��`A�M�A�~�A�~�A�A�z�A�7LA���A�z�A�K�A���A�r�A�ƨA�\)A®A�VA��A��TA�7LA��;Aŕ�Aŕ�A��#A��A��;AŅA�v�A��Aò-Aŕ�Aŉ7A��AœuAŅA�XAę�A�M�A�oA��9A��TAÍPA�Q�A��;A�XA�ffA�z�A��uA���Aġ�AŋDA�A�A��/A�XA�XA��A��A�ȴA�p�A���A���A�%A��AŅA���A�&�A�&�A�M�AÃA�&�A���AāAōPAŋDA�bNA�;dA�G�AŋDAŉ7A��;A�hsA�`BAŋDA�t�Aŉ7A�VA��DA�{Aŉ7AōPAŋDA��
AōPAÙ�A�ȴA�XA�r�AŇ+AŋDA�+Aŉ7Aŏ\A�XAōPAœuAōPA�|�A�z�Aŉ7AŁA�t�AŅA�z�AŋDAŇ+AŋDAŋDAŉ7Aŏ\AōPAŏ\Aŏ\Aŏ\AÇ+A��TA�~�A�v�AŋDAŃAŋDAŇ+Aŉ7AŅA�x�Aŉ7AőhAŗ�Aŉ7AŅAŋDAŏ\AōPAŋDAŇ+AŇ+Aŉ7AœuAœuAš�Aŗ�Aŏ\A�z�AŁAőhA�~�A�~�AŋDAœuAœuAŏ\Aŉ7AōPA��HAŇ+A�~�AŇ+AŇ+A�dZA�z�Aŝ�AőhAŗ�Aŝ�Aŉ7AœuAŗ�AŃAōPAŁAŏ\Aŉ7AŅAőhAŋDAŁAōPAš�Aş�AőhAŝ�Aś�AōPAŅAř�Aţ�Aş�Aş�Aŝ�Aş�Aš�Aš�Aş�Aţ�Aš�Aš�Aš�Aţ�Aţ�Aş�Aţ�Ať�Aţ�Aš�Ať�Aš�Aş�Aŧ�Aŧ�Ať�Aŝ�A�VAũ�Aţ�Ať�Aş�Aŗ�Aš�Ať�Aţ�Aţ�Aţ�Ať�Ať�Ať�Aš�Ať�Ať�Aş�A��`Aũ�Aş�Ať�Aś�A�\)Aŕ�Aş�AŬAũ�AŬAś�AŬAŮAŬAţ�Aũ�Aß�AŰ!AŰ!Aũ�AŲ-AŰ!AŬAŬAŮAŬAũ�Ať�AŮAŰ!Aŧ�Aţ�Aũ�Ať�Aş�Aŝ�Aš�Aŧ�Ať�Aţ�Aţ�Aũ�Aş�Ať�AŬAũ�Ať�Aŧ�Ať�Ať�Aŧ�Aš�Aš�Ať�Aţ�Aũ�AŬAŧ�AŬAŰ!AŰ!Aŧ�AŰ!AŮAť�Aũ�Ať�A�O�A�"�Aũ�AŲ-AŬAŬAŬAũ�Ať�AŬAŬAŧ�Ať�Aŧ�Aŧ�Ať�Ať�AŃAŬAŧ�Ať�Aŧ�Aţ�AŮAũ�Aś�A�K�Aũ�A�^5AŴ9AŮAŬAŬAŧ�AŬAŴ9AŬAŮAũ�AŰ!AŮAţ�Aŝ�Aš�Aş�A�bNAŗ�Aš�Aś�Aũ�Aş�Aŗ�Ař�Aţ�Aś�Aŗ�Ař�Aŕ�Aŗ�Aŝ�Aŝ�Aś�Aŕ�Aŏ\AőhAš�Aş�Aş�Aŝ�Aš�Aš�Aţ�Ař�Aŉ7AŇ+A�1'Aş�Aŝ�Aş�Aş�Aŝ�Aš�AŴ9Aš�AŸRAżjAź^Aź^AŶFAŶFAź^Aź^AŶFAŶFAŶFAź^AŸRAŴ9AŴ9AŸRAŴ9AŴ9AŲ-AŶFAŴ9AŴ9AŸRAŸRAŴ9AŸRAŶFAŸRAŶFAŸRAź^AŸRAŸRAŸRAŸRAŸRAżjAź^AŸRAŸRAŸRAŸRAź^Aź^Aź^AŸRAź^AżjAź^Aź^AżjAżjAżjAżjAź^Aź^Aź^Aź^Aź^AŸRAź^AżjAżjAŸRAŶFAŶFAŶFAŶFAŸRAź^AŸRAŶFAŸRAŶFAŶFAŶFAŶFAŶFAŸRAŶFAŸRAŶFAź^AżjAżjAżjAżjAŸRAŸRAź^AŸRAŸRAź^Aź^Aź^Aź^Aź^Aź^Aź^Aź^AŸRAŸRAŶFAŶFAŶFAŶFAŶFAŸRAŸRAŸRAŸRAŸRAź^Aź^AżjAź^AŸRAź^AżjAź^Aź^Aź^AżjAź^Aź^AżjA���AžwA���AžwAżjA���A���A���AžwA���A���AžwAžwA���A���A���A���A���AžwA���AžwA���AžwA���A���A���A���A���A���AžwAžwAżjAżjAżjAżjAžwAžwAżjAżjAžwAżjAżjAžwAżjAžwAžwAžwAżjAžwAžwA���A���AžwAžwA���AžwAžwA���A���AžwA���A���A���A���A�A���A�A�A���A���A�A�A�A�A�A�A���A�A�A�A���A�A�A���A�A�A�A�ĜA�A�A�A�A�A�A�ĜA�ĜA�A�A�A�ĜA�ĜA�A�A�Ĝ@�
=@�
=@�
=@�@��@��y@��H@��@��y@�@�o@�;d@�;d@�33@�C�@�K�@�\)@�\)@�S�@�K�@�;d@�;d@�+@�+@�+@�"�@�"�@��@�o@�o@�o@�o@�o@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�@�@�@�
=@�@�@�@�@�@��y@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��y@��H@��H@��H@��H@��H@��y@��H@��H@��H@��H@��H@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��H@��H@��H@��H@���@���@��@���@���@���@�ȴ@���@���@���@���@���@���@���@���@��R@��R@���@���@���@��R@��R@��R@��R@���@���@���@��\@�~�@�v�@�v�AŸRAŶFAŸRAź^AŸRAŸRAŸRAŸRAŸRAżjAź^AŸRAŸRAŸRAŸRAź^Aź^Aź^AŸRAź^AżjAź^Aź^AżjAżjAżjAżjAź^Aź^Aź^Aź^Aź^AŸRAź^AżjAżjAŸRAŶFAŶFAŶFAŶFAŸRAź^AŸRAŶFAŸRAŶFAŶFAŶFAŶFAŶFAŸRAŶFAŸRAŶFAź^AżjAżjAżjAżjAŸRAŸRAź^AŸRAŸRAź^Aź^Aź^Aź^Aź^Aź^Aź^Aź^AŸRAŸRAŶFAŶFAŶFAŶFAŶFAŸRAŸRAŸRAŸRAŸRAź^Aź^AżjAź^AŸRAź^AżjAź^Aź^Aź^AżjAź^Aź^AżjA���AžwA���AžwAżjA���A���A���AžwA���A���AžwAžwA���A���A���A���A���AžwA���AžwA���AžwA���A���A���A���A���A���AžwAžwAżjAżjAżjAżjAžwAžwAżjAżjAžwAżjAżjAžwAżjAžwAžwAžwAżjAžwAžwA���A���AžwAžwA���AžwAžwA���A���AžwA���A���A���A���A�A���A�A�A���A���A�A�A�A�A�A�A���A�A�A�A���A�A�A���A�A�A�A�ĜA�A�A�A�A�A�A�ĜA�ĜA�A�A�A�ĜA�ĜA�A�A�Ĝ@�
=@�
=@�
=@�@��@��y@��H@��@��y@�@�o@�;d@�;d@�33@�C�@�K�@�\)@�\)@�S�@�K�@�;d@�;d@�+@�+@�+@�"�@�"�@��@�o@�o@�o@�o@�o@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�@�@�@�
=@�@�@�@�@�@��y@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��H@��y@��H@��H@��H@��H@��H@��y@��H@��H@��H@��H@��H@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��H@��H@��H@��H@���@���@��@���@���@���@�ȴ@���@���@���@���@���@���@���@���@��R@��R@���@���@���@��R@��R@��R@��R@���@���@���@��\@�~�@�v�@�v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@M�E=�<6?p<K=��>'�>Y�#=���>6z?9�^=�7?�@��h@��1@9{�>N�@��=�	�?��0>_�?u�>Kۡ@���@��\>	p�@U��=�X>�D�@���@���>:��@��@J��=�/�>=D@�ݭ@��@��z>(�U@�A�@�ܜ@QBp>0r@��}@��8?���> �>&A�?��1@��@���?;+V>R ~@���=���>.Κ@��=�BF=��>1t@��?=�_�=�\�>�#y@*��?��b=�W�=�4/?�r�@|a�@�">�^�@��=΅4>>ʬ@P�>؄@�ۡ=���=���>At?@��e?!�=�5T>��^@gޔ@��>�T=���>�X@��+@��">�\h>� @%�@�V�>m%�>|��@���@��j=�r>l�@51�@��Y@�>�>w`�>e#�@s�&>�2#=�Ǐ>#0>�@���>ʿ4>���?�}@��=��u=��z?׿�@��8@��s=�SP>y?��i@_�Q?�=I<�=R�=�If=��=��=�|>g�n@��@���=��@>�D|@���=ӣ�?�;�=�Q�>_�j@��@���@1V�>	?�J�@��=� �?_X�?�&@��@��>'�@��\@��@��%@��%>Ec=���=�>�s?�c�@��=�R�>]y@ay�@��=���>�]@V��@��-@�LY=�v@Q�7@��?@�T�>8a�>�ؙ@��?�n=���>��@�_�@���>W)�@3c@��\@���@���?�'g? �b?ʯ�@��o@��@�}k>�!>��@���@��@7(@�o�@.A5@���?3�@��c@���>'��@��@��|@���@���@���@��@�ł?�e@��X@��@��6@��|@��%@��@��@���@��@���@��@���@��@��o@��R@��@���@��J@��o@���@���@��@��@��(@��@���@���@��|@��.@�L@��@��o@��@��V@���@��@��@��@��@��@���@��E@��@��@���@���@��|@��8@���@��|@��@��8@���@��@���@��@��@��(@��|@��_@���@���@��E@��4@��E@��@���@��@���@��@��@��@��>@��(@��U@���@���@���@���@��E@��@��@��|@��@��@��8@��@���@��8@��J@���@���@��@��Q@��w@��Q@��8@���@���@��Q@��	@���@���@��r@���@��	@��	@���@���@���@���@���@���@���@��@��r@��@���@���@��@���@��+@���@��@��@���@���@���@��@���@��	@���@���@���@��+@��@��;@���@���@���@���@���@��r?|�@��@��+@���@��	?ߔ�@���@��	@��L@���@���@��;@��L@��H@���@��a@��L@v�@���@���@���@���@���@��H@���@��H@���@���@��L@��	@��H@���@���@���@���@���@���@���@���@���@���@���@��;@���@���@���@���@���@���@��@���@��;@��+@���@���@��;@��L@���@���@���@���@���@���@���@���@���@��@��+@��;@`�A@���@��H@��L@���@��L@��;@��@���@��;@��;@��;@���@���@���@��@���@���@��;@��@���@���@���@��L@���@�")@���>P��@� i@���@��H@�  @���@��H@�  @���@��H@��X@��	@��+@��+@���@���@��Q@��_@��@���@���@��+@��+@���@���@���@���@���@��E@���@���@��w@���@��	@��4@��@��r@���@��+@���@��@��@��@��@��@��@���>|�8@���@���@��+@��@���@��r@�@��@�@��@�u@�u@��@�!@��@��@��@��@�!@��@�@�e@�e@�@��@��@��@�!@�2@��@�2@��@�2@�G@�2@�2@�2@��@��@��@��@��@��@��@�C@��@��@��@��@��@��@��@��@��@��@��@�S@��@�d@��@�S@��@��@�S@��@��@��@�@�d@�d@�d@��@��@��@�S@�@�h@�@��@��@�h@��@��@�@�@�d@�d@�d@��@�t@�t@��@�t@��@��@�t@�t@�!@�t@�t@�t@�t@��@�t@��@��@�t@�t@�!@��@��@��@�!@��@��@��@��@��@��@��@�	B@�	�@�	�@�	�@�	�@�	B@�	�@�	�@�	�@�	�@�
R@�
R@�
�@�c@��@��@�c@��@�@�_@�@�@�_@�@�@�@�_@�_@�@��@��@��@�t@��@��@��@��@��@��@�t@��@��@��@��@�t@��@��@� @��@�t@��@��@��@�0@�0@�@��@��@��@�0@�0@��@��@��@�,@�,@�,@��@��@��@��@��@��@��@��@�=@�R@��@��@��@��@��@��@��@��@�M@��@�M@��@��@�
@��@��@�
@�^@�I@�^@�^@��@��@�@��@��@�@�@�@�@�@�@��@�o@��@��@�+@��@�@�@�@P��@P��@P�s@P�N@P�(@P��@P�R@P��@P��@P�z@P�6@P��@P��@P��@P��@P�B@P��@P��@P��@P�S@P��@P��@P�\@P��@P��@P��@P�6@P��@P�;@P��@P��@P��@P�?@P�?@P��@P��@P�@P��@P�?@P�i@P�?@P�?@P�?@P��@P��@P��@P��@P�H@P�Q@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�L@P��@P�H@P��@P��@P��@P�H@P�H@P��@P��@P�H@P�H@P��@P��@P��@P��@P�D@P�D@P�D@P�D@P�D@P��@P��@P��@P�D@P��@P��@P��@P��@P��@P��@P��@P�Q@P��@P��@P�@P�Y@P�@P�@P��@P�@P��@P�^@P�^@P��@P�^@P�^@P�
@P��@P�@P��@P��@P�@P��@P�|@P�1@P��@P��@P��@P��@�7�@�7L@�7"@�8q@�8q@�8G@�8G@�8q@�8�@�9C@�9X@�8�@�8�@�8�@�8�@�9C@�9�@�9.@�9C@�9�@�:@�9�@�:@�:@�:�@�:�@�:�@�:�@�:@�:T@�:T@�:~@�: @�:~@�:�@�;@�:@�9C@�9m@�9.@�9C@�:T@�:?@�:*@�:*@�9�@�9�@�9�@�9�@�:T@�:*@�:?@�:~@�:T@�:~@�;�@�<6@�<6@�<@�<6@�;�@�;y@�<@�;�@�;�@�<@�;�@�<@�<!@�<K@�<`@�<6@�<@�;�@�<@�;O@�:�@�;:@�;:@�;:@�;�@�<�@�<`@�<`@�<�@�=\@�=2@�=@�=�@�=@�=�@�>@�=�@�=�@�=q@�>@�=G@�>@�>-@�?�@�?�@�?�@�?�@�?>@�@@�@%@�@%@�@%@�>�@�?�@�@@�@:@�@y@�@y@�@y@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A @�@�@�@�@�@�@�AJ@�@�@�@�@�@%@�@d@�@d@�@:@�@:@�@y@�@y@�@�@�@O@�@d@�@y@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A_@�At@�B1@�A�@�A�@�A�@�A�@�A�@�BF@�Bp@�A�@�B�@�BF@�C@�C@�CB@�C@�Cl@�CB@�Cl@�Cl@�C�@�C�@�C�@�D@�D@�D@�C�@�C�@�C�@�D@�D@�D=@�D=@�DR@�DR@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�E@�E@�E9@�EN@�Ex@�Ex@�E9@�Ex@P�!@P�u@P��@P��@Pʬ@P�`@P�e@Pƽ@P�@P�u@P҉@Pی@P�8@Pڐ@Pݭ@P�z@P�@P�@P�?@P�@P�P@P��@P�Y@Pܱ@P�3@P�]@P��@P�8@P�<@P�<@P��@Pپ@Pٔ@P�@@P��@P��@P��@P��@P�@P��@P�@P��@P��@P�@P��@P��@P�@P�@@P�@P��@P�V@PԀ@P�V@PԪ@PԪ@P��@P��@P��@P�(@P��@P�(@P��@P�R@P��@P�$@P�M@P�$@P�M@P��@P�w@P��@P��@P��@P��@P��@P�s@Pם@P��@P��@P��@P��@P�@P�@P��@P��@P��@Pם@Pם@P�I@P�$@P�R@Pզ@P�|@Pզ@P�(@Pӄ@P�1@P��@P�@P�@P�@P�@P��@Pҳ@P�@Pҳ@P�1@P�[@P�1@P҉@P�_@P�@P҉@PΚ@P�p@P�@Pʬ@Pȴ@P��@P�iG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             344444444443344344444334344334344433343334334444334434434443444444443343444434443444334443344434433444334434444344434443344434444444433443444433444344433433334444434433443334333443444334433344433344334343433433333334333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333343333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@M�FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��j@��2G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�@���@��[G�O�@U��G�O�G�O�@���@���G�O�@��
G�O�G�O�G�O�@�ݮ@��@��zG�O�@�A�@�ܟ@QBpG�O�@��@��<G�O�G�O�G�O�G�O�@�� @���G�O�G�O�@���G�O�G�O�@��G�O�G�O�G�O�@��CG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@|a�@�"G�O�@��G�O�G�O�G�O�G�O�@�ۡG�O�G�O�G�O�@��dG�O�G�O�G�O�@gޕ@��G�O�G�O�G�O�@��.@��"G�O�G�O�G�O�@�V�G�O�G�O�@���@��nG�O�G�O�G�O�@��Z@�>�G�O�G�O�@s�%G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�@��G�O�G�O�G�O�@��3@��qG�O�G�O�G�O�@_�UG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@���G�O�G�O�@���G�O�G�O�G�O�G�O�@��@���G�O�G�O�G�O�@��G�O�G�O�G�O�@��@��G�O�@��_@��@��(@��'G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�@ay�@��}G�O�G�O�@V��@��/@�LYG�O�@Q�:@��B@�T�G�O�G�O�@��G�O�G�O�G�O�@�_�@���G�O�G�O�@��_@���@���G�O�G�O�G�O�@��u@��@�}jG�O�G�O�@���@��G�O�@�o�G�O�@���G�O�@��d@���G�O�@��@��|@���@���@���@��@�ňG�O�@��X@��@��6@��@��"@��@��}@���@��@���@��@���@��@��r@��S@��@��@��M@��q@���@���@��@��@��*@��@���@���@��{@��.G�O�@��@��r@��@��X@���@��@��@��@��@��@���@��B@��@��@���@���@��|@��=@���@��}@��@��9@���@��@���@��@��@��,@��{@��b@���@���@��F@��4@��H@��@���@��@���@��@��@��@��>@��*@��W@���@���@���@���@��F@��
@��@��{@��@��@��:@��@���@��=@��J@���@���@��@��N@��w@��P@��9@���@���@��V@��
@���@���@��t@���@��@��
@���@���@���@���@���@���@���@��@��r@���@���@���@��@���@��.@���@��@��@���@���@���@��@���@��@���@���@���@��+@���@��>@���@���@���@���@���@��tG�O�@���@��+@���@��
G�O�@���@��@��L@���@���@��=@��I@��G@���@��^@��N@v�"@���@���@���@���@���@��J@���@��G@���@���@��L@��	@��F@���@���@���@���@���@���@���@���@���@���@���@��>@���@���@���@���@���@���@���@���@��=@��(@���@���@��;@��N@���@���@���@���@���@���@���@���@���@��@��,@��=@`�F@���@��K@��L@���@��L@��:@���@���@��;@��=@��>@���@���@���@���@���@���@��7@���@���@���@���@��K@���@�"*@���G�O�@� g@���@��F@� @���@��J@���@���@��G@��Y@��@��-@��+@���@���@��S@��a@��@���@���@��*@��.@���@���@���@���@���@��F@���@���@��w@���@��
@��2@��@��v@���@��.@���@��@��@��@��@��@��@���G�O�@���@���@��,@��@���@��s@�@��@�@��@�y@�w@��@�#@��@��@��@��@�@��@�@�f@�g@�@��@��@��@�#@�2@��@�/@��@�7@�H@�2@�7�@�7L@�7#@�8v@�8q@�8J@�8F@�8p@�8�@�9B@�9Y@�8�@�8�@�8�@�8�@�9C@�9�@�9/@�9F@�9�@�:@�9�@�:@�:@�:�@�:�@�:�@�:�@�:@�:W@�:U@�:�@�: @�:~@�:�@�;@�:@�9E@�9k@�9/@�9C@�:T@�:<@�:+@�:,@�9�@�9�@�9�@�9�@�:T@�:'@�:B@�:�@�:S@�:~@�;�@�<6@�<6@�<@�<6@�;�@�;y@�<@�;�@�;�@�<@�;�@�<@�<%@�<O@�<b@�<8@�<@�;�@�<@�;T@�:�@�;<@�;=@�;<@�;�@�<�@�<a@�<b@�<�@�=\@�=.@�=@�=�@�=@�=�@�>@�=�@�=�@�=r@�>@�=H@�>@�>*@�?�@�?�@�?�@�?�@�?>@�@@�@#@�@$@�@$@�>�@�?�@�@@�@;@�@y@�@y@�@y@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A@�@�@�@�@�@�@�AH@�@�@�@�@�@#@�@b@�@a@�@7@�@:@�@{@�@z@�@�@�@P@�@b@�@v@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A^@�Av@�B/@�A�@�A�@�A�@�A�@�A�@�BH@�Bq@�A�@�B�@�BD@�C@�C@�C>@�C@�Cl@�CB@�Cm@�Cn@�C�@�C�@�C�@�D@�D@�D@�D@�C�@�C�@�D@�D@�D>@�D;@�DR@�DU@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�E@�E@�E:@�EL@�Ev@�E{@�E6@�Ez@P�"@P�u@P��@P��@Pʮ@P�c@P�e@P��@P�
@P�s@P҆@Pۈ@P�=@Pڒ@Pݪ@P�}@P�@P�@P�@@P�@P�R@P� @P�Z@Pܰ@P�3@P�^@P��@P�8@P�:@P�8@P��@Pٽ@Pٖ@P�B@Pؾ@P��@P��@P��@P�@P��@P�@P��@P��@P�@P��@P��@P�@P�B@P�@P��@P�U@Pԃ@P�[@PԪ@Pԥ@P��@P��@P� @P�(@P��@P�(@P�@P�R@P��@P�#@P�M@P� @P�P@P��@P�x@P��@P��@P��@P��@P��@P�r@Pך@P��@P��@P��@P��@P�@P�@P��@P��@P��@Pם@Pם@P�K@P�#@P�P@Pը@P�}@Pը@P�(@Pӈ@P�.@P��@P�@P�@P�
@P�@P��@PҶ@P�
@Pҭ@P�0@P�`@P�0@PҊ@P�c@P�@PҊ@PΚ@P�r@P�@Pʭ@Pȸ@P��@P�f@�7�@�7L@�7#@�8v@�8q@�8J@�8F@�8p@�8�@�9B@�9Y@�8�@�8�@�8�@�8�@�9C@�9�@�9/@�9F@�9�@�:@�9�@�:@�:@�:�@�:�@�:�@�:�@�:@�:W@�:U@�:�@�: @�:~@�:�@�;@�:@�9E@�9k@�9/@�9C@�:T@�:<@�:+@�:,@�9�@�9�@�9�@�9�@�:T@�:'@�:B@�:�@�:S@�:~@�;�@�<6@�<6@�<@�<6@�;�@�;y@�<@�;�@�;�@�<@�;�@�<@�<%@�<O@�<b@�<8@�<@�;�@�<@�;T@�:�@�;<@�;=@�;<@�;�@�<�@�<a@�<b@�<�@�=\@�=.@�=@�=�@�=@�=�@�>@�=�@�=�@�=r@�>@�=H@�>@�>*@�?�@�?�@�?�@�?�@�?>@�@@�@#@�@$@�@$@�>�@�?�@�@@�@;@�@y@�@y@�@y@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A@�@�@�@�@�@�@�AH@�@�@�@�@�@#@�@b@�@a@�@7@�@:@�@{@�@z@�@�@�@P@�@b@�@v@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A^@�Av@�B/@�A�@�A�@�A�@�A�@�A�@�BH@�Bq@�A�@�B�@�BD@�C@�C@�C>@�C@�Cl@�CB@�Cm@�Cn@�C�@�C�@�C�@�D@�D@�D@�D@�C�@�C�@�D@�D@�D>@�D;@�DR@�DU@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�D�@�E@�E@�E:@�EL@�Ev@�E{@�E6@�Ez@P�"@P�u@P��@P��@Pʮ@P�c@P�e@P��@P�
@P�s@P҆@Pۈ@P�=@Pڒ@Pݪ@P�}@P�@P�@P�@@P�@P�R@P� @P�Z@Pܰ@P�3@P�^@P��@P�8@P�:@P�8@P��@Pٽ@Pٖ@P�B@Pؾ@P��@P��@P��@P�@P��@P�@P��@P��@P�@P��@P��@P�@P�B@P�@P��@P�U@Pԃ@P�[@PԪ@Pԥ@P��@P��@P� @P�(@P��@P�(@P�@P�R@P��@P�#@P�M@P� @P�P@P��@P�x@P��@P��@P��@P��@P��@P�r@Pך@P��@P��@P��@P��@P�@P�@P��@P��@P��@Pם@Pם@P�K@P�#@P�P@Pը@P�}@Pը@P�(@Pӈ@P�.@P��@P�@P�@P�
@P�@P��@PҶ@P�
@Pҭ@P�0@P�`@P�0@PҊ@P�c@P�@PҊ@PΚ@P�r@P�@Pʭ@Pȸ@P��@P�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             344444444443344344444334344334344433343334334444334434434443444444443343444434443444334443344434433444334434444344434443344434444444433443444433444344433433334444434433443334333443444334433344433344334343433433333334333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333343333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9���9��E9�� 9��U9��Q9��-9��)9��P9��g9��9��$9��f9��g9���9��d9��9���9���9��9��]9���9���9���9���9��D9���9���9��C9���9��9��
9��39���9��09���9���9���9��9��59���9��9��
9���9���9���9��r9���9���9���9��
9���9���9��39��	9��09��v9���9���9���9���9��Q9��9���9��u9��(9���9���9���9���9���9���9���9���9���9���9���9���9���9���9���9���9��9���9���9��!9���9���9��|9��09���9���9��d9���9���9���9��g9���9��g9���9���9��9��9��9���9��G9��W9��X9��X9��$9��9��H9��m9���9���9���9���9���9���9���9���9���9���9��9��=9��9���9��9��b9���9��9��W9���9���9��i9��l9���9���9���9���9���9���9��9��9��9��9���9���9��9��v9���9��59���9���9���9���9���9��L9��q9���9���9��H9���9��
9��,9���9��V9��09��W9��X9��9���9���9���9���9���9���9���9���9���9���9��9��9��(9��+9��f9���9���9���9���9���9���9���9���9���9���9���9���9���9��9��39��79���9��69>�,9>�w9>��9>�!9>��9>��9>��9>�[9>��9>�v9>�9>�P9>�9>�o9>�B9>��9>�59>��9>�[9>�h9>��9>�z9>��9>�^9>��9>�9>��9>�9>�9>�9>��9>��9>��9>�=9>��9>��9>��9>��9>�9>��9>�9>��9>��9>�9>��9>��9>�9>�=9>�/9>�9>��9>��9>��9>�9>�9>�X9>�S9>�Z9>�~9>�U9>�~9>�\9>��9>�?9>�c9>��9>�a9>��9>��9>��9>��9>��9>�&9>�$9>�$9>��9>��9>��9>��9>��9>�9>�/9>�19>�
9>��9>�
9>��9>��9>�r9>�c9>��9>��9>��9>��9>�~9>�9>��9>�e9>��9>��9>��9>��9>�e9>�C9>��9>�;9>��9>��9>��9>�9>��9>��9>�9>��9>�^9>�9>��9>�$9>�9>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBcTBbNBcTBjBr�B{�B�B-BF�B_;Bw�B��B�-B�^B�jB�wB�jB�jB�wB�wB��B��B��B��B��BŢB��B��B��B�B�B�NB�yB�yB�B��B	7B{B�B�B�B-B6FB=qBB�BF�B?}B-B �B�B�B�B �B�B�BuBbBPBB�B�B��B�9B��B��B��B�BjBR�BD�B2-BPB��B�B�mB��BɺBƨBĜB��B�FB��B�%Bn�BXBE�B6FB+B!�B�B	7B  B
��B
�yB
��B
��B
�bB
�oB
�DB
x�B
aHB
VB
J�B
"�B	�5B	B	�wB	��B	�3B	�B	p�B	ffB	aHB	`BB	dZB	_;B	W
B	T�B	Q�B	L�B	B�B	:^B	9XB	6FB	33B	1'B	(�B	�B	�B	\B	
=B	%B	B��B��B��B��B��B�B�B�sB�TB�;B�B��B��B��B��B��BǮBƨBÖBĜB��B�qB�?B�'B�B�B��B��B�oB�hB�DB�%B�B�B�B{�Bp�BhsBffBffBe`BdZBdZBe`Be`BdZBbNB_;B]/BYBR�BP�BO�BO�BN�BM�BK�BJ�BJ�BJ�BK�BK�BN�BQ�BS�BS�BR�BR�BQ�BP�BO�BP�BN�BJ�BF�BE�BD�BD�BC�BG�BH�BG�BE�BE�BF�BF�BE�BE�BK�BN�BL�BM�BN�BN�BO�BM�BK�BG�BI�BJ�BJ�BK�BK�BF�BA�BD�BK�BO�BO�BN�BM�BM�BN�BO�BR�BP�BN�BM�BM�BO�BQ�BVBT�B\)B_;B_;B_;B_;B_;B`BBbNBbNBdZBdZBcTBgmBhsBjBk�Bm�Bn�Br�Br�Bs�Bt�B~�B� B�B�B�B�%B�PB�VB�VB�hB�uB�uB��B��B��B��B��B��B��B��B��B�!B�'B�RB�dB�dB�jB�qBÖB��B��B��B�
B�B�)B�;B�BB�NB�`B�B�B��B��B��B��B��B	B	%B	1B	
=B	DB	bB	{B	�B	�B	"�B	$�B	&�B	'�B	(�B	+B	/B	6FB	;dB	C�B	D�B	E�B	J�B	M�B	O�B	P�B	Q�B	Q�B	S�B	S�B	W
B	W
B	XB	`BB	e`B	iyB	jB	iyB	jB	jB	p�B	v�B	x�B	x�B	x�B	z�B	|�B	}�B	�B	�B	�%B	�=B	�=B	�DB	�JB	�JB	�JB	�=B	�DB	�PB	�VB	�PB	�7B	�+B	�1B	�=B	�=B	�DB	�hB	�uB	�\B	�VB	�hB	�oB	�uB	�oB	�oB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�?B	�?B	�FB	�FB	�FB	�LB	�^B	�RB	�LB	�XB	�RB	�dB	�qB	��B	��B	ÖB	ŢB	ĜB	ĜB	ŢB	ɺB	��B	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�;B	�;B	�NB	�`B	�`B	�`B	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�-B	��B

	B
�B
jB
(�B
-�B
4B
<6B
C�B
GEB
K�B
TFB
X�B
\]B
b4B
g�B
m�B
shB
v�B
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���?&m�@�4�>�-(?I\d?�V&?D�?p��@���?�@1|B.�B/rA�m?�eBo6?�@�Q_?A~�@�'?��B-�B6�?4��A���?z?���B+ Bg8?qjYB9A�{�?�?t@)B'�B-|A�aa?Y��B˥B2�A��r?d@B)�B)gAK�?)r�?Yx�A;f�B#�B(�@��l?�3�BI?��?cR�Bf�>�6�?K�?d�B��>�V�?޾?ڑ�A��8A �8>���?�@� A�}6B^�?���Bx1?|R?w��Am��?D�7B�n>�c�?@d?{{�B.�@_.^>���?���A�tzB�P?�	S>��-?I��B-
B-�@W�?At�A}�wBl�?��9?���B*zB@+?�F?�UTA��B&�B x�?�b�?���A�g�?���?��?Q�F?��B/[@�?���@C�B��?��?0�A ��B$�B8�?�V?=ֿ@���A�;.@S�|>���>��>�<�>���>���>�G�?�M'B0�Bx$?""�@�vB^F?

�@޶�?�?��B,~B+�A�v�?,z�A+\�B5>؝I@�Q�AYB,�B3�?DkB%�B+�B?B��?8��>��]?�?>�@ն�BH|>�#W?LK�A��B9d>��j?3��A�ʪB.B�
?�QA�$A��/A�H=?r?�)B<A'��?%�??PB8=B1�?�8*A�BO�BGGB�+AuK@0�-AS�B1;B0kB
֬?1PD?���B/vB3mA�+2B
�nA���B2e@[��B0�A���?ZׄBZ�B4�B2�B2]Bt�B2JB�?A)g�B��B:�B-�B3�BP�B3B1nB��B2BB0'B1�B4�B6�B2�B3B7/B1:B7TB2B3�B2LB2�B3�B2B29B1�B1�B2HA���AV��B55B9�B2�B6B2TB48B2!B4B8mB5�B3�B1�B3�B5mB4EB2�B3$B4�B4�B5�B3�B1kB4�B0iB2B0�B4�B7�B1�B5�B4�B2]B3\B2fB4�B3vB3zBt0B6�B7lB3VB2	B>#B: B0dB3�B3B2�B6�B3dB4_B68B3,B7B2�B5rB7VB3�B4�B4�B:�B1�B3B6B2`B2B8|B=$B5.B.�B2B1�B2�B2�B2B1SB2B1AB2"B2�B2�B26B26B3�B1�B0B2�B3	B0mB2IB3�B0�B0JB0�B1�Bh^B1hB2.B0�B2�B5RB3B1�B26B2�B2�B2�B1kB1|B3	B1kB1B2�@8=�B0zB4"B1&B3�A&��B9�B6�B1UB1hB0�B6�B1fB1uB0�B4�B2A�q`B0dB0dB3aB1,B0!B2HB2�B1uB1�B1�B3�B1:B0�B3B4B1pB1B3~B4IB3�B2�B3LB2?B3HB1"B3�B3\B0�B1�B2VB39B2B2^B1�B3OB3%B3B3�B2B1�B2CB1�B1�B0\B2;B0�B/�B11B/&B1�BTA�"B3�B/�B1UB2�B1UB1B2B0�B0OB1�B2�B23B1�B2^B2B?�B1�B1�B2B0fB3!B1B2 B7fA�KqB2f?���B1B1�B27B3�B3�B2HB0�B1�B1}B4 B0oB.�B2�B4IB4�B0�B@�B4gB2�B3nB0,B4B3�B57B1�B2{B3aB0�B2�B5B2qB1�B3�B1�B3�B8B4�B4B2�B1�B2QB2QB1�B3�B9�B
�?���B2�B1�B4B3B3�B1�B1�B6�B1�B/�B0oB0gB1`B1�B0�B0YB1GB1?B1�B/�B/�B1~B1�B/�B1�B1�B2�B1JB3
B2�B1lB1B2�B1gB2B2B1:B2�B1�B2�B1�B1�B1�B1hB1B0�B2`B1rB16B19B1�B1�B1RB1JB1�B2/B0�B1�B2oB1�B0�B1!B1�B2B1tB1�B0�B2B1�B1B1B0UB2	B2 B2FB2B1vB2�B2vB1�B2B2LB2DB2�B2yB2�B1�B2�B1iB3�B1"B2*B1�B1FB2	B1�B1�B2B2SB1�B1pB22B2xB1NB1�B1�B1�B1�B1�B2�B2�B4LB2)B2�B2�B2�B2�B3CB2hB2�B2<B2�B2�B2iB2`B2B3B2�B3PB2tB3�B2�B2@B2�B3%B2RB1�B2AB2�B2B2vB1�B1�B2]B2UB2DB2�B1�B2+B2�B2�B1�B2cB2�B1�B2�B2�B2uB1�B1fB2wB2[B31B2^B2B3B2EB2kB2�B2�B2#B2B2�B37B3�B3B3lB2�B2�B22B2�B3:B32B2�B2(B2�B2�B2�B3�B3*B3B3B3WB4B3GB3�B2�B4+B3PB2}B3�B37B3B2AB3B3dB2�B2�B2�B3�B3�B2�B2�B2�B4B3B3�B3B4*B3WB3�B4%B2B3�B3�B3B2�B3nB2�B3.B3�B3�B3�B41B3�B4gB4^B4VB	��B	��B	��B	�1B	�]B	�B	�uB	�6B	��B	�pB	�kB	�jB	�|B	�B	�aB	��B	��B	�/B	��B	��B	ǅB	ȕB	�iB	ɋB	��B	ȸB	�`B	��B	ɮB	�DB	�UB	�B	��B	ɶB	�kB	�^B	�bB	�7B	�gB	�xB	�]B	�BB	�5B	ɭB	��B	ɅB	�xB	�uB	�B	��B	��B	��B	�3B	�&B	�B	��B	��B	�"B	�B	�B	�;B	�[B	��B	�0B	�qB	�VB	ˇB	�zB	˪B	�"B	�EB	�8B	�hB	˙B	ˋB	�aB	ʑB	ʄB	ˇB	�iB	�OB	ʀB	��B	��B	�+B	ʕB	ʈB	��B	�JB	�@B	��B	��B	ʝB	�B	�RB	��B	�B	��B	ʳB	�[B	ʋB	�QB	�B	��B	�B	��B	ɴB	�iB	�B	əB	�mB	�jB	ȰB	ɔB	�dB	ǸB	ȷB	�gB	�\B	ɨBb1Bb�Ba�Ba�Bb�BbBbwBb�Bb�Ba�Bb�BbwBboBb�BbVBb2Bb�BbBb�BbNBa�BbpBb�Ba�Bb3Bb�Bb�Bb�Bb_Bb�BbxBb�Bb�Bb~Bb Bb#Bb�Bb�Bb�Bb�Bb�Bb�Ba�Bb�Bc@Ba�Bb�Bb�Bb�Bc-Bb�Bb5Bc2Bb8BcBb�Bb9Bb)Ba�BbBc(Bb�Bb�Bc.Bb�BbpBbUBb`BbcBb�Bb�BbUBb%Bb�Bb�Bb�BbxBb�Bb�Bb�Bb�Bb�Bb�Bb�Bb�Bb�Bb�Ba�BcBc.Bb�BbVBb�Bb�BbwBb,Bb?Bb�Bb:BbBb�BbBb�BcBb+Bb6Bb.Bb�B`�Ba�Bb�Bb�BbBBb9Bb1BbcBbGBcBb#BcBbMBcBbPBb�Bb?BbBbBBb�Bb�Bb�Bb�Bc%BcBb�BbBbMBcBcBbBb�Bb�Bb�BcRBbBbnBb+Bb�BbUBb�Ba�Bb�Bb�Bb�BbCBb�Bb�BboBb�Bb�Bb�BbNBb�BcBbLBb�BbbBb3BcBcBbhBb�Bb�Bb�Bb�Bb�BcZBbsBbvBb�BcDBb�Bb�Bc]Bb�Bb�Bb�BbBb�Bb�Bb�Bb�Bb�Bb�Ba�Ba�Bb�Bb�Bb�BbBb:Bb�Bb�BbB	�B	�@B	�B	�!B	�>B	�B	��B	�YB	� B	��B	�XB	�B	�KB	��B	��B	�B	�B	��B	�B	�B	��B	�~B	�^B	��B	�lB	�B	�B	�B	��B	��B	�B	�YB	�-B	��B	�B	�|B	�bB	�sB	�B	�YB	�{B	�#B	�5B	�6B	��B	� B	�B	�#B	�>B	��B	�B	��B	�{B	��B	�B	��B	��B	��B	��B	�B	�B	�eB	�B	��B	�B	�B	��B	��B	�NB	��B	�&B	�B	�+B	�B	�B	�BB	�SB	�eB	�XB	�JB	�OB	�`B	�SB	�8B	�B	�B	��B	��B	�mB	�B	��B	�B	��B	�B	�B	�eB	�+B	��B	��B	��B	��B	��B	�B	�eB	�B	�NB	�B	�B	�rB	��B	��B	�vB	��B	�B	��B	�B	�B	��B	�dB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344444444443344344444334344334344433343334334444334434434443444444443343444434443444334443344434433444334434444344434443344434444444433443444433444344433433334444434433443334333443444334433344433344334343433433333334333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333343333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 BbPBbNBbNBbMBbNBbNBbLBbNBbLBbMBbMBbLBbPBbNBcRBbNBcTBj�Br�B{�B�B-BF�B_?Bw�B��B�.B�_B�jB�vB�lB�jB�{B�xB��B��B��B��B��BŢB��B��B��B�B�B�RB�yB�xB�B��B	:B{B�B�B�B-B6IB=tBB�BF�B?}B-B �B�B�B�B �B�B�BwBdBPBB�B�B��B�:B��B��B��B�BjBR�BD�B2+BQB��B�B�mB��BɻBƪBĜB��B�GB��B�$Bn�BXBE�B6DB+B!�B�B	6B B
��B
�xB
��B
��B
�dB
�qB
�EB
x�B
aHB
VB
J�B
"�B	�5B	B	�xB	��B	�5B	�B	p�B	feB	aGB	`AB	d[B	_;B	W
B	T�B	Q�B	L�B	B�B	:_B	9XB	6DB	33B	1'B	(�B	�B	�B	[B	
;B	$B	B��B��B��B��B��B�B�B�pB�VB�;B�B��B��B��B��B��BǭBƧBÖBĜB��B�rB�?B�)B�B�B��B��B�nB�hB�CB�$B�B�B�B{�Bp�BhsBffBfgBe`BdYBdYBeaBe_Bd\BbPB_:B]2BYBR�BP�BO�BO�BN�BM�BK�BJ�BJ�BJ�BK�BK�BN�BQ�BS�BS�BR�BR�BQ�BP�BO�BP�BN�BJ�BF�BE�BD�BD�BC�BG�BH�BG�BE�BE�BF�BF�BE�BE�BK�BN�BL�BM�BN�BN�BO�BM�BK�BG�BI�BJ�BJ�BK�BK�BF�BA�BD�BK�BO�BO�BN�BM�BM�BN�BO�BR�BP�BN�BM�BM�BO�BQ�BVBT�B\*B_:B_<B_<B_=B_9B`CBbNBbLBdYBd[BcTBgnBhqBj}Bk�Bm�Bn�Br�Br�Bs�Bt�B~�B� B�B�B� B�&B�NB�WB�WB�hB�sB�uB��B��B��B��B��B��B��B��B��B�!B�&B�QB�dB�bB�iB�qB×B��B��B��B�
B�B�)B�9B�CB�MB�`B�B�B��B��B��B��B��B	B	$B	1B	
;B	BB	`B	zB	�B	�B	"�B	$�B	&�B	'�B	(�B	+B	/B	6EB	;cB	C�B	D�B	E�B	J�B	M�B	O�B	P�B	Q�B	Q�B	S�B	S�B	WB	WB	XB	`BB	eaB	iyB	j�B	iyB	j�B	j~B	p�B	v�B	x�B	x�B	x�B	z�B	|�B	}�B	�B	�B	�$B	�>B	�<B	�DB	�JB	�JB	�KB	�<B	�FB	�OB	�VB	�QB	�8B	�*B	�2B	�=B	�=B	�EB	�hB	�vB	�ZB	�TB	�jB	�pB	�wB	�mB	�pB	�jB	�hB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�'B	�3B	�<B	�>B	�GB	�FB	�FB	�LB	�]B	�SB	�JB	�WB	�RB	�eB	�rB	��B	��B	ÕB	ŠB	ęB	ĝB	ŢB	ɺB	��B	ǰB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�0B	�;B	�<B	�NB	�`B	�bB	�`B	�nB	�iB	�wB	�B	�B	�B	�B	�G�O�B	�.B	��B


B
�B
jB
(�B
-�B
4 B
<4B
C�B
GFB
K�B
TDB
X�B
\]B
b4B
g�B
m�B
sjB
v�B
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B.�B/sG�O�G�O�Bo6G�O�G�O�G�O�G�O�G�O�B-�B6�G�O�A���G�O�G�O�B+Bg:G�O�B9
G�O�G�O�G�O�B'�B-{A�a_G�O�B˥B2�A��rG�O�B)�B)jG�O�G�O�G�O�G�O�B#�B(�G�O�G�O�BI G�O�G�O�Bf�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�}6B^�G�O�Bx3G�O�G�O�G�O�G�O�B�mG�O�G�O�G�O�B.�G�O�G�O�G�O�A�tzB�OG�O�G�O�G�O�B-B-�G�O�G�O�G�O�Bl�G�O�G�O�B*yB@-G�O�G�O�G�O�B&�B x�G�O�G�O�A�g�G�O�G�O�G�O�G�O�B/]G�O�G�O�G�O�B��G�O�G�O�G�O�B$�B8�G�O�G�O�G�O�A�;0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B0�Bx&G�O�G�O�B^FG�O�G�O�G�O�G�O�B,{B+�G�O�G�O�G�O�B5G�O�G�O�G�O�B,�B3�G�O�B%�B+�B?B��G�O�G�O�G�O�G�O�G�O�BHzG�O�G�O�A��B9bG�O�G�O�A�ʬB.B�G�O�A�$A��4A�H>G�O�G�O�B<G�O�G�O�G�O�B8=B1�G�O�G�O�BO�BGHB�+G�O�G�O�G�O�B1=B0lB
֩G�O�G�O�B/wB3nG�O�B
�nG�O�B2fG�O�B0�A���G�O�BZ�B4�B2�B2]Bt�B2KB�BG�O�B��B:�B-�B3�BP�B3B1lB��B2CB0(B1�B4�B6�B2�B3B71B1<B7UB2B3�B2LB2�B3�B2
B2:B1�B1�B2HA���G�O�B55B9�B2�B6B2SB46B2!B4B8lB5�B3�B1�B3�B5nB4FB2�B3#B4�B4�B5�B3�B1jB4�B0iB2B0�B4�B7�B1�B5�B4�B2]B3\B2fB4�B3vB3{Bt0B6�B7lB3UB2B>%B:B0eB3�B3B2�B6�B3dB4]B69B3+B7B2�B5rB7VB3�B4�B4�B:�B1�B3B6B2`B2B8|B=$B5-B.�B2B1�B2�B2�B2
B1PB2B1AB2"B2�B2�B25B25B3�B1�B0B2�B3B0pB2JB3�B0�B0GB0�B1�Bh]B1gB2.B0�B2�B5SB3B1�B25B2�B2�B2�B1jB1{B3B1jB1B2�G�O�B0{B4"B1'B3�G�O�B9�B6�B1SB1gB0�B6�B1dB1tB0�B4�B2A�qaB0dB0dB3aB1+B0B2JB2�B1tB1�B1�B3�B1;B0�B3}B4B1pB1�B3B4JB3�B2�B3IB2?B3IB1#B3�B3_B0�B1�B2UB39B2B2\B1�B3NB3%B3B3�B2B1�B2DB1�B1�B0\B2:B0�B/�B13B/&B1�BTA�&B3�B/�B1SB2�B1SB1B2B0�B0OB1�B2�B23B1�B2\B2B?�B1�B1�B2B0dB3"B1B2B7gA�KpB2cG�O�B1B1�B25B3�B3�B2JB0�B1�B1{B3�B0pB.�B2�B4JB4�B0�B@�B4fB2�B3nB0,B4B3�B58B1�B2{B3bB0�B2�B5B2qB1�B3�B1�B3�B8B4�B4B2�B1�B2RB2RB1�B3�B9�B
�G�O�B2�B1�B4B3B3�B1�B1�B6�B1�B/�B0rB0hB1bB1�B0�B0XB1FB1@B1�B/�B/�B1�B1�B/�B1�B1�B2�B1KB3	B2�B1jB1B2�B1gB2Bb2Bb�Ba�Ba�Bb�BbBbvBb�Bb�Ba�BbBbvBboBb�BbTBb2Bb�BbBb�BbNBa�BboBb�Ba�Bb4Bb�Bb�Bb�Bb`Bb�BbxBb�Bb�Bb}Bb Bb%Bb�Bb�Bb�Bb�Bb�Bb�Ba�Bb�BcABa�Bb�Bb�Bb�Bc-Bb�Bb7Bc4Bb7BcBb�Bb:Bb)Ba�BbBc*Bb�Bb�Bc-Bb�BboBbWBb]BbdBb�Bb�BbUBb'Bb�Bb�Bb�BbxBb�Bb�Bb�BbBb�Bb�Bb�Bb�Bb�Bb�Ba�Bc	Bc-Bb�BbTBb�Bb�BbxBb+Bb?Bb�Bb7Bb	Bb�BbBb�BcBb+Bb4Bb+Bb�B`�Ba�Bb�Bb�BbCBb:Bb1BbdBbGBcBb#BcBbMBcBbPBbBb?BbBbEBb�Bb�Bb�Bb�Bc$BcBb�BbBbPBcBcBbBb�Bb�Bb�BcRBb}BbmBb-Bb�BbRBb�Ba�Bb�Bb�Bb�BbBBb�Bb�BbqBb�Bb�Bb�BbLBb�BcBbIBb�BbdBb3BcBcBbiBb�Bb�Bb�Bb�Bb�Bc\BbqBbvBb�BcGBb�Bb�Bc\Bb�Bb�Bb�BbBb�Bb�Bb�Bb�Bb�Bb�Ba�Ba�Bb�Bb�Bb�BbBb9Bb�Bb�BbB	�B	�AB	�B	�"B	�AB	�B	��B	�\B	�B	��B	�WB	�B	�MB	��B	��B	�B	�B	��B	�B	�B	��B	�~B	�^B	��B	�mB	�B	�B	�B	��B	��B	�B	�XB	�-B	��B	�B	�zB	�aB	�sB	�B	�XB	�{B	�"B	�6B	�8B	��B	� B	�B	�$B	�<B	��B	�B	��B	�|B	��B	�B	��B	��B	��B	��B	�B	�B	�gB	�B	��B	�B	�B	��B	� B	�MB	��B	�$B	�B	�-B	�B	�B	�?B	�PB	�gB	�WB	�IB	�NB	�`B	�TB	�8B	�B	�B	��B	��B	�nB	�B	��B	�B	��B	�B	�B	�fB	�)B	��B	��B	��B	��B	��B	�B	�fB	�B	�LB	�B	�B	�rB	��B	��B	�wB	��B	�B	��B	�B	�B	��B	�dB	��Bb2Bb�Ba�Ba�Bb�BbBbvBb�Bb�Ba�BbBbvBboBb�BbTBb2Bb�BbBb�BbNBa�BboBb�Ba�Bb4Bb�Bb�Bb�Bb`Bb�BbxBb�Bb�Bb}Bb Bb%Bb�Bb�Bb�Bb�Bb�Bb�Ba�Bb�BcABa�Bb�Bb�Bb�Bc-Bb�Bb7Bc4Bb7BcBb�Bb:Bb)Ba�BbBc*Bb�Bb�Bc-Bb�BboBbWBb]BbdBb�Bb�BbUBb'Bb�Bb�Bb�BbxBb�Bb�Bb�BbBb�Bb�Bb�Bb�Bb�Bb�Ba�Bc	Bc-Bb�BbTBb�Bb�BbxBb+Bb?Bb�Bb7Bb	Bb�BbBb�BcBb+Bb4Bb+Bb�B`�Ba�Bb�Bb�BbCBb:Bb1BbdBbGBcBb#BcBbMBcBbPBbBb?BbBbEBb�Bb�Bb�Bb�Bc$BcBb�BbBbPBcBcBbBb�Bb�Bb�BcRBb}BbmBb-Bb�BbRBb�Ba�Bb�Bb�Bb�BbBBb�Bb�BbqBb�Bb�Bb�BbLBb�BcBbIBb�BbdBb3BcBcBbiBb�Bb�Bb�Bb�Bb�Bc\BbqBbvBb�BcGBb�Bb�Bc\Bb�Bb�Bb�BbBb�Bb�Bb�Bb�Bb�Bb�Ba�Ba�Bb�Bb�Bb�BbBb9Bb�Bb�BbB	�B	�AB	�B	�"B	�AB	�B	��B	�\B	�B	��B	�WB	�B	�MB	��B	��B	�B	�B	��B	�B	�B	��B	�~B	�^B	��B	�mB	�B	�B	�B	��B	��B	�B	�XB	�-B	��B	�B	�zB	�aB	�sB	�B	�XB	�{B	�"B	�6B	�8B	��B	� B	�B	�$B	�<B	��B	�B	��B	�|B	��B	�B	��B	��B	��B	��B	�B	�B	�gB	�B	��B	�B	�B	��B	� B	�MB	��B	�$B	�B	�-B	�B	�B	�?B	�PB	�gB	�WB	�IB	�NB	�`B	�TB	�8B	�B	�B	��B	��B	�nB	�B	��B	�B	��B	�B	�B	�fB	�)B	��B	��B	��B	��B	��B	�B	�fB	�B	�LB	�B	�B	�rB	��B	��B	�wB	��B	�B	��B	�B	�B	��B	�dB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344444444443344344444334344334344433343334334444334434434443444444443343444434443444334443344434433444334434444344434443344434444444433443444433444344433433334444434433443334333443444334433344433344334343433433333334333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333343333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649192020083116491920200831164919202008311649192020083116491920200831164919202008311649192020083116491920200831164919202008311649192020083116491920200831164919AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817132019021918171320190219181713    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817132019021918171320190219181713  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817132019021918171320190219181713  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649192020083116491920200831164919  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                