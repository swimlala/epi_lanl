CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ]   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:37Z creation      
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
_FillValue                  0 �Argo profile    3.1 1.2 19500101000000  20190219181637  20200831164727  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @լXbj��@լXbj��@լXbj��111 @լX�Q��@լX�Q��@լX�Q��@6��t�j@6��t�j@6��t�j�cMhr� ��cMhr� ��cMhr� �111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @333@y��@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�	�D�T{D��qD���D�
�D�<)D���D��=D��\D�P D���D��qD��)D�I�D�{�D���D���D�7�D�{3D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��    =��;L�;L��        �L�;L�;L��    �L��        �L�;L�ͽ��;L�;L�;L��    =��;L�ͽ���    ���;L�ͽ���    �L�;L�;L�ͽ���    ���ͽ���    �L�;L�ͽ���    �L�;L�ͽ���=��;L�;L�;L�ͽ���    �L�ͽ��ͽ��;L�;L�ͽ��;L�;L�ͽ��ͽ���    �L�ͽ��ͽ��;L��    �L�;L��=���    �L��        ���;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L��    ���;L��        �L�ͽ���=���>L�ͽ��;L�;L�ͽ��;L��    ���;L�;L�ͽ���    �L��        �L�;L�ͽ���    ���;L�ͽ���=��ͽ��;L�ͽ��ͽ��ͽ��;L�ͽ��ͽ��;L�ͽ��;L�;L�;L�ͽ��;L�;L�;L�ͽ��;L��    =��;L�;L�;L�;L�ͽ��;L�;L�ͽ���>L��=��ͽ��;L�ͽ��;L��        �L�ͽ��ͽ��;L�;L�ͽ���            �L�;L�ͽ��ͽ���    ���;L��            �L�;L�ͽ��;L�ͽ��;L��=��;L�ͽ��;L�ͽ��ͽ���=���            �L�ͽ��ͽ��ͽ���=��ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L��    ����=���    ����                            ����=���=���=���=���=���    =���    =���    =���=���=���=���=���=���        =���=���=��ͽ���=���=���=���    =���=���        =���=���    =���=���=���=���=���=���    =���    >L��=���=���=���=���=���        =���=���=���=���=���    =���=���>L��    =���=���>L��=���>L��=���=���    =���    =���>L��>L��>L��=���=���    =���    >L��=���    =���            =���=���=���=���=���>L��=���=���=���=���>L��=���=���=���=���    =���=���    =���    =���=���=���=���>L��=���=���=���    =���=���        =���=���=���>L��=���>L��>L��>L��>L��>L��>L��=���                    =���=���    =���=���    =���=���    >L��=���=���=���>L��=���=���>L��            ����=���=���        =���    =���        =���=���=���    =���    =���    >L��        =���    >L��        =���=���    =���=���>L��>L��=���=���=���>L��            =���=���=���=���    =���=���=���=���=���>L��>L��                =���=���=���            =���    =���=���>L��>L��>L��>L��>L��=���=��ͽ���        =���=���=���    =���>L��>L��=���=���        ����        =���    >L��>L��=���    =���>L��=���=���=���    ����    =���=���=���=���=���=���>L��=���=���                    >L��>L��>���>���>���>���?��?��?333?L��?L��?fff?�  ?���?���?���?�ff?�33?�  ?���?���?ٙ�?�ff?�33?�33@   @ff@��@33@��@��@   @&ff@333@9��@@  @Fff@L��@Y��@`  @fff@l��@y��@�  @�33@�ff@���@�  @�33@���@���@�  @�33@���@���@�  @�ff@���@���@�  @�ff@ə�@���@�33@�ff@ٙ�@���@�33@�ff@陚@���@�33@�ff@���@���A��A33A��AffA  A	��A33A��AffA��A33A��AffA  A��A��AffAffA   A!��A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A;33A<��A>ffA@  AA��AA��AC33AFffAFffAH  AI��AK33AL��ANffAP  AQ��AQ��AS33AT��AVffAX  AY��AY��A[33A\��A^ffA`  A`  Aa��Ac33Ad��AfffAh  Ah  Ai��Ak33Al��AnffAp  Aq��Aq��As33At��AvffAx  Ay��A{33A|��A|��A�  A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���Ař�A�33A�  A���A�ffA�33A���A͙�A�ffA�  A���A�ffA�33A���Aՙ�A�33A�  A���A�ffA�  A���Dp��Dq  Dq�Dq3Dq�Dq&fDq,�Dq33Dq9�DqFfDqL�DqS3DqY�DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3@&ff@333@9��@@  @Fff@L��@Y��@`  @fff@l��@y��@�  @�33@�ff@���@�  @�33@���@���@�  @�33@���@���@�  @�ff@���@���@�  @�ff@ə�@���@�33@�ff@ٙ�@���@�33@�ff@陚@���@�33@�ff@���@���A��A33A��AffA  A	��A33A��AffA��A33A��AffA  A��A��AffAffA   A!��A$��A&ffA(  A)��A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A;33A<��A>ffA@  AA��AA��AC33AFffAFffAH  AI��AK33AL��ANffAP  AQ��AQ��AS33AT��AVffAX  AY��AY��A[33A\��A^ffA`  A`  Aa��Ac33Ad��AfffAh  Ah  Ai��Ak33Al��AnffAp  Aq��Aq��As33At��AvffAx  Ay��A{33A|��A|��A�  A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���Ař�A�33A�  A���A�ffA�33A���A͙�A�ffA�  A���A�ffA�33A���Aՙ�A�33A�  A���A�ffA�  A���Dp��Dq  Dq�Dq3Dq�Dq&fDq,�Dq33Dq9�DqFfDqL�DqS3DqY�DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @<��@���@���AffA"ffABffAbffA�33A�ffA�33A�33A�33A�33A�  A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��By  B�L�B�L�B��B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC@ C&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq4Dq��Dr	�Dr��Ds	�Ds��Dt	�Dt��Dy�RD��D�YHD��>D�ֹD�\D�@�D���D��
D�)D�T�D���D��>D���D�N�DڀRD�ϮD���D�<{D� D�ҐG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��>��>�  �L�νL��>��>���L�νL�νL��>���L��>��>���L�νL��=L�̽L�νL�νL��>��>�  �L��=L��>��=L�̽L��=L��>���L�νL�νL��=L��>��=L��=L��>���L�νL��=L��>���L�νL��=L��>�  �L�νL�νL��=L��>���L��=L��=L�̽L�νL��=L�̽L�νL��=L��=L��>���L��=L��=L�̽L��>���L�νL��>�  >���L��>��>��=L�̽L�νL�νL�νL��=L�̽L�νL�νL�νL�νL�νL��>��=L�̽L��>��>���L��=L��>�  >�33=L�̽L�νL��=L�̽L��>��=L�̽L�νL��=L��>���L��>��>���L�νL��=L��>��=L�̽L��=L��>�  =L�̽L��=L��=L��=L�̽L��=L��=L�̽L��=L�̽L�νL�νL��=L�̽L�νL�νL��=L�̽L��>��>�  �L�νL�νL�νL��=L�̽L�νL��=L��>�33>�  =L�̽L��=L�̽L��>��>���L��=L��=L�̽L�νL��=L��>��>��>���L�νL��=L��=L��>��=L�̽L��>��>��>���L�νL��=L�̽L��=L�̽L��>�  �L��=L�̽L��=L��=L��>�  >��>��>���L��=L��=L��=L��>�  =L�̽L��=L��=L��=L��=L��=L��=L��=L�̽L��>��=L��>�  >��=L��>��>��>��>��>��>��>��=L��>�  >�  >�  >�  >�  >��>�  >��>�  >��>�  >�  >�  >�  >�  >�  >��>��>�  >�  >�  =L��>�  >�  >�  >��>�  >�  >��>��>�  >�  >��>�  >�  >�  >�  >�  >�  >��>�  >��>�33>�  >�  >�  >�  >�  >��>��>�  >�  >�  >�  >�  >��>�  >�  >�33>��>�  >�  >�33>�  >�33>�  >�  >��>�  >��>�  >�33>�33>�33>�  >�  >��>�  >��>�33>�  >��>�  >��>��>��>�  >�  >�  >�  >�  >�33>�  >�  >�  >�  >�33>�  >�  >�  >�  >��>�  >�  >��>�  >��>�  >�  >�  >�  >�33>�  >�  >�  >��>�  >�  >��>��>�  >�  >�  >�33>�  >�33>�33>�33>�33>�33>�33>�  >��>��>��>��>��>�  >�  >��>�  >�  >��>�  >�  >��>�33>�  >�  >�  >�33>�  >�  >�33>��>��>��=L��>�  >�  >��>��>�  >��>�  >��>��>�  >�  >�  >��>�  >��>�  >��>�33>��>��>�  >��>�33>��>��>�  >�  >��>�  >�  >�33>�33>�  >�  >�  >�33>��>��>��>�  >�  >�  >�  >��>�  >�  >�  >�  >�  >�33>�33>��>��>��>��>�  >�  >�  >��>��>��>�  >��>�  >�  >�33>�33>�33>�33>�33>�  >�  =L��>��>��>�  >�  >�  >��>�  >�33>�33>�  >�  >��>��=L��>��>��>�  >��>�33>�33>�  >��>�  >�33>�  >�  >�  >��=L��>��>�  >�  >�  >�  >�  >�  >�33>�  >�  >��>��>��>��>��>�33>�33>�fg?��?��?��?@  ?@  ?Y��?s33?s33?�ff?�33?�  ?�  ?���?���?�ff?�33?�  ?�  ?���?���@33@33@	��@  @fg@��@#34@#34@)��@0  @<��@C34@I��@P  @Vfg@c34@i��@p  @vfg@���@���@�  @�33@���@���@�  @�fg@���@���@�  @�fg@���@���@�33@�fg@���@���@�33@�fg@љ�@�  @�33@�fg@ᙚ@�  @�33@�fg@�@�  @�33@�fgA ��A  A��A33A��A
ffA  A��A33A��A  A��A33A��AffA  A33A ��A ��A"ffA$  A'33A(��A*ffA,  A-��A/33A0��A2ffA4  A5��A733A8��A:ffA<  A=��A?33A@��ABffAD  AD  AE��AH��AH��AJffAL  AM��AO33AP��ARffAT  AT  AU��AW33AX��AZffA\  A\  A]��A_33A`��AbffAbffAd  Ae��Ag33Ah��AjffAjffAl  Am��Ao33Ap��ArffAt  At  Au��Aw33Ax��AzffA|  A}��A33A33A�33A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���AÙ�A�ffA�  A���A�ffA�33A�  A˙�A�ffA�  A���Aϙ�A�33A�  Aә�A�ffA�  A���A�ffA�33A�  Aۙ�A�33A�  Dq4Dq	�DqgDq�Dq#4Dq0 Dq6gDq<�DqC4DqP DqVgDq\�Dqc4Dqp DqvgDq|�Dq��Dq� Dq�gDq��Dq�4Dq� Dq�gDq��DqɚDq� Dq�gDq��Dq�Dq� Dq�gDr4Dr	�Dr DrgDr#4Dr)�Dr0 Dr6gDrC4DrI�DrP Dr\�Drc4Dri�Drp Dr|�Dr�4Dr��Dr�gDr��Dr�4Dr��Dr�gDr��Dr�4Dr� Dr�gDr��Dr�4Dr� Dr�gDr��Ds	�Ds DsgDs�Ds)�Ds0 Ds6gDsC4DsI�DsP Ds\�Dsc4Dsi�DsvgDs|�Ds�4Ds��Ds�gDs��Ds��Ds� Ds�gDs�4DsɚDs� Ds�gDs�4Ds�Ds�gDs��Dt4Dt	�DtgDt�Dt#4Dt0 Dt6gDt<�DtI�DtP DtVgDtc4Dti�Dtp Dt|�Dt�4Dt��Dt�gDt��Dt�4Dt��Dt�gDt��Dt�4Dt� Dt�gDt��@0  @<��@C34@I��@P  @Vfg@c34@i��@p  @vfg@���@���@�  @�33@���@���@�  @�fg@���@���@�  @�fg@���@���@�33@�fg@���@���@�33@�fg@љ�@�  @�33@�fg@ᙚ@�  @�33@�fg@�@�  @�33@�fgA ��A  A��A33A��A
ffA  A��A33A��A  A��A33A��AffA  A33A ��A ��A"ffA$  A'33A(��A*ffA,  A-��A/33A0��A2ffA4  A5��A733A8��A:ffA<  A=��A?33A@��ABffAD  AD  AE��AH��AH��AJffAL  AM��AO33AP��ARffAT  AT  AU��AW33AX��AZffA\  A\  A]��A_33A`��AbffAbffAd  Ae��Ag33Ah��AjffAjffAl  Am��Ao33Ap��ArffAt  At  Au��Aw33Ax��AzffA|  A}��A33A33A�33A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���AÙ�A�ffA�  A���A�ffA�33A�  A˙�A�ffA�  A���Aϙ�A�33A�  Aә�A�ffA�  A���A�ffA�33A�  Aۙ�A�33A�  Dq4Dq	�DqgDq�Dq#4Dq0 Dq6gDq<�DqC4DqP DqVgDq\�Dqc4Dqp DqvgDq|�Dq��Dq� Dq�gDq��Dq�4Dq� Dq�gDq��DqɚDq� Dq�gDq��Dq�Dq� Dq�gDr4Dr	�Dr DrgDr#4Dr)�Dr0 Dr6gDrC4DrI�DrP Dr\�Drc4Dri�Drp Dr|�Dr�4Dr��Dr�gDr��Dr�4Dr��Dr�gDr��Dr�4Dr� Dr�gDr��Dr�4Dr� Dr�gDr��Ds	�Ds DsgDs�Ds)�Ds0 Ds6gDsC4DsI�DsP Ds\�Dsc4Dsi�DsvgDs|�Ds�4Ds��Ds�gDs��Ds��Ds� Ds�gDs�4DsɚDs� Ds�gDs�4Ds�Ds�gDs��Dt4Dt	�DtgDt�Dt#4Dt0 Dt6gDt<�DtI�DtP DtVgDtc4Dti�Dtp Dt|�Dt�4Dt��Dt�gDt��Dt�4Dt��Dt�gDt��Dt�4Dt� Dt�gDt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�9XA�=qA�K�A�A�A�9XA�?}A�G�A�G�A�C�A�K�A�K�A�K�A�K�A�M�A�M�A�O�A�K�A�G�A�G�A�I�A��A��;A���Ḁ�ȂhA�A�A�z�A���A�dZA�$�A��A�?}AȰ!A�;dAƥ�Aě�A�v�A�n�A��A�r�A�A��A��HA�9XA��A�JA�hsA�
=A���A�\)A�ffA���A��
A�(�A�|�A�A�A�ZA�dZA�;dA��#A�C�A�(�A�bA���A��9A�ȴA�l�A�K�A���A� �A�;dA���A��A��hA�JA�z�A��A�&�A�A�\)A��RA�ƨA���A���A���A�ƨA���A���A��mA�1A�n�A��A��uA���A���A�?}A�33A���A���A���A��DA��`A��A�p�A�ZA�?}A��mA�/A���A���A���A��TA�t�A�ƨA�G�A��HA�O�A��#A�  A�jA~-Az�Aw��AvAtv�Aq�#Apn�Anz�AkK�Ail�Agt�AeC�Ab��A^�jA]K�A[��AX��AV��AUt�ATQ�AR�`AQ`BAP1'AOXAN  AM%AKO�AI��AIS�AH1'AG�7AF^5AD�!AC�wAB�!AA�AA�A?�TA>�uA<�A;��A:�`A:{A9�7A9dZA8�9A8�A7�;A7;dA5�A5\)A5A4bA3/A2-A17LA0{A.��A-7LA+%A)�;A)�FA)"�A(jA'&�A&��A&$�A$�A#��A#p�A"VA!p�A!?}A �DA {A"�AVAO�AE�A�;A��A�An�A�TAĜAG�AbA�hA"�Av�A�-AVA�
A33A�jAXA{A
=Ax�A
r�A
Q�A
 �A	��A	33An�A1A`BA�uA�A{Az�A�FA ��@��w@�"�@���@��P@��@�E�@���@�C�@��@�@�A�@��m@���@�@�V@��@��@���@�9@��@�+@�J@��y@�7@ް!@�t�@��@�v�@�7L@���@�l�@���@ҏ\@�V@�@с@�X@��@���@��@��/@�j@�~�@́@�G�@�/@�7L@�V@�1@���@ʰ!@�^5@��T@�&�@�E�@���@�
=@���@��y@�+@��y@��j@�E�@��@�t�@�@���@��@�b@��@�%@�A�@��m@��P@�-@���@��
@���@�33@���@�E�@�@� �@�l�@�
=@��R@�v�@��T@�x�@�&�@��j@��@�bN@�I�@���@�\)@�"�@���@�V@�E�@��@�X@�&�@��@��@��9@�j@�I�@�(�@�1@���@��@�"�@�^5@���@�x�@�V@���@��j@��@�(�@���@�dZ@�33@�o@���@��!@���@���@��\@�~�@��+@�V@�E�@�E�@��T@���@�`B@�hs@�p�@�X@�`B@�x�@�@���@���@�n�@��R@���@���@�;d@�C�@��H@��@���@���@��\@�V@�=q@��7@�V@��`@���@��`@��j@�b@���@��w@���@�|�@�\)@�C�@�|�@�A�@�I�@�1'@� �@��@���@�|�@�C�@�
=@��@���@�v�@���@�z�@�|�@�v�@���@���@�5?@�$�@��T@�Ĝ@��;@��;@�ƨ@���@��@��7@�%@���@���@���@�9X@��@�C�@�
=@�
=@�o@���@���@��\@��\@���@���@���@���@�M�@��#@��-@��@��@��@��@��@���@�9X@� �@�b@��;@��F@�K�@�"�@���@��!@���@�ȴ@���@�n�@�M�@���@�X@�?}@��@��
@�  @�bN@�Z@���@��@���@���@���@�dZ@�=�@x:�@q�7@k9�@`9X@Y \@Rȴ@L�|@EF@=��@7��@2�@+��@&~�@!��@��@g8@�V@33@
�MG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA��A�M�A���Ḁ�A���A�A�K�A��yA�ffA���A�33A���A�1'A�p�A�t�A�n�A��A�%AȰ!A�"�A̴9A�ffA̶FA��/A�;dAɲ-A�JA�^5A�ffA��A�O�A�VA�%A�C�A��HA�|�A�33A�1'A�bA���A���A�
=A��AȲ-A�bNA�  A��;A�$�A�dZA��A��A�ȴA�VA���A�bNA�ȴA�+A��
A�v�A��HA�ĜA�{A7A���A��mA�&�A�ƨA�$�AčPA�dZA��A̧�A���A��A��A��A���A��PA��9A��yA��7AA��/A�ȴA̾wA�;dA��`A�oA�l�A�M�A��A��A�%A£�A�"�A�~�A�S�A���A̟�A�dZA�+A�ĜA�t�A��/A�x�A��A��A�+A��A��A���A�JA�"�A�$�A�1A�x�A�9XA̝�A�ZA�1'Aĺ^A��;A�|�A�7LA�JA�?}A��AŰ!A��yA��mA�VA�VA���A��/A��A�9XA���A���A��A˓uAô9AĴ9A���A��A�oȀ\AōPA�=qAƗ�A���A��AĲ-Aˉ7A�(�A���A���A�`BA��A��A�bA�I�A���AĬA��/A�VA��A�7LA���A�{A�JA�bA��AʮA�p�A���A��/A��A̧�A�7LA�S�AőhAɃA�dZA��A̡�A�%A�1A��
A�+A�v�A��A�JA�^5A�  A��yA�ffA̙�A�&�A��A�`BA˥�A��A��A�XA��A�A�1A��A��A��A˟�A�z�A��A̩�A��A�$�A� �A� �A��A��A��A��A̼jA��A��A��A��A� �A� �A��A��A�{A�"�A��A��AˍPA�oA��A��A� �A�"�A� �A��A�"�A� �A�(�A�$�A�$�A�+A�(�A�(�A�+A�$�A�"�A�$�A�$�A� �A�$�A�"�A�&�A�&�A�&�A�+A�(�A�(�A�(�A�+A�&�A�$�A�"�A��A�+A�/A�+A�&�A�$�A�+A�+A�(�A�-A�+A�&�A�$�A�"�A�(�A�/A�/A�1'A�/A�/A�+A�"�A�/A�1'A�1'A�/A�+A�/A��A��A˶FA�$�A�+A�+A�-A�-A�(�A�/A�-A�-A�+A�-A�33A�33A�ȴA�1'A�33A�7LA�33A�1'Ạ�A�/A�33A���A�33A�33A�1'A�5?A�33A�33A�33A�/A�5?A�/A�+A�/A�33A�5?A�7LA�5?A�33A�7LA�5?A�7LA�7LA�7LA�1'A�1'A�+A�33A�33A�1'A�1'A�-A�33A�5?A�/A�7LA�&�A�/A�1'A�33A�+A�/A�33A�1'A�-A�1'A�/A�(�A�+A̸RA�+A�$�A�1'A�&�A�(�A�/A�-A�&�A�5?A�/A�-A�-A�/A�"�A�1'A�33A�1'A�33A�-A��A��A�/A�9XA�33A�$�A�"�A�"�A�"�A̰!A�&�A�1'A�33A�1'A��A��A�/A�/A�ȴA�+A�-A�5?A�7LA�5?A�5?A�5?A�5?A�1'A�5?A�33A�7LA�7LA�;dA�5?A�9XA�33A�33A�7LA�7LA�7LA�1'A�1'A�7LA�7LA�33A�9XA�33A�7LA�9XA�;dA�9XA�9XA�7LA�7LA�7LA�9XA�5?A�7LA�9XA�9XA�/A�;dA�;dA�7LA�;dA�9XA�5?A���A�33A�/A�9XA�7LA�7LA�;dA�;dA�7LA�;dA�9XA�9XA�7LA�9XA�5?A�A�|�A�1'A�33A�;dA�5?A�;dA�7LA�7LA�;dA�9XA��A�(�A�+A�33A�+A�33A�9XA�;dA�;dA�=qA�=qA�;dA�;dA�9XA�=qA�=qA�9XA�;dA�=qA�;dA�=qA�=qA�=qA�;dA�9XA�;dA�;dA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�9XA�=qA�;dA�;dA�?}A�?}A�=qA�?}A�C�A�A�A�=qA�9XA�5?A�7LA�;dA�5?A�=qA�9XA�9XA�9XA�7LA�1'A�5?A�;dA�5?A�33A�5?A�33A�1'A�5?A�I�A�I�A�G�A�O�A�Q�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�M�A�I�A�G�A�G�A�G�A�O�A�C�A�O�A�C�A�C�A�E�A�K�A�Q�A�C�A�I�A�=qA�G�A�C�A�A�A�=qA�=qA�E�A�I�A�?}A�7LA�33A�7LA�5?A�5?A�33A�1'A�33A�5?A�33A�33A�E�A�7LA�5?A�5?A�5?A�7LA�E�A�;dA�7LA�?}A�?}A�G�A�G�A�;dA�9XA�?}A�?}A�?}A�?}A�C�A�1'A�1'A�9XA�7LA�;dA�1'A�7LA�7LA�A�A�I�A�;dA�I�A�G�A�G�A�E�A�M�A�C�A�Q�A�G�A�I�A�I�A�M�A�O�A�O�A�G�A�O�A�G�A�E�A�K�A�I�A�G�A�7LA�A�A�;dA�;dA�A�A�C�A�?}A�C�A�A�A�?}A�E�A�E�A�O�A�O�A�K�A�K�A�G�A�Q�A�S�A�M�A�G�A�=qA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�?}A�A�A�A�A�E�A�C�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�O�A�K�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�K�A�I�A�G�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A�G�A�G�A�I�A�I�A�K�A�M�A�K�A�M�A�K�A�I�A�K�A�K�A�I�A�K�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�Q�A�I�@�  @�  @�b@�1@�1@�1@�b@� �@�A�@�A�@�Q�@�Q�@�Q�@�Z@�Z@�Z@�bN@�bN@�bN@�bN@�z�@�j@��@��D@��u@��@�z�@�z�@�r�@�r�@�r�@�r�@�j@�Z@�Q�@�A�@�A�@�1'@� �@�b@�1@�1@�1@�  @�  @��@��@�  @���@��@��m@��@��m@��;@��m@��;@��;@��m@�  @�  @�  @���@���@���@��@��m@��;@��;@��;@��;@��;@��
@��
@���@��
@��
@���@���@���@���@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@��w@��F@��@���@���@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��@��A�;dA�=qA�=qA�;dA�=qA�A�A�C�A�9XA�9XA�5?A�5?A�33A�1'A�5?A�5?A�;dA�7LA�9XA�1'A�1'A�33A�1'A�33A�33A�33A�1'A�1'A�5?A�I�A�C�A�E�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�O�A�M�A�O�A�O�A�M�A�K�A�G�A�A�A�A�A�I�A�A�A�C�A�C�A�A�A�E�A�=qA�O�A�A�A�G�A�A�A�K�A�C�A�A�A�E�A�C�A�?}A�9XA�E�A�G�A�?}A�5?A�7LA�1'A�5?A�33A�33A�33A�5?A�5?A�5?A�;dA�5?A�9XA�7LA�33A�7LA�;dA�E�A�7LA�C�A�?}A�;dA�K�A�E�A�E�A�;dA�;dA�9XA�?}A�?}A�;dA�33A�33A�5?A�=qA�5?A�1'A�33A�9XA�G�A�A�A�E�A�I�A�I�A�E�A�E�A�I�A�M�A�Q�A�K�A�M�A�I�A�M�A�O�A�I�A�G�A�K�A�K�A�M�A�E�A�I�A�=qA�E�A�E�A�;dA�=qA�A�A�C�A�?}A�A�A�C�A�A�A�G�A�G�A�E�A�M�A�M�A�M�A�O�A�S�A�O�A�G�A�=qA�=qA�;dA�=qA�;dA�;dA�=qA�;dA�=qA�?}A�A�A�C�A�C�A�E�A�I�A�I�A�I�A�I�A�M�A�M�A�M�A�M�A�M�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�I�A�I�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�I�A�I�A�M�A�M�A�M�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�@�  @�  @�1@�b@�1@�1@�b@� �@�9X@�I�@�Q�@�Z@�I�@�Q�@�Q�@�Z@�bN@�bN@�bN@�bN@�j@�r�@��@��@��u@��u@��@�z�@�r�@�r�@�r�@�r�@�r�@�bN@�Q�@�A�@�A�@�A�@�1'@�b@�b@�1@�1@�  @��@���@��@���@���@���@��@��@��m@��m@��m@��;@��;@��;@��@�  @�  @���@���@���@��@��m@��m@��;@��;@��;@��;@��
@��
@��
@���@��
@���@���@���@���@���@�ƨ@�ƨ@��w@�ƨ@��w@��w@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@���@���@���@���@���@��P@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�=qA�9XA�=qA�K�A�A�A�9XA�?}A�G�A�G�A�C�A�K�A�K�A�K�A�K�A�M�A�M�A�O�A�K�A�G�A�G�A�I�A��A��;A���Ḁ�ȂhA�A�A�z�A���A�dZA�$�A��A�?}AȰ!A�;dAƥ�Aě�A�v�A�n�A��A�r�A�A��A��HA�9XA��A�JA�hsA�
=A���A�\)A�ffA���A��
A�(�A�|�A�A�A�ZA�dZA�;dA��#A�C�A�(�A�bA���A��9A�ȴA�l�A�K�A���A� �A�;dA���A��A��hA�JA�z�A��A�&�A�A�\)A��RA�ƨA���A���A���A�ƨA���A���A��mA�1A�n�A��A��uA���A���A�?}A�33A���A���A���A��DA��`A��A�p�A�ZA�?}A��mA�/A���A���A���A��TA�t�A�ƨA�G�A��HA�O�A��#A�  A�jA~-Az�Aw��AvAtv�Aq�#Apn�Anz�AkK�Ail�Agt�AeC�Ab��A^�jA]K�A[��AX��AV��AUt�ATQ�AR�`AQ`BAP1'AOXAN  AM%AKO�AI��AIS�AH1'AG�7AF^5AD�!AC�wAB�!AA�AA�A?�TA>�uA<�A;��A:�`A:{A9�7A9dZA8�9A8�A7�;A7;dA5�A5\)A5A4bA3/A2-A17LA0{A.��A-7LA+%A)�;A)�FA)"�A(jA'&�A&��A&$�A$�A#��A#p�A"VA!p�A!?}A �DA {A"�AVAO�AE�A�;A��A�An�A�TAĜAG�AbA�hA"�Av�A�-AVA�
A33A�jAXA{A
=Ax�A
r�A
Q�A
 �A	��A	33An�A1A`BA�uA�A{Az�A�FA ��@��w@�"�@���@��P@��@�E�@���@�C�@��@�@�A�@��m@���@�@�V@��@��@���@�9@��@�+@�J@��y@�7@ް!@�t�@��@�v�@�7L@���@�l�@���@ҏ\@�V@�@с@�X@��@���@��@��/@�j@�~�@́@�G�@�/@�7L@�V@�1@���@ʰ!@�^5@��T@�&�@�E�@���@�
=@���@��y@�+@��y@��j@�E�@��@�t�@�@���@��@�b@��@�%@�A�@��m@��P@�-@���@��
@���@�33@���@�E�@�@� �@�l�@�
=@��R@�v�@��T@�x�@�&�@��j@��@�bN@�I�@���@�\)@�"�@���@�V@�E�@��@�X@�&�@��@��@��9@�j@�I�@�(�@�1@���@��@�"�@�^5@���@�x�@�V@���@��j@��@�(�@���@�dZ@�33@�o@���@��!@���@���@��\@�~�@��+@�V@�E�@�E�@��T@���@�`B@�hs@�p�@�X@�`B@�x�@�@���@���@�n�@��R@���@���@�;d@�C�@��H@��@���@���@��\@�V@�=q@��7@�V@��`@���@��`@��j@�b@���@��w@���@�|�@�\)@�C�@�|�@�A�@�I�@�1'@� �@��@���@�|�@�C�@�
=@��@���@�v�@���@�z�@�|�@�v�@���@���@�5?@�$�@��T@�Ĝ@��;@��;@�ƨ@���@��@��7@�%@���@���@���@�9X@��@�C�@�
=@�
=@�o@���@���@��\@��\@���@���@���@���@�M�@��#@��-@��@��@��@��@��@���@�9X@� �@�b@��;@��F@�K�@�"�@���@��!@���@�ȴ@���@�n�@�M�@���@�X@�?}@��@��
@�  @�bN@�Z@���@��@���@���G�O�@�dZ@�=�@x:�@q�7@k9�@`9X@Y \@Rȴ@L�|@EF@=��@7��@2�@+��@&~�@!��@��@g8@�V@33@
�MG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA��A�M�A���Ḁ�A���A�A�K�A��yA�ffA���A�33A���A�1'A�p�A�t�A�n�A��A�%AȰ!A�"�A̴9A�ffA̶FA��/A�;dAɲ-A�JA�^5A�ffA��A�O�A�VA�%A�C�A��HA�|�A�33A�1'A�bA���A���A�
=A��AȲ-A�bNA�  A��;A�$�A�dZA��A��A�ȴA�VA���A�bNA�ȴA�+A��
A�v�A��HA�ĜA�{A7A���A��mA�&�A�ƨA�$�AčPA�dZA��A̧�A���A��A��A��A���A��PA��9A��yA��7AA��/A�ȴA̾wA�;dA��`A�oA�l�A�M�A��A��A�%A£�A�"�A�~�A�S�A���A̟�A�dZA�+A�ĜA�t�A��/A�x�A��A��A�+A��A��A���A�JA�"�A�$�A�1A�x�A�9XA̝�A�ZA�1'Aĺ^A��;A�|�A�7LA�JA�?}A��AŰ!A��yA��mA�VA�VA���A��/A��A�9XA���A���A��A˓uAô9AĴ9A���A��A�oȀ\AōPA�=qAƗ�A���A��AĲ-Aˉ7A�(�A���A���A�`BA��A��A�bA�I�A���AĬA��/A�VA��A�7LA���A�{A�JA�bA��AʮA�p�A���A��/A��A̧�A�7LA�S�AőhAɃA�dZA��A̡�A�%A�1A��
A�+A�v�A��A�JA�^5A�  A��yA�ffA̙�A�&�A��A�`BA˥�A��A��A�XA��A�A�1A��A��A��A˟�A�z�A��A̩�A��A�$�A� �A� �A��A��A��A��A̼jA��A��A��A��A� �A� �A��A��A�{A�"�A��A��AˍPA�oA��A��A� �A�"�A� �A��A�"�A� �A�(�A�$�A�$�A�+A�(�A�(�A�+A�$�A�"�A�$�A�$�A� �A�$�A�"�A�&�A�&�A�&�A�+A�(�A�(�A�(�A�+A�&�A�$�A�"�A��A�+A�/A�+A�&�A�$�A�+A�+A�(�A�-A�+A�&�A�$�A�"�A�(�A�/A�/A�1'A�/A�/A�+A�"�A�/A�1'A�1'A�/A�+A�/A��A��A˶FA�$�A�+A�+A�-A�-A�(�A�/A�-A�-A�+A�-A�33A�33A�ȴA�1'A�33A�7LA�33A�1'Ạ�A�/A�33A���A�33A�33A�1'A�5?A�33A�33A�33A�/A�5?A�/A�+A�/A�33A�5?A�7LA�5?A�33A�7LA�5?A�7LA�7LA�7LA�1'A�1'A�+A�33A�33A�1'A�1'A�-A�33A�5?A�/A�7LA�&�A�/A�1'A�33A�+A�/A�33A�1'A�-A�1'A�/A�(�A�+A̸RA�+A�$�A�1'A�&�A�(�A�/A�-A�&�A�5?A�/A�-A�-A�/A�"�A�1'A�33A�1'A�33A�-A��A��A�/A�9XA�33A�$�A�"�A�"�A�"�A̰!A�&�A�1'A�33A�1'A��A��A�/A�/A�ȴA�+A�-A�5?A�7LA�5?A�5?A�5?A�5?A�1'A�5?A�33A�7LA�7LA�;dA�5?A�9XA�33A�33A�7LA�7LA�7LA�1'A�1'A�7LA�7LA�33A�9XA�33A�7LA�9XA�;dA�9XA�9XA�7LA�7LA�7LA�9XA�5?A�7LA�9XA�9XA�/A�;dA�;dA�7LA�;dA�9XA�5?A���A�33A�/A�9XA�7LA�7LA�;dA�;dA�7LA�;dA�9XA�9XA�7LA�9XA�5?A�A�|�A�1'A�33A�;dA�5?A�;dA�7LA�7LA�;dA�9XA��A�(�A�+A�33A�+A�33A�9XA�;dA�;dA�=qA�=qA�;dA�;dA�9XA�=qA�=qA�9XA�;dA�=qA�;dA�=qA�=qA�=qA�;dA�9XA�;dA�;dA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�9XA�=qA�;dA�;dA�=qA�=qA�;dA�=qA�A�A�C�A�9XA�9XA�5?A�5?A�33A�1'A�5?A�5?A�;dA�7LA�9XA�1'A�1'A�33A�1'A�33A�33A�33A�1'A�1'A�5?A�I�A�C�A�E�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�O�A�M�A�O�A�O�A�M�A�K�A�G�A�A�A�A�A�I�A�A�A�C�A�C�A�A�A�E�A�=qA�O�A�A�A�G�A�A�A�K�A�C�A�A�A�E�A�C�A�?}A�9XA�E�A�G�A�?}A�5?A�7LA�1'A�5?A�33A�33A�33A�5?A�5?A�5?A�;dA�5?A�9XA�7LA�33A�7LA�;dA�E�A�7LA�C�A�?}A�;dA�K�A�E�A�E�A�;dA�;dA�9XA�?}A�?}A�;dA�33A�33A�5?A�=qA�5?A�1'A�33A�9XA�G�A�A�A�E�A�I�A�I�A�E�A�E�A�I�A�M�A�Q�A�K�A�M�A�I�A�M�A�O�A�I�A�G�A�K�A�K�A�M�A�E�A�I�A�=qA�E�A�E�A�;dA�=qA�A�A�C�A�?}A�A�A�C�A�A�A�G�A�G�A�E�A�M�A�M�A�M�A�O�A�S�A�O�A�G�A�=qA�=qA�;dA�=qA�;dA�;dA�=qA�;dA�=qA�?}A�A�A�C�A�C�A�E�A�I�A�I�A�I�A�I�A�M�A�M�A�M�A�M�A�M�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�I�A�I�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�I�A�I�A�M�A�M�A�M�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�@�  @�  @�1@�b@�1@�1@�b@� �@�9X@�I�@�Q�@�Z@�I�@�Q�@�Q�@�Z@�bN@�bN@�bN@�bN@�j@�r�@��@��@��u@��u@��@�z�@�r�@�r�@�r�@�r�@�r�@�bN@�Q�@�A�@�A�@�A�@�1'@�b@�b@�1@�1@�  @��@���@��@���@���@���@��@��@��m@��m@��m@��;@��;@��;@��@�  @�  @���@���@���@��@��m@��m@��;@��;@��;@��;@��
@��
@��
@���@��
@���@���@���@���@���@�ƨ@�ƨ@��w@�ƨ@��w@��w@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@���@���@���@���@���@��P@��A�;dA�=qA�=qA�;dA�=qA�A�A�C�A�9XA�9XA�5?A�5?A�33A�1'A�5?A�5?A�;dA�7LA�9XA�1'A�1'A�33A�1'A�33A�33A�33A�1'A�1'A�5?A�I�A�C�A�E�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�O�A�M�A�O�A�O�A�M�A�K�A�G�A�A�A�A�A�I�A�A�A�C�A�C�A�A�A�E�A�=qA�O�A�A�A�G�A�A�A�K�A�C�A�A�A�E�A�C�A�?}A�9XA�E�A�G�A�?}A�5?A�7LA�1'A�5?A�33A�33A�33A�5?A�5?A�5?A�;dA�5?A�9XA�7LA�33A�7LA�;dA�E�A�7LA�C�A�?}A�;dA�K�A�E�A�E�A�;dA�;dA�9XA�?}A�?}A�;dA�33A�33A�5?A�=qA�5?A�1'A�33A�9XA�G�A�A�A�E�A�I�A�I�A�E�A�E�A�I�A�M�A�Q�A�K�A�M�A�I�A�M�A�O�A�I�A�G�A�K�A�K�A�M�A�E�A�I�A�=qA�E�A�E�A�;dA�=qA�A�A�C�A�?}A�A�A�C�A�A�A�G�A�G�A�E�A�M�A�M�A�M�A�O�A�S�A�O�A�G�A�=qA�=qA�;dA�=qA�;dA�;dA�=qA�;dA�=qA�?}A�A�A�C�A�C�A�E�A�I�A�I�A�I�A�I�A�M�A�M�A�M�A�M�A�M�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�I�A�I�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�I�A�I�A�M�A�M�A�M�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�@�  @�  @�1@�b@�1@�1@�b@� �@�9X@�I�@�Q�@�Z@�I�@�Q�@�Q�@�Z@�bN@�bN@�bN@�bN@�j@�r�@��@��@��u@��u@��@�z�@�r�@�r�@�r�@�r�@�r�@�bN@�Q�@�A�@�A�@�A�@�1'@�b@�b@�1@�1@�  @��@���@��@���@���@���@��@��@��m@��m@��m@��;@��;@��;@��@�  @�  @���@���@���@��@��m@��m@��;@��;@��;@��;@��
@��
@��
@���@��
@���@���@���@���@���@�ƨ@�ƨ@��w@�ƨ@��w@��w@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@���@���@���@���@���@��P@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>P#d@�Vm@�]�>(��@U�s@�U@��@8��>�W@#O@2>/��@�M�@�S�=�v�=��x> ��?�d�=�BF>��X@�n�@�B�>#,�@�U�@�[W>� >e��@�V�>�V>iv=�0�>&�5@�N�@�)_>G�>��
@�U=�Ѣ>6��@�R~@oT=�(?R�"@�D@�_p=���=�MU=��@C�*?!�>���@}\}>��/>��@]#�>�k{=��>�a�=���>���?�Z�>0�8@�Z=��?��?@�\�>$�?
�l@�^?�M�>J�|@�[�@�ag=o
�=���=If{=��c=�s>D|=f�;=��m?Cc�=�8�> [�?��@@�V>��_?��@�Z@�S�> �H@�[�@�V�@�QY>��@�s�=���=�(>M��@�[�>��=��>W�?��@�d�>�T�@�W�?@�?G]y>#a@�H�@�O�>ł><��@�\�@�]O=�~�>7��@�OL=���=̭X>�@%��=�>9&?�˧=��R=�9?
��@�=�5?>�a�>�k@th>s9@�\>@�[�=���=��|>*?<��?�[�=�c�>)��@�Zq@�^t@�{�=���>c�4>
�?�&l@�N>���?�~R@[Y=�_�=�p�>H�b@�Y�@�[�@�\>?B[>�Z@U�?OA�@�`B@�V�>�y?��@�]d@�\>@�~�=�%�?�$@�]?�w2@���?5�@�`->y>?�	->��@���>�<�@�\�@�[�@�_?�ί>6bx?�]�>���@�_p@�b�> ��@4w�@��c@��K@j��@��@�Q�@G�I?��@���@�`->�?}@�bN@�a�?P�@�c5@�bx@�c@x�@yIR@�c5@�`�?��@�e�@�f@�ek@�f�@�f�@�a�@�d�@�b$@�a�@�c@�c@�c�@�c5@�c5@�b@�`�@�c�@�ek@�ek@�d�@��<@�dE@�f@�e�@�f{@�e�@�e�@�e�@�d�@�eA@�g�@�g�@�f{@�g�@�g�@�hI@�f{@�g#@�f@�fQ@�ek@�f@�e@�f�@�gb@�f�@�f@�g�@�hI@�i�@�h�@�i�@�h�@�g�@�hI@�h�@�i@�hI@�g�@�f�@�fQ@�i�@�i�@�h�@�i�@�f�@�fQ@�e�@�g�@�i@�in@�i�@�j@�j@�j@�i�@�i�@�j@@�j@�j@�h�@�hs@�i�@�c�@�e�@��@�j@�i�@�i�@�j@�j�@�i�@�k'@�j@�j@�i�@�k�@�lL@�k�@���@�k'@�k�@�kQ@�l@�lL@U�@�k�@�in@�hI@�kQ@�k�@�k�@�lL@�k�@�l@�lv@�lL@�l�@�k'@�j�@�l@�lL@�l�@�l�@�m�@�m�@�l�@�m]@�m]@�m]@�nD@�j@�k�@�k'@�k'@�lv@�lv@�k'@�j�@�j@�kQ@�kQ@�k�@�lL@�j�@�k�@�l@�j@�in@�j@�i�@�i�@�j@�j�@�g�@�f{@�gb@�gb@�f{@�j@�i@�in@�j@@�gb@�h�@�k�@�i�@�i�@�j@@�k'@�k�@�m	@�m�@�j@@�i�@�j@�d�@�d�@�l�@�m	@�j@�f�@�f�@�hs@�gb@�e@�k'@�hI@�k�@�gb@�c�@�i�@�g�@�lL@���@�j@�k�@�m]@�m�@�m�@�m	@�l�@�m	@�l�@�lv@�l�@�m�@�n�@�n�@�nD@�m�@�m]@�m	@�m�@�m�@�m�@�l�@�m]@�m�@�l�@�m@�lL@�n�@�n�@�n�@�n�@�n@�n@�m�@�m�@�n�@�m�@�n@�n�@�n@�m�@�lv@�n�@�n�@�n�@�m�@�oT@�j@�>�@�m	@�l�@�n�@�n�@�nD@�o*@�o*@�n�@�n�@�o @�n�@�o*@�n@�n�@�k�@���@�o*@�o*@�o*@�n�@�o @�n@�m]@�n�@�nD@�h@�j@�j�@�l�@�g#@�o*@�o�@�o�@�o�@�o�@�o�@�o�@�o*@�o�@�o�@�o�@�o?@�o�@�p@�o�@�o�@�o�@�pP@�oT@�o�@�o�@�o�@�o�@�o�@�o�@�pP@�o�@�pP@�p@�p@�q@�q@�qa@�qv@�q@�qa@�q�@�q�@�r�@�pP@�oT@�o�@�pe@�q7@�o?@�o�@�o�@�o�@�o�@�oi@�o @�p�@�o�@�pz@�p@�pP@�o�@�o�@�o?@�uy@�w�@�w�@�v�@�z:@�z�@�z�@�z�@�z�@�z:@�y�@�y}@�y@�y}@�y}@�y@�y@�y@�x@�x@�v�@�v�@�w�@�y�@�x@�xl@�v6@�w\@�x@�x�@�v�@�v�@�v�@�v�@�u�@�v�@�uy@�sX@�ti@�ti@�t�@�t�@�s@�pe@�p@�p�@�p�@�p@�pe@�q�@�q"@�q�@�s�@�s@�rG@�r\@�q�@�rG@�ti@�v�@�t@�sm@�u�@�w@�xl@�u�@�ti@�t~@�u�@�s�@�u:@�u�@�s�@�p�@�sm@�s�@�s@�rG@�r�@�t�@�t~@�v�@�wp@�y>@�z�@�z�@�zN@�zN@�z�@�zN@�{_@�z�@�z�@�z�@�{_@�zN@�{_@�zN@�{@�y�@�y�@�z�@�zN@�w@�v�@�wp@�v�@�x@�x�@�y�@�x�@�y>@�x�@�y�@�{@�{�@�|�@�}�@�}A@�|�@�~R@�~�@�~�@�|1@�y�@�x�@�w�@�w�@�wp@�w�@�w�@�x�@�yS@�x�@�zc@�zc@�{@�{@�|�@�}A@�|�@�|�@�}�@�}�@�~g@�~g@�}�@�|�@�|�@�|�@�}V@�}V@�}V@�}V@�|�@�}V@�|�@�|�@�}V@�|�@�~g@�~�@�~g@�~@�}V@�}V@�}@�}@�~@�~g@�~g@�~�@�~g@�~g@�~�@�~g@�~@�}�@�~@�~g@�$@�$@�$@��@��@��4@��@���@���@���@���@��I@Q�*@Q�z@Q�"@Q��@Q�@Q�@Q�`@Q��@Q�p@Q��@Q��@Q��@Q�c@Q�4@Q�0@Q�V@Q��@Q��@Q��@Q��@Q�n@Q��@Q�@Q��@Q��@Q�<@Q�@Q�n@Q�@Q��@Q��@Q��@Q��@Q�9@Q�@Q�p@Q��@Q��@Q��@Q�@Q�d@Q�:@Q�@Q�?@Q�C@Q��@Q��@Q��@Q�@Q��@Q�q@Q�q@Q�q@Q�q@Q��@Q��@Q�h@Q�1@Q�-@Q��@Q�-@Q��@Q�@Q��@Q�6@Q��@Q��@Q��@Q�:@Q��@Q�?@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q�L@Q��@Q��@Q�L@Q�P@Q� @Q�Y@Q��@Q� @Q�T@Q��@Q�T@Q�T@Q� @Q� @Q��@Q� @Q��@Q� @Q��@Q��@Q��@Q�@Q�Y@Q��@Q�	@Q��@Q�]@Q��@Q��@Q��@Q��@Q��@Q�]@Q��@Q�]@Q��@Q��@Q��@���@���@���@��`@��@���@���@���@���@���@��}@���@���@���@��S@��p@���@���@��C@��@�� @��@���@�� @���@���@���@��C@��F@��5@���@���@��x@��I@���@��s@��^@��
@���@���@���@��@��^@���@��$@���@��9@��@��g@��x@���@��@��c@���@��@��s@��t@���@���@��@���@���@���@���@���@���@���@��=@���@��l@���@���@���@���@���@���@��@��@��`@���@��F@���@��-@���@��1@���@���@���@���@��_@��B@���@���@���@��c@���@���@��9@���@��9@���@���@���@���@���@���@��@��$@��9@���@���@���@���@���@���@��4@���@��'@��0@���@���@��@��'@���@���@��Z@���@���@��N@���@���@���@���@���@��_@���@���@��V@���@���@��k@���@���@���@���@��'@���@���@���@���@���@��A@���@���@��1@��@��1@��p@��1@��@���@��g@��@��x@���@��@��Z@���@���@��@��'@��'@��Q@��k@��E@��@���@��V@��@���@��@���@��V@��@��k@��o@��E@��Q@��@���@��#@���@��{@���@��E@��o@���@���@��M@���@���@��V@��V@���@���@��A@���@���@���@��@��{@��#@���@���@���@��8@��@���@Q�@Q�<@Q��@Q��@Q�7@Q��@Q�]@Q�z@Q��@Q�t@Q��@Q��@Q��@Q��@Q�A@Q�@Q�
@Q�^@Q�
@Q��@Q�,@Q�@Q��@Q�e@Qı@QŬ@Q��@Q�j@Q��@Q��@Q�D@Q�@Q��@Q��@Q��@Q�A@Q��@Q��@Q�N@Q��@Q��@Q�@Q��@Q��@Q�z@Q�u@Q��@Q�G@Q�G@Q��@Q�z@Q�P@Q�P@Q�z@Q��@Q��@Q�~@Q��@Q�@Q�@Q��@Q�@Q�`@Q�`@Q��@Q�@Q�h@Q��@Q��@Q��@Q�m@Q��@Q�@Q�@Q��@Q�@Q��@Q��@Q��@Q�u@Q��@Q��@Q��@Q��@Q��@Q��@Q�T@Q�]@Q�7@Q�@Q��@Q��@Q�]@Q�3@Q�]@Q�3@Q�3@Q��@Q��@Q��@Q�	@Q�	@Q�	@Q��@Q�a@Q�a@Q�<@Q�e@Q�e@Q�@Q�@Q��@Q�e@Q��@Q��@Q��@Q��@Q��@Q�@Q��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             433433444444334444443343344344443344344344433444444344344444443443443443344444444444434433433343444344443434443344334434444444444444434334444444333444434434443334434334433344343434443433344443344333334433433433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Vl@�]�G�O�@U�p@�UG�O�G�O�G�O�G�O�G�O�G�O�@�M�@�S�G�O�G�O�G�O�G�O�G�O�G�O�@�n�@�B�G�O�@�U�@�[WG�O�G�O�@�V�G�O�G�O�G�O�G�O�@�N�@�)^G�O�G�O�@�VG�O�G�O�@�RG�O�G�O�G�O�@�D@�_qG�O�G�O�G�O�G�O�G�O�G�O�@}\}G�O�G�O�@]#�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ZG�O�G�O�@�\�G�O�G�O�@�^G�O�G�O�@�[�@�aiG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�VG�O�G�O�@�Z@�S�G�O�@�[�@�V�@�QZG�O�@�s�G�O�G�O�G�O�@�[�G�O�G�O�G�O�G�O�@�d�G�O�@�W�G�O�G�O�G�O�@�H�@�O�G�O�G�O�@�\�@�]NG�O�G�O�@�OLG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@tcG�O�@�\=@�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Zq@�^s@�{�G�O�G�O�G�O�G�O�@�NG�O�G�O�@[YG�O�G�O�G�O�@�Y�@�[�@�\:G�O�G�O�@U�G�O�@�`A@�V�G�O�G�O�@�]b@�\A@�~�G�O�G�O�@�]G�O�@���G�O�@�`.G�O�G�O�G�O�@���G�O�@�\�@�[�@�_G�O�G�O�G�O�G�O�@�_o@�b�G�O�G�O�@��e@��J@j��@��@�Q�G�O�G�O�@���@�`.G�O�@�bN@�a�G�O�@�c4@�bv@�c@x�@yIN@�c4@�`�G�O�@�e�@�f@�en@�f�@�f�@�a�@�d�@�b@�a�@�c@�c
@�c�@�c8@�c8@�b
@�`�@�c�@�ej@�ek@�d�@��=@�dF@�f@�e�@�f@�e�@�e�@�e�@�d�@�eB@�g�@�g�@�f~@�g�@�g�@�hK@�f|@�g$@�f@�fS@�ek@�f@�e @�f�@�gg@�f�@�f@�g�@�hK@�i�@�h�@�i�@�h�@�g�@�hK@�h�@�i@�hJ@�g�@�f�@�fQ@�i�@�i�@�h�@�i�@�f�@�fO@�e�@�g�@�i@�il@�i�@�j@�j~@�j~@�i�@�i�@�jA@�j@�j@�h�@�hv@�i�@�c�@�e�@��@�j~@�i�@�i�@�j@�j�@�i�@�k'@�j~@�j~@�i�@�k�@�lN@�k�@���@�k'@�k�@�kS@�l@�lK@U� @�k�@�io@�hK@�kR@�k�@�k�@�lJ@�k�@�l@�lw@�lR@�l�@�k*@�j�@�l@�lN@�l�@�l�@�m�@�m�@�l�@�m]@�m[@�m[@�nC@�j~@�k�@�k-@�k'@�lw@�lw@�k'@�j�@�j�@�kQ@�kT@�k�@�lR@�j�@�k�@�l@�j@�il@�j�@�i�@�i�@�j@�j�@�g�@�f}@�gf@�gd@�f|@�j~@�i@�io@�jA@�gb@�h�@�k�@�i�@�i�@�jB@�k'@�k�@�m@�m�@�j>@�i�@�j@�d�@�d�@�l�@�m
@�j}@�f�@�f�@�hq@�gf@�e@�k'@�hH@�k�@�gb@�c�@�i�@�g�@�lN@���@�j�@�k�@�m`@�m�@�m�@�m@�l�@�m@�l�@�lx@�l�@�m�@�n�@�n�@�nI@�m�@�md@�m@�m�@�m�@�m�@�l�@�m[@�m�@�l�@�m@�lJ@�n�@�n�@�n�@�n�@�n@�n@�m�@�m�@�n�@�m�@�n@�n�@�n@�m�@�lx@�n�@�n�@�n�@�m�@�oY@�j@�>�@�m	@�l�@�n�@�n�@�nF@�o*@�o*@�n�@�n�@�o @�n�@�o0@�n@�n�@�k�@���@�o(@�o(@�o.@�n�@�o@�n@�m]@�n�@�nA@�h @�j~@�j�@�l�@�g"@�o+@�o�@�o�@�o�@�o�@�o�@�o�@�o*@�o�@�o�@�o�@�o<@�o�@�p@�o�@�o�@�o�@�pQ@�oT@�o�@�o�@�o�@�o�@�o�@�o�@�pN@�o�@�pR@�p@�p@�q@�q@�qc@���@���@���@��_@��@���@���@���@���@���@���@���@���@���@��W@��r@���@���@��D@��@��@��@���@��@���@���@���@��F@��B@��:@���@���@��|@��L@���@��v@��]@��@���@���@���@�� @��`@���@��%@���@��=@��
@��h@��|@���@��@��g@���@��@��v@��v@���@���@��@���@���@���@���@���@���@���@��?@���@��m@���@���@���@���@���@���@��@��@��_@���@��H@���@��0@���@��5@���@���@���@���@��f@��D@���@���@���@��f@���@���@��=@���@��;@���@���@���@���@���@���@��	@��%@��>@���@���@���@���@���@���@��4@���@��&@��.@���@���@��@��&@���@���@��\@���@���@��Q@���@���@���@���@���@��`@���@���@��S@���@���@��i@���@���@���@���@��*@���@���@���@���@���@��A@���@���@��6@��@��2@��s@��2@��@���@��d@��@��z@���@��@��Z@���@���@��@��&@��'@��O@��l@��B@��@���@��X@��@���@��@���@��W@��@��i@��n@��G@��S@��@���@��"@���@��{@���@��B@��o@���@���@��I@���@���@��X@��V@���@���@��A@���@���@���@��@��{@��@���@���@���@��6@��@���@Q�@Q�=@Q��@Q��@Q�6@Q��@Q�]@Q�x@Q��@Q�s@Q��@Q��@Q��@Q��@Q�E@Q�@Q�@Q�^@Q�@Q��@Q�+@Q�"@Q��@Q�f@QĶ@QŮ@Q��@Q�k@Q��@Q��@Q�F@Q�@Q��@Q��@Q��@Q�B@Q��@Q��@Q�R@Q��@Q��@Q�@Q��@Q��@Q�{@Q�u@Q��@Q�F@Q�C@Q��@Q�x@Q�R@Q�R@Q�~@Q��@Q��@Q��@Q��@Q�@Q�
@Q��@Q�@Q�`@Q�b@Q��@Q�@Q�f@Q��@Q��@Q��@Q�n@Q��@Q�@Q�@Q��@Q�@Q��@Q��@Q��@Q�v@Q��@Q��@Q��@Q��@Q��@Q��@Q�S@Q�`@Q�6@Q�@Q��@Q��@Q�Z@Q�2@Q�^@Q�3@Q�0@Q��@Q��@Q��@Q�
@Q�@Q�@Q��@Q�c@Q�c@Q�;@Q�h@Q�e@Q�@Q�@Q��@Q�h@Q��@Q��@Q��@Q��@Q��@Q�@Q��@���@���@���@��_@��@���@���@���@���@���@���@���@���@���@��W@��r@���@���@��D@��@��@��@���@��@���@���@���@��F@��B@��:@���@���@��|@��L@���@��v@��]@��@���@���@���@�� @��`@���@��%@���@��=@��
@��h@��|@���@��@��g@���@��@��v@��v@���@���@��@���@���@���@���@���@���@���@��?@���@��m@���@���@���@���@���@���@��@��@��_@���@��H@���@��0@���@��5@���@���@���@���@��f@��D@���@���@���@��f@���@���@��=@���@��;@���@���@���@���@���@���@��	@��%@��>@���@���@���@���@���@���@��4@���@��&@��.@���@���@��@��&@���@���@��\@���@���@��Q@���@���@���@���@���@��`@���@���@��S@���@���@��i@���@���@���@���@��*@���@���@���@���@���@��A@���@���@��6@��@��2@��s@��2@��@���@��d@��@��z@���@��@��Z@���@���@��@��&@��'@��O@��l@��B@��@���@��X@��@���@��@���@��W@��@��i@��n@��G@��S@��@���@��"@���@��{@���@��B@��o@���@���@��I@���@���@��X@��V@���@���@��A@���@���@���@��@��{@��@���@���@���@��6@��@���@Q�@Q�=@Q��@Q��@Q�6@Q��@Q�]@Q�x@Q��@Q�s@Q��@Q��@Q��@Q��@Q�E@Q�@Q�@Q�^@Q�@Q��@Q�+@Q�"@Q��@Q�f@QĶ@QŮ@Q��@Q�k@Q��@Q��@Q�F@Q�@Q��@Q��@Q��@Q�B@Q��@Q��@Q�R@Q��@Q��@Q�@Q��@Q��@Q�{@Q�u@Q��@Q�F@Q�C@Q��@Q�x@Q�R@Q�R@Q�~@Q��@Q��@Q��@Q��@Q�@Q�
@Q��@Q�@Q�`@Q�b@Q��@Q�@Q�f@Q��@Q��@Q��@Q�n@Q��@Q�@Q�@Q��@Q�@Q��@Q��@Q��@Q�v@Q��@Q��@Q��@Q��@Q��@Q��@Q�S@Q�`@Q�6@Q�@Q��@Q��@Q�Z@Q�2@Q�^@Q�3@Q�0@Q��@Q��@Q��@Q�
@Q�@Q�@Q��@Q�c@Q�c@Q�;@Q�h@Q�e@Q�@Q�@Q��@Q�h@Q��@Q��@Q��@Q��@Q��@Q�@Q��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             433433444444334444443343344344443344344344433444444344344444443443443443344444444444434433433343444344443434443344334434444444444444434334444444333444434434443334434334433344343434443433344443344333334433433433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��9�`9��9�79��9�t9 �9��9��9�9��9�9�9��9�m9�)9��9��9�z9�C9�!9�C9��9�#9�9��9�9�|9f9}9 9g9=9�9*999�9b9�9b9�99`9�9�9�9S9J9�99X9�9�9��99�9�9�9 9E9#99#9F9�O9�9&99 9��9��9��9��9��9��9��9��9�79 c9�9��9��9��9��9��9�9�s9�9�9��9K9�9�9�9�9 P9�9C9�9��9��9�n9R9 ?9��9��9�99�9�9�9�9�9�9�9^9	w9�9V9>9�9	w9�9�9�9:9�99�9�9�9�9�9�9�9�9V9�9�9j9�9^9a9�9	{9-9
9
�9
!9�9F9 9�9[9D9X9�9X9#9�9G9�9;9R9�9�99-9	f9	w9	x9	�9�9�9�9r9�9x99v9?9�9v9�9�9�9	�9	g9
!9
U9
29	�9	�9�9�9	T9
�9
x9�9	-9�9�9R9R9�9�9	9	19�9	�9
S9	�9	�9
29
g99
�8��8��@8���8��I8��8���8��8���8���8���8���8���8���8���8��!8���8���8���8���8��J8��q8��8��Y8���8���8���8��\8��8���8��W8��8���8���8��8��H8��8���8���8���8��y8��t8��8���8��8���8���8��&8��r8��o8��&8���8���8���8���8��8��$8�� 8��m8��8���8��8���8��8��8��x8���8��Q8���8���8���8��v8���8��K8��N8��8��N8��*8��8��N8���8��L8��I8��L8��K8��G8��m8���8��!8��8���8���8���8��8���8�� 8���8���8���8���8���8���8���8���8���8��C8��C8��>8��e8��c8��8��8���8��e8���8���8���8���8���8��8���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B� B� B� B� B� B� B� B� B~�B~�B~�Bw�Br�Bo�Bk�BiyBdZB\)BW
BL�BL�B]/B^5BZBiyB�1Bz�Bv�B�\B�+BɺB�B�sB�B�
B�#B�)B�BB�B��B�B33B33B2-B1'B,B&�B%�B)�B.B)�B!�B(�B-B0!B)�B�B1B��B�B�5B��B�B�sB�B�B�`B��B�B��B�^B��B�7BgmBG�B  B��B��BB	7B�B��BǮB��B+B�B�BB��B�HB�^B�3B��B�PB�JB�=B�+B� Bm�BcTBQ�B>wB/B!�BVB
�B
��B
�RB
��B
�=B
l�B
E�B
(�B
\B
B	�B	�NB	�B	ɺB	�9B	��B	��B	�1B	s�B	ZB	O�B	B�B	1'B	%�B	�B	�B	PB	%B	B��B��B�B�B�`B�ZB�TB�NB�BB�;B�5B�#B�#B�B��B��B��B��B��BɺBȴBǮBƨBŢBB��B�wB�jB�^B�LB�9B�!B�B�B��B��B��B��B�{B�oB�bB�JB�=B�1B�B~�B|�Bz�By�Bx�Bv�Bt�Bp�Bn�Bk�BiyBhsBgmBe`BbNB`BB\)BYBVBT�BR�BP�BM�BJ�BH�BF�BD�BA�B>wB<jB<jB<jB;dB:^B9XB9XB7LB5?B33B0!B-B)�B(�B&�B#�B!�B �B�B �B!�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B#�B$�B!�B)�B5?B49B.B/B)�B!�B�B�B�B�B!�B#�B#�B$�B$�B%�B%�B%�B&�B&�B)�B-B/B2-B5?B7LB;dB?}B>wB>wB?}B=qB8RB:^BA�BE�BI�BN�BS�BXB^5B`BBdZBhsBiyBjBm�Bt�Bx�B|�B}�B~�B�B�DB�bB�hB�uB��B��B��B��B��B��B��B�B�B�-B�9B�LB�RB�XB�XB�jB�wB��BĜBǮBǮBɺB��B��B��B��B��B��B�B�B�)B�5B�;B�NB�B�B�B��B��B��B��B	B	B	1B	
=B	DB	VB	bB	hB	uB	�B	�B	�B	#�B	)�B	,B	2-B	5?B	8RB	;dB	<jB	=qB	?}B	A�B	L�B	P�B	T�B	_;B	e`B	iyB	k�B	w�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�VB	�\B	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�LB	�}B	ÖB	ĜB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�HB	�TB	�TB	�NB	�NB	�HB	�BB	�5B	�;B	�NB	�TB	�TB	�`B	�fB	�mB	�mB	�B	��B
�B
�B
EB
_B
�B
'�B
.�B
9	B
>�B
G�B
L�B
Q�B
V�B
[�B
^�B
b�B
gB
k6B
q'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��	BW�B�j?VV�A��Ba�AkH�A��(?5��Ah]�A^ql?]R�BZ�B��>¾'>�o�?#��@���?
x?�"BB�LBm$?O!�B}�B:J?&��?�OfB]�@�?�z?�[?TLIBU�B�?F�?Ő�A�/?
��?f1rBX2A]��?t?@�M�BF@B>��?4�?EA��@@]@�@��Aǳ5?�?1�A�@r?��z>���@�N>�( ?ҊHA+�?_�B]�?Q	@�*BqB?*��@:;�BZ�@�k�?��B\B�>�s>�/>|�s>�^->�A�?4��>�C>�)@��?
�?!q�@�sB{
?��@���B^\B_F?J�&B[�BV�BZ�?)A�>��?~A?��B��?���?$�?1�A$,B��@$92BXc@�d�@�>@?M�iBLB]�?$�?m�BY�Be3?�?h\�B�x>�?? !\?EW&Azھ?�?iX!@�>��M?�@;��AX��>ǣ�@*�Z?JAûM?��qB](B	>�Ǌ>�;?0a�@��x@خ�>�r�?U_�B\SBbFB
�)?�)?���?3p�A�_BQ�?��A51cA��>�aN?	�@?~�B\pB\Ba@�o�?&0�A���@���Be�Bg0?���@�TcB`�Bb�Bs�?;�A	B�A^�B	�@x�B��?D1�A$��?D��Aю�?�
�B^JB�pBg�A4�B?f��A�I?��
Ba�Bh�?!w�A�gYB �BA�B�A�?9A�FB.A�}�@� oB�0BbW?뒟BaBk�@I�LBb�BbBcdA�4>A�V+Ba�B��@��Ba�Bc�Bc+BfBeBbBd�B��Ba^BcdBa�BcBa$Ba$Ba�B_�Bf�BbjBd�Bc5BҷBg�Bf#Be�Bd%Bb�BcxBdBBa�BcBbDBc�Bb�BaTBbBb�B`4Bc+Bb�BbsBa�Bc�Ba/Bc�Bb�Ba�BagBazBb�Bc�Bc+Bc5Bc�Bc�BeBg%Bb�B`GBarBbBbkBb�Bc,Bb�BbbB`�Ba�Ba�BdUBcYBaTBa_BaBbEBbNBc5Bf�BbBa,BaB`�Bb	Ba�BcBe�A׵�Bf?Bc5Bc5Bb�Bc+BdBb�BcBcBb�Bd0BbaBb A�-NBbBa�B_�Bb'Bc3A��BcPB_�Bv^BazBa�BbuBa�Bb Bb'Bb�Bc�Ba�Bb�Bc�Bc�BbaBa�BaBb�BcyBaBb�Ba�Ba�Bb�Ba�Bb�Bd�Ba\Bb�BcYBbBcdB`�B`�BcB`(BgBb�Bb�BbBc�BaTB`�B`�Bb�Ba$Bb�BbMB`<B�EBaBb�Ba�Bd$Bc�BbB`EBc�Ba?Ba�Bb1Bb�Bb�BhBc�Bc�BaJB`Bb�BdBfeBdhB`�B`�Bb�Bc�Be(Bd/B�@BfB_}Ba�B^�BcBjNB_�Bc�A�M�Bc�BdBb�Bb:Bb�BbDBa�BbDBcwBa�Bb�BbBb�Ba�BclBa+BccBcBbBb:Ba�Bc�Bd-BbBa@Bc)B`BdgBcBbOBaKBa�Ba�BbBbBb�BaxBcFBb�Ba�BapBd#Ba�Ba�BcB`�Bb�B_�B!BcBdJBb_Bb�Bb�Ba�Ba�Bc!Ba�Bb}BbOBcmBa�Bc�Bt�A��Be�BeBa�Bc�Ba�BbtBa�Ba8Ba�A�O�Bd�Bd]Bb�B`�Be	Bb�BbEBb=Ba�BaWBb!Ba�Bb�Ba�Ba6Bb}Ba�Ba�Ba�Ba~BavBa�Ba�Bb�Ba�Ba�Bb�Bb�Ba�BbSBa�BbBBb Ba�Bc�BbBcBc&Ba"BafBb�Ba�BaB_zB`Ba�Bd/BdB`�BcaB`2Ba�Ba�BakBa�Be�BcdBaqBc^BdZBcBc�Bd	Bh!Bb=Bb-Ba�BbBa�Ba}BauBb6BaBaqBaB`�Ba�Ba�Ba\BaTBbB`ABa�BaHBa�Bb1BaBdB_�BbBBcGBcBaiB\�BbyB`Bd�B`BbXBbBa�Bb�B_^B^"BbBc�Bb�B`�BbABb&Bb:BcIBc�BbQBcsBerB]�BbtBcIBb�Bc-BdGB`�Bb^BcPBbBceBa|B_4BbyBcVBa�B`OBa�Ba�B^�Bb�BeSBbpBb�B`<Bd�Bd.Bc�Ba�B_PBftBb5Bb�Bb�BcdB`�BdB_�Bb�Ba�Ba�BaB_9B`*BbPB_�Ba�BbTB`�BaeB_9Be Ba�Bc|Bd�BcBb�Bc�BbBb�BdbBcCBc�B`�Ba�Bb�BbcBeHBa�B`�B`�Ba%Bc�Bc�Bc�BccBc�Bc�BdDBd*Bb�BcBcoBbmBc.Bb6Ba�Ba�Ba�Bb4Ba�Ba�Ba�B`oBaBa�B`�Ba�Ba}Bb>Bb6Ba�BaSBa�Bb"Ba�B`�BbBa�Ba1B`�B`BbtBbBaEBb6Ba�B`�Ba�B`�Ba�Bb�BagBaBa~B`�BbBa�Bb�Ba�BbBbVBb�BbQBb�Bb�Bc�BahBc.B	�gB	�OB	͐B	�UB	�=B	��B	�$B	�QB	�B	ЬB	�VB	ΑB	��B	�OB	��B	��B	��B	��B	�uB	�>B	�4B	��B	��B	��B	͵B	�B	�FB	;B	�wB	�,B	�gB	̕B	�	B	�OB	�{B	�NB	��B	͇B	�B	οB	�FB	�B	��B	�KB	ΆB	��B	��B	ΏB	�5B	�B	гB	ϖB	ЙB	юB	АB	��B	��B	ӶB	�1B	�bB	�
B	ђB	�)B	��B	�xB	� B	҈B	�nB	ҞB	�B	ѾB	҄B	�9B	�/B	�B	ъB	�B	�5B	�eB	�B	ҖB	�B	�1B	�$B	��B	�B	�$B	ҬB	��B	�B	�0B	�SB	�B	��B	��B	ӦB	�\B	ӌB	�4B	�dB	�B	� B	��B	�{B	ӞB	�B	ӞB	��B	ӴB	��B	��B	ӽB	ӯB	ӢB	�KB	�{B	�0B	ӫB	ԮB	ԡB��B��B��B�B��B��B�8B�4B��B��B�tB��B�wB�tB�$B��B�}B��B��B�SB�hB�:B�B�GB�#B��B��B��B��B�PB�*B��B�ZB�HB�yB�UB�:B��B��B��B��B��B��B��B�%B�JB��B�>B�"B��B�"B�SB�sB��B��B�bB�LB� B�sB��B��B��B��B��B�XB��B�WB�B��B��B��B��B�aB�B�GB��B�fB�EB��B�}B�fB�cB�gB��B�hB�mB�B��B��B��B��B��B��B��B�B��B��B��B�QB�8B�DB�.B�0B� B�B��B�AB��B��B��B�\B�zB��B�B��B��B��B�=B��B��B��B��B��B�FB�B��B��B��B�B�
B��B��B��B��B�|B�B�!B�fB��B�B��B�qB�/B��B�tB��B��B��B��B��B�B��B�LB��B��B�5B�@B��B�'B�)B�B��B�wB��B�B��B�
B� B�>B��B��B��B��B�B��B�eB�#B��B�MB�B�5B��B�iB�B�cB�?B�B�LB�
B��B��B��B�B�DB�B�B�B�B��B�B�B�B�UB�B�B�!B�]B�B�B~B�5B��B�CB�NB�B�B��B�_B	��B	�B	��B	��B	�!B	��B	��B	��B	߻B	�jB	�lB	��B	�aB	�tB	�\B	��B	�B	�OB	�B	�B	�iB	�fB	�$B	�B	�B	�B	�B	�B	�AB	��B	�B	�sB	�)B	��B	��B	�B	�B	�B	��B	�B	��B	�wB	�>B	�B	��B	�kB	��B	��B	��B	�xB	�gB	�;B	�>B	�BB	�TB	�B	�eB	�B	�B	�B	��B	�jB	��B	��B	�OB	��B	�PB	��B	�B	�B	�YB	��B	�YB	�?B	�B	�%B	��B	��B	�)B	�xB	�B	�.B	�B	�B	��B	�B	�B	�)B	�UB	�B	�B	�B	��B	�B	��B	�B	�|B	�2B	��B	�B	�B	�B	��B	�B	�bB	�UB	�B	�B	�xB	�.B	�B	�cB	�GB	�B	�kB	�^B	�QB	�B	�B	�GG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999433433444444334444443343344344443344344344433444444344344444443443443443344444444444434433433343444344443434443344334434444444444444434334444444333444434434443334434334433344343434443433344443344333334433433433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B� B�B�B� B�B� B��B��B�B� B�B�B�B�B�B�B�B�B~�B~�B~�Bw�Br�Bo�BkxBilBdOB\BWBL�BL�B]!B^(BZBinB�&Bz�Bv�B�LB�BɯB�tB�gB�B��B�B�B�7B�B��B�B3$B3&B2 B1B+�B&�B%�B)�B.B)�B!�B(�B-B0B)�BB&B��B�B�'B��B�B�iB�wB�oB�PB��B�B��B�QB��B�'BgbBG�B��B��B��BB	*B�BʳBǠB̼BB�B�BB��B�8B�PB�%B�sB�@B�>B�0B�B�Bm�BcEBQ�B>iB/B!�BEB
�B
��B
�FB
��B
�,B
l~B
E�B
(�B
LB
�B	�B	�?B	�B	ɫB	�)B	��B	��B	�#B	s�B	ZB	O�B	B}B	1B	%�B	�B	xB	@B	B	 �B��B��B�B�mB�OB�GB�FB�=B�1B�*B�$B�B�B�B��B��B̾B˸B˵BɬBȣBǞBƘBŐBB�sB�fB�ZB�MB�<B�'B�B��B��B��B��B��B�oB�jB�]B�RB�:B�+B�B�	B~�B|�Bz�By�Bx�Bv�Bt�Bp�Bn�BksBihBhcBg\BeNBb<B`2B\BYBU�BT�BR�BP�BM�BJ�BH�BF�BD�BAvB>eB<WB<ZB<\B;TB:LB9GB9IB7:B5,B3B0B,�B)�B(�B&�B#�B!�B �B�B �B!�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B#�B$�B!�B)�B5-B4'B.B/B)�B!�B�B�B�B�B!�B#�B#�B$�B$�B%�B%�B%�B&�B&�B)�B,�B/	B2B5,B79B;PB?kB>dB>bB?jB=_B8@B:JBAuBE�BI�BN�BS�BW�B^!B`.BdFBh]BigBjkBm�Bt�Bx�B|�B}�B~�B�B�/B�NB�WB�bB�lB�qB��B��B��B��B��B��B�B�B�&B�:B�<B�DB�EB�VB�bB�pBĉBǞBǝBɨB;B��B��B��B��B��B��B�
B�B�!B�'B�:B�mB�B�B��B��B��B��B	�B	B	B	
(B	0B	DB	NB	UB	bB	lB	{B	�B	#�B	)�B	+�B	2B	5,B	8=B	;PB	<VB	=_B	?iB	AvB	L�B	P�B	T�B	_'B	eKB	igB	ktB	w�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�AB	�JB	�UB	�TB	�bB	�tB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�:B	�iB	ÁB	ĆB	ƓB	ƓB	ǜB	ɨB	ʭB	˳B	;B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�!B	�.B	�3B	�3B	�@B	�@B	�9B	�8B	�3B	�.B	�"B	�'B	�9B	�BB	�@B	�MB	�RB	�XG�O�B	�B	��B
�B
�B
3B
KB
�B
'�B
.�B
8�B
>�B
G�B
L�B
Q�B
V�B
[{B
^�B
b�B
g	B
k#B
qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BW�B�_G�O�A��Ba�G�O�G�O�G�O�G�O�G�O�G�O�BZ�B��G�O�G�O�G�O�G�O�G�O�G�O�B�=BmG�O�B}�B:=G�O�G�O�B]�G�O�G�O�G�O�G�O�BU�B�G�O�G�O�A�G�O�G�O�BX%G�O�G�O�G�O�BF2BG�O�G�O�G�O�G�O�G�O�G�O�Aǳ#G�O�G�O�A�@eG�O�G�O�G�O�G�O�G�O�G�O�G�O�B]�G�O�G�O�Bq9G�O�G�O�BZ�G�O�G�O�B\B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz�G�O�G�O�B^MB_<G�O�B[�BVwBZ�G�O�A�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�B��G�O�BXTG�O�G�O�G�O�BK�B]�G�O�G�O�BY�Be'G�O�G�O�B�kG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aû4G�O�B]B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\GBb8B
�G�O�G�O�G�O�G�O�BQ�G�O�G�O�A��G�O�G�O�G�O�B\cB\Ba
G�O�G�O�A���G�O�Be�Bg G�O�G�O�B`�Bb�Bs�G�O�G�O�B�G�O�B	{G�O�B��G�O�G�O�G�O�Aю�G�O�B^>B�fBg�G�O�G�O�G�O�G�O�Ba�Bh�G�O�G�O�B �6A�B�A�?-A�FB-�G�O�G�O�B�#BbJG�O�BaBk�G�O�Bb�BbBcWA�4,A�VBa�B��G�O�Ba�Bc�BcBe�BeBbBd�B��BaTBcWBa�Bb�BaBaBa�B_zBf�Bb\Bd�Bc(BҩBg�BfBe�BdBb�BckBd7Ba�BcBb6Bc~Bb�BaGBbBb�B`'BcBb�BbhBa�Bc�Ba"Bc�Bb�Ba�BaZBalBb�Bc�BcBc*Bc�Bc~Bd�BgBbyB`:BahBbBb^Bb�BcBb�BbRB`�Ba�Ba�BdLBcMBaDBaTBaBb8BbABc*Bf�BbBaBaB`�Ba�Ba�Bb�Be�A׵�Bf0Bc*Bc*Bb�Bc!BdBb�BcBcBb�Bd#BbTBa�A�-=BbBa�B_�BbBc&A��BcAB_�BvSBalBa�BbhBa�Ba�BbBb�Bc�Ba�Bb�Bc�Bc�BbTBa�BaBb�BcmBaBb|Ba�Ba�Bb�BauBbwBd{BaQBb�BcMBbBcYB`�B`�BcB`BgBb�Bb�BbBcvBaDB`�B`�Bb�BaBb�Bb=B`0B�;BaBb�BauBdBc�BbB`6Bc�Ba4Ba�Bb(Bb�Bb�Bh Bc�Bc|Ba;B`	Bb�Bc�BfYBd\B`�B`�Bb�Bc�BeBd#B�4BfB_qBa�B^�Bb�BjDB_�Bc�A�M�Bc�BdBb�Bb/Bb�Bb4Ba�Bb4BckBa�Bb�BbBb�BaxBcaBaBcYBcBbBb/Ba�BctBdBbBa4BcB_�Bd\BcBbABa=Ba�Ba�BbBbBb�BajBc:Bb�Ba�BaeBdBa�BaxBcB`�Bb�B_�BBcBd=BbPBb�Bb�Ba�Ba�BcBa�BboBbABccBa�Bc�Bt�A��Be�Bd�Ba�Bc�Ba�BbfBa�Ba,Ba�A�O�Bd�BdQBb�B`�Bd�Bb�Bb9Bb2Ba�BaGBbBa�Bb�Ba�Ba)BboBa�Ba�Ba�BaqBajBa�Ba�Bb�Ba�Ba�Bb�Bb|Ba�BbCBa�Bb7Ba�Ba�Bc�Bb BcB��B�~B��B�
B��B��B�*B�(B��B��B�jB��B�jB�jB�B��B�oB��B��B�HB�\B�/B�B�>B�B��B��B��B��B�EB�B��B�OB�<B�lB�JB�-B��B�~B�vB��B��B��B��B�B�<B�|B�1B�B��B�B�JB�gB��B��B�WB�AB�B�gB��B��B��B��B��B�PB��B�JB�B��B��B��B��B�WB�B�<B��B�YB�9B��B�rB�XB�WB�\B��B�^B�bB��B��B��B��B��B��B��B��B�tB��B��B��B�FB�-B�7B�"B�"B��B��B��B�5B��B��B��B�OB�lB��B��B��B��B��B�0B��B��B��B��B��B�6B��B��B��B�qB�B��B��B�tB��B��B�oB�B�B�VB��B�B��B�gB�#B��B�gB��B��B��B��B��B�B��B�AB��B�~B�(B�5B��B�B�B��B��B�jB��B�B��B��B�B�2B��B��B��B��B�B��B�WB�B��B�AB�B�(B��B�]B�B�UB�1B�B�?B�B��B��B��B��B�8B�B�B�B��B��B�B�	B�B�JB�B�B�B�QB�B�B~B�(B��B�8B�?B�B�B�~B�SB	��B	��B	��B	��B	�B	�mB	߭B	��B	ߧB	�UB	�XB	��B	�MB	�^B	�JB	��B	�B	�;B	��B	�{B	�TB	�SB	�B	�B	��B	�B	�B	�B	�+B	��B	�B	�]B	�B	��B	��B	�B	�B	�B	��B	�B	�B	�bB	�(B	�uB	�B	�VB	��B	��B	�B	�bB	�QB	�(B	�,B	�0B	�AB	�B	�QB	�B	�qB	�oB	�B	�WB	��B	�B	�:B	��B	�:B	��B	�B	�|B	�FB	��B	�CB	�,B	��B	�B	��B	�B	�B	�eB	��B	�B	� B	�B	��B	��B	�qB	�B	�@B	�	B	�wB	�zB	��B	�B	�B	�wB	�hB	�B	��B	�B	�	B	��B	��B	�B	�NB	�BB	�nB	�tB	�cB	�B	�B	�PB	�5B	�qB	�XB	�KB	�<B	�B	�B	�4B��B�~B��B�
B��B��B�*B�(B��B��B�jB��B�jB�jB�B��B�oB��B��B�HB�\B�/B�B�>B�B��B��B��B��B�EB�B��B�OB�<B�lB�JB�-B��B�~B�vB��B��B��B��B�B�<B�|B�1B�B��B�B�JB�gB��B��B�WB�AB�B�gB��B��B��B��B��B�PB��B�JB�B��B��B��B��B�WB�B�<B��B�YB�9B��B�rB�XB�WB�\B��B�^B�bB��B��B��B��B��B��B��B��B�tB��B��B��B�FB�-B�7B�"B�"B��B��B��B�5B��B��B��B�OB�lB��B��B��B��B��B�0B��B��B��B��B��B�6B��B��B��B�qB�B��B��B�tB��B��B�oB�B�B�VB��B�B��B�gB�#B��B�gB��B��B��B��B��B�B��B�AB��B�~B�(B�5B��B�B�B��B��B�jB��B�B��B��B�B�2B��B��B��B��B�B��B�WB�B��B�AB�B�(B��B�]B�B�UB�1B�B�?B�B��B��B��B��B�8B�B�B�B��B��B�B�	B�B�JB�B�B�B�QB�B�B~B�(B��B�8B�?B�B�B�~B�SB	��B	��B	��B	��B	�B	�mB	߭B	��B	ߧB	�UB	�XB	��B	�MB	�^B	�JB	��B	�B	�;B	��B	�{B	�TB	�SB	�B	�B	��B	�B	�B	�B	�+B	��B	�B	�]B	�B	��B	��B	�B	�B	�B	��B	�B	�B	�bB	�(B	�uB	�B	�VB	��B	��B	�B	�bB	�QB	�(B	�,B	�0B	�AB	�B	�QB	�B	�qB	�oB	�B	�WB	��B	�B	�:B	��B	�:B	��B	�B	�|B	�FB	��B	�CB	�,B	��B	�B	��B	�B	�B	�eB	��B	�B	� B	�B	��B	��B	�qB	�B	�@B	�	B	�wB	�zB	��B	�B	�B	�wB	�hB	�B	��B	�B	�	B	��B	��B	�B	�NB	�BB	�nB	�tB	�cB	�B	�B	�PB	�5B	�qB	�XB	�KB	�<B	�B	�B	�4G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999433433444444334444443343344344443344344344433444444344344444443443443443344444444444434433433343444344443434443344334434444444444444434334444444333444434434443334434334433344343434443433344443344333334433433433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647272020083116472720200831164727202008311647272020083116472720200831164727202008311647272020083116472720200831164727202008311647272020083116472720200831164727AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816372019021918163720190219181637    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816372019021918163720190219181637  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816372019021918163720190219181637  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647272020083116472720200831164727  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                