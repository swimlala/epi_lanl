CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  u   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:22:00Z creation      
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
resolution        =���   axis      Z        )|  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
`  n�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )|  x�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
`  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )|  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )|  �D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
`  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )| 
    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` 3�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )| =�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )| gx   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )| �T   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )| �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )| ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` "(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )| ,�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
` V   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )| `d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20181120212200  20200901153805  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @�9�i�@�9�i�@�9�i�111 @�9�s��<@�9�s��<@�9�s��<@4(1&�x�@4(1&�x�@4(1&�x��dF��n��dF��n��dF��n�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy~D��D�A�D��)D�ҏD�
D�7\D�|{D���D���D�AHD���DǺ�D���D�2�Dڇ
D��D��qD��D�f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    =���        =���        >L��>���        =���            =���                            >L��>L��        =���                        =���=���            =���>L��        =���    =���=���                    =���        =���                    =���                    =���            =���            =���                    =���            >L��        >L��            =���=���        =���        =���        =���=���                =���        =���>L��        >L��                                =���                                            >L��>L��=���        =���>���>���        >L��>L��=���    =���>L��>L��                            =���        =���>L��>���        =���=���=���        =���>���>L��        =���=���>L��            =���>L��>L��                    =���>L��>L��        =���=���    =���        =���            =���    >L��=���                =���=���=���    >L��=���=���=���    =���>L��>L��>L��=���=���=���>L��>���>L��=���>L��>L��>L��>���>L��>L��>L��>L��>���=���>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>���>���>L��>���>L��>���>���>L��>L��>L��>L��>L��>L��>L��>L��=���>L��=���>L��>L��>L��>���>���>L��>L��>���>���>���>���>���>���=���>���>L��>L��=���>L��>L��    >L��>���>L��>L��>L��>L��=���>L��>L��>���>���>L��>���>���>L��>���>L��>���>���>L��=���>L��>L��>���>L��=���>L��>���>L��>L��>���>���>L��>L��>L��>L��>L��>L��>L��>L��>L��>���>L��=���>L��>L��>���>���>L��>L��>���>���>L��>L��>L��>���=���>L��>���>���>���>L��>���>L��>L��>L��>L��>���>L��>���>���>L��>���>���>���>���>L��>���>L��>���>���>L��>���>L��>L��>L��>���>L��>���=���>L��>L��>���>L��>L��=���>���>���>���>L��>���>L��>���>L��=���>���>L��>L��>���=���>L��>���>L��>���>���>L��>���>���>���>���>L��>L��>L��>L��>L��>L��>L��>L��>���>L��>���>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>���>L��>���>L��>L��>���>���>L��=���>���>L��>L��>���=���>L��>���>���>���>���>���>L��=���>L��>���>���>L��>L��>L��>L��=���>���>���>L��>���>���>���>L��>L��>���>���>L��>L��>L��>L��>���>���>L��>L��>L��>���>���>L��>L��>L��>���>���>L��=���=���=���>L��>���>L��>���>���?   ?   ?��?333?L��?L��?fff?�  ?�  ?���?���?���?�ff?�33?�ff?�  ?���?���?ٙ�?ٙ�?�ff?�33@   @   @   @��@��@33@��@��@   @&ff@,��@,��@333@9��@@  @Fff@L��@S33@Y��@fff@fff@s33@y��@�  @�33@�ff@���@���@�  @�ff@�ff@���@�  @�33@�33@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�ff@�ff@ٙ�@�  @�33@�ff@陚@���@�  @�33@�ff@���A   A��A33A��AffA	��A33A��AffA  A33A��AffA  A��A��AffA   A!��A#33A&ffA(  A)��A+33A,��A0  A1��A333A4��A8  A9��A;33A<��A>ffAA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A^ffAa��Ac33Ad��AfffAh  Ai��Al��AnffAp  Aq��As33At��Ax  Ay��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�33A�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A���Aə�A�ffA�ffA�  A���A͙�A�ffA�33A�  A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�ffA�33A�  A���Aٙ�Aٙ�A�ffA�33A�  A���A���Aݙ�A�ffA�33Dp�3Dq  DqfDq�Dq3Dq�Dq&fDq,�Dq33Dq9�Dq@ DqL�DqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr�fDr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt��@333@9��@@  @Fff@L��@S33@Y��@fff@fff@s33@y��@�  @�33@�ff@���@���@�  @�ff@�ff@���@�  @�33@�33@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�ff@�ff@ٙ�@�  @�33@�ff@陚@���@�  @�33@�ff@���A   A��A33A��AffA	��A33A��AffA  A33A��AffA  A��A��AffA   A!��A#33A&ffA(  A)��A+33A,��A0  A1��A333A4��A8  A9��A;33A<��A>ffAA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A^ffAa��Ac33Ad��AfffAh  Ai��Al��AnffAp  Aq��As33At��Ax  Ay��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�33A�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A���Aə�A�ffA�ffA�  A���A͙�A�ffA�33A�  A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�ffA�33A�  A���Aٙ�Aٙ�A�ffA�33A�  A���A���Aݙ�A�ffA�33Dp�3Dq  DqfDq�Dq3Dq�Dq&fDq,�Dq33Dq9�Dq@ DqL�DqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr�fDr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @0  @vff@�33@�33A��A=��A]��A}��A���A���A���A���A���Aߙ�A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp� DqvfDq�fDrvfDr�fDsvfDs�fDti�DytzD�D�=D�\D���D�=D�2�D�w�D�� D��
D�<{D�� DǶD���D�.Dڂ=D��RD���D��D�a�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����L�̾������L�̾�����=L��>���������L�̾���������L�̾��������������������=L��=L�ξ������L�̾������������������L�̽L�̾���������L��=L�ξ������L�̾���L�̽L�̾���������������L�̾������L�̾���������������L�̾���������������L�̾���������L�̾���������L�̾���������������L�̾��������=L�ξ�����=L�ξ���������L�̽L�̾������L�̾������L�̾������L�̽L�̾������������L�̾������L��=L�ξ�����=L�ξ������������������������L�̾��������������������������������=L��=L�νL�̾������L��>��>��������=L��=L�νL�̾���L��=L��=L�ξ���������������������L�̾������L��=L��>���������L�̽L�̽L�̾������L��>��=L�ξ������L�̽L��=L�ξ���������L��=L��=L�ξ���������������L��=L��=L�ξ������L�̽L�̾���L�̾������L�̾���������L�̾��=L�νL�̾������������L�̽L�̽L�̾��=L�νL�̽L�̽L�̾���L��=L��=L��=L�νL�̽L�̽L��=L��>��=L�νL��=L��=L��=L��>��=L��=L��=L��=L��>���L��=L�νL��=L��=L��=L��=L��=L��=L��=L��=L��>��>��=L��>��=L��>��>��=L��=L��=L��=L��=L��=L��=L��=L�νL��=L�νL��=L��=L��=L��>��>��=L��=L��>��>��>��>��>��>���L��>��=L��=L�νL��=L��=L�ξ��=L��>��=L��=L��=L��=L�νL��=L��=L��>�  >��=L��>�  >�  =L��>��=L��>��>��=L�νL��=L��=L��>��=L�νL��=L��>��=L��=L��>��>��=L��=L��=L��=L��=L��=L��=L��=L��=L��>��=L�νL��=L��=L��>��>��=L��=L��>��>��=L��=L��=L��>���L��=L��>��>��>��=L��>��=L��=L��=L��=L��>��=L��>��>��=L��>��>��>��>��=L��>��=L��>��>��=L��>��=L��=L��=L��>��=L��>���L��=L��=L��>��=L��=L�νL��>��>��>��=L��>��=L��>��=L�νL��>��=L��=L��>���L��=L��>��=L��>��>��=L��>��>��>��>��=L��=L��=L��=L��=L��=L��=L��=L��>��=L��>��=L��=L��=L�νL��=L��=L��=L��=L��=L��=L��=L��>��=L��>��=L��=L��>��>��=L�νL��>��=L��=L��>���L��=L��>��>��>��>��>��=L�νL��=L��>��>��=L��=L��=L��=L�νL��>�  >��=L��>��>��>��=L��=L��>��>��=L��=L��=L��=L��>��>��=L��=L��=L��>��>��=L��=L��=L��>��>��=L�νL�̽L�̽L��=L��>��=L��>�  >�  >�33>�33>�fg?��?&fg?&fg?@  ?Y��?Y��?s34?�fg?s34?�33?�  ?�33?���?���?���?�fg?�fg?�33?�  ?���?���?���@33@33@	��@  @  @ff@��@#33@#33@)��@0  @6ff@<��@C33@I��@P  @\��@\��@i��@p  @vff@|��@���@���@�  @�33@���@���@���@�33@�ff@�ff@���@�  @�33@�ff@���@���@�  @�33@�ff@���@�  @�  @�33@љ�@љ�@���@�33@�ff@ᙙ@���@�  @�33@�ff@�@���@�33@�fgA ��AfgA  A34A��A
fgA  A��A��AfgA  A��A34AfgA  A��A34A ��A$  A%��A'34A(��A*fgA-��A/34A0��A2fgA5��A734A8��A:fgA<  A?34A@��ABfgAE��AG34AH��AJfgAM��AO34AP��ARfgAU��AW34AX��AZfgA\  A_34A`��AbfgAd  Ae��Ag34AjfgAl  Am��Ao34Ap��ArfgAu��Aw34Ax��AzfgA|  A}��A34A�33A�  A���A���A�fgA�33A�  A���A�fgA�33A�  A���A���A�fgA�33A�  A���A���A�fgA�33A���A���A�fgA�33A�  A���A���A�fgA�  A���A���A�fgA�33A�  A���A���A�fgA�  A�  A���A�fgA�33A�  A���A���A�33A�  A���A���A�fgA�33A�  A���A���A�fgA�33A���A���A�fgA�33A�  A���A���A�fgA�33A�  A���A�fgA�33A�  A���A���A�fgA�33A�  A���AÙ�A�fgA�33A�  AǙ�A�fgA�33A�33A���A˙�A�fgA�33A�  A���A���Aϙ�A�fgA�33A�  A���Aә�A�fgA�33A�33A�  A���Aי�A�fgA�fgA�33A�  A���Aۙ�Aۙ�A�fgA�33A�  Dp�Dp�fDp��Dq3Dq	�Dq Dq�Dq#3Dq)�Dq0 Dq6fDqC3DqI�DqP DqVfDq\�Dqi�Dqp DqvfDq|�Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3DqəDq�fDq��Dq�3Dq�Dq�fDq��Dr3Dr	�Dr Dr�Dr#3Dr)�Dr0 Dr<�DrC3DrI�DrP DrVfDrc3Dri�Drp DrvfDr�3Dr��Dr� Dr�fDr�3Dr��Dr� Dr�fDr�3DrəDr� Dr�fDr��Dr�Dr� Dr�fDs3Ds	�Ds DsfDs�Ds)�Ds0 Ds6fDsC3DsI�DsP DsVfDsc3Dsi�Dsp Ds|�Ds�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsəDs�fDs��Ds�3Ds�Ds�fDs��Dt3Dt	�DtfDt�Dt#3Dt0 Dt6fDt<�DtC3DtP DtVfDt\�Dti�Dtp DtvfDt|�Dt��Dt� Dt�fDt�3@)��@0  @6ff@<��@C33@I��@P  @\��@\��@i��@p  @vff@|��@���@���@�  @�33@���@���@���@�33@�ff@�ff@���@�  @�33@�ff@���@���@�  @�33@�ff@���@�  @�  @�33@љ�@љ�@���@�33@�ff@ᙙ@���@�  @�33@�ff@�@���@�33@�fgA ��AfgA  A34A��A
fgA  A��A��AfgA  A��A34AfgA  A��A34A ��A$  A%��A'34A(��A*fgA-��A/34A0��A2fgA5��A734A8��A:fgA<  A?34A@��ABfgAE��AG34AH��AJfgAM��AO34AP��ARfgAU��AW34AX��AZfgA\  A_34A`��AbfgAd  Ae��Ag34AjfgAl  Am��Ao34Ap��ArfgAu��Aw34Ax��AzfgA|  A}��A34A�33A�  A���A���A�fgA�33A�  A���A�fgA�33A�  A���A���A�fgA�33A�  A���A���A�fgA�33A���A���A�fgA�33A�  A���A���A�fgA�  A���A���A�fgA�33A�  A���A���A�fgA�  A�  A���A�fgA�33A�  A���A���A�33A�  A���A���A�fgA�33A�  A���A���A�fgA�33A���A���A�fgA�33A�  A���A���A�fgA�33A�  A���A�fgA�33A�  A���A���A�fgA�33A�  A���AÙ�A�fgA�33A�  AǙ�A�fgA�33A�33A���A˙�A�fgA�33A�  A���A���Aϙ�A�fgA�33A�  A���Aә�A�fgA�33A�33A�  A���Aי�A�fgA�fgA�33A�  A���Aۙ�Aۙ�A�fgA�33A�  Dp�Dp�fDp��Dq3Dq	�Dq Dq�Dq#3Dq)�Dq0 Dq6fDqC3DqI�DqP DqVfDq\�Dqi�Dqp DqvfDq|�Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3DqəDq�fDq��Dq�3Dq�Dq�fDq��Dr3Dr	�Dr Dr�Dr#3Dr)�Dr0 Dr<�DrC3DrI�DrP DrVfDrc3Dri�Drp DrvfDr�3Dr��Dr� Dr�fDr�3Dr��Dr� Dr�fDr�3DrəDr� Dr�fDr��Dr�Dr� Dr�fDs3Ds	�Ds DsfDs�Ds)�Ds0 Ds6fDsC3DsI�DsP DsVfDsc3Dsi�Dsp Ds|�Ds�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsəDs�fDs��Ds�3Ds�Ds�fDs��Dt3Dt	�DtfDt�Dt#3Dt0 Dt6fDt<�DtC3DtP DtVfDt\�Dti�Dtp DtvfDt|�Dt��Dt� Dt�fDt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A։7A�r�A�^5A�I�A�?}A�1'A�$�A��mA�Q�A���Aԟ�AԁA�%A�VAёhAξwA�JA�7LAǅA�7LA���A�Q�AœuAÏ\A�oA���A�A�?}A��\A�A��A�E�A��/A�ffA��\A�A�jA�7LA��A�z�A�A�VA��7A���A��!A�?}A���A�A���A���A��PA�ZA�oA��A���A�E�A�XA���A�=qA��RA�1'A�^5A�=qA���A�1'A�hsA��A���A��`A�z�A�&�A���A���A��HA�ȴA�\)A��HA�XA�VA�{A��A��A���A� �A��;A�v�A�n�A���A�/A�z�A�A�;dA��A�p�A�z�A�/A�JA�1'A{hsAx�As�FAq�7AoXAn �Al �Ai%AhbAg�;Af�Ae�wAe?}Ad(�Ab�A`��A_S�A^��A]%AY��AX��AV��AUoAS`BAR�AR�\AQAP�+AOAM�FAL=qAJ=qAH�HAG��AE��ADĜAC�A@�A?/A>^5A=�#A<�A:��A8�/A7�;A7�-A7`BA6��A5;dA333A2  A0VA/�wA/�A-`BA,A*n�A)\)A(�+A'�A'%A%�A$��A#��A"�9A!�A��AjA�^A��A  AffA�`A�AdZA��AM�A;dAffAƨA�-A&�A��A�A�AA"�AJA�A
ȴA
~�A
bA	�-A	`BA	%A��A��A��An�At�A/A��A�;A�A1'A��A�HA��A 1@��@��H@�~�@�$�@���@�?}@���@�  @�33@�n�@�/@�Q�@��@���@�+@�=q@�O�@�l�@�x�@�ƨ@�o@�M�@��T@��#@���@���@�x�@�A�@�\@�^@��@���@�$�@�@�1@��H@�j@ە�@��@���@�x�@�A�@��@��@�G�@�(�@�ȴ@У�@���@�
=@�@�V@�A�@�ƨ@�l�@�?}@�1@�K�@�-@�`B@��`@Ĵ9@�9X@��;@ÍP@���@�{@��h@�7L@���@��D@��@��D@��j@���@���@���@�$�@��T@��!@��y@��\@��T@���@��@�?}@�S�@��\@�ff@�@�?}@� �@���@�ȴ@�J@��h@���@��D@��@���@�ƨ@�dZ@�"�@���@���@�%@��@��@��m@���@�C�@��y@�~�@�^5@��#@�p�@�7L@�G�@�`B@�?}@��9@�I�@��;@�\)@��y@��@�K�@�l�@��P@�+@�@���@��@���@�5?@�@�p�@�hs@�X@��@��u@�A�@���@��;@��;@��
@��
@�ƨ@���@�C�@�+@�~�@�{@��#@���@��7@�`B@��@���@���@��j@��@�z�@��m@�|�@�S�@��@��+@�{@���@�O�@��@��D@�Q�@� �@��m@��F@�C�@�ȴ@�v�@�=q@��@��-@�7L@���@��`@��9@�j@��;@��P@�\)@�S�@��@��y@���@�=q@���@��h@�O�@�V@�Ĝ@���@�r�@�A�@���@��F@��P@�@���@�5?@�@�`B@�/@��@���@���@�z�@��@�t�@�
=@���@��R@�M�@�{@���@�&�@���@��u@�r�@�A�@��m@��w@�t�@�C�@�+@��@��!@�n�@���@��T@�@�hs@�V@���@�A�@�1'@�(�@���@�dZ@��@�~�@���@���@��@�?}@��@���@�A�@��
@���@�l�@�33@��!@�=q@���@��T@��#@���@��^@�hs@���@���@��D@�Q�@���@�@��R@��+@�^5@�5?@�J@��-@~	@q�@hz�@`�@X��@R�@I�X@C�K@=k�@:GE@49X@+�@%��@ m�@�@h
@#:@<6@
��@m�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�AǑhA�n�A�z�A�n�A�A�A�  AփA�\)A϶FA�dZA�I�A�&�Aȉ7A�z�AɶFA�ƨAʟ�A�S�A�~�A�M�A��`A�/A֏\A�|�A��#Aէ�A��TA�M�A���A�ZA�=qA΍PA�^5A�x�A�JA�  Aˏ\A��A֏\AՑhA��`A�5?A�v�A˸RA�|�A�33A�M�A�bNA���A�33Aԉ7AΓuA���A��mA�{A�G�A�1A��/AȺ^A��A�-A�~�A��AŰ!A�=qAӇ+A�z�A��AȃAк^A���A�^5AζFAҮA�\)A�&�AжFA�5?A��Aϲ-AƼjA���A�&�A�  Aӕ�A�z�A�Q�AԸRA��/AΩ�A��HA�XA�jAōPAȓuAʧ�A�;dA�$�A�=qA���A�7LA�ffA��A��HA��`AŁA�oA�
=A�5?A�I�AօA�%A�E�A���A�A���A�Q�A�\)A�/A�VA�G�A���A�x�A×�A�oA®A�oA�S�AÏ\AŴ9A�%Aχ+A��A�9XA�ffA֋DA�x�A�O�A���A�$�A�|�AցAԬA���A։7AօA�t�A�p�A�r�A�
=A։7A��A��A�  A�~�A��A��`A���A�;dA�$�A�r�A�I�A�t�A�~�AՅA�7LA�;dA���A�I�A�ffAЗ�A�5?A�|�AցA�-A�5?A�5?A��A֓uA�=qA�A�AζFA��A�jA֋DA�x�A��A̧�A�33A��A�p�A֍PA։7A��A�v�A��A�Q�AӇ+A�oAՋDA�JA�M�A��TA��/A���A��/A�~�A�XAօAк^A�dZA�M�A�9XAՋDA։7A�l�A�$�A�^5A֍PA֍PA֍PAԬA�A֑hA֍PA�p�A։7AօA֍PA֋DA֍PA֑hA֍PA֏\A֋DA֏\A֋DA֓uA֏\A֏\A֕�A֡�A֓uA֙�A։7A։7AօAփAև+AօAօA։7Aև+AօA։7A֍PA֍PAօA։7A֋DA։7A։7A֍PA֍PAօAև+Aև+A֏\A�`BA֋DA։7AօA֋DA֋DA։7A֍PAև+A֋DA։7A֍PA֏\A֕�A֍PA֋DA֋DA֋DA։7AփA֍PA֍PA֕�AօA�bA֑hA֑hAօA֏\A֗�Aև+A֋DA֍PA֕�AօAփA։7A֏\AփAև+Aև+AփAցA։7AցA�~�A�~�AփA�|�A�~�A֑hA֕�Aև+AփAփA։7A։7A֗�Aև+AօA��mAև+A։7Aև+Aև+A֑hAօAև+Aև+Aև+Aև+A֋DAցA։7A։7A֏\AփAցA�~�AօAփA�~�A�~�AօAցAփAցAև+A�~�A�~�AցAցAօA�|�AցA�z�AցA�|�A�|�AցA�~�A�~�A�|�A�z�AօA�|�A�z�A�x�A�~�A�z�A�|�A�x�A�z�A�v�A�v�A�z�AօA�z�A�z�AցAօA։7A։7AցA֏\AփA֍PAփAև+A֑hA�~�A�~�AցAփAև+A�|�A�z�A�~�A�~�A�x�A�z�A�x�A�~�A�x�A�z�A�x�A�x�A�x�A�|�A�z�A�x�A�x�A�t�A�x�A�x�A�z�A�v�A�v�A�p�A�p�A�x�AՋDA�t�A�v�A�|�A�t�A�z�A�z�A�z�A�z�A�t�A�z�A�z�A�|�A�r�A�z�A�~�A�|�A։7A�x�A�|�AփA�|�A�x�A�x�A�x�A�v�A�l�A�t�A�x�A�v�A�t�A�n�A�n�A�t�A�t�A�r�A�r�A�p�A�t�A�r�A�z�A�t�A�n�A�v�A�x�A�r�A�x�A�r�A�t�A�z�AցAփA�x�A�r�A�z�A�n�A�r�A�t�A�n�A�r�A�z�A�x�A�r�A�x�A�~�AօA�~�A�z�A�|�A�x�A�v�A�|�AօA�~�A�p�A�t�A�v�A�r�A�ffA�n�A�r�A�jA�l�A�z�A�x�A�v�AփA�r�A�r�A�v�A�r�A�z�A�x�A�|�A�x�A�~�A�|�A�z�A�|�A�~�AցA�|�A�x�A�x�A�x�A�z�A�|�AցA�v�A�n�A�x�A�jA�bNA�n�A�bNA�`BA�^5A�\)A�\)A�\)A�\)A�^5A�bNA�^5A�bNA�dZA�bNA�ZA�XA�ZA�bNA�\)A�VA�\)A�ZA�ZA�O�A�S�A�VA�O�A�O�A�Q�A�O�A�K�A�E�A�I�A�C�A�M�A�;dA�?}A�I�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�=qA�E�A�E�A�C�A�=qA�C�A�=qA�C�A�?}A�A�A�A�A�9XA�;dA�5?A�5?A�1'A�1'A�5?A�1'A�33A�33A�5?A�33A�1'A�+A�-A�1'A�(�A�+A�+A�(�A�(�A�&�A�$�A� �A�"�A��A��A��A��A��A��A��A��A��A��A��A��A�oA�{A�oA�oA�JA�JA�
=A�1A�%A�%A�A���A���A��A��`A��HA�ƨAմ9AնFA՛�A�v�A�ffA�^5A�ZA�ZA�ZA�XA�Q�A�O�A�K�A�K�A�C�A�A�A�A�A�?}A�=qA�;dA�5?A�1'A�1'A�/A�(�A�$�A� �A��A��A��A�
=A�A���A��A��A��yA��
A�ƨAԲ-AԮAԬAԣ�Aԟ�Aԟ�Aԝ�Aԛ�Aԙ�Aԗ�Aԗ�Aԗ�Aԗ�Aԕ�AԓuAԕ�AԓuAԏ\AԍPAԍPAԋDAԋDAԉ7AԅAԇ+AԅAԅAԁA�x�A�x�A�x�A�v�A�v�A�t�A�t�A�p�A�l�A�jA�dZA�dZA�\)A�VA�M�A�E�A�=qA�;dA�$�A�{A�  A��A��A��/A��#A��
A�ȴA�A���AӾwA���A���AӾwAӾwAӼjA���A���AӾwA�A�A�ĜA�A�ĜAӗ�A�z�A�A�@���@�dZ@�\)@�S�@�K�@�K�@�C�@�33@�33@�+@�+@�"�@��@��@��@�o@�
=@�@���@���@��@��y@��H@��@���@���@���@���@�ȴ@�ȴ@���@���@��R@��R@��!@��!@���@���@���@���@���@���@���@���@���@��\@��\@��\@��+@�~�@�~�@�~�@�v�@�v�@�v�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�ff@�ff@�^5@�^5@�V@�V@�M�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�-@�-@�-@�-@�-@�-@�$�@�$�@��@�$�@��@��@��@��@�{@�{@�J@�@���@���@���@���@��@��@��#@���@���@���@�@�@��^@��-@��-@��-@���@���@���@���@���AօA֋DA։7A�~�A�z�AփA�~�A�z�A�t�A�~�A�hsA�hsA�hsA�jA�l�A�jA�n�A�ffA�ffA�hsA�n�A�l�A�ffA�dZA�jA�hsA�dZA�ffA�dZA�`BA�dZA�`BA�ZA�XA�\)A�VA�VA�S�A�VA�O�A�O�A�Q�A�G�A�I�A�K�A�G�A�I�A�K�A�I�A�I�A�I�A�G�A�E�A�E�A�G�A�C�A�E�A�E�A�C�A�E�A�C�A�E�A�C�A�A�A�A�A�C�A�A�A�;dA�;dA�9XA�;dA�9XA�7LA�;dA�7LA�7LA�5?A�5?A�5?A�1'A�5?A�33A�1'A�1'A�1'A�1'A�-A�-A�+A�/A�+A�(�A�&�A�&�A�&�A�"�A�"�A�&�A�"�A�$�A�$�A�"�A��A�"�A�"�A��A��A��A��A�{A�bA�oA�VA�  A�  A���A���A��mA���A�ĜAմ9Aա�AՉ7A�l�A�dZA�ffA�ffA�bNA�`BA�ZA�XA�VA�O�A�M�A�K�A�I�A�G�A�E�A�E�A�;dA�9XA�5?A�7LA�/A�-A�/A�-A� �A��A�bA�
=A���A���A��A��A��TA�ĜAԺ^AԸRAԲ-AԬAԧ�Aԩ�Aԥ�Aԣ�Aԡ�Aԡ�Aԡ�Aԟ�Aԟ�Aԝ�Aԛ�Aԟ�Aԛ�Aԙ�Aԕ�AԑhAԕ�AԑhAԑhAԑhAԏ\Aԏ\AԋDAԉ7Aԉ7AԁAԁAԁA�~�A�|�A�x�A�x�A�t�A�r�A�l�A�l�A�dZA�`BA�ZA�VA�E�A�;dA�-A��A��A�  A���A��`A��TA��;A���A���A���A���A�ȴA�ȴA�ƨA�ĜA�ƨA�ȴA�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�A�@���@�|�@�dZ@�\)@�S�@�S�@�K�@�C�@�;d@�33@�33@�+@�"�@�"�@�"�@��@�o@�
=@�@�@���@��@��y@��y@��H@��@��@��@���@�ȴ@�ȴ@�ȴ@���@���@��R@��R@��!@���@���@���@���@���@���@���@���@���@���@���@��\@��\@��+@�~�@�~�@�~�@�~�@�v�@�v�@�v�@�n�@�v�@�n�@�n�@�n�@�ff@�ff@�ff@�ff@�^5@�V@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�5?@�-@�-@�-@�-@�-@�-@�$�@�$�@�$�@�-@�$�@�$�@��@��@��@�{@�J@�@�@�@�@�@���@��@��T@��#@���@���@���@�@�@��^@��^@��^@��-@���@��-@��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A։7A�r�A�^5A�I�A�?}A�1'A�$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AёhG�O�G�O�A�7LAǅA�7LA���A�Q�AœuG�O�A�oA���G�O�G�O�A��\A�A��A�E�A��/A�ffA��\A�A�jA�7LA��A�z�A�A�VA��7A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��`A�z�A�&�A���A���A��HA�ȴA�\)A��HA�XA�VA�{A��A��A���A� �A��;A�v�A�n�A���A�/A�z�A�A�;dA��A�p�A�z�A�/A�JA�1'A{hsAx�As�FAq�7AoXAn �Al �Ai%AhbAg�;Af�Ae�wAe?}Ad(�Ab�A`��A_S�A^��A]%AY��AX��AV��AUoAS`BAR�AR�\AQAP�+AOAM�FAL=qAJ=qAH�HAG��AE��ADĜAC�A@�A?/A>^5A=�#A<�A:��A8�/A7�;A7�-A7`BA6��A5;dA333A2  A0VA/�wA/�A-`BA,A*n�A)\)A(�+A'�A'%A%�A$��A#��A"�9A!�A��AjA�^A��A  AffA�`A�AdZA��AM�A;dAffAƨA�-A&�A��A�A�AA"�AJA�A
ȴA
~�A
bA	�-A	`BA	%A��A��A��An�At�A/A��A�;A�A1'A��A�HA��A 1@��@��H@�~�@�$�@���@�?}@���@�  @�33@�n�@�/@�Q�@��@���@�+@�=q@�O�@�l�@�x�@�ƨ@�o@�M�@��T@��#@���@���@�x�@�A�@�\@�^@��@���@�$�@�@�1@��H@�j@ە�@��@���@�x�@�A�@��@��@�G�@�(�@�ȴ@У�@���@�
=@�@�V@�A�@�ƨ@�l�@�?}@�1@�K�@�-@�`B@��`@Ĵ9@�9X@��;@ÍP@���@�{@��h@�7L@���@��D@��@��D@��j@���@���@���@�$�@��T@��!@��y@��\@��T@���@��@�?}@�S�@��\@�ff@�@�?}@� �@���@�ȴ@�J@��h@���@��D@��@���@�ƨ@�dZ@�"�@���@���@�%@��@��@��m@���@�C�@��y@�~�@�^5@��#@�p�@�7L@�G�@�`B@�?}@��9@�I�@��;@�\)@��y@��@�K�@�l�@��P@�+@�@���@��@���@�5?@�@�p�@�hs@�X@��@��u@�A�@���@��;@��;@��
@��
@�ƨ@���@�C�@�+@�~�@�{@��#@���@��7@�`B@��@���@���@��j@��@�z�@��m@�|�@�S�@��@��+@�{@���@�O�@��@��D@�Q�@� �@��m@��F@�C�@�ȴ@�v�@�=q@��@��-@�7L@���@��`@��9@�j@��;@��P@�\)@�S�@��@��y@���@�=q@���@��h@�O�@�V@�Ĝ@���@�r�@�A�@���@��F@��P@�G�O�@�5?@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��R@�M�@�{@���@�&�@���@��u@�r�@�A�@��m@��w@�t�@�C�@�+@��@��!@�n�@���@��T@�@�hs@�V@���@�A�@�1'@�(�@���@�dZ@��@�~�@���@���@��@�?}@��@���@�A�@��
@���@�l�@�33@��!@�=q@���@��T@��#@���@��^@�hs@���@���@��D@�Q�@���@�@��R@��+@�^5@�5?@�JG�O�@~	@q�@hz�@`�@X��@R�@I�X@C�K@=k�@:GE@49X@+�@%��@ m�@�@h
@#:@<6@
��@m�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�AǑhA�n�A�z�A�n�A�A�A�  AփA�\)A϶FA�dZA�I�A�&�Aȉ7A�z�AɶFA�ƨAʟ�A�S�A�~�A�M�A��`A�/A֏\A�|�A��#Aէ�A��TA�M�A���A�ZA�=qA΍PA�^5A�x�A�JA�  Aˏ\A��A֏\AՑhA��`A�5?A�v�A˸RA�|�A�33A�M�A�bNA���A�33Aԉ7AΓuA���A��mA�{A�G�A�1A��/AȺ^A��A�-A�~�A��AŰ!A�=qAӇ+A�z�A��AȃAк^A���A�^5AζFAҮA�\)A�&�AжFA�5?A��Aϲ-AƼjA���A�&�A�  Aӕ�A�z�A�Q�AԸRA��/AΩ�A��HA�XA�jAōPAȓuAʧ�A�;dA�$�A�=qA���A�7LA�ffA��A��HA��`AŁA�oA�
=A�5?A�I�AօA�%A�E�A���A�A���A�Q�A�\)A�/A�VA�G�A���A�x�A×�A�oA®A�oA�S�AÏ\AŴ9A�%Aχ+A��A�9XA�ffA֋DA�x�A�O�A���A�$�A�|�AցAԬA���A։7AօA�t�A�p�A�r�A�
=A։7A��A��A�  A�~�A��A��`A���A�;dA�$�A�r�A�I�A�t�A�~�AՅA�7LA�;dA���A�I�A�ffAЗ�A�5?A�|�AցA�-A�5?A�5?A��A֓uA�=qA�A�AζFA��A�jA֋DA�x�A��A̧�A�33A��A�p�A֍PA։7A��A�v�A��A�Q�AӇ+A�oAՋDA�JA�M�A��TA��/A���A��/A�~�A�XAօAк^A�dZA�M�A�9XAՋDA։7A�l�A�$�A�^5A֍PA֍PA֍PAԬA�A֑hA֍PA�p�A։7AօA֍PA֋DA֍PA֑hA֍PA֏\A֋DA֏\A֋DA֓uA֏\A֏\A֕�A֡�A֓uA֙�A։7A։7AօAփAև+AօAօA։7Aև+AօA։7A֍PA֍PAօA։7A֋DA։7A։7A֍PA֍PAօAև+Aև+A֏\A�`BA֋DA։7AօA֋DA֋DA։7A֍PAև+A֋DA։7A֍PA֏\A֕�A֍PA֋DA֋DA֋DA։7AփA֍PA֍PA֕�AօA�bA֑hA֑hAօA֏\A֗�Aև+A֋DA֍PA֕�AօAփA։7A֏\AփAև+Aև+AփAցA։7AցA�~�A�~�AփA�|�A�~�A֑hA֕�Aև+AփAփA։7A։7A֗�Aև+AօA��mAև+A։7Aև+Aև+A֑hAօAև+Aև+Aև+Aև+A֋DAցA։7A։7A֏\AփAցA�~�AօAփA�~�A�~�AօAցAփAցAև+A�~�A�~�AցAցAօA�|�AցA�z�AցA�|�A�|�AցA�~�A�~�A�|�A�z�AօA�|�A�z�A�x�A�~�A�z�A�|�A�x�A�z�A�v�A�v�A�z�AօA�z�A�z�AցAօA։7A։7AցA֏\AփA֍PAփAև+A֑hA�~�A�~�AցAփAև+A�|�A�z�A�~�A�~�A�x�A�z�A�x�A�~�A�x�A�z�A�x�A�x�A�x�A�|�A�z�A�x�A�x�A�t�A�x�A�x�A�z�A�v�A�v�A�p�A�p�A�x�AՋDA�t�A�v�A�|�A�t�A�z�A�z�A�z�A�z�A�t�A�z�A�z�A�|�A�r�A�z�A�~�A�|�A։7A�x�A�|�AփA�|�A�x�A�x�A�x�A�v�A�l�A�t�A�x�A�v�A�t�A�n�A�n�A�t�A�t�A�r�A�r�A�p�A�t�A�r�A�z�A�t�A�n�A�v�A�x�A�r�A�x�A�r�A�t�A�z�AցAփA�x�A�r�A�z�A�n�A�r�A�t�A�n�A�r�A�z�A�x�A�r�A�x�A�~�AօA�~�A�z�A�|�A�x�A�v�A�|�AօA�~�A�p�A�t�A�v�A�r�A�ffA�n�A�r�A�jA�l�A�z�A�x�A�v�AփA�r�A�r�A�v�A�r�A�z�A�x�A�|�A�x�A�~�A�|�A�z�A�|�A�~�AցA�|�A�x�A�x�A�x�AօA֋DA։7A�~�A�z�AփA�~�A�z�A�t�A�~�A�hsA�hsA�hsA�jA�l�A�jA�n�A�ffA�ffA�hsA�n�A�l�A�ffA�dZA�jA�hsA�dZA�ffA�dZA�`BA�dZA�`BA�ZA�XA�\)A�VA�VA�S�A�VA�O�A�O�A�Q�A�G�A�I�A�K�A�G�A�I�A�K�A�I�A�I�A�I�A�G�A�E�A�E�A�G�A�C�A�E�A�E�A�C�A�E�A�C�A�E�A�C�A�A�A�A�A�C�A�A�A�;dA�;dA�9XA�;dA�9XA�7LA�;dA�7LA�7LA�5?A�5?A�5?A�1'A�5?A�33A�1'A�1'A�1'A�1'A�-A�-A�+A�/A�+A�(�A�&�A�&�A�&�A�"�A�"�A�&�A�"�A�$�A�$�A�"�A��A�"�A�"�A��A��A��A��A�{A�bA�oA�VA�  A�  A���A���A��mA���A�ĜAմ9Aա�AՉ7A�l�A�dZA�ffA�ffA�bNA�`BA�ZA�XA�VA�O�A�M�A�K�A�I�A�G�A�E�A�E�A�;dA�9XA�5?A�7LA�/A�-A�/A�-A� �A��A�bA�
=A���A���A��A��A��TA�ĜAԺ^AԸRAԲ-AԬAԧ�Aԩ�Aԥ�Aԣ�Aԡ�Aԡ�Aԡ�Aԟ�Aԟ�Aԝ�Aԛ�Aԟ�Aԛ�Aԙ�Aԕ�AԑhAԕ�AԑhAԑhAԑhAԏ\Aԏ\AԋDAԉ7Aԉ7AԁAԁAԁA�~�A�|�A�x�A�x�A�t�A�r�A�l�A�l�A�dZA�`BA�ZA�VA�E�A�;dA�-A��A��A�  A���A��`A��TA��;A���A���A���A���A�ȴA�ȴA�ƨA�ĜA�ƨA�ȴA�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�A�@���@�|�@�dZ@�\)@�S�@�S�@�K�@�C�@�;d@�33@�33@�+@�"�@�"�@�"�@��@�o@�
=@�@�@���@��@��y@��y@��H@��@��@��@���@�ȴ@�ȴ@�ȴ@���@���@��R@��R@��!@���@���@���@���@���@���@���@���@���@���@���@��\@��\@��+@�~�@�~�@�~�@�~�@�v�@�v�@�v�@�n�@�v�@�n�@�n�@�n�@�ff@�ff@�ff@�ff@�^5@�V@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�5?@�-@�-@�-@�-@�-@�-@�$�@�$�@�$�@�-@�$�@�$�@��@��@��@�{@�J@�@�@�@�@�@���@��@��T@��#@���@���@���@�@�@��^@��^@��^@��-@���@��-@��`AօA֋DA։7A�~�A�z�AփA�~�A�z�A�t�A�~�A�hsA�hsA�hsA�jA�l�A�jA�n�A�ffA�ffA�hsA�n�A�l�A�ffA�dZA�jA�hsA�dZA�ffA�dZA�`BA�dZA�`BA�ZA�XA�\)A�VA�VA�S�A�VA�O�A�O�A�Q�A�G�A�I�A�K�A�G�A�I�A�K�A�I�A�I�A�I�A�G�A�E�A�E�A�G�A�C�A�E�A�E�A�C�A�E�A�C�A�E�A�C�A�A�A�A�A�C�A�A�A�;dA�;dA�9XA�;dA�9XA�7LA�;dA�7LA�7LA�5?A�5?A�5?A�1'A�5?A�33A�1'A�1'A�1'A�1'A�-A�-A�+A�/A�+A�(�A�&�A�&�A�&�A�"�A�"�A�&�A�"�A�$�A�$�A�"�A��A�"�A�"�A��A��A��A��A�{A�bA�oA�VA�  A�  A���A���A��mA���A�ĜAմ9Aա�AՉ7A�l�A�dZA�ffA�ffA�bNA�`BA�ZA�XA�VA�O�A�M�A�K�A�I�A�G�A�E�A�E�A�;dA�9XA�5?A�7LA�/A�-A�/A�-A� �A��A�bA�
=A���A���A��A��A��TA�ĜAԺ^AԸRAԲ-AԬAԧ�Aԩ�Aԥ�Aԣ�Aԡ�Aԡ�Aԡ�Aԟ�Aԟ�Aԝ�Aԛ�Aԟ�Aԛ�Aԙ�Aԕ�AԑhAԕ�AԑhAԑhAԑhAԏ\Aԏ\AԋDAԉ7Aԉ7AԁAԁAԁA�~�A�|�A�x�A�x�A�t�A�r�A�l�A�l�A�dZA�`BA�ZA�VA�E�A�;dA�-A��A��A�  A���A��`A��TA��;A���A���A���A���A�ȴA�ȴA�ƨA�ĜA�ƨA�ȴA�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�A�@���@�|�@�dZ@�\)@�S�@�S�@�K�@�C�@�;d@�33@�33@�+@�"�@�"�@�"�@��@�o@�
=@�@�@���@��@��y@��y@��H@��@��@��@���@�ȴ@�ȴ@�ȴ@���@���@��R@��R@��!@���@���@���@���@���@���@���@���@���@���@���@��\@��\@��+@�~�@�~�@�~�@�~�@�v�@�v�@�v�@�n�@�v�@�n�@�n�@�n�@�ff@�ff@�ff@�ff@�^5@�V@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�=q@�=q@�=q@�5?@�-@�-@�-@�-@�-@�-@�$�@�$�@�$�@�-@�$�@�$�@��@��@��@�{@�J@�@�@�@�@�@���@��@��T@��#@���@���@���@�@�@��^@��^@��^@��-@���@��-@��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�;oG�O�G�O�;o;o;o;o;o;oG�O�;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���?���>�;@���@��c>.�?Ց�@�Ԁ@��8=�2v?Pqa@�ԕ=�^�=�Ë>d�?��>R�@�=?��=���=�=�b�>�5@��Z@�9>	�;?��?�x�>n;�=��w=��|=���> �>n&�@���@�ײ=��>��??��@��[@��=�!�>���?1JM>{@�Б?nP>y�@���=�y�>�#>�s@8��>�5?��@��=���=�nn?Z&=�k'>4,=@�x�=���=�`�>oi>��>|�v@��w=�i�=Ӹ�> �]@�Bp=�}?M{�>R�&?�_=��V>*82@v��?)!>>=>�K�=��t=�?Y}k@��Z>Yu?���@���=�P�=ѷ>,��@��F=��=��>
�m>��=�?}>&�@���>��?b�@��%>C/E?��=P��=d:=�s�=�==�{�>A�@�Ӯ@�ם=ˬq?:%1@��>8��@%��=�I=�e>��=�R>!�6@�ѷ>��=?)=Ai�=R�=^J8=��=�Se=��>H��>R��=���?�&@��+@��H=�^_=��@>?h@��Q@���@��r>n�@�ɛ@�ć@�g8=���>^*�@0�@��?��=�\)>��?�W�?i�7=�{_>	�W@qx?��>*�>�~@��@�ܜ@��@>@RA_@M�n@��\@��W=�M@@��@��n@�ޔ@��=��>��q@��e@���@��@=�a|>A�@�� @��]@��@��+=�Y�>'8=�8>
�?��`@���@���?��=�0�?���@��@8��@�jU@��]=��}?*��?�F5=Ǯ?i�z>��@��@�K�@��@3b>	�H>�`@���@��@���@�G>�:@"��@��@��@���@��@���@��@��@��?@��z@��z@��@��@��@���@��@��6@��@��@��@��@���@��@���@���@��@��@��@��@��@��@��@���@��@��@��z@��@��z@��@��z@��@���@��@��@��@��@��@��@���@��z@���@��@��@��@��@��z@��H@��H@���@���@��]@��@��@���@��@��?@���@�ޔ@��@��@���@��e@���@���@��H@�Q/@��e@��H@��"@��e@��@��e@��"@���@��7@���@�ޔ@��e@��H@��"@���@���@���@���@���@��@���@���@��@���@���@���@���@���@��+@���@��]@���@���@��@���@�ީ@��"@��"@���@��+@��"@���@��"@��"@���@��"@��@�ޔ@��+@���@���@��"@��+@��@���@�ޔ@��e@���@���@���@��7@���@��"@��e@���@��n@���@���@��@���@��+@��@�ޔ@��e@�ޔ@��@�ޔ@��@��+@�݃@��n@��n@�ܜ@�ޔ@�ޔ@��+@�ޔ@���@���@���@��@���@��+@���@���@��H@�ޔ@��+@��]@�ܜ@���@��@���@���@��@���@��#@��n@��@�ީ@���@��#@��]@��@�ܜ@��+@��n@�ܜ@�ں@�ں@�ܱ@��@��#@�ܜ@�ܜ@��#@��H@��#@���@�ܜ@��w@�ܱ@���@��w@��w@��w@b~R@��w@��Q@���@��@@�ܜ@��#@��@���@��w@��#@��w@��n@��n@�ں@���@�ں@�ܜ@��H@���@��Q@��H@�ܜ@��n@��H@��Q@��f@��#@��H@��#@��#@���@��Z@��@��Q@�ں@�ں@��@@��#@�ޔ@��n@��H@���@��@���@��@��@��Q@�٩@���@���@���@�ں@���@���@���@���@��H@�ܜ@���@��Q@�ں@�ں@�ں@�ں@��#@��w@��8@���@��w@��@���@��Q@��Q@��Q@�ם@�ם@�ٔ@�؄@�ٔ@��+@���@��w@���@�ޔ@��@��n@�ܱ@�ޔ@���@���@���@���@��]@��7@��7@��@��e@��@@��@��@���@��7@��7@��@��@�ܱ@��@�ی@���@���@���@��R@�Ԁ@��R@��5@���@�Ԁ@��[@��5@��,@��[@��9@�ջ@�ԕ@�ԕ@��>@��@��x@��x@�Ҟ@��c@�Ц@�ρ@���@���@���@��G@��@�ɰ@���@���@���@��m@���@�Ȋ@��?@��e@��!@��P@�ǹ@��P@��?@���@��*@��?@��?@�Ɠ@��e@�Ɠ@��?@��?@��m@���@��@��m@�ı@��H@���@��"@�¹@���@���@��+@��+@��+@���@��+@���@��n@��n@��H@���@���@���@���@��w@���@��w@���@���@���@���@���@���@���@���@���@��I@���@��@���@���@��R@��@��[@��[@��5@��5@��c@���@���@��F@��!@��d@���@���@��@���@��@���@���@��@��}@���@��@���@�N@�}V@�|�@�|1@�{@�y@�w@�v@�t�@�sC@�q�@�p�@�o @�m�@�mr@�k�@�i/@�h
@�f@�d0@�a@�_@�]�@�[�@�Wi@�S;@�O�@�K�@�G�@�E�@�A�@�<�@�7L@�0U@�*0@�)�@�'�@�%@�#�@�$_@�#:@�!�@�!B@� �@��@��@�`@��@��@��@��@�@�L@�&@�j@�@��@�@�b@��@�j@��@��@�@��@��@�N@�	�@�@�}@�@�P@��v@��Y@��b@��@��@�� @��!@��u@�߹@�خ@��@���@��@�ł@�õ@���@���@���@���@���@��|@��V@���@���@���@���@��@��:@��@��@��@��;@��3@���@���@��[@R�l@R��@R�}@R��@R��@Rȴ@Rǹ@R��@R��@Rŗ@R�H@R�L@R�"@R�z@R�@R��@R��@R�4@R�8@R�<@R�A@R��@R��@R�x@R��@R�(@R��@R�,@R��@R�1@R��@R��@R��@R��@R��@R��@R��@R��@R��@R��@R�F@R��@R�!@R�!@R��@R�%@R��@R�}@R��@R�.@R��@R��@R�2@R��@R��@R�@R�@R�@R��@R��@R�e@R�@R�i@R�?@R��@R��@R��@R��@R�v@R��@R��@R��@R�'@R�z@R��@R��@R�@R�U@R�Y@R�@R�^@R�^@R�^@R�^@R�
@R��@R�b@R�@R��@R��@R��@R�f@R�@R�k@R��@R�E@R��@R��@R��@R��@R��@R��@R��@R��@R��@R�>@R�B@R�F@R��@R��@R�O@R��@R��@R�.@R�@R��@R��@R�@R��@R�?@�L�@�O�@�M�@�L@�J�@�M@�L�@�JM@�Ft@�J�@�Bp@�At@�B�@�B@�C�@�B�@�B�@�Bp@�A@�A�@�D@�D|@�C@�@�@�At@�C�@�?�@�@y@�@O@�>l@�At@�?@�>�@�;�@�<�@�;�@�9�@�:�@�9�@�7�@�6e@�7a@�5�@�5~@�5?@�5�@�5�@�5+@�4�@�4@�4�@�4@�4@�4@�4D@�3�@�4D@�3�@�3�@�3�@�3�@�4@�3�@�3]@�2�@�2�@�1�@�0@@�/�@�/E@�/o@�/E@�/0@�/0@�/�@�/E@�.�@�.4@�.
@�.@�.I@�.@�.4@�.@�.4@�-8@�,@�,=@�,R@�,@�+�@�+,@�*�@�+,@�*�@�*@�)t@�*�@�(�@�*Z@�)�@�)�@�(�@�(�@�(�@�'�@�'g@�%�@�%�@�%1@�%@�$�@�#�@� @��@�?@��@�e@�0@��@�	�@�`@��7@��@��@���@��@��@��@��@��@���@��@��@��!@��T@���@��@��@��@@��
@���@��8@�خ@�ֶ@�֌@��@���@��y@��\@�ł@���@��b@��@���@��)@���@��@���@���@��_@���@��)@���@���@��K@��6@��@��:@��O@��@���@��@���@���@���@�� @��*@��@���@���@���@��@���@��@���@��@���@��Z@��@��@�~�@�}�@�|p@�{5@�x�@�w�@�t�@�qv@�o~@�l�@�e�@�a�@�Y�@�Ta@�N�@�F�@�A�@�<�@�:�@�9X@�6z@�1<@�1f@�0@�0@�0U@�/@�.�@�/�@�/@�-�@�-b@�-b@�-#@�/�@�/Z@�/�@�-�@�+�@QI@Q	�@Q�@QW@Q�@Q�@Q6@Q �@P�C@P��@P�r@P��@P��@P��@P��@P�/@P�3@P��@P��@P��@P�j@P��@P��@P��@P�@P��@P��@P�@P�@P�@P�c@P�@P�g@P��@P�l@P�@P��@P�@P�!@P�@P�@P�O@P��@P�}@P�)@P��@P�X@P��@P�@P��@P�@P��@P�@P�@P�@P�i@P�i@P�@P�m@P�C@P�@P�C@P�@P�@P�r@P��@P�L@P��@P��@P�@P��@P݃@P�@Pܱ@P܇@P܇@P�3@P��@P��@P�8@P�@P��@Pپ@Pپ@P��@P��@P�@P��@P�@@P�@P��@P��@P��@P�o@P�@P�s@P��@P�$@Pզ@PԀ@P�,@P��@P�V@P��@Pӄ@PБ@P�p@P�K@P�y@P��@P�)@P��@P�.@P��@Pȴ@P�`@P�@P�@P�;@P�iG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     344334433443444444444443444444444433444334444344344444434444434444434443444444344444434434443444444344344444444334434444444344444444444433444333433344434444444344433344433433334433344333344444334443433444444333444333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@���@��gG�O�G�O�@�ԁ@��:G�O�G�O�@�ԘG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@�״G�O�G�O�G�O�@��[@��G�O�G�O�G�O�G�O�@�ЕG�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�@�x�G�O�G�O�G�O�G�O�G�O�@��{G�O�G�O�G�O�@�BvG�O�G�O�G�O�G�O�G�O�G�O�@v��G�O�G�O�G�O�G�O�G�O�G�O�@��[G�O�G�O�@���G�O�G�O�G�O�@��FG�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@��&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Ӯ@�ךG�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ѻG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��.@��GG�O�G�O�G�O�@��R@���@��qG�O�@�ɝ@�ĉ@�g:G�O�G�O�G�O�@��}G�O�G�O�G�O�G�O�G�O�G�O�G�O�@qxG�O�G�O�G�O�@��@�ܚ@��AG�O�G�O�G�O�@��^@��XG�O�@��@��r@�ޖ@��G�O�G�O�@��f@���@��AG�O�G�O�@��"@��\@��@��(G�O�G�O�G�O�G�O�G�O�@���@���G�O�G�O�G�O�@��G�O�@�jV@��^G�O�G�O�G�O�G�O�G�O�G�O�@��@�K�@��G�O�G�O�G�O�@���@��@���@�G
G�O�G�O�@��@��	@���@��
@���@��@��@��B@��w@��|@��@��@��@���@��@��9@��@��@��@��@���@��@���@���@��@��@��@��@��@��@��@���@��@��@��{@��@��{@��@��y@��@���@��@��@��@��@��@��@���@��{@���@��@��@��@��@��v@��C@��E@���@���@��]@��@��@���@��@��A@���@�ޖ@��@��~@���@��f@���@���@��G@�Q0@��c@��J@��#@��d@��@��d@�� @���@��:@���@�ސ@��d@��M@��%@���@���@���@���@���@��@���@���@��@���@���@���@���@���@��.@���@��^@���@���@��@���@�ީ@�� @��"@���@��)@��#@���@�� @��%@���@�� @��@�ޖ@��.@���@���@��&@��,@��@���@�ސ@��f@���@���@���@��9@���@��!@��f@���@��o@���@���@��@���@��+@��@�ޓ@��i@�ޖ@��@�ޖ@��@��(@�݂@��t@��k@�ܚ@�ޖ@�ޔ@��.@�ޖ@���@���@���@��@���@��+@���@���@��J@�ޔ@��/@��]@�ܝ@���@��@���@���@��@���@��#@��o@��@�ު@���@�� @��a@��@�ܚ@��(@��j@�ܝ@�ڸ@�ں@�ܰ@��@��"@�ܝ@�ܞ@��"@��G@��"@���@�ܝ@��s@�ܲ@���@��{@��x@��u@b~P@��u@��S@���@��?@�ܞ@�� @��@���@��u@��"@��s@��t@��o@�ھ@���@�ڶ@�ܙ@��J@���@��T@��K@�ܚ@��j@��J@��P@��f@��#@��G@��&@��@���@��V@��@��Q@�ڸ@�ڽ@��D@��"@�ޓ@��n@��D@���@��@���@��@�ـ@��Q@�٦@���@���@���@�ڶ@���@���@���@���@��F@�ܞ@���@��O@�ڸ@�ڸ@�ڸ@�ں@��%@��v@��8@���@��z@��@���@��O@��V@��P@�מ@�כ@�ٓ@�؆@�ٙ@��*@���@��w@���@�ޔ@��@��q@�ܰ@�ޔ@���@���@���@���@��]@��:@��8@���@��c@��B@��@��@���@��6@��9@���@�L�@�O�@�M�@�L@�J�@�M�@�L�@�JP@�Fr@�J�@�Br@�Ax@�B�@�B@�C�@�B�@�B�@�Bp@�A@�A�@�D@�D~@�C@�@�@�Av@�C�@�?�@�@y@�@R@�>r@�Av@�?@�>�@�;�@�<�@�;�@�9�@�:�@�9�@�7�@�6e@�7e@�5�@�5�@�5@@�5�@�5�@�5+@�4~@�4@�4�@�4@�4@�4@�4B@�3�@�4F@�3�@�3�@�3�@�3�@�4 @�3�@�3c@�2�@�2�@�1�@�0B@�/�@�/H@�/q@�/H@�/.@�/2@�/�@�/B@�.�@�.2@�.
@�.@�.J@�.!@�.6@�.!@�.3@�-9@�,@�,@@�,R@�,@�+�@�+-@�*�@�+*@�*�@�*@�)r@�*�@�(�@�*[@�)�@�)�@�(�@�(�@�(�@�'�@�'j@�%�@�%�@�%1@�%@�$�@�#�@� 
@��@�A@��@�i@�0@��@�	�@�`@��6@��@��	@���@��@��@��@��@��@���@��@��
@��$@��V@���@��@��@��F@��@���@��;@�ت@�ִ@�֎@��@���@��{@��^@�Ń@���@��b@��@���@��-@���@��@���@���@��a@���@��(@���@���@��H@��4@��@��:@��P@��@���@��@���@���@���@��@��-@��@���@���@���@��@���@��!@���@��@���@��^@��@��@�~�@�}�@�|o@�{3@�x�@�w�@�t�@�q{@�o}@�l�@�e�@�a�@�Y�@�Tb@�N�@�F�@�A�@�<�@�:�@�9V@�6{@�1=@�1i@�0@�0@�0X@�/@�.�@�/�@�/@�-�@�-b@�-c@�-%@�/�@�/X@�/�@�-�@�+�@QJ@Q	�@Q�@QX@Q�@Q�@Q5@Q �@P�F@P��@P�s@P��@P��@P��@P��@P�2@P�5@P��@P��@P��@P�k@P��@P��@P��@P�@P��@P��@P�@P�@P�@P�b@P�@P�f@P��@P�m@P�@P��@P�@P�"@P�@P�@P�P@P��@P�{@P�*@P�@P�Z@P��@P�@P��@P�@P��@P�@P�@P�@P�h@P�k@P�@P�k@P�C@P�@P�C@P�@P�@P�u@P��@P�N@P��@P��@P�@P��@P݃@P�@Pܳ@P܈@P܈@P�5@P��@P��@P�5@P�@P��@Pٽ@Pٺ@P��@P��@P�@P��@P�>@P�@P��@P��@P��@P�p@P� @P�u@P��@P�&@Pգ@P�~@P�+@P��@P�X@P��@Pӆ@PЕ@P�n@P�N@P�{@P��@P�-@P��@P�+@P��@Pȳ@P�b@P�@P�@P�8@P�j@�L�@�O�@�M�@�L@�J�@�M�@�L�@�JP@�Fr@�J�@�Br@�Ax@�B�@�B@�C�@�B�@�B�@�Bp@�A@�A�@�D@�D~@�C@�@�@�Av@�C�@�?�@�@y@�@R@�>r@�Av@�?@�>�@�;�@�<�@�;�@�9�@�:�@�9�@�7�@�6e@�7e@�5�@�5�@�5@@�5�@�5�@�5+@�4~@�4@�4�@�4@�4@�4@�4B@�3�@�4F@�3�@�3�@�3�@�3�@�4 @�3�@�3c@�2�@�2�@�1�@�0B@�/�@�/H@�/q@�/H@�/.@�/2@�/�@�/B@�.�@�.2@�.
@�.@�.J@�.!@�.6@�.!@�.3@�-9@�,@�,@@�,R@�,@�+�@�+-@�*�@�+*@�*�@�*@�)r@�*�@�(�@�*[@�)�@�)�@�(�@�(�@�(�@�'�@�'j@�%�@�%�@�%1@�%@�$�@�#�@� 
@��@�A@��@�i@�0@��@�	�@�`@��6@��@��	@���@��@��@��@��@��@���@��@��
@��$@��V@���@��@��@��F@��@���@��;@�ت@�ִ@�֎@��@���@��{@��^@�Ń@���@��b@��@���@��-@���@��@���@���@��a@���@��(@���@���@��H@��4@��@��:@��P@��@���@��@���@���@���@��@��-@��@���@���@���@��@���@��!@���@��@���@��^@��@��@�~�@�}�@�|o@�{3@�x�@�w�@�t�@�q{@�o}@�l�@�e�@�a�@�Y�@�Tb@�N�@�F�@�A�@�<�@�:�@�9V@�6{@�1=@�1i@�0@�0@�0X@�/@�.�@�/�@�/@�-�@�-b@�-c@�-%@�/�@�/X@�/�@�-�@�+�@QJ@Q	�@Q�@QX@Q�@Q�@Q5@Q �@P�F@P��@P�s@P��@P��@P��@P��@P�2@P�5@P��@P��@P��@P�k@P��@P��@P��@P�@P��@P��@P�@P�@P�@P�b@P�@P�f@P��@P�m@P�@P��@P�@P�"@P�@P�@P�P@P��@P�{@P�*@P�@P�Z@P��@P�@P��@P�@P��@P�@P�@P�@P�h@P�k@P�@P�k@P�C@P�@P�C@P�@P�@P�u@P��@P�N@P��@P��@P�@P��@P݃@P�@Pܳ@P܈@P܈@P�5@P��@P��@P�5@P�@P��@Pٽ@Pٺ@P��@P��@P�@P��@P�>@P�@P��@P��@P��@P�p@P� @P�u@P��@P�&@Pգ@P�~@P�+@P��@P�X@P��@Pӆ@PЕ@P�n@P�N@P�{@P��@P�-@P��@P�+@P��@Pȳ@P�b@P�@P�@P�8@P�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     344334433443444444444443444444444433444334444344344444434444434444434443444444344444434434443444444344344444444334434444444344444444444433444333433344434444444344433344433433334433344333344444334443433444444333444333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9|�9|�(9|��9|�9|��9|� 9|�M9|�9|�9|�E9|�X9|��9|ۑ9|��9|�9|��9|��9|�U9|�Y9|�d9|ݫ9|�D9|�C9|�;9|��9|�R9|�A9|؈9|�P9|գ9|��9|֍9|վ9|ѥ9|ӄ9|�9|��9|�"9|��9|�&9|�'9|˕9|ɓ9|��9|ȅ9|�x9|�9|�g9|�q9|��9|�v9|ƾ9|��9|��9|�9|�i9|�!9|�n9|�59|�9|�n9|ƽ9|�9|��9|��9|��9|þ9|�f9|�w9|�9|�<9|�9|��9|��9|��9|��9|�J9|�u9|�<9|�T9|��9|�]9|�{9|�]9|�w9|�9|�s9|��9|��9|�m9|�9|�'9|��9|�"9|��9|��9|��9|��9|��9|��9|�e9|��9|��9|��9|��9|��9|��9|��9|�O9|��9|��9|�
9|��9|�D9|��9|�K9|�9|��9|��9|�a9|�9|�9|x�9|i�9|e�9|ep9|c�9|c�9|bF9|_�9|\�9|[�9|Y�9|W�9|V?9|S�9|R�9|R�9|Q;9|Ls9|IF9|FX9|H9|Ds9|A�9|Aq9|?Z9|9A9|39|.�9|)!9|�9|�9|�9|9|�9{��9{�9{�
9{�o9{��9{�F9{�9{�9{��9{�z9{�]9{߽9{��9{�9{�X9{��9{�X9{ܐ9{�+9{ٍ9{�9{�W9{ձ9{� 9{�]9{�]9{��9{��9{�J9{ϼ9{��9{ɋ9{��9{�o9{�o9{�e9{�9{��9{� 9{��9{�y9{��9{�C9{�k9{��9{��9{��9{��9{��9{�9{t�9{mw9{e�9{b�9{a,9{]9{U�9{U�9{T�9{S�9{TX9{Rr9{R;9{S�9{R�9{P�9{P9{P 9{O�9{S�9{R�9{S29{P�9{M�9w99J9;9�99�9�9�9&99�9(999�9J99Y9
�9
M9	 9�9�9�9S99�9�99�9V9�9�9,9�9�9�9�9/9.9 �9 �9 \9 "9 9��9�39��9��9�C9��9�79��9��9��9��9��9�	9��9�)9��9��9�v9�Y9��9��9�/9��9��9��9��9�/9��9��9��9��9�]9�^9��9��9��9��9��9��9��9�9��9�}9��9�D9�D9�F9��9��9�79�s9�I9��9�9��9�9��9�9�i9�P9��9��9�c9��9�u9�69�9��9�9�w9�;9�9�9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B;dB<jB<jB;dB;dB<jB=qBD�BP�BL�BF�BD�B?}B9XB�B��BB�3B�B�B�B�B�B�'B�LB�9B�9B�9B�LB�LBBÖBɺB��B�mB�mB�ZB�NB�;B�/B�#B�B��BBPBhBuB{BuBbB\B\BPB1B��B�B��B�}B�wB�!B��BjB`BB[#BVBJ�BC�B6FB#�B �B �B�B�B%B�B��BɺB�wB��B�7Bw�BffB\)BE�B=qB49B1B
�B
�ZB
�`B
��B
�LB
��B
��B
�=B
�B
r�B
W
B
49B
�B
%B	��B	�B	�NB	��B	ŢB	�}B	�qB	�^B	�?B	�'B	��B	��B	��B	�hB	�PB	�B	k�B	bNB	T�B	K�B	E�B	C�B	C�B	A�B	:^B	33B	/B	&�B	�B	�B	PB	+B	B��B��B�B�yB�fB�HB�B��B��B��B��B��BŢB�}B�^B�RB�FB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�VB�JB�JB�JB�=B�1B�+B�+B�%B�B�B�B�B�+B�+B�+B�7B�7B�JB�PB�PB�PB�PB�\B�bB�bB�bB�bB�uB�uB�uB��B�oB�VB�=B�B~�By�Bu�Bv�Bv�Bw�Bx�Bz�B|�B�B�B� B~�B�B�B�B�B�B�B�B� B� B~�B~�B}�B}�B}�B}�B|�B{�B|�B|�B|�B� B� B�B�B�B�=B�DB�JB�\B�bB�uB��B��B��B��B��B��B��B�B�-B�LB�^B�dB�jB��B��B��BÖBŢBǮBǮBȴBɺB��B��B��B��B�
B�;B�NB�`B�B�B�B�B�B�B��B��B	B	B	DB	JB	DB	
=B	1B	1B		7B	
=B	DB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	+B	+B	)�B	(�B	)�B	+B	/B	1'B	7LB	8RB	8RB	7LB	8RB	:^B	<jB	<jB	=qB	=qB	?}B	C�B	I�B	O�B	R�B	S�B	T�B	XB	YB	[#B	^5B	^5B	]/B	_;B	cTB	gmB	iyB	k�B	p�B	r�B	u�B	u�B	u�B	u�B	u�B	v�B	v�B	x�B	x�B	{�B	~�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�1B	�=B	�JB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�-B	�9B	�?B	�LB	�LB	�RB	�RB	�^B	�jB	�wB	�}B	��B	��B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B  B	�B	�B	�B	�B	�)B	�)B	�5B	�;B	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
%B
%B
+B
+B
+B
1B

=B
�B
	B
#�B
+B
1�B
6�B
;�B
A�B
HB
K^B
P�B
X�B
^�B
c�B
h�B
m]B
q�B
vB
y>B
{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�j@�I�?7N�B�B�!?3�nA��B�B�?@�K�B>ы�?\�?�w�@��W?@�AF��@���>�B>̩?�?Ձ�B��A?�6?*�A"�@Ц�?�@�>�U�>�2�>��?�+?� CB�mB0�? ��?1�)@~��B�GB��?�.?��%@s��??��B��@���?&�;Bn2>��]?.��?��A��|?4ѷA- �B�>�@�>��u@�̯?�r?^וBk�>��H>�$[?5�?���?���B�>�o�?�|?EXdBɉ?Ho@��?���@�1�?
��?Q�A�:�@dO?l�?���>�>�?X�@��	B�?.��@ψBN�>���>�L�?S�$B�>�v>�}|?-;@
�a>�vq?LY�B�P?)|�@�9zB��?v6�@��>pX>�$>���>�`�? ��?pGB��B��>��@w+�B?�?g�Az��>��B?�?&�I?��?G��B��@$Cc>i�/>l�q>��>�>�>�ɧ>� �?c?y��?���?[�@.ZhB��B��>�oz>��A�v:B��B�wBZJ?EܥB��B��BZa?��?��A�[$B�"@��N>��f?<#!@�L�@��n>ȭ�?(r;A�@A5�?B�?�-#B��B��B�?1�?A���A�I�A���B�"?�A�y�B�oB��Bڙ?
gA?І/B�B�(B��>ڳh?/�OA���B��B��B��?{t?O�?�W?'�@�ٌB��B��A��?�!@�+ B�4A�:AȞ�BC?�@ao�@��>�xh@��!?�B��A�N�B�qA�d?(d�?6HsA��6A��B��B&�@��AoYxB��B��B��Bb A�U=B�$B�tB�]B� B��B��B��B�JB�B�B�TB�B�!B��B�JB�<B��B�B�#B�BB��B�B��B�B�;B�B�&B��B�<B��B��B��B��B�lB��B�KB�6B�<B�DB��B��B�qB�AB��B�<B�RB�zB�DB�>B�6B�WB�!B��B�B�jB�<B��B�,B��B�JB��B��B�B�4B��B��B�BB��B��A�úB�HB��B��B�B�XB�9B�WB�:B�qB�QB�	B�oB��B�gB��B��B�4B��B��B��B��B��B�;B�+B�B�XB��B��B��B��B�,B��B�HB��B��B�+B��B�B��B�B��B�aB��B��B��B��B��B��B�UB�	B��B�oB�|B�>B��B�	B�iB��B��B��B��B��B��B�aB��B��B�1B��B��B�)B��B�LB�fB�#B��B�6B��B� B��B�JB�^B�1B�6B��B�9B�B��B��B�B�3B��B�iB��B��B�lB�?B��B�MB��B��B��B��B�#B��B�-B�.B��B��B��B��B��B�B��B�6B�6B��B��B��B��B��B�QB��B��B��B�tB��B��B�~B��B�>B�dB�B�^B�^B�fB�6A��~B��B��B� B�KB�tB�B��B��B��B�!B�dB�^B�YB��B�9B��B��B��B�	B�=B�WB�6B��B��B��B��B��B��B��B�vB��B�[B��B��B��B��B�YB�~B�XB�)B�~B��B��B��B��B�rB��B�+B�9B��B��B��B�.B�B�~B��B��B�0B��B�\B��B��B��B�7B�7B��B�+B�<B�&B��B��B�ZB��B�-B�#B�YB��B�hB�5B�;B��B��B��B��B�B��B�=B��B�mB�B��B��B�]B��B��B��B�RB�xB��B��B��B��B��B�B�<B��B�hB�'B��B��B��B�IB��B�9B�.B��B��B��B��B�^B��B��B��B�=B�cB�>B�"B�:B�hB�@B�{B�'B��B�B�@B�2B�'B�
B�	B�DB�jB��B�&B� B�&B�lB�zB�5B��B�AB��B�B�NB��B��B��B�B�`B�sB� B��B�{B�kB�B��B��B��B�?B��B��B�1B�B��B��B�LB��B�B��B�yB�;B��B��B�B�vB��B�eB��B�B�eB�bB��B�eB��B��B��B��B�#B��B��B��B�tB�B�|B�YB�;B��B��B��B��B��B��B�$B��B��B�)B�9B�%B�B��B��B��B�B��B�~B�B�jB��B�)B�WB��B��B��B�B��B�XB��B��B�\B��B��B�
B�B�DB��B��B�
B��B�YB��B��B��B�B��B�#B�`B�'B�GB��B�B� B�nB��B�`B�JB��B�$B��B�0B��B��B� B��B��B��B�eB��B��B��B��B��B��B�QB�
B��B�oB�tB�cB�B��B� B�B��B�aB��B�CB�hB��B��B��B��B�SB��B��B��B�1B��B�2B�"B�dB��B�&B�B�9B�AB��B�CB�[B�CB�.B�B�<B�mB�{B��B�cB�pB��B��B�0B�5B��B��B�+B��BN�BSGBSWBS�BT/BSJBS�BU	BT$BUBTBTGBU.BT�BS�BTKBUDBU5BU�BT�BT�BU�BU�BVBV�BU�BU�BU#BU�BUcBU�BU�BV^BVBV�BVEBV�BU�BV�BV�BVlBU�BUzBVsBV(BV�BVhBVBV�BW%BV�BV�BWJBV�BV�BW^BWCBW6BV�BV�BW�BWNBV�BV�BW$BW	BW�BV�BWjBXBW�BW�BWQBX�BW�BW�BW=BWmBW�BWoBV�BW�BW�BW�BWmBW"BV�BW�BWGBXMBW,BW�BW�BWBV_BW'BV�BV�BWiBX2BXbBW�BW�BW�BW$BXBXhBX�BX-BX�BX"BX�BX�BX�BXVBYBXKBX�BXlBWZB;�B;�B;
B=9B=[B<�B=�B=B;�B;�B<�B;�B<�B;�B<LB<PB:�B=iB<)B<B;�B<�B=�B<�B:�B=�B;VB;UB;�B;�B<�B<LB>B<=B;�B=HB;)B<�B;!B;�B:oB:�B=!B;�B:�B<�B;�B:�B:�B:gB:�B;B;�B;�B;$B<;B;�B;cB;�B;B<B;sB;�B<TB;�B:�B:�B;�B;B;�B:�B;~B<+B:�B<�B<B<lB;�B;�B=EB;�B<mB=8B=B=(B<6B<�B<�B=�B;�B=B=FB=�B=�B=�B>�B=�B=�B=YB=�B=cB=�B>�B<�B=!B=�B?�B? B>�B?B@{B?`B?�BBBA�BA�BB�BEUBF�BH�BJ�BKBO�BQ;BQ�BP�BO�BQ`BQ+BQ�BP�BP�BQ�BQEBQ6BPWBP�BQ&BP;BQBO�BO�BO�BP�BO�BN�BNBN�BNBNaBM:BL�BKrBMCBK�BKlBKBI�BG�BH�BH>BH:BF�BG�BF�BGeBGJBF9BGBG*BF�BGDBE�BF[BF9BF�BF�BEBBE�BEIBEyBF:BD�BF�BC[BD�BC�BDBB�BClBD,BC�BB�BCBB�BB�BBBB+B@�BA_B@�B@ZB@xB>�B@DB=�B?B>vB?�B>�B?:B@�B?�B?�B>�B?VB?�B?!B?�B?�B>SB>�B=�B>FB>B?�B?�B?RB?`B=lB
B
�B

B
�B
�B
�B
UB
CB
2B
�B
�B
oB
�B
pB
bB
QB
�B
UB
�B
�B
tB
CB
�B
�B
�B
QB
B
�B
bB
�B
UB

B
�B
)B
�B
xB
?B
kB
B
�B
}B
2B
�B
�B
7B
B
�B
)B
�B
ZB
 B
�B
�B
�B
}B
bB
HB
�B
�B
HB
}B
2B
B
�B
�B

B
�B
)B
�B
�B
�B
YB
�B
�B
}B
oB
B
�B
�B
HB
B
:B
B
B
B
	B
B
�B
jB
�B
B
�B
�B
�B
:B
�B
�B
�B
KB
wB
-B
�B
$B
�B
�B
]B
�B
�B
fB
�B
YB
	B
�B
�B
jB
 B
�B
	#B
$B
"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344334433443444444444443444444444433444334444344344444434444434444434443444444344444434434443444444344344444444334434444444344444444444433444333433344434444444344433344433433334433344333344444334443433444444333444333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B;qB<wB<tB;rB;qB<sB=~G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�B�<B�#B�(B�B�B�*G�O�B�WB�HG�O�G�O�B�]B�[BBâB��B�B�zB�yB�fB�_B�KB�>B�0B��B��B&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6VB#�B �B �B�B�B0B�B�B��B��B�B�GBw�BfrB\9BE�B=B4HBAB
�B
�gB
�nB
��B
�[B
� B
��B
�MB
�B
r�B
WB
4HB
�B
6B	��B	�B	�^B	�B	ŲB	��B	��B	�lB	�OB	�9B	�B	��B	��B	�yB	�_B	�B	k�B	b_B	UB	K�B	E�B	C�B	C�B	A�B	:pB	3BB	/,B	&�B	�B	�B	aB	;B	B��B��B��B�B�vB�WB�)B�B��B��B��B��BŲB��B�nB�dB�VB�DB�2B�%B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�gB�[B�[B�]B�NB�CB�=B�=B�8B�1B�2B�-B�2B�<B�<B�>B�JB�HB�ZB�aB�cB�aB�bB�nB�sB�uB�uB�sB��B��B��B��B�B�gB�NB�0BBy�Bu�Bv�Bv�Bw�Bx�Bz�B} B�B�B�BB�B�%B�-B�&B�&B�#B�B�B�BBB~	B~B~	B~B} B{�B|�B} B} B�B�B�&B�%B�3B�PB�XB�^B�nB�uB��B��B��B��B��B��B�B�B�B�?B�bB�rB�xB��B��B��B��BéBŴBǾB��B��B��B��B��B�B�B�B�NB�aB�sB�B�B��B��B��B��B��B�B	4B	6B	VB	^B	VB	
PB	DB	EB		JB	
OB	YB	vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	+B	+B	*B	)	B	*B	+B	//B	1<B	7`B	8gB	8hB	7`B	8fB	:qB	<~B	<�B	=�B	=�B	?�B	C�B	I�B	O�B	SB	TB	UB	X$B	Y(B	[8B	^JB	^JB	]BB	_MB	cfB	g�B	i�B	k�B	p�B	r�B	u�B	u�B	u�B	u�B	u�B	v�B	v�B	x�B	x�B	{�B	B	�B	�"B	�$B	�,B	�4B	�8B	�?B	�@B	�@B	�DB	�RB	�^B	�dB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�*B	�7B	�5B	�8B	�?B	�NB	�QB	�`B	�`B	�fB	�fB	�pB	�|B	��B	��B	��B	��B	êB	ëB	ıB	ŶB	ƻB	��B	��B	��G�O�B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�0B	�?B	�<B	�KB	�MB	�^B	�[B	�cB	�dB	�fB	�lB	�rB	�sB	�zB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B
 B
 B
 B
 B
B
B
B
$B
.B
:B
8B
@B
?B
>B
DG�O�B
�B
B
#�B
+B
1�B
6�B
;�B
A�B
H)B
KtB
P�B
X�B
^�B
c�B
h�B
mqB
q�B
v&B
yRB
{�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�uG�O�G�O�B�B�.G�O�G�O�B�B�G�O�G�O�B'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�xB0�G�O�G�O�G�O�B�SB��G�O�G�O�G�O�G�O�B��G�O�G�O�BnAG�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�Bk�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�BɘG�O�G�O�G�O�G�O�G�O�G�O�A�;G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�BN�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�B�[G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�G�O�G�O�B?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��G�O�G�O�G�O�B��B��BZXG�O�B��B��BZoG�O�G�O�G�O�B�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�*G�O�G�O�G�O�B��B��B�G�O�G�O�G�O�A��B�.G�O�A�y�B�|B��BڥG�O�G�O�B�+B�3B��G�O�G�O�A��B��B��B��G�O�G�O�G�O�G�O�G�O�B��B��G�O�G�O�G�O�B�@G�O�AȞ�BPG�O�G�O�G�O�G�O�G�O�G�O�B��A�N�B�~G�O�G�O�G�O�A��IA��B��B&�G�O�G�O�B��B��B��Bb+A�UMB�2B�~B�jB�
B��B��B��B�UB�'B�B�_B�B�.B��B�WB�HB��B�%B�0B�NB�	B��B��B�B�GB�B�3B��B�IB��B��B�B��B�vB��B�VB�BB�HB�QB��B��B�~B�MB��B�HB�_B��B�QB�IB�?B�aB�*B��B�B�wB�IB��B�8B��B�TB�B��B�'B�=B�B��B�KB��B��A���B�RB��B��B�B�dB�EB�cB�GB�B�aB�B�xB��B�sB��B��B�>B��B��B��B��B��B�GB�6B�B�cB��B��B��B��B�9B��B�VB��B��B�7B��B�%B��B�,B��B�nB��B��B��B��B��B��B�aB�B��B�|B��B�KB��B�B�wB�B��B��B��B��B��B�kB��B��B�>B��B��B�5B��B�XB�qB�1B��B�CB��B�B��B�WB�nB�<B�>B��B�DB�B�B��B�(B�>B��B�vB��B��B�xB�KB��B�[B��B��B��B��B�-B��B�9B�;B��B��B��B��B��B�$B��B�CB�>B��B��B��B��B��B�[B��B��B��B��B��B��B��B��B�IB�mB�&B�iB�mB�rB�@A�ҍB��B�B�B�WB��B�$B��B��B��B�-B�mB�nB�fB��B�DB�B��B��B�B�JB�dB�>B��B��B��B�
B��B��B��B�B��B�dB��B��B��B��B�fB��B�dB�5B��B��B��B��B��B�}B��B�4B�HB��B��B��B�:B�B��B� B��B�<B�	B�eB��B��B��B�AB�CB��B�7B�IB�2B��B��B�cB��B�8B�1B�dB��B�uB�CB�HB��B��B�B��B�+B��B�HB��B�zB�B��B��B�hB��B��B��B�^B��B��B�B��B��B��B�B;�B;�B;B=DB=jB<�B=�B=B;�B;�B<�B<B=B;�B<VB<YB:�B=uB<5B<B;�B<�B=�B<�B:�B=�B;cB;aB;�B;�B<�B<YB>%B<IB;�B=UB;6B<�B;-B;�B:yB:�B=-B;�B:�B=B;�B:�B:�B:sB:�B;B;�B;�B;/B<GB;�B;rB<B;B<B;|B;�B<dB;�B:�B:�B;�B;%B;�B:�B;�B<5B:�B<�B<&B<xB;�B;�B=OB;�B<zB=EB=)B=4B<BB<�B<�B=�B;�B='B=QB=�B=�B=�B>�B=�B=�B=eB=�B=oB=�B>�B=B=-B=�B?�B?B>�B?B@�B?mB?�BBBA�BA�BB�BEdBF�BH�BJ�BK%BO�BQIBRBP�BO�BQkBQ7BQ�BP�BP�BRBQRBQCBPdBP�BQ2BPIBQ(BO�BO�BO�BP�BO�BN�BNBN�BN#BNoBMGBL�BK}BMMBK�BK{BKBI�BG�BH�BHKBHBBF�BG�BF�BGpBGUBFFBG*BG7BF�BGOBFBFeBFCBF�BF�BEOBE�BEWBE�BFIBD�BF�BCjBD�BC�BDBCBCxBD7BC�BB�BCBB�BB�BBBB5B@�BAgB@�B@gB@�B?B@SB=�B?B>�B@B>�B?EB@�B?�B?�B? B?dB?�B?+B?�B?�B>aB>�B=�B>RB>B?�B@B?bB?iB=xB
*B
�B
B
B
�B
�B
gB
UB
GB
�B
�B
�B
�B
�B
vB
eB
�B
gB
�B
�B
�B
WB
�B
�B
�B
dB
B
�B
tB
 B
iB
B
�B
=B
�B
�B
SB
}B
B
�B
�B
FB
�B
�B
JB
1B
�B
>B
�B
nB
3B
�B
B
�B
�B
uB
]B
B
�B
[B
�B
FB
B
�B
�B
B
�B
=B
�B
�B
�B
lB
B
�B
�B
�B
*B
�B
�B
YB
#B
OB
4B
B
+B
B
2B
B
}B
�B
'B
�B
�B
�B
OB
�B
�B
�B
]B
�B
?B
�B
9B
�B
�B
rB
�B
B
|B
�B
nB
	2B
�B
�B
~B
3B
�B
	6B
5B
"0B;�B;�B;B=DB=jB<�B=�B=B;�B;�B<�B<B=B;�B<VB<YB:�B=uB<5B<B;�B<�B=�B<�B:�B=�B;cB;aB;�B;�B<�B<YB>%B<IB;�B=UB;6B<�B;-B;�B:yB:�B=-B;�B:�B=B;�B:�B:�B:sB:�B;B;�B;�B;/B<GB;�B;rB<B;B<B;|B;�B<dB;�B:�B:�B;�B;%B;�B:�B;�B<5B:�B<�B<&B<xB;�B;�B=OB;�B<zB=EB=)B=4B<BB<�B<�B=�B;�B='B=QB=�B=�B=�B>�B=�B=�B=eB=�B=oB=�B>�B=B=-B=�B?�B?B>�B?B@�B?mB?�BBBA�BA�BB�BEdBF�BH�BJ�BK%BO�BQIBRBP�BO�BQkBQ7BQ�BP�BP�BRBQRBQCBPdBP�BQ2BPIBQ(BO�BO�BO�BP�BO�BN�BNBN�BN#BNoBMGBL�BK}BMMBK�BK{BKBI�BG�BH�BHKBHBBF�BG�BF�BGpBGUBFFBG*BG7BF�BGOBFBFeBFCBF�BF�BEOBE�BEWBE�BFIBD�BF�BCjBD�BC�BDBCBCxBD7BC�BB�BCBB�BB�BBBB5B@�BAgB@�B@gB@�B?B@SB=�B?B>�B@B>�B?EB@�B?�B?�B? B?dB?�B?+B?�B?�B>aB>�B=�B>RB>B?�B@B?bB?iB=xB
*B
�B
B
B
�B
�B
gB
UB
GB
�B
�B
�B
�B
�B
vB
eB
�B
gB
�B
�B
�B
WB
�B
�B
�B
dB
B
�B
tB
 B
iB
B
�B
=B
�B
�B
SB
}B
B
�B
�B
FB
�B
�B
JB
1B
�B
>B
�B
nB
3B
�B
B
�B
�B
uB
]B
B
�B
[B
�B
FB
B
�B
�B
B
�B
=B
�B
�B
�B
lB
B
�B
�B
�B
*B
�B
�B
YB
#B
OB
4B
B
+B
B
2B
B
}B
�B
'B
�B
�B
�B
OB
�B
�B
�B
]B
�B
?B
�B
9B
�B
�B
rB
�B
B
|B
�B
nB
	2B
�B
�B
~B
3B
�B
	6B
5B
"0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344334433443444444444443444444444433444334444344344444434444434444434443444444344444434434443444444344344444444334434444444344444444444433444333433344434444444344433344433433334433344333344444334443433444444333444333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011538052020090115380520200901153805202009011538052020090115380520200901153805202009011538052020090115380520200901153805202009011538052020090115380520200901153805AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202122002018112021220020181120212200    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122002018112021220020181120212200  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122002018112021220020181120212200  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011538052020090115380520200901153805  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                