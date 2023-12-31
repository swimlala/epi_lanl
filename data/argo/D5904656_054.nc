CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ~   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:40Z creation      
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
resolution        =���   axis      Z        )�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
|  n�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )�  yt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
|  �\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )�  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
| �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� $   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
| 6   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� @�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )� jp   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
| �X   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
| ȼ   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )� �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
| '   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� 1�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
| [l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )� e�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173040  20200828145512  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               6   6   6AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @׷ a��\@׷ a��\@׷ a��\111 @׷ �r@׷ �r@׷ �r@4Լj~��@4Լj~��@4Լj~���cg�z�H�cg�z�H�cg�z�H111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    6   6   6ADA BDA  DA BDA @9��@�  @�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt�fDt�fDyk�D�	HD�=D�w\D��
D��D�@RD��{D��)D��D�@ D��=D��RD�
�D�?\DچfDతD��D�8�D�mqD��\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>���    =���=���            >L��        =���                                >L��>L��                        =���        >L��=���        =���=���                                        =���>L��                            =���        =���            =���                    >L��>���        =���>���        =���            >L��>L��            =���            =���>L��        =���=���        =���                                    =���        >L��=���        =���                >L��                                            =���                    =���        =���        =���>L��        =���            >L��>L��=���            =���        >L��=���=���            =���                =���    >L��        =���    >L��>L��=���    >L��>L��>L��=���            =���=���=���            >L��=���    =���    =���=���=���=���                =���>L��    >L��=���    =���=���=���>L��=���>L��=���=���    >L��=���=���=���>L��>L��=���=���>L��=���>���>L��>L��>���>L��=���>L��>L��>L��>L��>���>L��>L��>���>L��>���>���>���>L��>L��>L��=���>L��>���>���=���>���>L��>L��>L��>L��>L��>L��>L��>���>L��=���>L��>���>L��>L��>���>L��>���>L��>���>L��>L��>L��>���>���>���>���>L��>���>L��=���>L��>���>L��>���>���>L��>���>���>L��>L��>���>L��>���>L��>L��>L��>L��>L��>���>L��>���>L��>L��>L��>���>L��>���>���>���>���>L��>L��>���>L��>L��>���>���>L��>���>L��>���>���>���>���>���>���>L��=���>L��>���>L��>L��>L��>L��>���>L��>L��>���>L��>L��>L��>L��>���>L��>L��>L��>���>���>L��>L��>���>L��>���>L��>L��>���>���>���>L��>���>L��=���>L��>���>���>���>L��>���>L��>L��>���>���>���>���>L��>���>���>L��>L��>���>L��>���>L��>���>L��>L��>L��>���>���>���>���>L��>���>L��>���>L��>���>L��>���>L��>���>���>L��>L��>���>���>���>L��>L��>L��>L��>L��>���>���>���>���>���>L��>L��>���>���>���>���>���>���>���>L��>���>L��>L��>L��>L��>���=���>���>���>L��>L��>L��>L��>L��>���>L��>L��>L��>L��>���=���>L��>L��>���>L��>���>L��>���>L��>L��>L��>L��>���>L��>L��>���?   >���>L��>L��=���>L��>L��>���>L��>���=���    >L��>L��>L��>���>L��>���=���=���>L��>���>���=���=���>���>L��=���>���>���=���>L��>���>L��>L��>L��>���>L��>L��>���>L��>L��>L��=���>L��>���>���>���>���?   ?��?333?333?L��?���?�  ?�  ?���?���?�ff?�33?�  ?�  ?���?ٙ�?�ff?�33@   @ff@��@33@��@   @&ff@,��@,��@9��@@  @Fff@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�  @�ff@ə�@���@�33@�ff@ٙ�@���@�  @�ff@陚@���@�33@�ff@���@���A��A33A��AffA  A33A��AffA  A��A��AffA  A��A33AffA   A!��A$��A&ffA(  A)��A,��A.ffA0  A333A4��A6ffA9��A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAP  AQ��AT��AVffAX  AY��A\��A^ffA`  Aa��Ad��AfffAh  Ai��Ak33AnffAp  Aq��As33At��AvffAx  A{33A{33A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�  A�  A���A͙�A�ffA�33A�  A���A���Aљ�A�ffA�33A�33A�  A���Aՙ�Aՙ�A�ffA�33A�  A�  A���Aٙ�Aٙ�A�ffA�33A�  A���A���Aݙ�A�ffA�33Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq� Dq�fDq��Dq��Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�Dr@ DrFfDrL�DrY�Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��Dt  Dt�Dt3Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt�3Dt��Duf@9��@@  @Fff@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�  @�ff@ə�@���@�33@�ff@ٙ�@���@�  @�ff@陚@���@�33@�ff@���@���A��A33A��AffA  A33A��AffA  A��A��AffA  A��A33AffA   A!��A$��A&ffA(  A)��A,��A.ffA0  A333A4��A6ffA9��A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAP  AQ��AT��AVffAX  AY��A\��A^ffA`  Aa��Ad��AfffAh  Ai��Ak33AnffAp  Aq��As33At��AvffAx  A{33A{33A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�  A�  A���A͙�A�ffA�33A�  A���A���Aљ�A�ffA�33A�33A�  A���Aՙ�Aՙ�A�ffA�33A�  A�  A���Aٙ�Aٙ�A�ffA�33A�  A���A���Aݙ�A�ffA�33Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq� Dq�fDq��Dq��Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�Dr@ DrFfDrL�DrY�Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��Dt  Dt�Dt3Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt�3Dt��DufG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @7
>@}p�@��R@��RAA=A_\)A\)A��A��A��A��AϮA�z�A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BX=pB_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C>]C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDqwDq�qDr}qDr�qDs}qDs�qDt��Dt��Dyh�D� D�;�D�vD���D�{D�?
D��3D���D� �D�>�D���D��
D�	�D�>DڅD�\D��RD�7�D�l)D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�>���#�
=u=u�#�
�#�
�#�
>#�
�#�
�#�
=u�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
>#�
>#�
�#�
�#�
�#�
�#�
�#�
�#�
=u�#�
�#�
>#�
=u�#�
�#�
=u=u�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
=u>#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
=u�#�
�#�
=u�#�
�#�
�#�
=u�#�
�#�
�#�
�#�
�#�
>#�
>���#�
�#�
=u>���#�
�#�
=u�#�
�#�
�#�
>#�
>#�
�#�
�#�
�#�
=u�#�
�#�
�#�
=u>#�
�#�
�#�
=u=u�#�
�#�
=u�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
=u�#�
�#�
>#�
=u�#�
�#�
=u�#�
�#�
�#�
�#�
>#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
�#�
=u�#�
�#�
�#�
�#�
�#�
=u�#�
�#�
=u�#�
�#�
=u>#�
�#�
�#�
=u�#�
�#�
�#�
>#�
>#�
=u�#�
�#�
�#�
=u�#�
�#�
>#�
=u=u�#�
�#�
�#�
=u�#�
�#�
�#�
�#�
=u�#�
>#�
�#�
�#�
=u�#�
>#�
>#�
=u�#�
>#�
>#�
>#�
=u�#�
�#�
�#�
=u=u=u�#�
�#�
�#�
>#�
=u�#�
=u�#�
=u=u=u=u�#�
�#�
�#�
�#�
=u>#�
�#�
>#�
=u�#�
=u=u=u>#�
=u>#�
=u=u�#�
>#�
=u=u=u>#�
>#�
=u=u>#�
=u>��>#�
>#�
>��>#�
=u>#�
>#�
>#�
>#�
>��>#�
>#�
>��>#�
>��>��>��>#�
>#�
>#�
=u>#�
>��>��=u>��>#�
>#�
>#�
>#�
>#�
>#�
>#�
>��>#�
=u>#�
>��>#�
>#�
>��>#�
>��>#�
>��>#�
>#�
>#�
>��>��>��>��>#�
>��>#�
=u>#�
>��>#�
>��>��>#�
>��>��>#�
>#�
>��>#�
>��>#�
>#�
>#�
>#�
>#�
>�Q�>#�
>��>#�
>#�
>#�
>��>#�
>��>��>�Q�>��>#�
>#�
>��>#�
>#�
>��>��>#�
>��>#�
>��>��>�Q�>��>�Q�>��>#�
=u>#�
>��>#�
>#�
>#�
>#�
>��>#�
>#�
>��>#�
>#�
>#�
>#�
>��>#�
>#�
>#�
>�Q�>��>#�
>#�
>��>#�
>��>#�
>#�
>��>��>��>#�
>��>#�
=u>#�
>��>��>��>#�
>��>#�
>#�
>��>��>��>��>#�
>��>��>#�
>#�
>��>#�
>��>#�
>��>#�
>#�
>#�
>��>��>��>��>#�
>��>#�
>��>#�
>��>#�
>��>#�
>��>��>#�
>#�
>��>��>��>#�
>#�
>#�
>#�
>#�
>��>��>��>��>��>#�
>#�
>��>��>��>��>��>��>��>#�
>��>#�
>#�
>#�
>#�
>��=u>�Q�>��>#�
>#�
>#�
>#�
>#�
>��>#�
>#�
>#�
>#�
>��=u>#�
>#�
>�Q�>#�
>��>#�
>��>#�
>#�
>#�
>#�
>��>#�
>#�
>��>�>��>#�
>#�
=u>#�
>#�
>��>#�
>��=u�#�
>#�
>#�
>#�
>��>#�
>��=u=u>#�
>��>��=u=u>��>#�
=u>��>��=u>#�
>��>#�
>#�
>#�
>��>#�
>#�
>��>#�
>#�
>#�
=u>#�
>��>��>�Q�>�Q�>�?\)?(��?(��?B�\?��?u?u?��?�z�?�G�?�{?��H?��H?Ǯ?�z�?�G�?�{?��H@�
@
=q@��@
>@p�@#�
@*=q@*=q@7
>@=p�@C�
@P��@W
>@c�
@j=q@p��@}p�@��@��@�Q�@��R@��@��@��@��R@��@��@��@��R@��@�Q�@��@��R@��@�Q�@˅@��@��@�Q�@ۅ@޸R@��@�Q�@�@��@��@�Q�@��A ��A�\A(�AA\)A
�\A(�AA\)A��A(�AA\)A��A�\AA\)A ��A$(�A%A'\)A(��A,(�A-A/\)A2�\A4(�A5A8��A:�\A<(�A=A@��AB�\AEAG\)AH��AL(�AMAO\)AP��AT(�AUAW\)AX��A\(�A]A_\)A`��Ad(�AeAg\)Ah��Aj�\AmAo\)Ap��Ar�\At(�AuAw\)Az�\Az�\A}A\)A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A��GA��A�z�A�G�A�zA��GA��A�z�A�zA��GA��A�z�A�G�A�zA��GA�z�A�G�A�zA��GA��A�z�A�G�A��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GAîA�z�A�G�A�zA��GAǮA�z�A�G�A�zA��GAˮAˮA�z�A�G�A�zA��GAϮA�z�A�z�A�G�A�zA��GA��GAӮA�z�A�G�A�G�A�zA��GA׮A׮A�z�A�G�A�G�A�zA��GAۮA�z�A�z�A�G�A�zA��GDq��Dq�>Dq��Dq�qDq��Dq�>Dq��Dq�Dq��Dq�>DqФDq�Dq�qDq��Dq�>Dq�Dq�qDr�Dr
>Dr�DrDr#�Dr*>Dr0�Dr7Dr=qDrC�DrJ>DrWDr]qDrc�Drj>Drp�Dr}qDr��Dr�>Dr��Dr�Dr�qDr�>Dr��Dr�Dr�qDr��Dr�>DrФDr�qDr��Dr�>Dr�Dr�Dr�qDs
>Ds�DsDsqDs#�Ds0�Ds7Ds=qDsC�DsJ>DsP�DsWDsc�Dsj>Dsp�DswDs}qDs��Ds��Ds�Ds�qDs��Ds��Ds�Ds�qDs��Ds�>DsФDs�Ds��Ds�>Ds�Ds�Ds�qDt
>Dt�DtDtqDt#�Dt0�Dt7Dt=qDtC�DtJ>DtWDt]qDtc�Dtj>Dtp�DtwDt��Dt�>Dt��Dt�Dt�qDt��Dt��Dt�Dt�qDt��Dt�>Dt�Dt�qDt��Dt�>Dt�Dt�Du�@7
>@=p�@C�
@P��@W
>@c�
@j=q@p��@}p�@��@��@�Q�@��R@��@��@��@��R@��@��@��@��R@��@�Q�@��@��R@��@�Q�@˅@��@��@�Q�@ۅ@޸R@��@�Q�@�@��@��@�Q�@��A ��A�\A(�AA\)A
�\A(�AA\)A��A(�AA\)A��A�\AA\)A ��A$(�A%A'\)A(��A,(�A-A/\)A2�\A4(�A5A8��A:�\A<(�A=A@��AB�\AEAG\)AH��AL(�AMAO\)AP��AT(�AUAW\)AX��A\(�A]A_\)A`��Ad(�AeAg\)Ah��Aj�\AmAo\)Ap��Ar�\At(�AuAw\)Az�\Az�\A}A\)A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A��GA��A�z�A�G�A�zA��GA��A�z�A�zA��GA��A�z�A�G�A�zA��GA�z�A�G�A�zA��GA��A�z�A�G�A��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�G�A�zA��GA��A�z�A�G�A�zA��GA��A�z�A�G�A�zA��GAîA�z�A�G�A�zA��GAǮA�z�A�G�A�zA��GAˮAˮA�z�A�G�A�zA��GAϮA�z�A�z�A�G�A�zA��GA��GAӮA�z�A�G�A�G�A�zA��GA׮A׮A�z�A�G�A�G�A�zA��GAۮA�z�A�z�A�G�A�zA��GDq��Dq�>Dq��Dq�qDq��Dq�>Dq��Dq�Dq��Dq�>DqФDq�Dq�qDq��Dq�>Dq�Dq�qDr�Dr
>Dr�DrDr#�Dr*>Dr0�Dr7Dr=qDrC�DrJ>DrWDr]qDrc�Drj>Drp�Dr}qDr��Dr�>Dr��Dr�Dr�qDr�>Dr��Dr�Dr�qDr��Dr�>DrФDr�qDr��Dr�>Dr�Dr�Dr�qDs
>Ds�DsDsqDs#�Ds0�Ds7Ds=qDsC�DsJ>DsP�DsWDsc�Dsj>Dsp�DswDs}qDs��Ds��Ds�Ds�qDs��Ds��Ds�Ds�qDs��Ds�>DsФDs�Ds��Ds�>Ds�Ds�Ds�qDt
>Dt�DtDtqDt#�Dt0�Dt7Dt=qDtC�DtJ>DtWDt]qDtc�Dtj>Dtp�DtwDt��Dt�>Dt��Dt�Dt�qDt��Dt��Dt�Dt�qDt��Dt�>Dt�Dt�qDt��Dt�>Dt�Dt�Du�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�t�A�t�A�t�A�t�A�|�AЇ+AЛ�AП�AЩ�A�-A�C�A���A�`BA͟�A�p�A�^5A�K�A�C�A̕�A���A�C�A���A�bNA���A�v�Aț�A�C�A�VAƧ�A�jA���A�5?A��A�~�A�;dA�ƨA�C�A��mA��mA��A��A���A��DA�|�A��A�hsA���A�VA�I�A�$�A�ZA�~�A�%A��A�ĜA�~�A���A�G�A��A�33A���A��9A�VA�bA��yA��!A�1A�{A�S�A��HA�v�A���A��7A�I�A�%A��jA�I�A�jA��mA�ffA���A�VA��A��A���A�9XA��PA���A���A�-A���A�+A�-A���A� �A���A���A�VA�r�A�\)A��PA��A�?}A{��Aw?}AtĜAr  Apv�Ap�+Ao��Amp�Al�Ak&�Ai�Af=qAe`BAdn�Ab9XA_G�A]A[33AX��AW�
AU�#AQ�TAQl�APn�AN�9AMx�ALĜAK�wAJ�/AH�\AE&�AEoAC��AAp�A>��A;�hA:�A:�A8�A5O�A3|�A1�#A0�A/�hA.  A-t�A,=qA+/A*�/A)�^A(ZA%�;A$�yA#�TA#�A"�RA!|�A!A �RA VA��A��An�AA�^A�9A�`AVAƨAC�A��AA�A�A?}AbNA�TAr�A
=A�FA=qA�A1AA�
A7LA
��A&�AG�A	l�A�9A�mA�hAS�A7LAA��A��A��A ZA (�@��@�l�@�o@���@��@�@�ȴ@���@�$�@�p�@���@�I�@�I�@�1'@� �@��P@��@��D@�V@��H@���@�|�@�+@�V@���@�7@���@���@��@�^5@�`B@��/@��@�r�@���@�"�@�!@旍@�S�@�!@�b@�-@�V@ڰ!@�Q�@�-@Ցh@�Z@с@с@Гu@�C�@ͩ�@̼j@˝�@ʧ�@ʇ+@ʸR@�-@�1@��H@�@�p�@��@�A�@�|�@�/@�`B@��u@�1@�\)@���@�^5@�$�@���@��@��@��j@���@���@�-@�M�@�E�@�n�@���@�{@�A�@���@��@���@�1'@��@��@���@�$�@��^@��j@���@��+@��#@��7@��@��@�v�@��7@�v�@�O�@���@�(�@�1@��@��@��7@�r�@�X@��@�?}@���@���@�hs@��@��@�(�@�  @���@�5?@�b@�x�@�`B@���@��j@��F@��@�E�@���@�G�@��@�%@�%@���@���@���@�Ĝ@�ƨ@���@���@�t�@�K�@���@��@���@��@��#@�E�@�M�@�5?@�$�@�{@��@��T@��h@��@��@�p�@�?}@�O�@�&�@��D@���@�7L@���@�@���@��^@��@�x�@�p�@�p�@�`B@��@���@��@��@�r�@�Z@�1'@� �@��m@���@��P@�l�@�K�@�
=@��@��R@���@���@��R@���@�5?@��@�@��T@���@�`B@�?}@��@�  @��@�$�@�@�X@��@���@��/@�Ĝ@���@��D@�z�@�r�@�Z@�9X@� �@��@�1@��@���@���@���@�|�@�l�@�C�@�o@�ȴ@�v�@�E�@�-@���@��#@�@��^@���@���@���@��h@��h@�X@��@��`@���@�bN@�9X@���@��F@���@�t�@�dZ@�\)@�o@���@���@��+@�n�@�E�@�-@�-@�=q@�{@���@��-@��^@���@��@���@�z�@�Q�@�(�@�1@�@|�@|4n@t�e@lu�@c�@\	�@V3�@P/�@I?}@A��@;s@5m]@-<6@(m�@#�@|�@C�@C@�m@�M@
�R@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�l�A���A�VA�A���A�A�ĜA�ĜA�K�A�?}A��PA���A�ZA���A�C�A���A���A��A���A�^5A�I�A���A�1A��!A�x�A���A��A��!A���A��A��`A�z�A�ĜA��A�A�A��\A�ƨA�|�A��jA�Q�A���A��A���A�dZA��uA��A�"�A�hsA�VA���A��A��+A�A��HA�z�A�A�&�A�K�A���A��A��FA�oA��A���A��+A�E�A��A�C�A�O�A�l�A̧�A�bA�1AɁA���A�  A�M�A��A���A�
=A�;dA�S�A���A�%A�~�A�5?A�E�A��PA� �A��PAϾwA��A���A���A���A�1A���A�ƨA�~�A���A�VA��A���A��PA�ZA�z�A�G�A�ZA�A�A� �A�VA��^A�^5A�jA���A�r�A�G�AüjA��HA���A��;A�ƨA�ƨA�33A�dZA��A�=qA��yA�-A��
A�/A�r�A��A�x�A���A�ZA�&�A�A�r�A�
=A�+A�v�A��7A��A�^5A��;A�I�A�1'A���A�(�A��;Aʟ�A�1'A�v�A�XA�ZA¶FA��
A�-A�;dA̓A���A�A�ĜA�1A�JA�l�A���A�%A��!A���A�VA��yA�7LA���A�x�A�
=A�G�A���A�-A�"�A��A�VA�l�A�n�AƮA�`BA���A�M�A�jA���A��A��;A��`AìAϬA�?}A�=qA��A��FA�-A��`A�hsA���A��A�jA�S�A�$�A��A�n�A�ȴA�%A�{A�VA�t�A�l�A�+A���A�ĜA�A�?}A�oAÁAɼjAĸRA�1A�ffAϸRA�bNA���A���A�  A��A�|�A�v�A�x�A�t�A�x�A�oA��A�ĜA�v�A�t�A�r�A�v�A�;dAϕ�A�r�A�t�A�x�A�~�A�x�A�+A�r�A��mA�v�AϾwA�r�A�S�A�bNA�t�A�oA� �A�v�A�r�A�p�A̲-A�r�A�l�A�?}Aˡ�A�r�A�t�A�v�A�t�A�v�A�A�p�A�7LA�p�A�l�A϶FA�5?A�r�A�t�A�p�A�l�A�r�A�C�A���A��yA�r�A�jA�hsA�p�A�p�A�Q�A�t�A�r�A;wAϸRA�l�A�n�A�r�A�n�A�l�A�bNA�n�A�p�A�n�A��A�n�Aϧ�A�n�A�r�A�t�A��AΥ�A�v�A�x�A�t�A�&�A��yA�hsAȡ�A�p�A�z�A�t�A�t�A�E�A�t�A�v�A�x�A�x�A�v�A�v�A��/A�7LA�XA�G�A�x�AΧ�A�x�A�r�A�x�A�jA�"�A�x�A�t�A�+A�l�A�VA�t�A�r�A�p�A�33A�l�A�v�A�n�A�VA�v�A�r�A͛�A�p�A�l�A�v�A�v�A�t�A�v�A�n�A�bNA��A�
=A�r�A�r�A�p�A���A�r�A�hsA�z�A�t�A�r�A�r�A�p�A�n�A���A�n�A�=qA�XA�Q�A�r�A�p�A�n�A�\)A�Q�A�r�A�z�A�v�A�n�A�t�A�r�A�l�A�p�A�t�A�hsA�?}A�r�A�p�A�l�A�n�A�`BA�t�A�&�A�n�A�r�A�v�A�x�A�t�A�S�A�x�A�5?A���A�r�A�x�A�t�A�r�A�p�A�l�AϑhA�r�A�r�A�p�A�t�A�r�A�r�A�t�A�p�AƲ-A�p�A�G�A�r�A�p�A���A�M�A�jA�r�A�x�A� �A�l�A�t�A˴9A�v�A���A�t�A�t�A�I�A�l�A��TA�r�A�p�A���A�t�A�r�A�r�A�r�A�p�A�M�A�/A�l�A˩�A�bNA�^5A�p�A�l�A�t�AǗ�A�p�A��A�n�A�bNA�t�A��A�l�A̮A��
A��A�t�A�p�A�jA�t�A�p�A�33AɶFA�l�A�v�A�n�A�p�A�A�ffA�n�A�ffAΉ7A�r�A�ffA���A�n�A�(�A�p�Aϣ�A�ffA�hsA�dZA���A�ffA�~�A�`BA�5?A�-A�n�A�p�A�t�A�l�AЁA�v�A�z�A�x�A�x�A�z�A�z�A�t�A�~�A�|�A�x�A�z�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�z�A�|�A�~�A�|�A�|�A�z�A�z�A�z�A�x�A�|�A�z�A�z�A�z�A�z�A�x�A�~�A�x�A�|�A�~�A�x�A�|�A�x�A�z�A�|�A�~�A�z�A�|�A�z�A�|�A�z�A�z�A�|�A�z�A�|�A�|�A�z�A�z�A�z�A�|�A�~�A�|�A�|�A�z�A�z�A�x�A�|�A�|�A�|�A�z�A�z�A�z�A�|�A�z�A�z�A�|�A�|�A�~�A�z�A�z�A�|�A�x�A�|�AЁA�|�A�|�A�~�A�~�A�z�A�z�A�~�A�z�A�|�A�|�A�~�A�|�A�~�A�|�A�|�A�~�A�~�A�~�A�|�A�~�A�~�A�~�A�|�A�~�A�|�A�~�A�~�A�|�A�|�A�z�A�z�A�~�A�~�A�~�A�|�A�~�A�z�A�|�A�~�A�|�A�z�A�z�A�|�A�z�A�z�A�z�A�z�A�|�A�|�A�z�A�|�A�z�A�|�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�|�A�z�A�|�A�~�A�z�A�|�A�|�A�~�A�|�A�z�A�z�A�z�A�|�A�|�A�z�A�~�A�|�A�|�A�z�A�z�A�z�A�x�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�z�A�x�A�z�A�z�A�z�A�|�A�|�A�z�A�z�A�z�A�z�A�|�A�z�A�z�A�|�A�z�A�|�A�z�A�|�A�|�A�z�A�|�A�|�A�|�A�~�A�|�AЃAЁA�~�AЃAЃAЁA�~�AЁAЁAЃAЃAЁAЃAЃAЃAЁAЃAЃAЅAЇ+AЅAЅAЉ7AЇ+AЅAЇ+AЇ+AЋDAЉ7AЋDAЋDAЍPAЏ\AЍPAБhAБhAГuAГuAГuAГuAГuAБhAГuAЗ�AЙ�AЙ�AЗ�AЛ�AЗ�AЙ�AЛ�AЛ�AН�AП�@��/@���@��9@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@��u@��D@��D@��D@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�r�@�j@�j@�j@�j@�bN@�bN@�Q�@�I�@�A�@�9X@�(�@� �@� �@� �@� �@� �@� �@� �@� �@�(�@� �@� �@�(�@� �@�(�@�(�@� �@� �@�(�@�(�@�(�@�(�@�(�@�(�@�1'@�9X@�(�@��@�1@�  @�@�@��@��@��@�w@��@�w@�w@�w@�w@�w@�w@�w@�w@�w@�w@�w@�w@�w@�@�@��@�P@��@��@�P@�P@��@�P@�P@�P@l�@\)A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�r�A�t�A�v�A�t�A�v�A�t�A�v�A�t�A�t�A�r�A�t�A�t�A�t�A�t�A�t�A�v�A�r�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�r�A�r�A�r�A�t�A�t�A�v�A�t�A�r�A�r�A�t�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�v�A�t�A�v�A�t�A�v�A�t�A�t�A�v�A�t�A�v�A�v�A�t�A�t�A�v�A�x�A�v�A�v�A�v�A�t�A�x�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�t�A�t�A�r�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�v�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�x�A�v�A�t�A�t�A�t�A�t�A�v�A�v�A�z�A�x�A�z�A�|�A�z�A�|�A�|�A�|�A�z�A�|�A�|�A�z�A�z�A�|�A�|�A�|�A�~�A�z�A�~�A�|�A�|�AЁAЁAЁAЁAЃAЃAЁAЃAЅAЅAЃAЅAЉ7AЉ7AЉ7AЉ7AЏ\AЏ\AЏ\AЏ\AЍPAЍPAЏ\AЏ\AГuAЏ\AГuAЕ�AЕ�AГuAБhAГuAЗ�AЗ�AЛ�@���@��j@��9@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@��u@��D@��D@��@��@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�bN@�bN@�Z@�Q�@�I�@�9X@�(�@� �@� �@�(�@� �@� �@� �@� �@� �@� �@� �@� �@�(�@�(�@�(�@�(�@�(�@� �@� �@� �@�(�@�(�@�(�@�1'@�1'@�9X@�1'@��@�b@�1@�@�@��@�w@��@��@�w@�w@��@�w@�w@�w@��@�w@�w@�w@�w@�w@�w@�@�@��@��@��@��@��@�P@��@�P@�P@�P@�P@l�@l�@l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�t�A�t�A�t�A�t�A�|�AЇ+AЛ�AП�AЩ�A�-A�C�A���A�`BA͟�A�p�A�^5A�K�A�C�A̕�A���A�C�A���A�bNA���A�v�Aț�A�C�A�VAƧ�A�jA���A�5?A��A�~�A�;dA�ƨA�C�A��mA��mA��A��A���A��DA�|�A��A�hsA���A�VA�I�A�$�A�ZA�~�A�%A��A�ĜA�~�A���A�G�A��A�33A���A��9A�VA�bA��yA��!A�1A�{A�S�A��HA�v�A���A��7A�I�A�%A��jA�I�A�jA��mA�ffA���A�VA��A��A���A�9XA��PA���A���A�-A���A�+A�-A���A� �A���A���A�VA�r�A�\)A��PA��A�?}A{��Aw?}AtĜAr  Apv�Ap�+Ao��Amp�Al�Ak&�Ai�Af=qAe`BAdn�Ab9XA_G�A]A[33AX��AW�
AU�#AQ�TAQl�APn�AN�9AMx�ALĜAK�wAJ�/AH�\AE&�AEoAC��AAp�A>��A;�hA:�A:�A8�A5O�A3|�A1�#A0�A/�hA.  A-t�A,=qA+/A*�/A)�^A(ZA%�;A$�yA#�TA#�A"�RA!|�A!A �RA VA��A��An�AA�^A�9A�`AVAƨAC�A��AA�A�A?}AbNA�TAr�A
=A�FA=qA�A1AA�
A7LA
��A&�AG�A	l�A�9A�mA�hAS�A7LAA��A��A��A ZA (�@��@�l�@�o@���@��@�@�ȴ@���@�$�@�p�@���@�I�@�I�@�1'@� �@��P@��@��D@�V@��H@���@�|�@�+@�V@���@�7@���@���@��@�^5@�`B@��/@��@�r�@���@�"�@�!@旍@�S�@�!@�b@�-@�V@ڰ!@�Q�@�-@Ցh@�Z@с@с@Гu@�C�@ͩ�@̼j@˝�@ʧ�@ʇ+@ʸR@�-@�1@��H@�@�p�@��@�A�@�|�@�/@�`B@��u@�1@�\)@���@�^5@�$�@���@��@��@��j@���@���@�-@�M�@�E�@�n�@���@�{@�A�@���@��@���@�1'@��@��@���@�$�@��^@��j@���@��+@��#@��7@��@��@�v�@��7@�v�@�O�@���@�(�@�1@��@��@��7@�r�@�X@��@�?}@���@���@�hs@��@��@�(�@�  @���@�5?@�b@�x�@�`B@���@��j@��F@��@�E�@���@�G�@��@�%@�%@���@���@���@�Ĝ@�ƨ@���@���@�t�@�K�@���@��@���@��@��#@�E�@�M�@�5?@�$�@�{@��@��T@��h@��@��@�p�@�?}@�O�@�&�@��D@���@�7L@���@�@���@��^@��@�x�@�p�@�p�@�`B@��@���@��@��@�r�@�Z@�1'@� �@��m@���@��P@�l�@�K�@�
=@��@��R@���@���@��R@���@�5?@��@�@��T@���@�`B@�?}@��@�  @��@�$�@�@�X@��@���@��/@�Ĝ@���@��D@�z�@�r�@�Z@�9X@� �@��@�1@��@���@���@���@�|�@�l�@�C�@�o@�ȴ@�v�@�E�@�-@���@��#@�@��^@���@���@���@��h@��h@�X@��@��`@���@�bN@�9X@���@��F@���@�t�@�dZ@�\)@�o@���@���@��+@�n�@�E�@�-@�-@�=q@�{@���@��-@��^@���@��@���@�z�@�Q�@�(�@�1@�G�O�@|4n@t�e@lu�@c�@\	�@V3�@P/�@I?}@A��@;s@5m]@-<6@(m�@#�@|�@C�@C@�m@�M@
�R@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�l�A���A�VA�A���A�A�ĜA�ĜA�K�A�?}A��PA���A�ZA���A�C�A���A���A��A���A�^5A�I�A���A�1A��!A�x�A���A��A��!A���A��A��`A�z�A�ĜA��A�A�A��\A�ƨA�|�A��jA�Q�A���A��A���A�dZA��uA��A�"�A�hsA�VA���A��A��+A�A��HA�z�A�A�&�A�K�A���A��A��FA�oA��A���A��+A�E�A��A�C�A�O�A�l�A̧�A�bA�1AɁA���A�  A�M�A��A���A�
=A�;dA�S�A���A�%A�~�A�5?A�E�A��PA� �A��PAϾwA��A���A���A���A�1A���A�ƨA�~�A���A�VA��A���A��PA�ZA�z�A�G�A�ZA�A�A� �A�VA��^A�^5A�jA���A�r�A�G�AüjA��HA���A��;A�ƨA�ƨA�33A�dZA��A�=qA��yA�-A��
A�/A�r�A��A�x�A���A�ZA�&�A�A�r�A�
=A�+A�v�A��7A��A�^5A��;A�I�A�1'A���A�(�A��;Aʟ�A�1'A�v�A�XA�ZA¶FA��
A�-A�;dA̓A���A�A�ĜA�1A�JA�l�A���A�%A��!A���A�VA��yA�7LA���A�x�A�
=A�G�A���A�-A�"�A��A�VA�l�A�n�AƮA�`BA���A�M�A�jA���A��A��;A��`AìAϬA�?}A�=qA��A��FA�-A��`A�hsA���A��A�jA�S�A�$�A��A�n�A�ȴA�%A�{A�VA�t�A�l�A�+A���A�ĜA�A�?}A�oAÁAɼjAĸRA�1A�ffAϸRA�bNA���A���A�  A��A�|�A�v�A�x�A�t�A�x�A�oA��A�ĜA�v�A�t�A�r�A�v�A�;dAϕ�A�r�A�t�A�x�A�~�A�x�A�+A�r�A��mA�v�AϾwA�r�A�S�A�bNA�t�A�oA� �A�v�A�r�A�p�A̲-A�r�A�l�A�?}Aˡ�A�r�A�t�A�v�A�t�A�v�A�A�p�A�7LA�p�A�l�A϶FA�5?A�r�A�t�A�p�A�l�A�r�A�C�A���A��yA�r�A�jA�hsA�p�A�p�A�Q�A�t�A�r�A;wAϸRA�l�A�n�A�r�A�n�A�l�A�bNA�n�A�p�A�n�A��A�n�Aϧ�A�n�A�r�A�t�A��AΥ�A�v�A�x�A�t�A�&�A��yA�hsAȡ�A�p�A�z�A�t�A�t�A�E�A�t�A�v�A�x�A�x�A�v�A�v�A��/A�7LA�XA�G�A�x�AΧ�A�x�A�r�A�x�A�jA�"�A�x�A�t�A�+A�l�A�VA�t�A�r�A�p�A�33A�l�A�v�A�n�A�VA�v�A�r�A͛�A�p�A�l�A�v�A�v�A�t�A�v�A�n�A�bNA��A�
=A�r�A�r�A�p�A���A�r�A�hsA�z�A�t�A�r�A�r�A�p�A�n�A���A�n�A�=qA�XA�Q�A�r�A�p�A�n�A�\)A�Q�A�r�A�z�A�v�A�n�A�t�A�r�A�l�A�p�A�t�A�hsA�?}A�r�A�p�A�l�A�n�A�`BA�t�A�&�A�n�A�r�A�v�A�x�A�t�A�S�A�x�A�5?A���A�r�A�x�A�t�A�r�A�p�A�l�AϑhA�r�A�r�A�p�A�t�A�r�A�r�A�t�A�p�AƲ-A�p�A�G�A�r�A�p�A���A�M�A�jA�r�A�x�A� �A�l�A�t�A˴9A�v�A���A�t�A�t�A�I�A�l�A��TA�r�A�p�A���A�t�A�r�A�r�A�r�A�p�A�M�A�/A�l�A˩�A�bNA�^5A�p�A�l�A�t�AǗ�A�p�A��A�n�A�bNA�t�A��A�l�A̮A��
A��A�t�A�p�A�jA�t�A�p�A�33AɶFA�l�A�v�A�n�A�p�A�A�ffA�n�A�ffAΉ7A�r�A�ffA���A�n�A�(�A�p�Aϣ�A�ffA�hsA�dZA���A�ffA�~�A�`BA�5?A�-A�n�A�p�A�t�A�l�AЁA�v�A�z�A�x�A�x�A�z�A�z�A�t�A�~�A�|�A�x�A�z�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�z�A�|�A�~�A�|�A�|�A�z�A�z�A�z�A�x�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�r�A�t�A�v�A�t�A�v�A�t�A�v�A�t�A�t�A�r�A�t�A�t�A�t�A�t�A�t�A�v�A�r�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�r�A�r�A�r�A�t�A�t�A�v�A�t�A�r�A�r�A�t�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�v�A�t�A�v�A�t�A�v�A�t�A�t�A�v�A�t�A�v�A�v�A�t�A�t�A�v�A�x�A�v�A�v�A�v�A�t�A�x�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�t�A�t�A�r�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�v�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�x�A�v�A�t�A�t�A�t�A�t�A�v�A�v�A�z�A�x�A�z�A�|�A�z�A�|�A�|�A�|�A�z�A�|�A�|�A�z�A�z�A�|�A�|�A�|�A�~�A�z�A�~�A�|�A�|�AЁAЁAЁAЁAЃAЃAЁAЃAЅAЅAЃAЅAЉ7AЉ7AЉ7AЉ7AЏ\AЏ\AЏ\AЏ\AЍPAЍPAЏ\AЏ\AГuAЏ\AГuAЕ�AЕ�AГuAБhAГuAЗ�AЗ�AЛ�@���@��j@��9@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@��u@��D@��D@��@��@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�bN@�bN@�Z@�Q�@�I�@�9X@�(�@� �@� �@�(�@� �@� �@� �@� �@� �@� �@� �@� �@�(�@�(�@�(�@�(�@�(�@� �@� �@� �@�(�@�(�@�(�@�1'@�1'@�9X@�1'@��@�b@�1@�@�@��@�w@��@��@�w@�w@��@�w@�w@�w@��@�w@�w@�w@�w@�w@�w@�@�@��@��@��@��@��@�P@��@�P@�P@�P@�P@l�@l�@l�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�r�A�t�A�v�A�t�A�v�A�t�A�v�A�t�A�t�A�r�A�t�A�t�A�t�A�t�A�t�A�v�A�r�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�r�A�r�A�r�A�t�A�t�A�v�A�t�A�r�A�r�A�t�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�v�A�t�A�v�A�t�A�v�A�t�A�t�A�v�A�t�A�v�A�v�A�t�A�t�A�v�A�x�A�v�A�v�A�v�A�t�A�x�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�t�A�t�A�r�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�v�A�v�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�x�A�v�A�t�A�t�A�t�A�t�A�v�A�v�A�z�A�x�A�z�A�|�A�z�A�|�A�|�A�|�A�z�A�|�A�|�A�z�A�z�A�|�A�|�A�|�A�~�A�z�A�~�A�|�A�|�AЁAЁAЁAЁAЃAЃAЁAЃAЅAЅAЃAЅAЉ7AЉ7AЉ7AЉ7AЏ\AЏ\AЏ\AЏ\AЍPAЍPAЏ\AЏ\AГuAЏ\AГuAЕ�AЕ�AГuAБhAГuAЗ�AЗ�AЛ�@���@��j@��9@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@��u@��D@��D@��@��@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�bN@�bN@�Z@�Q�@�I�@�9X@�(�@� �@� �@�(�@� �@� �@� �@� �@� �@� �@� �@� �@�(�@�(�@�(�@�(�@�(�@� �@� �@� �@�(�@�(�@�(�@�1'@�1'@�9X@�1'@��@�b@�1@�@�@��@�w@��@��@�w@�w@��@�w@�w@�w@��@�w@�w@�w@�w@�w@�w@�@�@��@��@��@��@��@�P@��@�P@�P@�P@�P@l�@l�@l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�/?8��=�JM@.�>x=���=쪹>^j@�v�=ң@,O�=�e�=��?h�=Y@y=|�=��`=�b$=�C>��@�3�@w�=�e,=Y�F=e�=we�=���>=�=�~|=�W�>�Uq@���=��j=�i/=��r>�8=n�=	=)i=P	-=M��=iv=�4�=�r�=��=���>0݃@�(c=�(�=X��=R�<=e��=Z0=�5+=���=��=��^=ڪ;>PȊ?���=4C�=&��=={ =Mj=}Vm=}޾=���=�v>;�)@�,@�5�=�N�=ڕB>h@�@��=�iY=W
==�P�=�S�=�!W>;�@�,�?	|�=8{�=<�[=NP�=nN'=�A�=� >�@U�@��X=?��=�5=�&�=W��=,'�=(��=5��=4֡=B��=WI(=U<�=���=���=�,�=�t�>M?EqL=��x=��k@*�=��W>�?��>G��=��.=��=�W�?���>;{�='=\=#�'=K�:=\y=Y=b�=~�u=��=� �=�;y?�0=n��=r�=�,=�@�=�%�=�I>�y�>�f@��Y=��=���>%m]@�<`@�>-��@u=�k=��=��M>8��@�3�@J��> ��=Δ�=��>	��=��u=�l"?nv�?�@iȟ=��9=�q�>h?��=�OL=NP�=nm�=��=���>3 �?
Q�@�J=�7�=�$�=�{�>��@���@���=��>��T@�>@�=@���=�ŗ=���=��>w�@�<@-s�=��@>��O>M?ـ�?+��=�t�=`k{=� =�C�>)�H?ϫ>j��?��=�v`=��,=敖>�b@�GE>--�>ۆn=���=���=��T?6�=�H?Ԯ�@QR*>K�@@�N?��I?|*E>H>B@�D�>�"@� �?ͧ�@�J�>�x@�I(?��@���>�s�@�I�@�I@�J@�I�@�I(>���@�2�@�Jw@�J@�I�@�I(?��)@�C�@�I@�K@�J8@�I�@�Jw@�^�@�I�@�If@�8@�n�@�I(@�J@���@�If?NZ�@�/@�I�@�e@�H�?@�[@�If@�E�?��b>&^�@�If@�If@�I�@�I�@�I@�@�H�@(XO@�H@�I(@Wi�@�P]@�J�@�J@�I@�G�@�H�@8�P?�@�@� @�I�@�I�@�H�@�I�@�If@�C-@�J8@�HV@Sz@�}�@�Go@�F�@�HV@�G�@�H@�F�@�H@�J@�If?N��@�H@�=�@�I@�J8@�J8@iL@���@�K@�J@�I�@�H@�+�@�I�@v0@�K�@�J@�J8@�I�@�Jw@�K�@�J�@�Jw@�J�@�K@�J�@Xek@V�@�d�@T�r@�J�@�>�@�K�@�I�@�J8@�I�@Bt�@�KI@�I�@�FJ@�I�@�I�@�I�@�I�@�If@�Go@�J@�J@�I?&�+@�Jw@�I�?�1@�Go@�J8@�J@�I�@�If@�H�@�Go@�G�>"��@a@�If@�If@�H�@�D=@�I@�H@D�@�J8@�I@�HV@�H�@�H@�H@�HV@r��@�G�@��@�I(@�H@�G�>�*�@�GE@��!@�J�@�J@�H�@�If@�I(@�G�@�F�@�H�@��@�G@�H�@�H@�H�@�I(@�I@�I�@�G�@�If@�I�@�J�@�J@�Jw@+��@�J�@�J8@z5�@�J�@�Jw@�J�@�J@�H�@�I�@��@�Jw@�I�@�I�@�I�@�If@�I�@�B�@�H�>EF@�G�@�I�@�I@�I�@�_F@�HV?�%[@�I�@�J8@�J#@�Jw@�J@(�6@�Jw@�I�@�J8@�I�@d�j@�G@�If@�c5@�If@>Go@�I�@�If@�I�@�I�@�H�@�E�@��j@�I(?�@�I�@�I@�If@�I(@�I�?���@�I(@�I@�-b@�G�@�I(?�r�@�H@�҉?��
>���@�I�@�I(@�G�@�If@�H�@�H�>��@�H�@�If@�H�@�H@��@�N<@�I@�IR?��@�If@�H>Yjj@�HV@�F5@�H�@uO@�F5@�GE@�Go@6ӄ@�E�?�W�@�Go@��u>�}@�If@�HV@�KI@�J�@�Ks@�K�@�J�@�K�@�K�@�K�@�K�@�Ln@�KI@�K�@�K4@�K�@�N�@�M@�M�@�M@@�M�@�M@�M�@�MU@�MU@�M@�M@�M�@�MU@�MU@�L�@�M@�M�@�M@�M@�M�@�Nf@�MU@�M�@�M�@�N<@�O�@�N�@�N�@�N<@�O�@�OL@�OL@�O@�O@�O@�O�@�O�@�P@�O�@�P@�P	@�O�@�O�@�O�@�O�@�O�@�P]@�P	@�O�@�P�@�P3@�P]@�P]@�P�@�P3@�P3@�P]@�Pr@�P]@�P�@�P�@�Q�@�Q�@�Q@�P�@�P�@�P�@�P�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�Q�@�QD@�QD@�QD@�Q�@�QD@�RT@�RT@�RT@�RT@�R?@�Q�@�RT@�RT@�RT@�R?@�R�@�R�@�R�@�S@�R�@�R�@�R�@�Se@�S@�Se@�SP@�S�@�Se@�Se@�Se@�SP@�SP@�S�@�Sz@�S@�T"@�T@�T"@�T"@�Tv@�U�@�WT@�Z@�Z�@�Z�@�Z�@�[-@�[-@�Z�@�[-@�[-@�\>@�\>@�\�@�\�@�]O@�]O@�]O@�]�@�]�@�^@�^@�^_@�^_@�^�@�_�@�a�@�a�@�a�@�a�@�a�@�b9@�b�@�c^@�b�@�d@�d@�d�@�d�@�do@�e@�d�@�e@�d@�d@�do@�do@�e,@�e�@�f�@�g�@�g8@�g�@�h^@�h�@�h�@�iY@�i�@�j�@�k�@�l"@�k�@�m	@�m	@�m�@�nD@�oT@�p;@�qa@�q@�r�@�sm@�t*@�t�@�t�@�t�@�t�@�u:@�u�@�uO@�uO@�u�@�v@�v@�v`@�w\@�x@�x�@�zN@�zN@�z�@�{�@�}@�}@�}�@�}�@�~|@��@��@���@���@���@��D@��@��{@���@���@���@���@���@���@��z@���@��.@���@���@���@��@��@���@��x@��[@���@���@Px�@Pv�@Puy@Pt�@Pt~@Pt*@Ps�@Ps�@Ps�@Ps�@Ps�@Ps�@Ps�@Ps.@Ps.@Ps.@Pr�@Ps�@Pr�@Ps.@Pr�@Pr�@Pr2@Pq7@Pp�@Pp�@Pp@Po�@Po�@Poi@Po�@Poi@Po@Pn�@Po@Pn�@Pn�@Pnn@Pnn@Pnn@Pnn@Pnn@Pn@Pn@Pm�@Pm�@Pmr@Pmr@Pm@Pl"@Pk�@Pj+@Ph�@Pg�@Pff@Pd@Pcs@Pc @Pc @Pc @Pcs@Pc @Pb�@Pc�@Pd@Pdo@PdE@Pd@Pdo@Pdo@Pd�@Pd�@Pdo@Pe@Pd�@Pf@Pff@Pff@Pf�@Pg@Pgb@Pf�@PdE@Pb�@Pa|@P`@P_[@P^_@P]�@P]�@P]�@P]@P\�@P]@P]@P]@P]d@P]d@P]@P]@P\�@P\h@P]@P\h@P\>@P[l@P[@PZ�@PZq@PZq@PZq@PZ@PZG@PY�@PY�@PX�@PX�@PW @PWT@PV�@�Jw@�Jw@�K@�J@�K
@�J�@�K@�J�@�J�@�K�@�J�@�K^@�J�@�K�@�J�@�KI@�KI@�L@�K�@�K�@�K�@�K�@�L@�K�@�L@�K�@�K�@�K�@�K�@�L@�L@�K�@�L@�LY@�K
@�L@�L@�LD@�LY@�L�@�M@�L�@�L�@�LD@�M@�M@�M@@�M@�L�@�L�@�M@�M@�M�@�M�@�M�@�M�@�N<@�N@�M�@�M�@�M�@�M�@�M�@�MU@�M@�N@�N<@�N@�N{@�N<@�N@�M�@�NQ@�NQ@�N�@�N�@�NQ@�O@�N�@�N�@�N�@�N�@�O�@�OL@�O7@�O�@�Ov@�O@�O7@�OL@�O7@�N�@�N�@�Ov@�N�@�N�@�N�@�N�@�O@�Ov@�O7@�Ov@�R@�R�@�V�@�V�@�V�@�Vm@�Vm@�W@�V�@�W*@�W�@�X@�X%@�X�@�X�@�X�@�X�@�X�@�Y`@�Yu@�Y`@�Y�@�Y�@�Y�@�\)@�\�@�\�@�]@�]d@�]O@�^ @�^ @�^J@�^�@�^�@�_�@�_�@�_�@�`�@�`B@�`�@�`�@�`@�_�@�_�@�`�@�`�@�`k@�c @�b�@�b�@�bc@�b�@�c�@�d@�d�@�c�@�g@�h�@�hs@�g�@�f{@�gM@�hs@�h
@�i�@�l�@�k�@�lL@�o?@�n�@�o�@�p�@�pP@�o�@�p@�o�@�p&@�pP@�pe@�p;@�o�@�qa@�pe@�s@�q"@�q�@�t*@�s�@�tT@�w@�v�@�wp@�x�@�wG@�xl@�x-@�w�@�x�@�},@�x@�}�@�~�@��w@��@���@��s@���@���@���@��@��@��	@���@���@��"@���@��@��;@���@���@���@Pq@Pnn@Pk�@Pk'@Pj@Pj+@Pj@Pi�@Pi�@PiY@PiY@PiY@Pi/@Pi@Pi@Pi@Pi/@Pi/@Pi@Pi/@Ph�@Ph�@Ph4@Pg�@Pg@Pff@Pf@Pek@Pe@Pe@Pe@Pd�@Pd�@Pd�@PdE@PdE@PdE@Pd@PdE@Pd@Pd@PdE@Pd@Pd@Pd@Pc�@Pc�@Pcs@Pcs@Pcs@Pb�@Pa(@P_[@P^@P\�@PYK@PX%@PW�@PX�@PXO@PXO@PX%@PW�@PX�@PY!@PYK@PY!@PYu@PY�@PY�@PYu@PY�@PY�@PY�@PZG@P[l@P[�@P[l@P\�@P]d@P^@P^�@P[B@PYK@PW�@PU�@PU\@PS�@PR?@PR�@PR�@PR?@PQ�@PR?@PR�@PR?@PR?@PRi@PR?@PR�@PRi@PR@PR�@PR?@PQ�@PQ@PPr@PP�@PO�@POv@PPr@POL@PPH@PO�@PO�@PO"@POL@PM+@PN'@PL�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              344444443444444444443344444444434444444444444443444444444444444444444334443444444344444444334444444444444444444444444444444444444444444444443444344444443444444444344444444444344443344333444444444444444444444443444444443444443434343434333334333334333333333333333443334334433333434333333333444333333334333333333343333333333333333333333333333333333343333333333334334333333334333333343333333333333433333333333333333333333343333333333333333334333333433333433333333343333333343333343333343344333333433333333433433343334343343333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�3�@w�&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�(eG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�,@�5�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�@�,�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@U�@��YG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��VG�O�G�O�G�O�@�<bG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@iȞG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�JG�O�G�O�G�O�G�O�@���@���G�O�G�O�@�>@�= @���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�GBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@QR&G�O�G�O�G�O�G�O�G�O�@�D�G�O�@� �G�O�@�J�G�O�@�I(G�O�@���G�O�@�I�@�I@�J@�I�@�I&G�O�@�2�@�Jx@�J@�I�@�I)G�O�@�C�@�I@�K@�J9@�I�@�Jy@�^�@�I�@�Ig@�8@�n�@�I*@�J@���@�IjG�O�G�O�@�I�@�b@�H�G�O�@�If@�E�G�O�G�O�@�If@�Ij@�I�@�I�@�IG�O�@�H�G�O�@�H@�I'@Wi�@�PZ@�J�@�J@�I@�G�@�H~G�O�G�O�G�O�@�I�@�I�@�H�@�I�@�Ii@�C-@�J6@�HYG�O�@�}�@�Gn@�F�@�HY@�G�@�H @�F�@�H@�J@�IjG�O�@�H@�=�@�I@�J6@�J6@iS@���@�K @�J@�I�@�H@�+@�I�@v0�@�K�@�J@�J6@�I�@�Jv@�K�@�J�@�Jy@�J�@�K @�J�@Xep@V�@�d�@T�v@�J�@�>�@�K�@�I�@�J3@�I�G�O�@�KI@�I�@�FL@�I�@�I�@�I�@�I�@�Ie@�Go@�J@�J@�IG�O�@�Jx@�I�G�O�@�Gp@�J:@�J@�I�@�Ij@�H�@�Gp@�G�G�O�@a@�If@�Ig@�H�@�D>@�I@�HG�O�@�J:@�I@�HY@�H�@�H@�G�@�H\@r��@�G�@��@�I*@�H@�G�G�O�@�GF@��#@�J�@�J@�H�@�Ij@�I*@�G�@�F�@�H�@��@�G@�H�@�H@�H�@�I)@�I@�I�@�G�@�Ic@�I�@�J�@�J@�JxG�O�@�J�@�J:@z5�@�J�@�Jy@�J�@�J@�H�@�I�@��@�Jv@�I�@�I�@�I�@�Ig@�I�@�B�@�H�G�O�@�G�@�I�@�I@�I�@�_G@�HWG�O�@�I�@�J9@�J!@�Jr@�JG�O�@�Jx@�I�@�J:@�I�@d�k@�G@�Ij@�c2@�IeG�O�@�I�@�If@�I�@�I�@�H�@�E�@��k@�I'G�O�@�I�@�I@�Ie@�I*@�I�G�O�@�I*@�I@�-a@�G�@�I.G�O�@�H @�҈G�O�G�O�@�I�@�I*@�G�@�Ij@�H�@�H�G�O�@�H�@�Ie@�H�@�H@��@�N=@�I@�IQG�O�@�Ig@�HG�O�@�HY@�F5@�H�G�O�@�F4@�GJ@�GnG�O�@�E�G�O�@�Gn@��yG�O�@�Ic@�HX@�KH@�J�@�Ks@�K�@�J�@�K�@�K�@�K�@�K�@�Lm@�KG@�K�@�K6@�K�@�N�@�M@�M�@�M?@�M�@�M@�M�@�MX@�MU@�M�@�M~@�M�@�MU@�MR@�L�@�M@�Jx@�Jx@�K"@�J@�K@�J�@�K@�J�@�J�@�K�@�J�@�K^@�J�@�K�@�J�@�KH@�KK@�L@�K�@�K�@�K�@�K�@�L@�K�@�L@�K�@�K�@�K�@�K�@�L@�L@�K�@�L@�LZ@�K@�L@�L@�LC@�L\@�L�@�M@�L�@�L�@�LF@�M@�M@�M:@�M@�L�@�L�@�M@�M@�M�@�M�@�M�@�M�@�N>@�N@�M�@�M�@�M�@�M�@�M�@�MY@�M@�N@�N9@�N@�Nz@�N:@�N@�M�@�NR@�NR@�N�@�N�@�NU@�O@�N�@�N�@�N�@�N�@�O�@�OJ@�O:@�O�@�Ox@�O@�O:@�OO@�O9@�N�@�N�@�Ov@�N�@�N�@�N�@�N�@�O@�O{@�O6@�Oz@�R@�R�@�V�@�V�@�V�@�Vj@�Vm@�W@�V�@�W.@�W�@�X@�X)@�X�@�X�@�X�@�X�@�X�@�Ya@�Yw@�Y_@�Y�@�Y�@�Y�@�\*@�\�@�\�@�]@�]f@�]K@�^@�^@�^H@�^�@�^�@�_�@�_�@�_�@�`�@�`A@�`�@�`�@�`@�_�@�_�@�`�@�`�@�`j@�c"@�b�@�b�@�bd@�b�@�c�@�d@�d�@�c�@�g@�h�@�hr@�g�@�f~@�gO@�hs@�h
@�i�@�l�@�k�@�lO@�oB@�n�@�o�@�p�@�pN@�o�@�p@�o�@�p(@�pQ@�pd@�p;@�o�@�q`@�pd@�s@�q#@�q�@�t,@�s�@�tV@�w@�v�@�wn@�x�@�wF@�xo@�x/@�w�@�x�@�}.@�v@�}�@�~�@��w@��@���@��s@���@���@���@��@��@��	@���@���@�� @���@��@��>@���@���@���@Pq@Pnk@Pk�@Pk&@Pj�@Pj+@Pj@Pi�@Pi�@Pi[@Pi^@PiX@Pi.@Pi@Pi@Pi@Pi2@Pi.@Pi@Pi.@Ph�@Ph�@Ph5@Pg�@Pg@Pfk@Pf@Pek@Pe@Pe@Pe@Pd�@Pd�@Pd�@PdF@PdC@PdF@Pd@PdH@Pd@Pd@PdC@Pd@Pd@Pd@Pc�@Pc�@Pcn@Pcu@Pcu@Pb�@Pa&@P_^@P^@P\�@PYK@PX#@PW�@PX�@PXM@PXP@PX&@PW�@PX�@PY"@PYN@PY"@PYu@PY�@PY�@PYu@PY�@PY�@PY�@PZK@P[k@P[�@P[r@P\�@P]c@P^
@P^�@P[F@PYN@PW�@PU�@PU`@PS�@PRC@PR�@PR�@PR;@PQ�@PR;@PR�@PR@@PRC@PRj@PR>@PR�@PRh@PR@PR�@PR@@PQ�@PQ@PPs@PP�@PO�@POr@PPp@PON@PPK@PO�@PO�@PO(@POH@PM+@PN&@PL�@�Jx@�Jx@�K"@�J@�K@�J�@�K@�J�@�J�@�K�@�J�@�K^@�J�@�K�@�J�@�KH@�KK@�L@�K�@�K�@�K�@�K�@�L@�K�@�L@�K�@�K�@�K�@�K�@�L@�L@�K�@�L@�LZ@�K@�L@�L@�LC@�L\@�L�@�M@�L�@�L�@�LF@�M@�M@�M:@�M@�L�@�L�@�M@�M@�M�@�M�@�M�@�M�@�N>@�N@�M�@�M�@�M�@�M�@�M�@�MY@�M@�N@�N9@�N@�Nz@�N:@�N@�M�@�NR@�NR@�N�@�N�@�NU@�O@�N�@�N�@�N�@�N�@�O�@�OJ@�O:@�O�@�Ox@�O@�O:@�OO@�O9@�N�@�N�@�Ov@�N�@�N�@�N�@�N�@�O@�O{@�O6@�Oz@�R@�R�@�V�@�V�@�V�@�Vj@�Vm@�W@�V�@�W.@�W�@�X@�X)@�X�@�X�@�X�@�X�@�X�@�Ya@�Yw@�Y_@�Y�@�Y�@�Y�@�\*@�\�@�\�@�]@�]f@�]K@�^@�^@�^H@�^�@�^�@�_�@�_�@�_�@�`�@�`A@�`�@�`�@�`@�_�@�_�@�`�@�`�@�`j@�c"@�b�@�b�@�bd@�b�@�c�@�d@�d�@�c�@�g@�h�@�hr@�g�@�f~@�gO@�hs@�h
@�i�@�l�@�k�@�lO@�oB@�n�@�o�@�p�@�pN@�o�@�p@�o�@�p(@�pQ@�pd@�p;@�o�@�q`@�pd@�s@�q#@�q�@�t,@�s�@�tV@�w@�v�@�wn@�x�@�wF@�xo@�x/@�w�@�x�@�}.@�v@�}�@�~�@��w@��@���@��s@���@���@���@��@��@��	@���@���@�� @���@��@��>@���@���@���@Pq@Pnk@Pk�@Pk&@Pj�@Pj+@Pj@Pi�@Pi�@Pi[@Pi^@PiX@Pi.@Pi@Pi@Pi@Pi2@Pi.@Pi@Pi.@Ph�@Ph�@Ph5@Pg�@Pg@Pfk@Pf@Pek@Pe@Pe@Pe@Pd�@Pd�@Pd�@PdF@PdC@PdF@Pd@PdH@Pd@Pd@PdC@Pd@Pd@Pd@Pc�@Pc�@Pcn@Pcu@Pcu@Pb�@Pa&@P_^@P^@P\�@PYK@PX#@PW�@PX�@PXM@PXP@PX&@PW�@PX�@PY"@PYN@PY"@PYu@PY�@PY�@PYu@PY�@PY�@PY�@PZK@P[k@P[�@P[r@P\�@P]c@P^
@P^�@P[F@PYN@PW�@PU�@PU`@PS�@PRC@PR�@PR�@PR;@PQ�@PR;@PR�@PR@@PRC@PRj@PR>@PR�@PRh@PR@PR�@PR@@PQ�@PQ@PPs@PP�@PO�@POr@PPp@PON@PPK@PO�@PO�@PO(@POH@PM+@PN&@PL�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              344444443444444444443344444444434444444444444443444444444444444444444334443444444344444444334444444444444444444444444444444444444444444444443444344444443444444444344444444444344443344333444444444444444444444443444444443444443434343434333334333334333333333333333443334334433333434333333333444333333334333333333343333333333333333333333333333333333343333333333334334333333334333333343333333333333433333333333333333333333343333333333333333334333333433333433333333343333333343333343333343344333333433333333433433343334343343333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�U�9�U�9�VR9�Um9�V<9�U�9�VN9�U�9�V
9�V�9�V19�V�9�V9�V�9�V9�Vq9�Vt9�W 9�V�9�V�9�V�9�V�9�W9�V�9�W"9�V�9�V�9�V�9�V�9�W9�W9�V�9�W9�WS9�V@9�W9�W9�W@9�WU9�Ws9�W�9�W�9�W�9�WC9�W�9�W�9�X9�W�9�W�9�W�9�W�9�W�9�X�9�X{9�X�9�X�9�X�9�X�9�Xx9�XY9�Xx9�XX9�X|9�X&9�XE9�X�9�X�9�X�9�Y9�X�9�X�9�X�9�X�9�X�9�YY9�YK9�X�9�Y�9�YZ9�Y[9�Y'9�YY9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y|9�YZ9�Y�9�Y]9�YF9�Y&9�Y$9�Y�9�Y�9�Y�9�Y�9�\9�\�9�_�9�_�9�_�9�_�9�_�9�`.9�_�9�`C9�`�9�`�9�a9�aw9�a�9�a�9�a�9�a�9�b9�b%9�b9�bZ9�b79�b59�d_9�d�9�d�9�e9�ed9�eN9�e�9�e�9�f9�f�9�f�9�g49�g19�gH9�g�9�g�9�h9�h9�g�9�gh9�g69�h*9�hJ9�g�9�j9�i�9�i�9�i�9�i�9�j�9�j�9�kB9�j�9�m]9�n�9�n�9�m�9�l�9�m�9�n�9�n,9�o�9�q�9�ql9�q�9�t!9�s�9�t�9�uC9�t�9�t�9�t�9�t�9�t�9�u 9�u9�t�9�t�9�u�9�u9�wH9�u�9�v9�x/9�w�9�xQ9�z�9�zW9�z�9�|9�z�9�{�9�{~9�{I9�|9��9��9��I9��9���9���9���9��p9��9��(9���9���9��H9��99���9���9���9���9���9���9���9���9��9+�9+��9+��9+�?9+��9+�p9+�P9+�*9+�9+��9+��9+��9+��9+�~9+�}9+�}9+��9+��9+�~9+��9+�Z9+�:9+��9+�i9+��9+�X9+�9+�9+�@9+�@9+�B9+�9+�9+��9+�9+�9+�9+�o9+�9+�o9+�r9+�9+�o9+�o9+�o9+�09+�9+��9+��9+��9+�99+��9+�9+�p9+�69+�9+�9+�O9+�=9+�9+�9+�9+�n9+�?9+�a9+�9+�a9+�9+��9+��9+�9+��9+��9+�9+�V9+�D9+�i9+�J9+�z9+��9+�n9+��9+�&9+�9+�L9+�9+�H9+�9+�9+�9+�9+�9+�m9+�9+�9+�9+�9+��9+�9+�9+��9+�9+�9+�9+�m9+��9+�79+�}9+��9+�c9+�59+�E9+�9+�9+�9+�&9+�@9+߂9+�Q9+�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
)�B
.B
49B
:^B
I�B
S�B
aHB
��B
�fB
�B
�B
��B
��B
��B
��B
��B	7B�B&�B1'B;dBJ�BT�Bq�B|�B��B�B�-BÖB��B��B�/B�HB�B��B��BBuB"�B1'B6FBK�Bm�Bz�B�PBcTBr�B�%B� B|�B{�B{�B{�By�Bw�Bu�Bt�Bs�Br�Bs�Bv�Bw�BW
BR�BZBz�B��B�!B��B�DBo�B=qB��BǮB�wB��B��B�VBdZB`BBcTBcTBffBVB(�B�B�B'�B$�B�B
=BBhB
�)B
ŢB
�RB
�LB
�dB
�7B
R�B
)�B	�`B	��B	�!B	��B	��B	��B	�B	�hB	�B	{�B	q�B	n�B	q�B	s�B	k�B	XB	K�B	=qB	.B	&�B	�B		7B	B��B�B�B�mB�HB�)B��BǮBŢB��B�LB�B�B�B�9B�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B�{B�PB�=B�B~�B|�B{�B{�By�B}�B�B�B�B�%B�+B�B�B~�B}�B|�B{�B{�Bw�Bu�Bo�BiyBffBdZBdZBdZBdZBdZBffBjBq�Bs�B� B~�B{�Bx�Bw�Bv�Bs�Bk�BdZBdZBaHB`BB`BB`BB`BB`BB`BB`BB`BB_;B_;B^5B]/B]/B]/B\)B\)BZBW
BS�BP�BK�BI�BH�BJ�BI�BG�BI�BO�BZB_;BaHBbNBcTBhsBl�Bn�Bm�Bm�Bo�Bv�Bu�Bl�BjB[#BR�BR�BS�BW
BS�BXBaHBffBl�Bu�Bx�Bz�B� B�%B�DB�PB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�'B�-B�FB�^B�wBĜBȴB��B��B��BǮB��B�B�/B�/B�/B�)B�B�B�5B�5B�TB�sB�B�B�B�B�B�yB�ZB�NB�BB�5B�5B�B�#B�NB�HB�yB�B��B��B	B��B	  B	B	B	B	B	B��B��B��B	B	B	  B��B��B��B	B	+B		7B	
=B	
=B	DB	JB	JB	�B	�B	�B	�B	 �B	'�B	,B	-B	/B	0!B	;dB	@�B	C�B	D�B	F�B	L�B	S�B	`BB	bNB	dZB	ffB	jB	n�B	q�B	p�B	r�B	y�B	}�B	�B	�B	�B	�DB	�\B	�bB	�bB	�bB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�dB	�qB	�}B	��B	��B	��B	B	B	ÖB	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
B
B
"NB
)B
/�B
A�B
B[B
E�B
LB
T,B
ZB
^�B
c�B
h�B
l�B
qB
s�B
v�B
|G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�@�]>�A�T@� >�P?�C?�3�B
=u?��A�F�>� �?�@�[6>��]>���>�M? �%?��?�Q�B
`A�/M>�OH>�m�>��n>���>�m?8�*>�ӑ?�~?Ď�A���>���>��>�w?�V�>C��>CJE>Q@>�Y>�8;>�^[>�q>�҉>�T?�Z?`Q"B
p>�$h>��>��R>��M>�&�>�qz>�C_?��>�n�?
�O?�-�@�P�>_iD>N�">jݜ>~��>���>��;>�y�?�D?j��B
5B{I>�u:?�j?���B
$>�O�>��{>��>�fI?[�?l�EB
!@:�1>cZB>g�.>~�>�s�>�n�>�F,?$��A���B	��>mB>>�{�>Έ{>�i�>T��>O�J>_e�>_��>p��>�in>���>���>�>ѴW>���?>��@��@>�q&?A���>�j�?$��@A~(?��>�-�>���?�A��?s.j>O�>I�>}�u>�F�>�˛>�� >��>�H�>�{F>��*@4��>�a�>�J>�w�>�8�>��?��@��?&�"B�>ɉ7?�?R�B
mAJS�?c
�AYFX?�>ݕ??g�0B
!�A��1?$Ĭ?Z�>���?.�!?��?!@�[�@O��A���>�0^>�e�?)�JA�e>�y>}f'>�^�>���>�:!?b?F@7,�B"�>�+�>ӎ?��?I��A��BM�?~?���B
�B
�A��@>���>�c|>�/?Iv%Ak�dA���?x�@y�?0�Ad�@o�l>�>>�`�>ʸA??Y�]@B�^?�+�@�C�>��>���?�@�hB
#?[
N@L<>�@>���?!�H@|G�?uOA��A�}[?�Q+AZ�%@ۓ@�V�?}�B��?;B�B	�9A&�B
#�?���B
��@C!�B%M?��jB
_B
�B
RB
�B
@�?��QB�B
�B
RB
�B
�@�,KB
j�B
+B
IB
�B
IB
A�8�B
�B
Q B;oA�WCB
>B
(�A�jhB
�@�D�AW�B
gB	�1B
�@��UB
B
�A=�?OVTB
B
�B
B
,B
�Al��B
�A|.�B
B
�A�"�B��B
�B
JB
�B
%B
�A�b"A8;AD�MB
�B
�B
�B
�B
=B
��B
pB
�Ad��A��<B
�B
�B
�B
hB
�B
 7B
�B
�B
�@�XB
�B	u�B
�B
>B
pA���A�~�B
|B
�B
�B
7�A��B
 �A�&B
3B
�B
pB
,B
.gB
�B

B
B
bB
|B
NA�Y$A���A�A��VB
jB
�B
B
�B
�B
�A���B
�B
�B
�gB
B
'rB
,B
�B
EB
2�B
hB
|B
�@`zB
�B
�@C4B
tB
�B
�B
9B
�B
B
BB
! ?Ke�A�'B
B
wB
kB
F�B
3B
A���B
xB
+B
�B
�B
�B
H�B
A�^B
%
A���B
>B
B
_@7*B
&�B��B
�B
�B
WB
�B
>B
KB

B
AǶIB
-�B
�B
�B
JB
�B
#$B
�B
7tB

B
�B
VB
�B
�A�cRB
LB
4cA�݄B
�B
B
�B
B
�B
:B
fB
wB
�B
�B
�B
wB
�B
�B
�?{(AB
�B
,�B
3B
kA���B
)bA(n�B
�B
�B
��B
�B
RAQ%B
�B
JuB
xB
A��B
�B
R�B	K�B
EA� �B
B
B
�B
�B
kB
'/B��B
�@,��B
"�B
#�B
EB
�B
A�sB
B
LBM.B
!B
�AjB
�A��gARz@	�iB
B
B
B
�B
�B
3�?@�&B
RB
�B
WB
�B��B��B
�BDF@܅�B
wB
�?�ٌB
B
5XB
�AN�(B
:B
dB
 A� ]B
�@�YB
!�A?/B

B
EB
gB
 )B
�B
B
�B
B
B
(B
B
9B
TB
�B
�B
LB
�B
�B
�B
wB
	B
|B
�B
jB
�B
�B
�B
�B
BB
:B
�B
�B
�B
�B
8B
tB
�B
�B
�B
B
�B
qB
�B
/B
7B
�B
�B
�B
B
;B
�B
�B
eB
�B
�B
�B
�B
�B
KB
CB
3B
�B
B
�B
MB
�B
ZB
=B
�B
�B
lB
)B
?B
JB
jB
MB
�B
TB
_B
 B
B
aB
@B
B
`B
�B
cB
B
9B
}B
�B
�B
�B
HB
rB
�B
�B
IB
|B
9B
)B
IB
�B
LB
B
4B
B
TB
8B
OB
lB
xB
qB
&B
�B
B
EB
�B
�B
�B
XB
�B
�B
$B
�B
:B
�B
�B
�B
pB
hB
�B
�B
 hB
"�B
$OB
#�B
$?B
#�B
${B
$'B
$kB
$cB
$�B
$�B
$�B
% B
%\B
&B
%LB
$�B
&MB
%�B
%�B
%VB
&B
''B
(B
)�B
)B
(�B
)�B
'�B
)IB
)�B
+B
*�B
+�B
,FB
+�B
+�B
+�B
,WB
,B
,GB
*�B
+8B
,TB
+�B
,*B
,�B
,�B
-lB
-�B
.HB
.�B
/	B
.�B
/�B
/�B
/�B
1�B
1=B
1�B
1�B
1�B
38B
3B
3�B
4�B
4�B
5bB
4�B
5�B
7WB
66B
6zB
77B
7�B
7`B
7�B
6�B
6�B
7�B
71B
7)B
7mB
9B
8�B
9�B
:B
9KB
:AB
;CB
:�B
;�B
="B
<TB
<�B
<HB
=RB
>'B
AB
@�B
@�B
B0B
A�B
D�B
D2B
C�B
B�B
B�B
D�B
FAB
GiB
F[B
F&B
GB
JNB
I-B
J�B
J{B
KEB
L�B
N_B
Q[B	�B	�UB	�eB	��B	�B	�\B	�B	�B	��B	��B	�B	��B	�B	�<B	�/B	�B	��B	�8B	�B	��B	�WB	�NB	�B	�NB	�B	�B	�_B	�%B	�B	��B	��B	��B	�ZB	�B	�DB	��B	��B	��B	��B	�yB	�lB	�^B	�B	�B	��B	�B	�gB	�ZB	�B	�HB	�B	��B	��B	�	B	�B	�jB	�B	��B	��B	��B	��B	��B	�\B	��B	�B	�+B	�B	��B	�B	�B	��B	�B	��B	�AB	��B	��B	��B	��B	�B	�EB	�eB	�B	�B	�B	�!B	�B	��B	��B	�^B	�1B	�CB	��B	�bB	��B	��B	��B	��B	��B	�TB	�GB	��B	��B	�B	��B	�]B	��B	�|B	�1B	��B	��B	��B	��B	��B	�0B	�1B	�\B	�OB	��B	�AB	��B
�B
�B
�B
YB
5B
�B
0B
hB
�B
�B
�B
sB
�B
�B
�B
�B
�B
�B
)B
@B
�B
'B
�B
�B
�B
�B

B
�B
�B
#B
B
�B
B
4B
6B
�B
�B
�B
B
"B
�B
VB
B
�B
dB
SB
�B
�B
�B
B
6B
B
B
�B
�B
�B
CB
�B
~B
�B
nB
{B
�B
�B
B
�B
B
�B
�B
�B
RB
�B
�B
�B
B
�B
�B
 B
�B
�B
[B
�B
bB
�B
B
BB
�B
�B
�B
�B
�B
JB
VB
�B
B
�B
�B
xB
B
9B
3B
\B
�B
 
B
#�B
#�B
#�B
#BB
"tB
#B
#�B
#�B
#^B
#�B
#�B
$5B
$fB
$^B
$VB
$aB
$�B
$�B
#�B
$'B
#�B
$�B
'B
&�B
'|B
'�B
(B
'�B
(�B
(�B
(�B
)*B
(pB
)�B
)�B
*�B
*~B
*=B
+GB
*�B
*�B
)�B
)uB
)�B
)�B
*B
,B
,QB
,6B
+�B
, B
-B
- B
-�B
,�B
/�B
/�B
0-B
0?B
/B
/�B
0�B
/�B
1@B
24B
2QB
1�B
3�B
3�B
4$B
4�B
4�B
4�B
4=B
4"B
5B
5$B
4iB
4;B
3�B
4pB
5B
5�B
4�B
5;B
6	B
5�B
6B
8�B
7�B
8B
:0B
7�B
8"B
7�B
8fB
8�B
:�B
<�B
;|B
<aB
?B
@dB
?�B
?�B
?B
?#B
@
B
@DB
BgB
C�B
B�B
DB
E-B
D�B
D�B
EB
G�B
G�B
I�B	��B	�3B	�hB	��B	�<B	�B	��B	�B	�pB	�%B	�)B	�B	��B	��B	�B	�B	�B	�B	�tB	�B	�;B	�B	�B	�PB	��B	�PB	�B	�B	�5B	�(B	�B	��B	��B	�B	�OB	�BB	�5B	�	B	�B	��B	��B	��B	�B	��B	��B	�eB	�+B	��B	��B	��B	�OB	�<B	��B	��B	��B	�B	��B	�kB	�%B	��B	��B	�tB	�HB	��B	��B	�
B	��B	��B	�.B	� B	��B	��B	��B	�B	�1B	��B	��B	��B	��B	�B	�uB	��B	�B	�B	�B	�B	�/B	�/B	��B	�<B	�/B	��B	�~B	�B	��B	�B	�zB	�zB	�_B	�B	�dB	�B	�zB	�B	��B	�.B	�B	��B	�2B	��B	�tB	�B	�;B	�B	�B	�?B	�QB	��B	��B	�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344444443444444444443344444444434444444444444443444444444444444444444334443444444344444444334444444444444444444444444444444444444444444444443444344444443444444444344444444444344443344333444444444444444444444443444444443444443434343434333334333334333333333333333443334334433333434333333333444333333334333333333343333333333333333333333333333333333343333333333334334333333334333333343333333333333433333333333333333333333343333333333333333334333333433333433333333343333333343333343333343344333333433333333433433343334343343333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
)�B
.B
4=B
:aB
I�B
S�B
aNB
��B
�iB
�B
�B
��B
��B
��B
��B
��B	9B�B&�B1(B;gBJ�BUBq�B|�B��B�B�0BÙB��B�B�2B�JB�B��B��B"BzB"�B1*B6KBK�Bm�Bz�B�TBcZBr�B�)B�B|�B{�B{�B{�By�Bw�Bu�Bt�Bs�Br�Bs�Bv�Bw�BWBR�BZBz�B��B�#B��B�HBo�B=yB��BǳB�yB��B��B�[Bd]B`GBcYBcYBfkBVB(�B�B�B'�B$�B�B
@B$BmB
�,B
ţB
�WB
�QB
�hB
�<B
R�B
* B	�eB	��B	�$B	��B	��B	��B	�B	�lB	�B	{�B	q�B	n�B	q�B	s�B	k�B	XB	K�B	=tB	.B	&�B	�B		;B	"B��B�B�B�qB�MB�.B��BǳBŧB��B�PB� B�B�B�@B�8B�&B�B��B��B��B��B��B��B��B��B��B��B��B��B�VB�@B�B~�B|�B{�B{�By�B}�B�B�#B�B�+B�0B�B�B~�B}�B|�B{�B{�Bw�Bu�Bo�Bi}BfkBd]Bd]Bd`Bd^Bd`BfkBj�Bq�Bs�B�B~�B{�Bx�Bw�Bv�Bs�Bk�Bd_Bd]BaMB`FB`HB`IB`GB`CB`GB`EB`FB_AB_?B^:B]3B]4B]3B\-B\.BZ!BWBS�BP�BK�BI�BH�BJ�BI�BG�BI�BO�BZ"B_@BaLBbUBc[BhvBl�Bn�Bm�Bm�Bo�Bv�Bu�Bl�Bj�B[)BR�BR�BS�BWBS�BXBaLBflBl�Bu�Bx�Bz�B�B�(B�JB�VB�fB�tB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B�&B�/B�2B�KB�cB�zBĠBȻB��B��B��BǳB��B�B�4B�4B�7B�,B�#B�B�9B�;B�YB�zB�B�B�B�B�B�B�_B�QB�GB�<B�9B�"B�)B�UB�LB�B�B��B��B	
B��B	 B	B	B	 B	B	B��B��B��B	B	B	 B��B��B��B	B	1B		<B	
EB	
AB	HB	OB	NB	�B	�B	�B	�B	 �B	'�B	,B	-B	/B	0$B	;hB	@�B	C�B	D�B	F�B	L�B	S�B	`EB	bRB	d`B	fkB	j�B	n�B	q�B	p�B	r�B	y�B	}�B	�B	�B	�#B	�HB	�aB	�jB	�hB	�gB	�mB	�{B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�"B	�'B	�!B	�"B	�B	�B	�'B	�4B	�:B	�>B	�CB	�LB	�TB	�VB	�]B	�^B	�_B	�bB	�hB	�vB	��B	��B	��B	��B	B	B	ÛB	ÛB	ŨB	ǴB	ȻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�$B	�#B	�*B	�1B	�0B	�6B	�9B	�<B	�IB	�GB	�GB	�MB	�RB	�[B	�^B	�`B	�^B	�eB	�kB	�rB	�wB	�vB	�B	�B	�B	�B	�B	�B	�G�O�B	�B
B
B
�B
"TB
)B
/�B
A�B
B_B
E�B
LB
T/B
Z B
^�B
c�B
h�B
l�B
qB
s�B
v�B
|G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
cA�/UG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
tG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
7B{LG�O�G�O�G�O�B
$G�O�G�O�G�O�G�O�G�O�G�O�B
"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�B
rG�O�G�O�G�O�G�O�G�O�G�O�G�O�B
!�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B"�G�O�G�O�G�O�G�O�A���BM�G�O�G�O�B
�B
�A��BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�}]G�O�G�O�G�O�G�O�G�O�B��G�O�B	�>G�O�B
#�G�O�B
��G�O�B%QG�O�B
cB
�B
TB
�B
@�G�O�B�B
�B
TB
�B
�G�O�B
j�B
.B
JB
�B
MB
"A�8�B
�B
QB;rA�WEB
DB
(�A�joB
�G�O�G�O�B
lB	�3B
�G�O�B
�B
�G�O�G�O�B
�B
�B
B
-B
�G�O�B
�G�O�B
B
�A�"�B��B
�B
PB
�B
*B
�G�O�G�O�G�O�B
�B
�B
�B
�B
AB
��B
sB
�G�O�A��?B
B
�B
�B
jB
�B
 9B
�B
�B
�G�O�B
�B	u�B
�B
?B
sA���A�~�B
~B
�B
B
7�A��B
 �A�&B
5B
�B
sB
-B
.iB
�B
B
"B
dB
~B
QA�Y-A���A�A��[B
nB
�B
B
�B
�B
�G�O�B
�B
B
�mB
 B
'sB
-B
�B
GB
2�B
kB
�B
�G�O�B
�B
�G�O�B
wB
�B
�B
<B
�B
B
EB
!G�O�A�'B
�B
yB
pB
F�B
3B
G�O�B
{B
.B
�B
�B
�B
H�B
A�^B
%A���B
DB
B
eG�O�B
&�B��B
�B
�B
[B
�B
DB
NB
B
AǶJB
-�B
�B
�B
MB
�B
#)B
B
7yB
B
�B
ZB
�B
�G�O�B
NB
4fA�݊B
�B
"B
�B
B
�B
?B
gB
zB
�B
�B
B
yB
�B
�B
�G�O�B
�B
- B
3B
jA���B
)dG�O�B
�B
�B
��B
�B
TG�O�B
�B
J{B
{B
A��B
�B
R�B	K�B
GG�O�B
B
�B
�B
�B
pB
'1B��B
�G�O�B
"�B
#�B
GB
�B
G�O�B
B
LBM0B
!
B
�G�O�B
�A��lG�O�G�O�B
B
B
B
�B
�B
3�G�O�B
VB
�B
[B
�B��B��B
�BDGG�O�B
yB
�G�O�B
B
5ZB
�G�O�B
<B
jB
 G�O�B
�G�O�B
!�AG�O�B
B
HB
jB
 -B
�B
B
�B
B
B
,B
B
<B
WB
�B
�B
NB
�B
�B
�B
yB
B
�B
�B
nB
�B
�B
�B
�B
FB
;B
�B
�B
�B
�B
�B
ZB
6B
�B
1B
lB
�B
�B
�B
tB
�B
�B
�B
�B
�B
�B
)B
DB
 B
+B
�B
�B
�B
B
B
�B
�B
%B
B
�B
B
8B
:B
�B
�B
�B
B
#B
�B
YB
B
�B
jB
TB
�B
�B
�B
B
8B
B
B
�B
 B
�B
HB
�B
�B
�B
oB
~B
�B
�B
B
�B
B
�B
�B
�B
TB
�B
�B
�B
B
�B
�B
#B
�B
�B
_B
�B
dB
�B

B
FB
�B
�B
�B
�B
�B
MB
YB
�B
B
�B
�B
yB
!B
?B
6B
aB
�B
 B
#�B
#�B
#�B
#BB
"vB
#B
#�B
#�B
#cB
#�B
#�B
$9B
$hB
$dB
$[B
$fB
$�B
$�B
#�B
$+B
#�B
$�B
'	B
&�B
'�B
'�B
(B
'�B
(�B
(�B
(�B
),B
(sB
)�B
)�B
*�B
*�B
*@B
+HB
*�B
*�B
)�B
)yB
)�B
)�B
*B
,�B
,SB
,8B
+�B
,B
-B
-"B
-�B
,�B
/�B
/�B
01B
0CB
/B
/�B
0�B
/�B
1BB
26B
2VB
1�B
3�B
3�B
4(B
4�B
4�B
4�B
4AB
4$B
5
B
5&B
4kB
4?B
3�B
4sB
5B
5�B
4�B
5@B
6B
5�B
6"B
8�B
7�B
8B
:4B
7�B
8&B
7�B
8iB
8�B
:�B
<�B
;�B
<dB
?B
@hB
?�B
?�B
?B
?'B
@B
@FB
BiB
C�B
B�B
DB
E.B
D�B
D�B
EB
G�B
G�B
I�B	�B	�9B	�mB	��B	�?B	�B	��B	�B	�tB	�+B	�1B	�!B	��B	��B	�B	�B	�B	�B	�{B	�B	�>B	�B	�B	�VB	��B	�XB	�B	�B	�:B	�/B	�!B	��B	��B	��B	�UB	�EB	�:B	�B	�!B	��B	��B	��B	�B	��B	��B	�lB	�0B	�B	��B	��B	�UB	�AB	��B	��B	��B	�B	��B	�qB	�(B	��B	��B	�zB	�MB	��B	��B	�B	��B	�B	�3B	�&B	��B	��B	�B	�B	�8B	��B	�B	��B	��B	�B	�yB	��B	�B	�B	�	B	�B	�5B	�2B	�B	�AB	�5B	��B	�B	�B	�B	�B	�B	�B	�cB	�B	�hB	�B	�B	�B	��B	�5B	�B	��B	�:B	��B	�wB	�B	�AB	�B	�B	�GB	�UB	��B	��B	�tB
�B
�B
�B
ZB
6B
�B
1B
lB
�B
�B
�B
tB
�B
�B
�B
�B
�B
�B
)B
DB
 B
+B
�B
�B
�B
B
B
�B
�B
%B
B
�B
B
8B
:B
�B
�B
�B
B
#B
�B
YB
B
�B
jB
TB
�B
�B
�B
B
8B
B
B
�B
 B
�B
HB
�B
�B
�B
oB
~B
�B
�B
B
�B
B
�B
�B
�B
TB
�B
�B
�B
B
�B
�B
#B
�B
�B
_B
�B
dB
�B

B
FB
�B
�B
�B
�B
�B
MB
YB
�B
B
�B
�B
yB
!B
?B
6B
aB
�B
 B
#�B
#�B
#�B
#BB
"vB
#B
#�B
#�B
#cB
#�B
#�B
$9B
$hB
$dB
$[B
$fB
$�B
$�B
#�B
$+B
#�B
$�B
'	B
&�B
'�B
'�B
(B
'�B
(�B
(�B
(�B
),B
(sB
)�B
)�B
*�B
*�B
*@B
+HB
*�B
*�B
)�B
)yB
)�B
)�B
*B
,�B
,SB
,8B
+�B
,B
-B
-"B
-�B
,�B
/�B
/�B
01B
0CB
/B
/�B
0�B
/�B
1BB
26B
2VB
1�B
3�B
3�B
4(B
4�B
4�B
4�B
4AB
4$B
5
B
5&B
4kB
4?B
3�B
4sB
5B
5�B
4�B
5@B
6B
5�B
6"B
8�B
7�B
8B
:4B
7�B
8&B
7�B
8iB
8�B
:�B
<�B
;�B
<dB
?B
@hB
?�B
?�B
?B
?'B
@B
@FB
BiB
C�B
B�B
DB
E.B
D�B
D�B
EB
G�B
G�B
I�B	�B	�9B	�mB	��B	�?B	�B	��B	�B	�tB	�+B	�1B	�!B	��B	��B	�B	�B	�B	�B	�{B	�B	�>B	�B	�B	�VB	��B	�XB	�B	�B	�:B	�/B	�!B	��B	��B	��B	�UB	�EB	�:B	�B	�!B	��B	��B	��B	�B	��B	��B	�lB	�0B	�B	��B	��B	�UB	�AB	��B	��B	��B	�B	��B	�qB	�(B	��B	��B	�zB	�MB	��B	��B	�B	��B	�B	�3B	�&B	��B	��B	�B	�B	�8B	��B	�B	��B	��B	�B	�yB	��B	�B	�B	�	B	�B	�5B	�2B	�B	�AB	�5B	��B	�B	�B	�B	�B	�B	�B	�cB	�B	�hB	�B	�B	�B	��B	�5B	�B	��B	�:B	��B	�wB	�B	�AB	�B	�B	�GB	�UB	��B	��B	�tG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344444443444444444443344444444434444444444444443444444444444444444444334443444444344444444334444444444444444444444444444444444444444444444443444344444443444444444344444444444344443344333444444444444444444444443444444443444443434343434333334333334333333333333333443334334433333434333333333444333333334333333333343333333333333333333333333333333333343333333333334334333333334333333343333333333333433333333333333333333333343333333333333333334333333433333433333333343333333343333343333343344333333433333333433433343334343343333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455122020082814551220200828145512202008281455122020082814551220200828145512202008281455122020082814551220200828145512202008281455122020082814551220200828145512AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730402019021417304020190214173040    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730402019021417304020190214173040  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730402019021417304020190214173040  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455122020082814551220200828145512  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                