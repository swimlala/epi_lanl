CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  r   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:26Z creation      
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
resolution        =���   axis      Z        )X  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  nh   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )X  x�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )X  �p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X 	x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X 2�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X =(   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )X f�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )X �0   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X È   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     )X ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X �8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X *�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X T@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X ^�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173026  20200828145437  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               	   	   	AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @�|�%��@�|�%��@�|�%��111 @�|�8� @�|�8� @�|�8� @6�G�{@6�G�{@6�G�{�c��O�;d�c��O�;d�c��O�;d111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    	   	   	ADA BDA  DA BDA @@  @�  @�  A��A   A>ffA^ffA�  A�  A�  A�  A�  A�  AᙚA�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B���B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dy�D��D�(�D��)D���D��D�@RD��D�ʏD��D�P�D���D��RD��D�MqD�s�D���D���D�/\D�z�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    >���>���=���    =���    =���    =���=���                =���>L��        =���>L��>���            =���                >L��>���        >L��=���        =���>���            =���                >���        >���=���    >L��>L��>L��                    >L��=���        >���>���        =���        =���>L��        =���=���        =���=���                >L��>L��        >L��=���    =���                    =���>���?           =���>L��        =���        =���    >L��>���>L��        =���>L��=���        >L��>���>���        =���        >���>L��    =���            =���>L��    =���        =���>L��=���    =���=���        >L��>���>���    =���=���>���            >L��=���                >L��>���>L��        =���=���>L��        >L��>L��=���    =���=���>���>L��>���=���        >L��>���>L��=���        =���=���=���>���>���>L��>L��=���=���=���=���>L��>L��>L��=���=���>L��>���>L��=���>L��>L��>L��>���>L��>���>L��=���>L��>���>���>���>L��>L��>L��>���>L��>���>���>���>���?   >���>���>L��>���>���>L��>���>L��>���>L��>L��>���>L��>���>���>���>���>���>L��>���>���>���>L��>L��>���>���>���>���>L��>L��>���>L��>L��>L��>���>���>���>L��>L��>���>���>���>���>L��>L��>���>���>���>���>���?   >���>���>���>���>���?   >���>���>���>���>���?   ?   >���>���>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>���>���>���>L��>L��>���>���>���>L��>���>���>���>L��>���=���>���>���>L��>���>���>L��=���>���>���>���>���>���>���>���>���>���>L��>L��>L��>L��>���>L��>L��>���>���>���>L��>���>���>���>���?   >���>���>L��>���>���>���>���>���>���>L��>���>���>L��>���>���>���?   >���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>L��>���>L��=���>���>L��>L��>L��>���>���>���=���>L��>L��>L��>���>L��>L��>L��=���>���>���>���>L��>L��>���>���>L��>L��>���>���>���=���>L��>���>���>L��>���>L��>L��>���>���=���=���=���>���=���>L��=���>L��>���>L��>L��>���>L��>���>L��>L��>���>L��>L��>���>L��>���>���>���>���>L��>���?   >���>���>���>���?��?��>���>���?   ?��?��?333?��?333?L��?L��?L��?333?333?L��?�  ?fff?fff?fff?fff?fff?�  ?���?���?���?���?���?���?�ff?�33?�33?�  ?�  ?���?���?ٙ�?ٙ�?�ff?�33@   @   @ff@��@��@33@��@   @   @   @&ff@,��@333@9��@@  @@  @Fff@L��@Y��@`  @`  @l��@l��@s33@�  @�33@�33@���@���@�  @�33@���@���@�33@�ff@���@���@�  @�ff@���@���@�  @�33@ə�@���@�  @�ff@�ff@ٙ�@���@�33@�ff@���@���@�  @�33@���@���A   A   A33A��A��A  A	��A33A33AffA  A  A��A33A��AffA  A��A33A��AffA   A!��A#33A$��A$��A&ffA(  A)��A+33A+33A,��A.ffA0  A1��A333A333A4��A6ffA8  A8  A9��A;33A<��A>ffA>ffA@  AA��AC33AD��AFffAFffAI��AI��AK33AL��ANffAP  AQ��AS33AT��AVffAX  AX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��AfffAh  Ai��Ak33Al��AnffAp  As33At��AvffAx  Ay��A{33A~ffA~ffA���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  Aə�A�ffA�33A���A͙�A�ffA�  A���A�ffA�33A���Aՙ�A�33A�  A���A�ffA�33A���Aݙ�DpY�Dp` DpffDps3Dpy�Dp� Dp��Dp�3Dp� Dp�fDp��Dp��Dp� Dp�fDp�3DpٚDp�fDp��Dp�3Dp��DqfDq�Dq3Dq  Dq&fDq,�Dq9�Dq@ DqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Ds  Ds�Ds3Ds�Ds&fDs,�Ds33Ds@ DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt@ DtFfDtL�DtY�Dt` @@  @@  @Fff@L��@Y��@`  @`  @l��@l��@s33@�  @�33@�33@���@���@�  @�33@���@���@�33@�ff@���@���@�  @�ff@���@���@�  @�33@ə�@���@�  @�ff@�ff@ٙ�@���@�33@�ff@���@���@�  @�33@���@���A   A   A33A��A��A  A	��A33A33AffA  A  A��A33A��AffA  A��A33A��AffA   A!��A#33A$��A$��A&ffA(  A)��A+33A+33A,��A.ffA0  A1��A333A333A4��A6ffA8  A8  A9��A;33A<��A>ffA>ffA@  AA��AC33AD��AFffAFffAI��AI��AK33AL��ANffAP  AQ��AS33AT��AVffAX  AX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��AfffAh  Ai��Ak33Al��AnffAp  As33At��AvffAx  Ay��A{33A~ffA~ffA���A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  Aə�A�ffA�33A���A͙�A�ffA�  A���A�ffA�33A���Aՙ�A�33A�  A���A�ffA�33A���Aݙ�DpY�Dp` DpffDps3Dpy�Dp� Dp��Dp�3Dp� Dp�fDp��Dp��Dp� Dp�fDp�3DpٚDp�fDp��Dp�3Dp��DqfDq�Dq3Dq  Dq&fDq,�Dq9�Dq@ DqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Ds  Ds�Ds3Ds�Ds&fDs,�Ds33Ds@ DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt@ DtFfDtL�DtY�Dt` G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @.{@n{@�
=@�=qA�A9�AY�A{�A�A�A�A�A�A�\)A�A�B�HB�HB�HB�HB&�HB.�HB6�HB>�HBF�HBN�HBWG�B^�HBgG�Bn�HBv�HB~�HB�p�B�p�B�p�B�p�B�p�B�
>B�=qB�p�B�
>B�=qB�p�B�p�B�
>B�
>B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D nD �DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D	nD	�D
nD
�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D nD �D!nD!�D"nD"�D#nD#�D$nD$�D%nD%�D&nD&�D'nD'�D(nD(�D)nD)�D*nD*�D+nD+�D,nD,�D-nD-�D.nD.�D/nD/�D0nD0�D1nD1�D2nD2�D3nD3�D4nD4�D5nD5�D6nD6�D7nD7�D8nD8�D9nD9�D:nD:�D;nD;�D<nD<�D=nD=�D>nD>�D?nD?�D@nD@�DAnDA�DBnDB�DCnDC�DDnDD�DEnDE�DFnDF�DGnDG�DHnDH�DInDI�DJnDJ�DKnDK�DLnDL�DMnDM�DNnDN�DOnDO�DPnDP�DQnDQ�DRnDR�DSnDS�DTnDT�DUnDU�DVnDV�DWnDW�DXnDX�DYnDY�DZnDZ�D[nD[�D\nD\�D]nD]�D^nD^�D_nD_�D`nD`�DanDa�DbnDb�DcnDc�DdnDd�DenDe�DfnDf�DgnDg�DhnDh�DinDi�DjnDj�DknDk�DlnDl�DmnDm�DnnDn�DonDo�DpnDp�Dqg�Dq�DrnDr�DsnDs�Dy|(D��D��D��3D�� D�
�D�7\D��)D���D��D�G�D���Dǿ\D��)D�D{D�j�D���D��D�&fD�q�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���\)<��<���8Q뾏\)�8Q뾏\)�8Q뾏\)�8Q�8Q뾏\)��\)��\)��\)�8Q뽣�
��\)��\)�8Q뽣�
<����\)��\)��\)�8Q뾏\)��\)��\)��\)���
<����\)��\)���
�8Q뾏\)��\)�8Q�<����\)��\)��\)�8Q뾏\)��\)��\)��\)<����\)��\)<���8Q뾏\)���
���
���
��\)��\)��\)��\)��\)���
�8Q뾏\)��\)<��<����\)��\)�8Q뾏\)��\)�8Q뽣�
��\)��\)�8Q�8Q뾏\)��\)�8Q�8Q뾏\)��\)��\)��\)���
���
��\)��\)���
�8Q뾏\)�8Q뾏\)��\)��\)��\)��\)�8Q�=�>aG���\)��\)�8Q뽣�
��\)��\)�8Q뾏\)��\)�8Q뾏\)���
<�����
��\)��\)�8Q뽣�
�8Q뾏\)��\)���
=�<����\)��\)�8Q뾏\)��\)<�����
��\)�8Q뾏\)��\)��\)�8Q뽣�
��\)�8Q뾏\)��\)�8Q뽣�
�8Q뾏\)�8Q�8Q뾏\)��\)���
=�<����\)�8Q�8Q�<����\)��\)��\)���
�8Q뾏\)��\)��\)��\)���
=����
��\)��\)�8Q�8Q뽣�
��\)��\)���
���
�8Q뾏\)�8Q�8Q�<�����
<���8Q뾏\)��\)���
<�����
�8Q뾏\)��\)�8Q�8Q�8Q�<��<�����
���
�8Q�8Q�8Q�8Q뽣�
���
���
�8Q�8Q뽣�
=����
�8Q뽣�
���
���
<�����
<�����
�8Q뽣�
<��<��<�����
���
���
<�����
<��<��<��=�>aG�=�=����
=�=����
<�����
<�����
���
<�����
<��<��<��<��<�����
<��<��<�����
���
=�=�=�=����
���
<�����
���
���
<��<��<�����
���
<��<��<��=����
���
<��=�=�<��<��>aG�=�=�=�=�=�>aG�=�=�=�=�=�>aG�>aG�=�=�=�=�=�<�����
<��<��<��<�����
<��<��=�<��<��=�<�����
���
<��<��<�����
<��=�<�����
<���8Q�<��<�����
<��=����
�8Q�<��<��=�=�<��<��=�<��<�����
���
���
���
<�����
���
<��<��<�����
<��=�<��=�>aG�<��<�����
=�=�<��<��=�=����
<��<�����
<��<��=�>aG�<��<��=�=�<��=�<��<��=�=�<��<��<��=�=�=�=�<��<��<��=�=�<�����
<�����
<�����
�8Q�<�����
���
���
<��<��<���8Q뽣�
���
���
=����
���
���
�8Q�<��=�<�����
���
=�=����
���
<��=�<���8Q뽣�
=�<�����
<�����
���
=�<���8Q�8Q�8Q�<���8Q뽣�
�8Q뽣�
=����
���
<�����
<�����
���
<�����
���
<�����
<��<��=�<�����
<��>aG�=�<��<��=�>��>��=�=�>aG�>��>��>�
=>��>�
=?�?�?�>�
=>�
=?�?8Q�?�R?�R?�R?�R?�R?8Q�?Q�?Q�?Q�?Q�?k� ?k� ?��\?�\)?�\)?�(�?�(�?���?���?�?�?\?�\)?�(�?�(�?���?�?�@G�@�@{@{@{@z�@�H@!G�@'�@.{@.{@4z�@:�H@G�@N{@N{@Z�H@Z�H@aG�@n{@tz�@tz�@���@��
@�
=@�=p@���@��
@�=p@�p�@���@��
@�
=@�p�@���@��
@�
=@�=p@���@��
@�
=@�p�@�p�@У�@��
@�=p@�p�@��
@��
@�
=@�=p@��@��
@�
=@�
=@�p�A Q�A Q�A�A�A�RA�RA	�A�A�A�A�RAQ�A�A�A�A�RAQ�A�A�A�A�RA Q�A Q�A!�A#�A%�A&�RA&�RA(Q�A)�A+�A-�A.�RA.�RA0Q�A1�A3�A3�A5�A6�RA8Q�A9�A9�A;�A=�A>�RA@Q�AA�AA�AE�AE�AF�RAHQ�AI�AK�AM�AN�RAPQ�AQ�AS�AS�AU�AV�RAXQ�AY�A[�A]�A^�RA`Q�Aa�Ac�Ae�Af�RAhQ�Ai�Ak�An�RApQ�Aq�As�Au�Av�RAy�Ay�A}�A~�SA�(�A���A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A�\)A�(�A���A�A��\A�\)A�(�A���A�A��\A�(�A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A�A�\)A�(�A���A�A��\A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A���A�A��\A�\)A���A�A��\A�(�A���A�A\A�(�A���A�A�\)A�(�A���Aʏ\A�\)A�(�A�AΏ\A�(�A���Aҏ\A�\)A���A�A֏\A�(�A���Aڏ\A�\)DpG�DpNDpTzDpaGDpg�DpnDpz�Dp�GDp�Dp�zDp��Dp��Dp�Dp�zDp�GDpǮDp�zDp��Dp�GDp�Dp�zDp��DqGDqDqzDq�Dq'�Dq.Dq:�DqAGDqG�DqNDqZ�DqaGDqg�DqtzDqz�Dq�GDq�Dq�zDq��Dq��Dq�Dq�zDq�GDqǮDq�Dq��Dq�GDq�Dq�zDq��Dr�DrDrzDr!GDr'�Dr.Dr:�DrAGDrG�DrTzDrZ�DraGDrnDrtzDrz�Dr��Dr�Dr��Dr�GDr��Dr�zDr��Dr�GDr�Dr�zDr��Dr�Dr�Dr��DsGDs�DszDs�Ds!GDs.Ds4zDs:�DsG�DsNDsZ�DsaGDsg�DstzDsz�Ds�GDs�Ds�zDs�GDs��Ds�Ds��Ds�GDsǮDs�zDs��Ds�GDs�Ds�zDtGDt�DtDt�Dt!GDt.Dt4zDt:�DtG�DtN@.{@.{@4z�@:�H@G�@N{@N{@Z�H@Z�H@aG�@n{@tz�@tz�@���@��
@�
=@�=p@���@��
@�=p@�p�@���@��
@�
=@�p�@���@��
@�
=@�=p@���@��
@�
=@�p�@�p�@У�@��
@�=p@�p�@��
@��
@�
=@�=p@��@��
@�
=@�
=@�p�A Q�A Q�A�A�A�RA�RA	�A�A�A�A�RAQ�A�A�A�A�RAQ�A�A�A�A�RA Q�A Q�A!�A#�A%�A&�RA&�RA(Q�A)�A+�A-�A.�RA.�RA0Q�A1�A3�A3�A5�A6�RA8Q�A9�A9�A;�A=�A>�RA@Q�AA�AA�AE�AE�AF�RAHQ�AI�AK�AM�AN�RAPQ�AQ�AS�AS�AU�AV�RAXQ�AY�A[�A]�A^�RA`Q�Aa�Ac�Ae�Af�RAhQ�Ai�Ak�An�RApQ�Aq�As�Au�Av�RAy�Ay�A}�A~�SA�(�A���A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A�\)A�(�A���A�A��\A�\)A�(�A���A�A��\A�(�A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A�A�\)A�(�A���A�A��\A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A���A�A��\A�\)A���A�A��\A�(�A���A�A\A�(�A���A�A�\)A�(�A���Aʏ\A�\)A�(�A�AΏ\A�(�A���Aҏ\A�\)A���A�A֏\A�(�A���Aڏ\A�\)DpG�DpNDpTzDpaGDpg�DpnDpz�Dp�GDp�Dp�zDp��Dp��Dp�Dp�zDp�GDpǮDp�zDp��Dp�GDp�Dp�zDp��DqGDqDqzDq�Dq'�Dq.Dq:�DqAGDqG�DqNDqZ�DqaGDqg�DqtzDqz�Dq�GDq�Dq�zDq��Dq��Dq�Dq�zDq�GDqǮDq�Dq��Dq�GDq�Dq�zDq��Dr�DrDrzDr!GDr'�Dr.Dr:�DrAGDrG�DrTzDrZ�DraGDrnDrtzDrz�Dr��Dr�Dr��Dr�GDr��Dr�zDr��Dr�GDr�Dr�zDr��Dr�Dr�Dr��DsGDs�DszDs�Ds!GDs.Ds4zDs:�DsG�DsNDsZ�DsaGDsg�DstzDsz�Ds�GDs�Ds�zDs�GDs��Ds�Ds��Ds�GDsǮDs�zDs��Ds�GDs�Ds�zDtGDt�DtDt�Dt!GDt.Dt4zDt:�DtG�DtNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�/A�+A�7LA�=qA�;dA�?}A�?}A�A�A�A�A�?}A�C�A�E�A�G�A�G�A�I�A�I�A�G�A�G�A�I�A�K�A�K�A�I�A�G�A�G�A�G�A�I�A�K�A�M�A�M�A�E�A��A�S�Aϡ�A�VA͗�A�A�E�A��;AĸRAé�A�ffA�`BA�(�A��A�bNA���A�^5A�7LA��A��mA�1A��wA�1'A���A���A���A�bNA��^A�&�A���A��A�z�A�K�A��7A�;dA�ȴA�=qA��A�/A��A��FA���A� �A�JA��#A�ƨA�|�A�Q�A�^5A���A���A��DA�M�A�"�A���A�r�A�\)A�7LA�JA��HA���A��9A�bA��FA�$�A�ĜA��A��DA�A�A�|�A���A��mA��
A�VA�\)A}�#A|�AxI�As��AnI�Ag33Aax�A^bA]S�A[�;AZ5?AY�FAY`BAY+AY%AXz�AVv�ATz�ASC�AR��AQ�;APv�AN �ALr�AK��AKC�AJQ�AI�AG33AD9XA@�A?K�A=S�A<v�A<�!A;�A;/A:�9A9G�A8�A7"�A6E�A5��A4��A2�DA1?}A0n�A/�A/��A.�`A.Q�A,��A*�yA)��A)C�A)7LA)�A)VA(��A(�HA(A�A'|�A&�A%x�A$�!A#�-A#oA"��A"~�A!A!l�A �!Av�A33An�A��A�PAffA�A+A�A�FA/A��AM�A�
Ax�AffA�
AVA�RAVA{At�A
��A
��A
��A
r�A	�A�!A+A�A�uA��A�-A�HA9XA �A�A ȴ@��9@�@��@�@���@�-@���@��@�x�@�hs@��@���@�9@�9X@�(�@�  @�@�V@���@��y@���@ܬ@�
=@�J@��T@�r�@Ԭ@���@�|�@�;d@��@щ7@��@Ь@��@�S�@���@Η�@Ώ\@�v�@��T@�&�@�|�@�ff@��/@��m@���@��@��;@�ƨ@�|�@�;d@¸R@�{@��-@��h@�x�@�X@��@� �@�"�@�$�@���@�9X@�  @�p�@�^5@��
@���@�v�@�ff@��@�J@��@�-@��R@�@���@���@�b@�ƨ@��@��@�E�@��j@���@��y@���@���@�~�@�=q@��T@��H@�bN@��7@�hs@��@�(�@�@�M�@�v�@�-@���@��-@��@�r�@���@���@�ff@��h@��@��@��m@�1'@�b@�"�@�ff@�M�@���@��@�z�@��D@���@�r�@��@�I�@��@��+@�~�@�^5@���@���@�hs@���@�Z@���@���@���@�t�@��@��H@��R@���@�@�C�@��P@���@���@�E�@�@���@�p�@�?}@��@��@��@���@��`@��/@���@���@�V@�%@���@��/@��@�1'@��@��@�1@�  @�1@��@��m@��@���@��F@�
=@��+@��R@���@��!@��+@��T@���@�p�@��@�%@��j@�I�@�b@���@���@��@�l�@�t�@�K�@�@��!@�~�@�M�@�-@�$�@��@�{@��@���@���@�@�@�hs@��`@���@���@���@�Ĝ@�I�@�  @�ƨ@�|�@�33@�@��y@�ȴ@��+@�n�@�M�@�J@��-@�hs@�X@�/@��`@��@�r�@���@�l�@�\)@�K�@�33@��@���@���@���@��\@�n�@�^5@�E�@�E�@�=q@��@���@�X@�X@�7L@���@���@���@��@��@��`@��@��;@���@�\)@�C�@�C�@�+@�
=@��H@���@���@��@x*�@p�	@k9�@a \@U��@O��@G��@@Ɇ@:��@3��@.:*@)-w@$�@ N�@�+@$�@�@m]@	�@�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�(�A�33A�S�A��A�K�A�?}A�(�A���A���A��A���A��A�C�A��`A�Q�A�A���A�jA�^5A��/AҶFA���A��HA�/A�9XA�VAAĺ^A�\)A�33A�1'A�z�A�&�A�JA�ȴA�\)A�=qA�^5A���A�A�A�"�A¶FA��/A�ȴA�v�AuA�l�A�p�A���AŶFA�%A�"�A��A�=qA�1'A�7LA�1'A���A��yA�C�A�dZA���A��A���A��A���A�5?Aқ�AǁA�{A���AŴ9Aʺ^A�-A�XAîA�G�A�  A���Aĉ7A�I�A�v�A�1'A��yA���A�x�A�^5A��A�  A���A�r�A�
=A�ZA��A�;dA�5?A�A�bAð!A��AҋDA�+A�S�A���A��A��`A�M�A�jAŅA���A�bNAAŏ\A�/A�1'A�-A���A�I�A�K�A��A�A� �A�;dA��A�7LA��A�
=Aŗ�A�1'A�{A��wA�`BA�(�A�-AƍPA�^5Aú^A�\)A�K�AЮA�ZAΣ�APA���A���A�ffA��A�{A�v�AÍPA���A�\)A�"�A�A�A�/A�bAț�A҅A��A��;A��9A�JA�K�A�A�
=A� �A�n�A���AœuA�/A��
A���A�5?A��AʋDA��A�(�A�A�"�A�/A���A��A��Aŉ7Aҝ�A���A�"�A�v�A�33AîA�{A�-A�1'AҋDAÓuA� �A� �A�ƨA�A�C�A�+A�$�Aʕ�AҾwAȾwA��A�bNAҶFA�A�(�Aҩ�AļjA�/A�/Aқ�A�p�AȑhA�{A��HA�7LA�&�A��A�/A���A�
=A�33A�(�A�7LAҟ�A��A�1'A�5?A�JA�;dA�1'A�33A�7LA�-A�+A�5?A� �A�ƨA�33A�
=A�9XA�;dA�7LA�7LA�p�A�E�A�  A�E�A�9XA�-A�33A�A�A�(�A���A�7LA�+A�+A�x�A�-A�(�A�-A�+A��A̾wA�(�A�+A� �A�+A�\)A�+A�(�A��A� �A�-A�7LA�-A�-A�+A�r�A�-A�/A�1'A�/A�-A�&�A�33A�1'A�1'A�-A�+A�-A�/A�1'A�33A�9XA�-A�(�A�$�A�"�A�(�A�/A�"�A�"�A��A��A�  A��A��A��HA��A̟�A�(�A��A��A��A�&�A� �A��A���A�&�A�$�A� �A�K�A�&�A� �A��A�bNA��A���A�VA��mA�JA�{A��A�{A̙�A��A��A�G�A�"�A�&�A�&�A�&�A�"�A�"�A�^5A� �A���AθRA�"�A��A�{A�%A��A� �AΑhA��`A��A� �A�"�A� �A� �A��A�~�A��A�(�A�&�A� �A�$�A� �A���A�+A�$�A��A�$�A� �A�"�A��A��A��A��A��A� �A�&�A��A� �A��A�"�A��A�"�A�ffA��A�"�A��A��A��A��A��Aҩ�A� �A�$�A��A�oA��A��A�{A�ffA�
=A��A�bNA��A�5?A�$�A�$�A��TA�
=A�&�A�jA�+A�&�A�(�A��A�&�A�z�A�(�A� �A��A�l�A�$�A�&�A� �A�ZA�+A�"�A�"�A�|�A��A�&�A�&�AΉ7A�(�A���A� �A�$�A�-A�1A�AѺ^A��AґhA��A�p�A��A�&�A��A�
=A�"�A��A��HA�JAҋDA�$�A�&�AѬA�&�A��A�&�A��A�+A�(�A�-A���A�-A�(�A�&�A�&�A�"�A�1'A�&�A�&�A��A��A�+A�(�A�+A�(�A�+A�+A�(�A�&�A�$�A� �A�&�A�$�A�+A�(�A�-A�/A�"�A�33A�1'A�1'A�5?A�-A�33A�1'A�+A�/A�33A�5?A�33A�33A�;dA�5?A�5?A�33A�9XA�=qA�=qA�;dA�;dA�;dA�;dA�9XA�;dA�=qA�=qA�7LA�9XA�;dA�9XA�;dA�9XA�;dA�7LA�5?A�5?A�/A�33A�/A�/A�/A�7LA�5?A�5?A�33A�33A�33A�33A�33A�/A�/A�-A�-A�+A�-A�-A�5?A�33A�5?A�1'A�33A�/A�33A�1'A�?}A�G�A�C�A�7LA�C�A�G�A�G�A�G�A�A�A�;dA�A�A�=qA�9XA�7LA�?}A�=qA�A�A�A�A�M�A�M�A�K�A�I�A�M�A�I�A�I�A�I�A�G�A�E�A�E�A�=qA�?}A�?}A�A�A�;dA�C�A�C�A�C�A�E�A�G�A�E�A�E�A�G�A�C�A�E�A�G�A�G�A�G�A�G�A�G�A�G�A�C�A�E�A�E�A�A�A�E�A�A�A�?}A�A�A�=qA�;dA�;dA�;dA�7LA�9XA�7LA�;dA�=qA�C�A�C�A�C�A�C�A�C�A�C�A�A�A�E�A�C�A�A�A�A�A�C�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�A�A�C�A�C�A�C�A�A�A�C�A�A�A�A�A�A�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�C�A�A�A�C�A�A�A�E�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�A�A�E�A�C�A�A�A�C�A�A�A�C�A�C�A�A�A�A�A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�A�A�C�A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�C�A�A�A�C�A�C�A�E�A�E�A�C�A�E�A�E�A�E�A�C�A�E�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�C�A�G�A�E�A�C�A�E�A�E�A�E�A�C�A�G�A�E�A�G�A�G�A�G�@�t�@�l�@�\)@�K�@�K�@�K�@�S�@�S�@�S�@�S�@�K�@�K�@�K�@�K�@�C�@�K�@�C�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�C�@�;d@�;d@�;d@�C�@�;d@�33@�+@�"�@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�"�@�o@��@�o@�o@�o@�
=@�@�@�
=@�@�@���@���@��@�
=@��@��H@��@��@��@��@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��R@��!@��!@���@���@���@���@���@���@���@���@�~�@�~�@�v�@�^5@�V@�M�A�1'A�33A�33A�1'A�/A�1'A�/A�-A�+A�/A�1'A�1'A�/A�+A�-A�-A�-A�-A�(�A�(�A�(�A�&�A�&�A�(�A�+A�+A�-A�(�A�"�A�&�A�$�A�&�A�1'A�1'A�=qA�+A�-A�=qA�?}A�=qA�/A�33A�1'A�1'A�1'A�33A�-A�/A�;dA�33A�A�A�A�A�?}A�=qA�=qA�?}A�?}A�=qA�?}A�9XA�;dA�9XA�7LA�;dA�7LA�9XA�=qA�?}A�?}A�=qA�?}A�?}A�=qA�?}A�?}A�=qA�=qA�?}A�?}A�?}A�?}A�?}A�=qA�?}A�?}A�=qA�;dA�=qA�9XA�;dA�7LA�5?A�5?A�7LA�5?A�5?A�33A�33A�7LA�5?A�?}A�?}A�?}A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�=qA�A�A�?}A�?}A�?}A�?}A�A�A�A�A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�=qA�=qA�?}A�=qA�?}A�=qA�A�A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�C�A�=qA�A�A�A�A�A�A�=qA�=qA�?}A�A�A�?}A�?}A�?}A�?}A�A�A�C�A�A�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�A�A�C�A�C�A�E�A�C�A�E�A�C�A�C�A�C�A�C�A�E�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�E�@�l�@�dZ@�\)@�K�@�K�@�K�@�S�@�S�@�S�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�C�@�C�@�;d@�;d@�+@�33@�+@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@��@�"�@�o@��@�o@�o@�o@�
=@�@�@�@���@���@���@���@���@���@��y@��H@��H@��@��@��@��@���@���@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��R@��R@��!@��!@���@���@���@���@���@���@���@��\@�~�@�v�@�n�@�V@�M�@�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�33A�/A�+A�7LA�=qA�;dA�?}A�?}A�A�A�A�A�?}A�C�A�E�A�G�A�G�A�I�A�I�A�G�A�G�A�I�A�K�A�K�A�I�A�G�A�G�A�G�A�I�A�K�A�M�A�M�A�E�A��A�S�Aϡ�A�VA͗�A�A�E�A��;AĸRAé�A�ffA�`BA�(�A��A�bNA���A�^5A�7LA��A��mA�1A��wA�1'A���A���A���A�bNA��^A�&�A���A��A�z�A�K�A��7A�;dA�ȴA�=qA��A�/A��A��FA���A� �A�JA��#A�ƨA�|�A�Q�A�^5A���A���A��DA�M�A�"�A���A�r�A�\)A�7LA�JA��HA���A��9A�bA��FA�$�A�ĜA��A��DA�A�A�|�A���A��mA��
A�VA�\)A}�#A|�AxI�As��AnI�Ag33Aax�A^bA]S�A[�;AZ5?AY�FAY`BAY+AY%AXz�AVv�ATz�ASC�AR��AQ�;APv�AN �ALr�AK��AKC�AJQ�AI�AG33AD9XA@�A?K�A=S�A<v�A<�!A;�A;/A:�9A9G�A8�A7"�A6E�A5��A4��A2�DA1?}A0n�A/�A/��A.�`A.Q�A,��A*�yA)��A)C�A)7LA)�A)VA(��A(�HA(A�A'|�A&�A%x�A$�!A#�-A#oA"��A"~�A!A!l�A �!Av�A33An�A��A�PAffA�A+A�A�FA/A��AM�A�
Ax�AffA�
AVA�RAVA{At�A
��A
��A
��A
r�A	�A�!A+A�A�uA��A�-A�HA9XA �A�A ȴ@��9@�@��@�@���@�-@���@��@�x�@�hs@��@���@�9@�9X@�(�@�  @�@�V@���@��y@���@ܬ@�
=@�J@��T@�r�@Ԭ@���@�|�@�;d@��@щ7@��@Ь@��@�S�@���@Η�@Ώ\@�v�@��T@�&�@�|�@�ff@��/@��m@���@��@��;@�ƨ@�|�@�;d@¸R@�{@��-@��h@�x�@�X@��@� �@�"�@�$�@���@�9X@�  @�p�@�^5@��
@���@�v�@�ff@��@�J@��@�-@��R@�@���@���@�b@�ƨ@��@��@�E�@��j@���@��y@���@���@�~�@�=q@��T@��H@�bN@��7@�hs@��@�(�@�@�M�@�v�@�-@���@��-@��@�r�@���@���@�ff@��h@��@��@��m@�1'@�b@�"�@�ff@�M�@���@��@�z�@��D@���@�r�@��@�I�@��@��+@�~�@�^5@���@���@�hs@���@�Z@���@���@���@�t�@��@��H@��R@���@�@�C�@��P@���@���@�E�@�@���@�p�@�?}@��@��@��@���@��`@��/@���@���@�V@�%@���@��/@��@�1'@��@��@�1@�  @�1@��@��m@��@���@��F@�
=@��+@��R@���@��!@��+@��T@���@�p�@��@�%@��j@�I�@�b@���@���@��@�l�@�t�@�K�@�@��!@�~�@�M�@�-@�$�@��@�{@��@���@���@�@�@�hs@��`@���@���@���@�Ĝ@�I�@�  @�ƨ@�|�@�33@�@��y@�ȴ@��+@�n�@�M�@�J@��-@�hs@�X@�/@��`@��@�r�@���@�l�@�\)@�K�@�33@��@���@���@���@��\@�n�@�^5@�E�@�E�@�=q@��@���@�X@�X@�7L@���@���@���@��@��@��`@��@��;@���@�\)@�C�@�C�@�+@�
=@��H@���G�O�@��@x*�@p�	@k9�@a \@U��@O��@G��@@Ɇ@:��@3��@.:*@)-w@$�@ N�@�+@$�@�@m]@	�@�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�(�A�33A�S�A��A�K�A�?}A�(�A���A���A��A���A��A�C�A��`A�Q�A�A���A�jA�^5A��/AҶFA���A��HA�/A�9XA�VAAĺ^A�\)A�33A�1'A�z�A�&�A�JA�ȴA�\)A�=qA�^5A���A�A�A�"�A¶FA��/A�ȴA�v�AuA�l�A�p�A���AŶFA�%A�"�A��A�=qA�1'A�7LA�1'A���A��yA�C�A�dZA���A��A���A��A���A�5?Aқ�AǁA�{A���AŴ9Aʺ^A�-A�XAîA�G�A�  A���Aĉ7A�I�A�v�A�1'A��yA���A�x�A�^5A��A�  A���A�r�A�
=A�ZA��A�;dA�5?A�A�bAð!A��AҋDA�+A�S�A���A��A��`A�M�A�jAŅA���A�bNAAŏ\A�/A�1'A�-A���A�I�A�K�A��A�A� �A�;dA��A�7LA��A�
=Aŗ�A�1'A�{A��wA�`BA�(�A�-AƍPA�^5Aú^A�\)A�K�AЮA�ZAΣ�APA���A���A�ffA��A�{A�v�AÍPA���A�\)A�"�A�A�A�/A�bAț�A҅A��A��;A��9A�JA�K�A�A�
=A� �A�n�A���AœuA�/A��
A���A�5?A��AʋDA��A�(�A�A�"�A�/A���A��A��Aŉ7Aҝ�A���A�"�A�v�A�33AîA�{A�-A�1'AҋDAÓuA� �A� �A�ƨA�A�C�A�+A�$�Aʕ�AҾwAȾwA��A�bNAҶFA�A�(�Aҩ�AļjA�/A�/Aқ�A�p�AȑhA�{A��HA�7LA�&�A��A�/A���A�
=A�33A�(�A�7LAҟ�A��A�1'A�5?A�JA�;dA�1'A�33A�7LA�-A�+A�5?A� �A�ƨA�33A�
=A�9XA�;dA�7LA�7LA�p�A�E�A�  A�E�A�9XA�-A�33A�A�A�(�A���A�7LA�+A�+A�x�A�-A�(�A�-A�+A��A̾wA�(�A�+A� �A�+A�\)A�+A�(�A��A� �A�-A�7LA�-A�-A�+A�r�A�-A�/A�1'A�/A�-A�&�A�33A�1'A�1'A�-A�+A�-A�/A�1'A�33A�9XA�-A�(�A�$�A�"�A�(�A�/A�"�A�"�A��A��A�  A��A��A��HA��A̟�A�(�A��A��A��A�&�A� �A��A���A�&�A�$�A� �A�K�A�&�A� �A��A�bNA��A���A�VA��mA�JA�{A��A�{A̙�A��A��A�G�A�"�A�&�A�&�A�&�A�"�A�"�A�^5A� �A���AθRA�"�A��A�{A�%A��A� �AΑhA��`A��A� �A�"�A� �A� �A��A�~�A��A�(�A�&�A� �A�$�A� �A���A�+A�$�A��A�$�A� �A�"�A��A��A��A��A��A� �A�&�A��A� �A��A�"�A��A�"�A�ffA��A�"�A��A��A��A��A��Aҩ�A� �A�$�A��A�oA��A��A�{A�ffA�
=A��A�bNA��A�5?A�$�A�$�A��TA�
=A�&�A�jA�+A�&�A�(�A��A�&�A�z�A�(�A� �A��A�l�A�$�A�&�A� �A�ZA�+A�"�A�"�A�|�A��A�&�A�&�AΉ7A�(�A���A� �A�$�A�-A�1A�AѺ^A��AґhA��A�p�A��A�&�A��A�
=A�"�A��A��HA�JAҋDA�$�A�&�AѬA�&�A��A�&�A��A�+A�(�A�-A���A�-A�(�A�&�A�&�A�"�A�1'A�&�A�&�A��A��A�+A�(�A�+A�(�A�+A�+A�(�A�&�A�$�A� �A�&�A�$�A�+A�(�A�-A�/A�"�A�33A�1'A�1'A�5?A�-A�33A�1'A�+A�/A�33A�5?A�33A�33A�;dA�5?A�5?A�33A�9XA�=qA�=qA�;dA�;dA�;dA�;dA�9XA�;dA�=qA�=qA�7LA�9XA�;dA�9XA�;dA�1'A�33A�33A�1'A�/A�1'A�/A�-A�+A�/A�1'A�1'A�/A�+A�-A�-A�-A�-A�(�A�(�A�(�A�&�A�&�A�(�A�+A�+A�-A�(�A�"�A�&�A�$�A�&�A�1'A�1'A�=qA�+A�-A�=qA�?}A�=qA�/A�33A�1'A�1'A�1'A�33A�-A�/A�;dA�33A�A�A�A�A�?}A�=qA�=qA�?}A�?}A�=qA�?}A�9XA�;dA�9XA�7LA�;dA�7LA�9XA�=qA�?}A�?}A�=qA�?}A�?}A�=qA�?}A�?}A�=qA�=qA�?}A�?}A�?}A�?}A�?}A�=qA�?}A�?}A�=qA�;dA�=qA�9XA�;dA�7LA�5?A�5?A�7LA�5?A�5?A�33A�33A�7LA�5?A�?}A�?}A�?}A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�=qA�A�A�?}A�?}A�?}A�?}A�A�A�A�A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�=qA�=qA�?}A�=qA�?}A�=qA�A�A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�C�A�=qA�A�A�A�A�A�A�=qA�=qA�?}A�A�A�?}A�?}A�?}A�?}A�A�A�C�A�A�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�A�A�C�A�C�A�E�A�C�A�E�A�C�A�C�A�C�A�C�A�E�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�E�@�l�@�dZ@�\)@�K�@�K�@�K�@�S�@�S�@�S�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�C�@�C�@�;d@�;d@�+@�33@�+@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@��@�"�@�o@��@�o@�o@�o@�
=@�@�@�@���@���@���@���@���@���@��y@��H@��H@��@��@��@��@���@���@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��R@��R@��!@��!@���@���@���@���@���@���@���@��\@�~�@�v�@�n�@�V@�M�@�M�A�1'A�33A�33A�1'A�/A�1'A�/A�-A�+A�/A�1'A�1'A�/A�+A�-A�-A�-A�-A�(�A�(�A�(�A�&�A�&�A�(�A�+A�+A�-A�(�A�"�A�&�A�$�A�&�A�1'A�1'A�=qA�+A�-A�=qA�?}A�=qA�/A�33A�1'A�1'A�1'A�33A�-A�/A�;dA�33A�A�A�A�A�?}A�=qA�=qA�?}A�?}A�=qA�?}A�9XA�;dA�9XA�7LA�;dA�7LA�9XA�=qA�?}A�?}A�=qA�?}A�?}A�=qA�?}A�?}A�=qA�=qA�?}A�?}A�?}A�?}A�?}A�=qA�?}A�?}A�=qA�;dA�=qA�9XA�;dA�7LA�5?A�5?A�7LA�5?A�5?A�33A�33A�7LA�5?A�?}A�?}A�?}A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�=qA�A�A�?}A�?}A�?}A�?}A�A�A�A�A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�=qA�=qA�?}A�=qA�?}A�=qA�A�A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�C�A�=qA�A�A�A�A�A�A�=qA�=qA�?}A�A�A�?}A�?}A�?}A�?}A�A�A�C�A�A�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�C�A�A�A�C�A�C�A�E�A�C�A�E�A�C�A�C�A�C�A�C�A�E�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�E�@�l�@�dZ@�\)@�K�@�K�@�K�@�S�@�S�@�S�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�;d@�;d@�;d@�C�@�C�@�;d@�;d@�+@�33@�+@�+@�+@�"�@�"�@�"�@�"�@�"�@�"�@��@�"�@�o@��@�o@�o@�o@�
=@�@�@�@���@���@���@���@���@���@��y@��H@��H@��@��@��@��@���@���@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��R@��R@��!@��!@���@���@���@���@���@���@���@��\@�~�@�v�@�n�@�V@�M�@�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���@��n@�	�=��>-R~=�=�u�>$�?�!=��.?!,�=��;=��=�~�>C-?��Z@|��=��]=��>G�@���?s�Z=j�v=��=��}=�Uq=�qL=��>3�1>^��@�@�!>8<�>c�4@��=�}�=�Vm>��?o@�q=�$t=Õ�>N<@;��=��=��I=��^?�KI@�~>e�?~H�@�#=��>i�4@��@�c�>(F =e�$=��=��x=�ҳ>@�]@��=Կ
=��?�]d@��@�0=�Ց>xdE=��=�<6>�@�_@�E=��=���?�.=���>^�>�{@��=|ؙ=��4=���>�{>�'�@��#=�w�>[>�U@���=���>DJb=��	=��=��=�[�=�[W>&�?�ܱ@�e@9�=Y��=�j�=�f=��=�c5=��=�e,=���=��?>hI>=D@��@��>��>5�.>��@a��@��=�dZ=��A>K�	@��@��@{>�]@�?X2v=��&>���@�f?�VC>��@��=�	�=��D>`�@�>�C�>�X�>���=�.I=���>_��?3� =�>>"�=�|1=�^t=���>Ɇ@vS�@��@��>?��@�@���@�bx=� >�>MV@��=��k=�mH>]�=���>+C@��@��=��w=��N=���>&��@�5�?
��>!�>�KI@�f>�#=�7�>�>	��@Y��@Z��@�?BS>�p=�K�>)[�@�@��@�{=��=�82=��6>	~?`��@{.�@��@��>)k�@��>%�@!��>$Jb@��>��a@�j@��=�%�>PZq@��@�&@8p�>$-�@�w>:xl@�\@��@��w@�&@��?���@�\@�@��@�	@���@�\@��@���@��@��@�\@�\@��@�7@��@��?�t�@��@�\@�\@��@��@��@v@�@��@�Q@��@��@�\@�@��@��@��@�&@��@[�@��@�j@�j@��@�?�0j@�&@�@��@�j@��@�j@�@��?7�C@��@�&@�j@�j@�@���@�
�@��@��@��@�@�&@�&@��@�7@��@��@�7@��@��@��@�j@�Y@��@�f@��@��@�@�f@�#@�#@��@�.4@�f@�@�,@��?�K@��@�#@��@�w@��@�0@�b>ac@�f@�U@��?iX�@���@�@��?��}@�0@��@��+@��@�@��@��@�?e�@��n@��@�y@��@�w@��@�#@��@�U>�@��?.��>��q@�f@�0?{_@��@��@�U@Roi?��@��@��@�0@�#@�#@��@R.�@�]�@�f@�f@��@��@�#@�w@��@�3@��@�#@�w@�f@��@��@�U@�U@��@��@��@��@��@��@��@�s@��@��@�@�U@��@��@��@�b@�@���@�#@��@��@�@�f@��@��@�)_@�1f@�0>���@�U?���@�Y@��@��@���@���>���@�@��@�3@�w@�'>��A@��@��@��?�@��@�Y@��@��]@�Y@�w@�f?�#�>O�6@��@��?�@��@���@�Y@�&@�@�>�\�@��Z@� G@�#>tFt@I?�@T@��@�f@�1@��@�3?�e�@��@�H�@��@��@<a�@�Y@�j@��@�w@��@��@�Y@��@�7@��@��@�Y@��@��@�j@��@�f@��@��@�&@�&@��@��@��@��@�Y@�@��@��@�L@��@��@�\@�\@�	@��@�*@��@��@�L@�@��@��@��@�O@�O@��@�O@��@��@��@��@� @� @� �@� �@� �@� �@� @��@��@� @� �@� �@� �@��@� �@�`@�@�@�`@��@��@��@��@��@��@��@��@� �@� @�`@�@� �@�u@�u@��@��@��@�O@�:@��@��@��@�u@� 2@��@��@��@� �@�"h@�"h@�$�@�!�@� �@�$5@�"�@�#%@�#y@�"�@�!�@�!�@�!�@�!�@�"h@�$�@�$t@�$�@�'(@�'(@�'|@�'(@�'(@�&�@�'(@�'(@�'|@�&�@�&l@�%[@�%1@�%�@�%�@�%�@�&@�'�@�'�@�'�@�(N@�(N@�'�@�'�@�(N@�'�@�(N@�(�@�(�@�(N@�'�@�'�@�'�@�'�@�'�@�'�@�'�@�'=@�&�@�&�@�'�@�&@�%�@�%�@�%@�%@�%@�&@�(N@�(�@�)_@�)@�)t@�)�@�)t@�)@�)@�)�@�)t@�)t@�)t@�)t@�)�@�)�@�*0@�*0@�)�@�*0@�*�@�*0@�*0@�*�@�*0@�*�@�*�@�*�@�*�@�*0@�*�@�*�@�*�@�*�@�*�@�*�@�*�@�*�@�)�@�*�@�*�@�*�@�*�@�+A@�+A@�+A@�+A@�+A@�+A@�+�@�+�@�+�@�,@�+�@�+�@�,@�,@�,g@�,g@�,@�,@�+�@�,|@�,�@�,�@�,|@�,�@�,|@�,�@�,�@�,�@�,|@�-8@�,�@�,�@�-8@�-8@�-8@�-8@�,�@�-8@�-8@�-8@�-M@�,�@�,�@�-8@�,�@�-8@�-8@�-�@�.I@�.�@�.I@�/o@�/@�/@�/@�/o@�/o@�/�@�/�@�/�@�/�@�0+@�0+@�0+@�0+@�0+@�0@�0�@�0�@�0�@�0�@�1Q@�0�@�1Q@�0�@�1�@�1Q@�1�@�2@�2a@�2#@�2a@�2a@�3@�3�@P�@Pپ@P�@P��@P��@P�o@P��@P�o@P��@P��@P�o@P�o@P�o@P�o@P�E@P��@P�o@P�o@P��@Pؙ@Pؙ@P��@P��@P��@P�@Pٔ@Pپ@Pپ@Pپ@Pپ@Pٔ@P�@P�f@P�@P�@P�@P�f@P�@Pڐ@Pٔ@Pٔ@P��@P�@@P��@P��@P�@@P��@P��@Pם@Pם@Pם@P�I@P��@P��@P�I@P�I@P�I@P�@P�I@P֡@Pզ@Pզ@P�R@Pզ@PԪ@P�@PӮ@P�[@P�[@P�@Pҳ@P�[@Pҳ@P�@Pӄ@P�9@PБ@P�>@P�h@P��@Pϖ@Pϖ@Pϖ@Pϖ@Pϖ@P��@P��@P��@P��@P�F@PΚ@P��@PΚ@P�B@Pϖ@Pϖ@P��@P��@P�@P��@P�>@P�>@P�B@PΚ@P�F@P͟@P�K@P��@P�O@P��@P˧@P�)@P�.@P�6@P�?@Pŗ@P�H@P��@P�/@P�/@�#y@�$5@�$@�#�@�"�@�#:@�"�@�!�@�!@�"@�#:@�#O@�"�@�!�@�!�@�!�@�"�@�"@� �@�!@� �@� �@� �@� �@�!B@�!�@�"�@� �@�K@� \@��@� @�$�@�%@�(�@�#%@�#�@�)�@�*�@�)�@�$ @�&-@�%[@�%�@�%p@�%1@�$ @�%@�) @�&@�*�@�+V@�*�@�*Z@�*�@�*�@�*�@�*Z@�*�@�)J@�*o@�(�@�'�@�*E@�($@�)_@�*o@�+@�+k@�*�@�+A@�+@�+@�+k@�+�@�+k@�+k@�+�@�+�@�+�@�+�@�+�@�+@�+�@�+�@�+A@�*�@�*�@�*�@�*0@�) @�(�@�(N@�)J@�(�@�'�@�'�@�($@�(�@�)@�,|@�,|@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�-#@�,�@�-8@�,�@�-8@�-b@�-b@�-w@�-w@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�.
@�-�@�-�@�-�@�-�@�-�@�-�@�.@�.@�.
@�.4@�.^@�.4@�.^@�.^@�.�@�.�@�.�@�.�@�.@�.�@�/@�/@�/@�/@�/�@�/@�/�@�/Z@�.�@�/Z@�/�@�0@�/�@�/�@�/�@�/�@�0@�0@�0@�0@�0@�0@�0@�/�@�0@@�0U@�/�@�0@�0�@�0@�0@@�/E@�/�@�0@�/�@�0@�0@�0�@�1@�1@�1f@�1�@�2#@�1�@�28@�28@�28@�2a@�2#@�2v@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�3@�3@�3@�3@�3]@�3r@�3r@�3�@�3�@�3�@�3�@�3�@�3�@�4@�4Y@�4Y@�4n@P�8@P��@P��@P֡@P֡@P�@Pם@Pם@Pם@P�s@P�I@P�s@Pם@P�s@P�@P�@P�s@Pם@P�s@P�@P�I@Pם@Pם@P��@P��@P�E@Pؙ@P��@P��@Pؙ@Pؙ@P��@P��@P�@P��@P��@P�@P�@P�@@P��@P�o@P��@P�o@P�@P��@Pؙ@P�@P�@P��@P��@P�w@P֡@P�M@P��@P�$@P�M@P�$@P�w@P�|@P�M@P�V@P��@P��@PԪ@P�@P�_@P�5@P�5@P��@P�9@P�9@Pэ@Pѷ@P�@Pѷ@Pл@P�l@P�B@PΚ@P�F@P��@P��@P�u@P��@P��@P̣@P��@P�O@P̣@P��@P��@P�K@P�K@P͟@P�p@P�p@PΚ@PΚ@P��@P�@P�l@Pϖ@P�B@P�@P��@P��@P�O@P�y@P�%@P��@P��@P�S@P��@PȊ@P�i@P�C@P�@P�U@P�]@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  433444444444444434443444444444334434444344444444344344334444443444334444433444444344444344434444444443444444444444334443344433343444344344434444444444444333433344434444433444434443444433344443334444433343444343344334434333334333333333333333343333334333333333333333333433333333433333333333333333333333333333334333334333333343334333433333333433333333343443343333433333333333333333333333333333333333333333333333333334343333343333343334333333344334333333433344433433433334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��n@�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@|��G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�!G�O�G�O�@��G�O�G�O�G�O�G�O�@�qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�~G�O�G�O�@�!G�O�G�O�@�}@�c�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@��@�1G�O�G�O�G�O�G�O�G�O�@�c@�GG�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�@��#G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��G�O�G�O�G�O�@a��@��G�O�G�O�G�O�@��@��@{G�O�@�"G�O�G�O�G�O�@�jG�O�G�O�@��G�O�G�O�G�O�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@vS�@��@��G�O�@�@���@�bxG�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�@�~@��G�O�G�O�G�O�G�O�@�5�G�O�G�O�G�O�@�gG�O�G�O�G�O�G�O�@Y��@Z��@�"G�O�G�O�G�O�G�O�@�@��@�zG�O�G�O�G�O�G�O�G�O�@{.�@��@��G�O�@��G�O�G�O�G�O�@��G�O�@�h@��G�O�G�O�@��@�*G�O�G�O�@�uG�O�@�^@��@��v@�&@��G�O�@�Z@�@��@�@���@�`@��@���@��@��@�[@�a@��@�4@��@��G�O�@��@�[@�a@��@��@��G�O�@�@��@�Q@��@��@�[@�@��@��@��@�)@��@[�@��@�l@�g@��@�G�O�@�$@�@��@�j@��@�n@�@��G�O�@��@�%@�j@�j@�@���@�
�@��@��@��@�@�'@�'@��@�6@��@��@�6@��@��@��@�j@�W@��@�d@�@��@�@�b@�$@�!G�O�@�.3@�j@�@�2@��G�O�@��@�$@��@�y@��@�1@�^G�O�@�f@�Y@��G�O�@���@�@��G�O�@�*@��@��-@��@�@��@��@�G�O�@��o@��@�y@��@�v@��@�!@��@�YG�O�@��G�O�G�O�@�f@�/G�O�@��@��@�W@RomG�O�@��@��@�3@�"@�&@��@R.�@�]�@�j@�i@��@��@�"@�x@��@�7@��@� @�w@�d@��@��@�Z@�[@��@��@��@��@��@��@��@�v@��@��@�@�R@��@��@��@�a@�@���@�"@��@��@�@�m@��@��@�)^@�1d@�3G�O�@�WG�O�@�]@��@��@���@���G�O�@�@��@�2@�u@�&G�O�@��@��@��G�O�@��@�Z@��@��Z@�Y@�v@�bG�O�G�O�@��@��G�O�@��@���@�Y@�'@�@�G�O�@��Y@� F@�&G�O�G�O�G�O�@��@�jG�O�@��@�5G�O�@��@�H�@��@��G�O�@�Y@�k@��@�x@��@��@�\@��@�7@��@��@�\@��@��@�h@��@�f@��@��@�)@�(@��@��@��@��@�_@�@��@��@�J@��@��@�\@�\@�
@��@�-@��@��@�O@�@��@��@��@�R@�Q@��@�N@��@��@��@��@� @� @� �@� �@� �@� �@� @��@��@� @� �@� �@� �@��@� �@�^@�#z@�$9@�$@�#�@�"�@�#>@�"�@�!�@�!@�"@�#:@�#U@�"�@�!�@�!�@�!�@�"�@�"@� �@�!@� �@� �@� �@� �@�!A@�!�@�"�@� �@�J@� [@��@� @�$�@�%@�(�@�#&@�#�@�)�@�*�@�)�@�$#@�&/@�%^@�%�@�%n@�%2@�$@�%@�)!@�&@�*�@�+U@�*�@�*Z@�*�@�*�@�*�@�*Z@�*�@�)L@�*q@�(�@�'�@�*C@�((@�)a@�*r@�+@�+k@�*�@�+D@�+@�+@�+n@�+�@�+j@�+m@�+�@�+�@�+�@�+�@�+�@�+@�+�@�+�@�+A@�*�@�*�@�*�@�*6@�)#@�(�@�(O@�)N@�(�@�'�@�'�@�("@�(�@�)@�,}@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�-&@�,�@�-;@�,�@�-<@�-a@�-d@�-w@�-z@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�.@�-�@�-�@�-�@�-�@�-�@�-�@�."@�."@�.@�.4@�.c@�.8@�.a@�.^@�.�@�.�@�.�@�.�@�."@�.�@�/@�/@�/@�/@�/�@�/@�/�@�/Z@�.�@�/Y@�/�@�0@�/�@�/�@�/�@�/�@�0@�0@�0@�0@�0@�0@�0@�/�@�0B@�0[@�/�@�0@�0�@�0�@�0A@�/C@�/�@�0�@�/�@�0
@�0@�0�@�1@�1@�1g@�1�@�2"@�1�@�2:@�29@�2:@�2g@�2&@�2u@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�3!@�3!@�3@�3"@�3^@�3s@�3w@�3�@�3�@�3�@�3�@�3�@�3�@�4@�4Z@�4X@�4n@P�:@P��@P��@P֞@P֣@P� @Pכ@Pם@Pמ@P�s@P�H@P�s@Pנ@P�r@P�@P� @P�x@Pכ@P�s@P�@P�H@Pנ@Pם@P��@P��@P�E@P؝@P��@P��@Pؘ@Pؘ@P��@P��@P�@P��@P��@P�@P�@P�B@P��@P�r@P��@P�p@P�@P��@Pؘ@P�@P�@P��@P��@P�v@P֠@P�M@P��@P�#@P�M@P�"@P�x@P�}@P�M@P�U@P��@P��@Pԫ@P�@P�`@P�2@P�8@P��@P�6@P�6@Pэ@PѺ@P�@Pѵ@Pл@P�m@P�@@PΛ@P�F@P��@P��@P�s@P��@P��@P̥@P��@P�N@P̠@P��@P��@P�K@P�J@P͞@P�p@P�r@PΛ@PΛ@P��@P�@P�h@Pϓ@P�E@P�@P��@P��@P�S@P�x@P�&@P��@P��@P�S@P��@Pȍ@P�k@P�@@P�"@P�V@P�^@P��@�#z@�$9@�$@�#�@�"�@�#>@�"�@�!�@�!@�"@�#:@�#U@�"�@�!�@�!�@�!�@�"�@�"@� �@�!@� �@� �@� �@� �@�!A@�!�@�"�@� �@�J@� [@��@� @�$�@�%@�(�@�#&@�#�@�)�@�*�@�)�@�$#@�&/@�%^@�%�@�%n@�%2@�$@�%@�)!@�&@�*�@�+U@�*�@�*Z@�*�@�*�@�*�@�*Z@�*�@�)L@�*q@�(�@�'�@�*C@�((@�)a@�*r@�+@�+k@�*�@�+D@�+@�+@�+n@�+�@�+j@�+m@�+�@�+�@�+�@�+�@�+�@�+@�+�@�+�@�+A@�*�@�*�@�*�@�*6@�)#@�(�@�(O@�)N@�(�@�'�@�'�@�("@�(�@�)@�,}@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�,�@�-&@�,�@�-;@�,�@�-<@�-a@�-d@�-w@�-z@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�-�@�.@�-�@�-�@�-�@�-�@�-�@�-�@�."@�."@�.@�.4@�.c@�.8@�.a@�.^@�.�@�.�@�.�@�.�@�."@�.�@�/@�/@�/@�/@�/�@�/@�/�@�/Z@�.�@�/Y@�/�@�0@�/�@�/�@�/�@�/�@�0@�0@�0@�0@�0@�0@�0@�/�@�0B@�0[@�/�@�0@�0�@�0�@�0A@�/C@�/�@�0�@�/�@�0
@�0@�0�@�1@�1@�1g@�1�@�2"@�1�@�2:@�29@�2:@�2g@�2&@�2u@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�3!@�3!@�3@�3"@�3^@�3s@�3w@�3�@�3�@�3�@�3�@�3�@�3�@�4@�4Z@�4X@�4n@P�:@P��@P��@P֞@P֣@P� @Pכ@Pם@Pמ@P�s@P�H@P�s@Pנ@P�r@P�@P� @P�x@Pכ@P�s@P�@P�H@Pנ@Pם@P��@P��@P�E@P؝@P��@P��@Pؘ@Pؘ@P��@P��@P�@P��@P��@P�@P�@P�B@P��@P�r@P��@P�p@P�@P��@Pؘ@P�@P�@P��@P��@P�v@P֠@P�M@P��@P�#@P�M@P�"@P�x@P�}@P�M@P�U@P��@P��@Pԫ@P�@P�`@P�2@P�8@P��@P�6@P�6@Pэ@PѺ@P�@Pѵ@Pл@P�m@P�@@PΛ@P�F@P��@P��@P�s@P��@P��@P̥@P��@P�N@P̠@P��@P��@P�K@P�J@P͞@P�p@P�r@PΛ@PΛ@P��@P�@P�h@Pϓ@P�E@P�@P��@P��@P�S@P�x@P�&@P��@P��@P�S@P��@Pȍ@P�k@P�@@P�"@P�V@P�^@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  433444444444444434443444444444334434444344444444344344334444443444334444433444444344444344434444444443444444444444334443344433343444344344434444444444444333433344434444433444434443444433344443334444433343444343344334434333334333333333333333343333334333333333333333333433333333433333333333333333333333333333334333334333333343334333433333333433333333343443343333433333333333333333333333333333333333333333333333333334343333343333343334333333344334333333433344433433433334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�#29�#�9�#�9�#u9�"l9�"�9�"�9�!�9� �9�!�9�"�9�#9�"o9�!c9�!|9�!b9�"�9�!�9� 09� �9� 9� ^9� s9� s9� �9�!89�"T9� v9��9��9�)9��9�$}9�$�9�(�9�"�9�#�9�)�9�*�9�)�9�#�9�&9�%,9�%i9�%=9�$�9�#�9�$�9�)9�%�9�*�9�+i9�*�9�*b9�*�9�*�9�*�9�*b9�*�9�)H9�*z9�(�9�'�9�*J9�(9�)^9�*{9�+9�+�9�*�9�+W9�+,9�+9�+�9�+�9�+9�+�9�+�9�+�9�+�9�+�9�+�9�+9�+�9�+�9�+T9�*�9�*�9�*�9�*=9�)9�(�9�(?9�)J9�(�9�'�9�'�9�(9�(�9�)9�,�9�,�9�,�9�,�9�,�9�,�9�,�9�- 9�-9�-9�,�9�-O9�-9�-e9�- 9�-f9�-�9�-�9�-�9�-�9�-�9�-�9�-�9�-�9�-�9�.9�-�9�.9�.9�.9�.?9�-�9�.9�.9�-�9�.9�-�9�.V9�.V9�.A9�.i9�.�9�.m9�.�9�.�9�.�9�.�9�/9�/9�.V9�/9�/G9�/\9�/G9�/J9�/�9�/\9�/�9�/�9�/ 9�/�9�/�9�0a9�0"9�0
9�09�/�9�0P9�0O9�0O9�0d9�0a9�0d9�0a9�09�0�9�0�9�/�9�0a9�0�9�0�9�0�9�/�9�/�9�0�9�0'9�0U9�0L9�0�9�1h9�1j9�1�9�2[9�2�9�2F9�2�9�2�9�2�9�2�9�2�9�2�9�39�3$9�3!9�3R9�39�3O9�3N9�3#9�3�9�3�9�3�9�3�9�3�9�3�9�3�9�49�49�4(9�4,9�4(9�4U9�4�9�4�9�4�9�4�9Ze,9Zc�9Zb�9Z`Z9Z`_9Z`�9Zac9Zae9Zaf9Za99Za9Za99Zah9Za89Z`�9Z`�9Za>9Zac9Za99Z`�9Za9Zah9Zae9Za�9Za�9Zb9Zbq9Zb�9Zb�9Zbk9Zbk9Zb�9Zb�9Zb�9Zb�9Zb�9Zb�9Zb�9Zc9Zb�9ZbD9Zb�9ZbA9Zb�9Zb�9Zbk9Zb�9Z`�9Za�9Z_�9Z`09Z`\9Z`9Z_�9Z_�9Z`9Z_�9Z`29Z_,9Z`9Z]�9Z^�9Z]y9Z^P9Z]�9Z[�9Z[�9Z[�9Z[j9ZZ�9ZZ�9Z[9Z[=9ZZ�9Z[89ZZ29ZX�9ZX�9ZW�9ZW�9ZX#9ZXP9ZV�9ZWL9ZX#9ZU�9ZU:9ZU�9ZU�9ZV9ZV9ZV�9ZV�9ZV�9ZW�9ZW�9ZW�9ZW�9ZXP9ZX9ZX�9ZX�9ZX�9ZWt9ZW9ZV9ZU�9ZU�9ZUh9ZU=9ZU9ZT�9ZT9ZQ�9ZOj9ZN19ZM9ZJ9ZH
9ZH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�VB�VB�VB�VB�PB�PB�PB�VB�PB�PB�PB�PB�PB�PB�PB�PB�PB�VB�VB�VB�VB�VB�VB�\B�\B�\B�\B�\B�VB�\B�bB��B�fB�B�B�B�B�B��B  BB�B��BBB��B+B�B �B�B"�B�B�B��B��BBŢBĜB�}B�XB�3B�B��B�{B�+Bq�BhsB_;BYBK�B=qB5?B+B�B\BJB
=BB�B�BB��B��B�B��B��B�Bm�Bk�BhsBe`BcTBbNBaHBVB(�BoBbBoBoBVBB
�ZB
�wB
��B
�1B
n�B
VB
I�B
�B	�B	��B	{�B	J�B	0!B	'�B	�B	{B	oB	hB	hB	uB	hB	B��B�B�B�B�B�mB�BB�mB�ZB�;B�BɺB�RB��B��B��B��B�3B�LB�FB�?B�!B��B��B��B��B��B��B��B��B��B��B��B��B�bB�7B�7B�1B�=B�=B�=B�=B�=B�DB�JB�%B�B� B~�B|�B{�By�Bw�Bt�Bq�Bl�BgmBdZBcTBcTBcTBaHB\)BW
BR�BP�BP�BN�BL�BK�BH�BG�BE�BC�BA�B=qB<jB:^B9XB9XB8RB6FB49B33B2-B1'B1'B1'B0!B0!B0!B.B,B+B)�B(�B(�B(�B)�B)�B+B+B+B+B+B)�B)�B)�B'�B&�B$�B$�B#�B$�B$�B%�B'�B&�B'�B.B1'B2-B2-B7LB9XB:^B;dB;dB>wBC�BG�BJ�BL�BQ�BW
BYBYBZB[#B]/B`BBq�B|�B}�B}�B}�B~�B� B�B�B�B�B�B� B|�B|�B|�B~�By�Bw�Bv�Bv�Bv�Bv�Bw�B{�B~�B�B�JB�\B�+B�%B�DB�VB�\B�bB�bB�oB�uB�oB�uB�uB�{B��B��B�9BÖB�B�B�#B�fB�yB�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	+B	
=B	DB	PB	bB	{B	{B	�B	�B	�B	�B	�B	�B	#�B	$�B	$�B	$�B	%�B	%�B	&�B	'�B	(�B	+B	/B	0!B	1'B	1'B	1'B	33B	9XB	>wB	A�B	F�B	H�B	G�B	G�B	F�B	F�B	H�B	J�B	M�B	N�B	N�B	Q�B	R�B	S�B	T�B	[#B	dZB	gmB	iyB	k�B	m�B	p�B	q�B	q�B	r�B	s�B	u�B	u�B	v�B	v�B	w�B	v�B	v�B	z�B	� B	� B	�B	�B	�B	�B	�%B	�+B	�+B	�7B	�JB	�VB	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�3B	�3B	�-B	�-B	�-B	�3B	�9B	�?B	�?B	�LB	�RB	�RB	�^B	�jB	�wB	�wB	�}B	��B	B	ÖB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�NB	�TB	�`B	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�fB
-B
B
�B
�B
'�B
.IB
7�B
=qB
CB
I�B
P�B
TaB
XyB
\�B
_�B
e,B
jeB
l�B
rGB
wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?���Bi B�z?��?]K8?xO>�W?C��@3�>��@_�>⦏?�?�^?F@��kA�y >�ɬ?�3?$bA�t�@�|�>�u�>���>��_>�}>��U?֦?e�?���Bz�A�1�?h�?��tB��>��~>�<�?$�@�V�B�b>�Q�>��?7�A��>�h>�_)?.oA��B
J?+&�@��CB��?� ?��B|B��?W��>�s>�G�>�8.?C�?q'B��?�?�|@��B{�B��?��?�Lw>��>��?.��B{�B�o>�ŗ>��f@K�n?oz?0�p?1�FB@�>��\>�BJ>��?!h�?��FBl�>?,��?��Bp/>��k?{
>��>�Se>���>�&>�	�?+�A+ҶBr�A_)�>��>���?4>��_>�Ra?0�>��C>�0@>�Y6?/*I?lY�B}B}?,��?i�P?1�A�mRA�:�>�'g?�?��B}oB�AŢV?14�B~�@���?n�@��B��A��?4�B��>���>ٞ�?4&�A�um?�O�@�?���>�&J?��?�P�@y�??Pn�>��Q>�Ã>�]?:��A�8�B��B��?r��B	**BY�B<�?w�?F�U?�)�B�(>�IM?��?26>�>?Y �B|�B�>�/>��v?�	?PZB �@=3�?.��@�2B�0@-�)?P<?%��?,ڲA��NA�cB��@��?'|x?��?T�B�:B�LB��>�Z�>�p">�@?-�@���A�asB�XB�?S�WBod?O6YAoYz?MNMB�\?�QiB�)B�R?
��?���B�B��A���?N&�B�O?h��B�[B��B
�B�sBn�@��|B��B�uB��B��A�>fB��B�oBگB�lB�LB��B�SB�B��B��B�B@���B�|B�|B��B�dB��B��A?6�B��B��B�B��B��B��B�B�*B��B��B�B��A��B��B�B��B�PB��@�t�B��B�B��B�`A��B�YB��B�&@v7�B��B�SB��B��B�BJBzaB�B�B�B�EB��B��B�B��B��B�ZB�)B��B�NB��B��B��B�nB� B�iB��B�uB��B�zB��AmZA�eB�0B�B�:B�@��B��B�B�}B�&B��B��B�?���B�PB�B�T@��B0�B�TB�}@�#�B�pB��A�ưB�7B��B'?B�WB�[@�hdBsWB�A���B�B�6B��B��B�B��?�B�@o��?佲B��B��@G��B�&B��B��A�m�@�kB�<B��B��B�BB�:B�A�ωA���B�xB�@B��B�\B�BB��B�B��B�'B��B��B��B��B��B�:B�rB��B�9B��B�B��B�WB�zB�B�7B��B��B��B�cB��B�B��B��A���B�BB�TB�B�3B�hB�B��A�P�BB B��?�E�B�BA>JB��B�B�iB/�A���?�%B�B�6B�)B�fBIK?��iB��B��B�M@T B�cB��B��Bp�B�jB��B��AZ�?�EcB�=B�,@D�B�~A��QB�RB�cB� B�V?ʡ�A��'B��B�?�qeA�&�Ad�LB��B��Ab��B�+B��A �B��B#�B�mB��A���B��B��B��B�^B��B�dB��B�SB�1B�fB�6B��B��B�B��B�/B�`B��B��B��B��B�_B��B��B�MB��B�<B�B�'B�<B�B��B�
B�BB��B�B�B�jB�/B�B�MB��B�eB�!B�4B�lB��B�,B�PB��B��B�hB�\B��B�gB��B�'B�B�aB��B�B��B��B�JB�zB��B�jB��B�B�=B�B�9B�~B��B��B��B�_B�`B�&B��B�2B�?B��B�8B�:B�*B�B��B��B�PB��B��B��B�]B��B��B��B�B��B��B�B��B��B�@B��B�sB�}B��B� B��B�B�B��B�B�0B�B��B�|B�sB��B�B�sB�3B�WB�+B�#B�gB��B��B��B��B��B�}B��B�\B��B��B��B�[B��B��B��B�{B��B�2B��B��B�RB�B��B��B�~B��B��B�6B��B�B��B��B��B�fB�^B��B�;B�cB�!B��B�B�OB��B�RB�oB�AB��B��B��B�)B��B��B�B�TB�LB�kB��B�GB��B��B�{B�sB��B��B��B�^B��B��B�2B�PB�xB�@B�0B�(B�XB�B��B��B��B�/B�sB�cB�nB��B��B�VB�~B�>B�B��B��B�B��B��B��B��B��B�&B��B�B�cB��B�TB�B��B�<B��B��B�B�B��B�*B��B��B�JB�BB�2B��B��B��B�	B��B��B�RB�B�iB�B�PB�HB��B�fB��B�NB��B�B�HB�@B�_B��B��B��B��B��B�B��B��B��B��B��B��B�bB�RB�JB��B�B��B�-B��B�`B��B��B��B�-B�B�NB��B�?B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�}B	�pB	�GB	�B	�?B	�!B	�QB	�6B	�B	�-B	� B	�$B	�6B	�B	�B	�|B	�bB	�DB	�)B	�xB	�B	�PB	�CB	�)B	�YB	�B	�PB	�B	�B	�B	�)B	�B	�|B	��B	�B	��B	�B	�B	�B	�:B	��B	��B	�B	��B	��B	��B	��B	�>B	�B	�nB	�4B	�dB	�B	�B	��B	�B	�jB	�"B	��B	�VB	��B	�IB	�B	�B	� B	��B	��B	�B	�)B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�=B	�`B	�SB	�B	�iB	�{B	�AB	�rB	�eB	�B	�B	��B	�JB	�B	��B	�/B	��B	�B	�#B	�nB	��B	��B	�B	�B	�UB	�6B	�:B�$B�B��B�MB�!B��B�,B�B�B�iB��B��B��B��B��B��B��B��B�B�bB��B��B��B�B��B��B�B��B��B�1B�FB��B�B�tB�B��B��B��B��B��B�B�YB�SB��B�VB�UB��B��B��B��B��B�2B��B��B��B�pB�/B��B�B�?B�xB��B�tB�:B��B�"B��B�4B��B��B�]B�/B��B�kB�}B�+B�"B��B��B��B�{B��B��B�=B�cB��B��B�hB��B�}B�B��B�	B�B�XB��B�?B��B�wB��B��B��B��B�B��B��B��B��B��B��B��B�.B��B�0B��B��B�B��B�AB�9B�B�NB�YB�IB�	B�'B��B�B�FB�6B�\B�}B��B��B�xB��B�hB�.B��B��B��B�?B�B�.B��B�B�B�-B�7B��B�_B�}B��B�mB�eB��B�'B��B�PB�B�xB��B�
B��B��B��B�xB��B��B��B��B��B��B��B�0B�B��B��B�pB��B��B�B�B��B��B��B��B��B�IB��B�B�B�B��B��B��B��B��B��B��B��B�	B�B�DB�"B��B�B�B�B�*B�RB�B�	B�2B�=B�eB�KB�:B�}B�mB�eB��B��B��B�B��B	�B	�'B	�qB	��B	�B	�B	�<B	�/B	�B	��B	��B	��B	��B	�B	�_B	�RB	�uB	�B	�kB	�!B	�%B	�UB	�HB	�kB	�?B	�B	�B	��B	��B	�}B	�pB	�B	�B	�B	�kB	�QB	�cB	�UB	�ZB	�B	�B	��B	�B	��B	�B	�xB	��B	�]B	�B	�^B	�B	�B	�wB	�,B	�>B	�BB	�B	�GB	�B	� B	�B	��B	�B	�B	�B	��B	��B	�B	�eB	��B	��B	�B	�B	�B	��B	�<B	�IB	�B	�B	�MB	�B	�B	�B	��B	�MB	��B	�LB	�|B	�B	�B	�B	��B	��B	�	B	�B	�{B	�B	�sB	�B	�B	��B	��B	��B	�B	�B	�B	�WB	�iB	�B	��B	�B	�RB	��B	�AB	��B	��B	�B	�)B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999433444444444444434443444444444334434444344444444344344334444443444334444433444444344444344434444444443444444444444334443344433343444344344434444444444444333433344434444433444434443444433344443334444433343444343344334434333334333333333333333343333334333333333333333333433333333433333333333333333333333333333334333334333333343334333433333333433333333343443343333433333333333333333333333333333333333333333333333333334343333343333343334333333344334333333433344433433433334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B�kB�kB�kB�kB�fB�hB�dB�kB�fB�dB�hB�hB�hB�fB�fB�fB�fB�kB�mB�kB�kB�kB�hB�sB�sB�uB�sB�sB�kB�sB�zB��B�}B��B�B�B�B�B��B B'B��B�	B9B4B�BAB�B �B�B"�B�B�B��B�B¬BźBĸB��B�qB�KB�B��B��B�EBq�Bh�B_UBY0BK�B=�B5YB+B�BuBgB
XB2B��B�_B�B��B�,B�
B��B�"Bm�Bk�Bh�BezBcqBbiBabBVB)B�BB�B�BtB,B
�uB
��B
�B
�MB
n�B
V!B
I�B
�B	��B	��B	|B	J�B	0=B	(B	�B	�B	�B	�B	�B	�B	�B	?B��B��B��B��B��B�B�`B�B�wB�ZB�4B��B�sB�B��B��B�B�PB�kB�eB�_B�@B�B��B��B�
B��B��B��B��B��B��B��B��B��B�ZB�XB�QB�\B�\B�]B�\B�\B�cB�lB�FB�0B� BB}B|	By�Bw�Bt�Bq�Bl�Bg�Bd|BcvBcwBcvBajB\LBW+BSBQBQBN�BL�BK�BH�BG�BE�BC�BA�B=�B<�B:B9|B9yB8tB6gB4[B3TB2OB1JB1HB1JB0DB0BB0AB.7B,+B+$B*B)B)B)B*B*B+%B+"B+'B+%B+%B*B*B*B(B'B$�B%B#�B% B$�B&B(B'B(B.7B1IB2PB2QB7oB9}B:�B;�B;�B>�BC�BG�BJ�BL�BRBW/BY;BY<BZAB[IB]RB`eBq�B}B~B~B~BB�#B�)B�/B�6B�6B�0B�"B}B}B}BBy�Bw�Bv�Bv�Bv�Bv�Bw�B|	BB�8B�nB�B�QB�JB�jB�yB�B��B��B��B��B��B��B��B��B��B��B�]BøB�(B�5B�FB�B�B��B��B��B��B��B��B��B��B��B�B	 $B	9B	>B	CB	PB	
cB	iB	tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%B	%B	%B	&B	&B	'B	(B	)B	+%B	/@B	0GB	1MB	1LB	1OB	3YB	9{B	>�B	A�B	F�B	H�B	G�B	G�B	F�B	F�B	H�B	J�B	M�B	N�B	N�B	RB	SB	TB	U B	[DB	dB	g�B	i�B	k�B	m�B	p�B	q�B	q�B	r�B	s�B	u�B	u�B	v�B	v�B	w�B	v�B	v�B	{B	�%B	�&B	�-B	�(B	�>B	�BB	�IB	�PB	�PB	�[B	�qB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�.B	�?B	�EB	�YB	�WB	�VB	�PB	�RB	�SB	�WB	�`B	�dB	�dB	�rB	�uB	�wB	��B	��B	��B	��B	��B	��B	´B	ûB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�	B	�B	�B	�.B	�5B	�:B	�=B	�CB	�@B	�HB	�TB	�rB	�{B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	��B
QB
%B
B
�B
'�B
.oB
7�B
=�B
C9B
I�B
P�B
T�B
X�B
]B
`B
ePB
j�B
mB
rmB
w:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bi7B̔G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�y?G�O�G�O�G�O�A�u$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz�A�1�G�O�G�O�B��G�O�G�O�G�O�G�O�B�zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
cG�O�G�O�B��G�O�G�O�B|&B�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B{�B�
G�O�G�O�G�O�G�O�G�O�B{�B҉G�O�G�O�G�O�G�O�G�O�G�O�B@�G�O�G�O�G�O�G�O�G�O�Bl�G�O�G�O�G�O�BpCG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Br�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B}3B}G�O�G�O�G�O�A�msA�;G�O�G�O�G�O�B}�B�'AŢ{G�O�B~�G�O�G�O�G�O�B��G�O�G�O�B��G�O�G�O�G�O�A�u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9B��B��G�O�B	*ABY�B<�G�O�G�O�G�O�B�<G�O�G�O�G�O�G�O�G�O�B|�B�5G�O�G�O�G�O�G�O�B �G�O�G�O�G�O�B�GG�O�G�O�G�O�G�O�A��iA��B�G�O�G�O�G�O�G�O�B�QB�eB��G�O�G�O�G�O�G�O�G�O�A�a�B�oB�)G�O�BoyG�O�G�O�G�O�B�tG�O�B�@B�gG�O�G�O�B�1B��G�O�G�O�B�cG�O�B�qB�B
�!B��Bn�G�O�B��B��B��B�A�>�B��B��B��B�B�fB��B�lB��B��B��B�WG�O�B��B��B��B�yB��B��G�O�B��B��B�B��B��B��B�/B�BB�B�B�B��A��4B��B�0B��B�gB��G�O�B��B�#B��B�wA��B�rB��B�>G�O�B��B�iB��B��B�BJ BzwB�5B�B�-B�[B��B��B�B��B��B�tB�@B��B�dB��B��B��B��B�B��B��B��B��B��B��G�O�A��B�JB�5B�SB�)G�O�B��B�*B��B�=B��B��B�4G�O�B�gB�3B�lG�O�B0�B�nB��G�O�B��B��A���B�MB��B'VB�pB�pG�O�BsoB�)A���B�3B�LB��B�B�3B��G�O�B�G�O�G�O�B��B��G�O�B�;B��B��A�nG�O�B�RB�B��B�XB�SB�4A�ϧA��B��B�YB�B�rB�XB��B�,B��B�;B��B��B��B��B��B�SB��B��B�PB��B�*B�B�pB��B�'B�MB��B��B��B�xB��B�.B��B��A���B�XB�jB�)B�JB��B�/B��A�P�BBB�G�O�B�ZG�O�B��B�B��B0A���G�O�B�+B�KB�>B�{BI`G�O�B��B��B�cG�O�B�{B�B��Bp�B��B��B��G�O�G�O�B�TB�DG�O�B��A��zB�hB�zB�7B�mG�O�A��KB�B�'G�O�G�O�G�O�B�B��G�O�B�BB��G�O�B��B#�B��B��G�O�B�B��B��B�vB��B�}B��B�iB�HB�|B�KB�B��B�B��B�EB�xB��B��B��B�B�xB��B��B�dB��B�RB��B�@B�PB�,B��B�!B�XB��B�B�*B��B�EB�B�fB��B�}B�9B�LB��B��B�BB�bB��B��B�}B�tB��B�B��B�@B�5B�wB�B�-B��B�
B�]B��B�B��B��B�<B�B��B�cB�8B��B�BB�+B�5B�~B��B��B�B��B��B��B��B��B�)B�yB��B��B�B�0B��B��B�!B�B��B�FB�\B��B�+B��B�*B�B��B��B��B��B�0B�sB�kB��B�kB�nB��B��B��B�B��B�IB��B��B�B��B�EB��B�!B�TB��B��B��B�PB��B�:B��B�MB��B��B�tB�IB��B��B��B�?B�:B��B��B��B��B��B��B�WB�|B��B��B�~B��B��B�)B��B�!B�5B�mB��B�TB��B��B��B��B��B��B�!B��B��B��B�B��B��B��B�EB��B�IB��B�B�B�B�WB�SB�5B�fB�rB�aB�B�:B�B�-B�\B�MB�tB��B��B��B��B��B�B�EB�B��B��B�ZB�*B�IB�B�-B�)B�DB�NB��B�tB��B��B��B�~B��B�?B��B�fB�'B��B��B�!B��B��B��B��B��B��B��B��B��B��B��B�DB��B�B��B��B��B��B�$B�0B��B��B��B��B��B�\B�B�'B�8B�!B��B��B�B��B��B�	B��B�B�B�-B�\B�<B��B�)B�B�#B�BB�hB�)B�!B�HB�RB�~B�_B�NB��B��B�~B��B��B��B�B��B	�@B	�MB	�B	��B	��B	�/B	�`B	�RB	�9B	�B	��B	��B	�
B	��B	�B	�xB	�B	�B	�B	�EB	�JB	�}B	�nB	�B	�dB	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�wB	�B	�zB	�B	�2B	��B	�B	�B	�B	��B	�B	��B	�B	��B	�B	��B	��B	�B	�PB	�bB	�iB	�:B	�lB	�B	�$B	��B	�B	�DB	��B	�=B	�B	��B	��B	�B	�B	��B	�'B	�.B	�B	�B	�bB	�oB	�?B	�B	�rB	�B	��B	�B	��B	�qB	�	B	�qB	�B	��B	��B	��B	�B	��B	�/B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�}B	�B	�6B	�B	��B	�wB	�B	�gB	��B	�B	�6B	�NB	��B	�#B�<B�B��B�cB�8B��B�BB�+B�5B�~B��B��B�B��B��B��B��B��B�)B�yB��B��B�B�0B��B��B�!B�B��B�FB�\B��B�+B��B�*B�B��B��B��B��B�0B�sB�kB��B�kB�nB��B��B��B�B��B�IB��B��B�B��B�EB��B�!B�TB��B��B��B�PB��B�:B��B�MB��B��B�tB�IB��B��B��B�?B�:B��B��B��B��B��B��B�WB�|B��B��B�~B��B��B�)B��B�!B�5B�mB��B�TB��B��B��B��B��B��B�!B��B��B��B�B��B��B��B�EB��B�IB��B�B�B�B�WB�SB�5B�fB�rB�aB�B�:B�B�-B�\B�MB�tB��B��B��B��B��B�B�EB�B��B��B�ZB�*B�IB�B�-B�)B�DB�NB��B�tB��B��B��B�~B��B�?B��B�fB�'B��B��B�!B��B��B��B��B��B��B��B��B��B��B��B�DB��B�B��B��B��B��B�$B�0B��B��B��B��B��B�\B�B�'B�8B�!B��B��B�B��B��B�	B��B�B�B�-B�\B�<B��B�)B�B�#B�BB�hB�)B�!B�HB�RB�~B�_B�NB��B��B�~B��B��B��B�B��B	�@B	�MB	�B	��B	��B	�/B	�`B	�RB	�9B	�B	��B	��B	�
B	��B	�B	�xB	�B	�B	�B	�EB	�JB	�}B	�nB	�B	�dB	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�wB	�B	�zB	�B	�2B	��B	�B	�B	�B	��B	�B	��B	�B	��B	�B	��B	��B	�B	�PB	�bB	�iB	�:B	�lB	�B	�$B	��B	�B	�DB	��B	�=B	�B	��B	��B	�B	�B	��B	�'B	�.B	�B	�B	�bB	�oB	�?B	�B	�rB	�B	��B	�B	��B	�qB	�	B	�qB	�B	��B	��B	��B	�B	��B	�/B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�}B	�B	�6B	�B	��B	�wB	�B	�gB	��B	�B	�6B	�NB	��B	�#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999433444444444444434443444444444334434444344444444344344334444443444334444433444444344444344434444444443444444444444334443344433343444344344434444444444444333433344434444433444434443444433344443334444433343444343344334434333334333333333333333343333334333333333333333333433333333433333333333333333333333333333334333334333333343334333433333333433333333343443343333433333333333333333333333333333333333333333333333333334343333343333343334333333344334333333433344433433433334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.28 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.28 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.28 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281454372020082814543720200828145437202008281454372020082814543720200828145437202008281454372020082814543720200828145437202008281454372020082814543720200828145437AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730262019021417302620190214173026    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730262019021417302620190214173026  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730262019021417302620190214173026  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281454372020082814543720200828145437  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                