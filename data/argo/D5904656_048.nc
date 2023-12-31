CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  d   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:38Z creation      
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
resolution        =���   axis      Z        (�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
,  m�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (�  w�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
,  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (�  �x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
,  �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
, /   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� 90   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (� a�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
, ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
, �l   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (� ǘ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
, �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� #$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
, K�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� V    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � ~�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173038  20200828145508  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               0   0   0AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @׮���8L@׮���8L@׮���8L111 @׮�)��@׮�)��@׮�)��@5�bM��@5�bM��@5�bM���c���+�c���+�c���+111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    0   0   0ADA BDA  DA BDA @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�33A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�{D��D�?
D���D���D�RD�?�D�{3D�θD��D�?
D���D��D��\D�FfD�~fD���D�D�H D�s3D��\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���                        =���>L��        >���>���            =���        >���=���    =���                    =���        =���        =���>L��                        =���>���>L��        =���                        =���>L��            =���                    =���=���    =���>���>L��                >L��    >L��>L��        >L��>L��        =���=���=���        =���>L��        =���            =���>���=���        =���        =���=���            =���>L��            =���>L��>L��        =���>L��>L��                            =���=���        =���>L��        >L��=���                >L��=���    =���>L��>L��        =���>���>���        =���=���                =���=���        =���>L��>L��            =���=���                >���>���>L��    =���=���=���        =���>L��>L��>L��        =���>L��=���    =���=���=���    >L��>L��=���        >L��=���=���=���=���>L��=���=���=���>L��=���=���=���=���>���=���>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>���>L��>���>���>���>���>���>���>���>L��>L��>L��>���>L��>���>���>���>L��>���>���>���>L��>���>���>L��>���>���>���>���>���>L��=���>L��>���>���=���>���>L��=���>L��=���>���>���>���>���>���>���>���>���>���>���>���>L��>���>L��>L��>L��>���>L��=���>L��>���>L��>L��>���>���>���>L��>L��>L��>���>L��>L��>���>���>���>���>���>���>���=���>���>L��>L��>L��>���>L��>���=���>���>L��>L��>L��=���>L��>���>���>���>���>���>���>L��>L��>L��>���>L��>L��>L��>L��>L��>L��>���>���>���>���>L��>���>���>���>���>���>���>���>L��>L��>���>���>���>���>���>���>L��>���?   >L��>L��>���>L��>L��>���>���>L��>L��>���>���>���>L��>���>L��>���>���>L��>���>���>L��>L��>���>���>���>L��>���>L��>���>���>L��>L��>���>L��>L��>L��>L��>L��>L��>L��>L��>���>L��>L��>L��>L��>���>L��>L��>���>���>���>���=���>L��>L��>���>���>���>���>���>L��>���>���>���>L��>���>���>���=���>���>L��>���>L��>���>���>L��>���>L��>L��>���>���>���>L��>���=���>L��>���=���>L��>���>L��>L��>���>���>L��>���>���>L��>���>L��>���>L��>���>���=���>L��>L��>���>���>L��>L��>���>L��>���>L��=���>���>���>���>���>���>L��>���>���>���>���>L��>L��>L��>���=���>L��>L��>L��>���>���>���?   ?��?333?333?L��?fff?�  ?�  ?�  ?���?�ff?�ff?�ff?�33?�  ?���?���?ٙ�?�ff?�ff?�33@   @ff@ff@��@��@��@��@   @   @&ff@,��@333@333@9��@@  @Fff@L��@S33@Y��@`  @fff@l��@y��@�  @�  @�ff@���@���@�  @�33@���@���@���@�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@ə�@���@�  @�33@�ff@���@�  @�33@�ff@陚@���@�  @�33@���@���A   A33A33A��A  A	��A33A��AffA  A��A��A��A  A��A33A��AffA   A!��A#33A$��A(  A)��A+33A,��A.ffA0  A1��A333A4��A6ffA8  A;33A;33A>ffA@  AA��AC33AD��AFffAH  AI��AL��ANffAP  AQ��AS33AT��AX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���A�ffA�33A�  A͙�A�ffA�33A���Aљ�A�ffA�  A���Aՙ�A�33A�  A���A�ffA�33A�  Aݙ�A�ffA�33Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Dr��Ds  DsfDs3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dsy�Ds� Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt�3Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt��Du  DufDu�@Fff@L��@S33@Y��@`  @fff@l��@y��@�  @�  @�ff@���@���@�  @�33@���@���@���@�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@ə�@���@�  @�33@�ff@���@�  @�33@�ff@陚@���@�  @�33@���@���A   A33A33A��A  A	��A33A��AffA  A��A��A��A  A��A33A��AffA   A!��A#33A$��A(  A)��A+33A,��A.ffA0  A1��A333A4��A6ffA8  A;33A;33A>ffA@  AA��AC33AD��AFffAH  AI��AL��ANffAP  AQ��AS33AT��AX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���A�ffA�33A�  A͙�A�ffA�33A���Aљ�A�ffA�  A���Aՙ�A�33A�  A���A�ffA�33A�  Aݙ�A�ffA�33Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Dr��Ds  DsfDs3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dsy�Ds� Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt�3Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt��Du  DufDu�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@~�R@�\)@�\)A�A?�A_�A�A��
A��
A�
=A�
=A��
A��A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��DqxRDq��Dr~�Dr��Ds~�Ds��Dt~�Dt�Dy�3D�D�>fD�� D��RD��D�?
D�z�D��D���D�>fD���D��pD���D�E�D�}�D��RD�{D�G\D�r�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=��
���
���
���
���
���
���
=��
>8Q켣�
���
>�\)>�\)���
���
���
=��
���
���
>�\)=��
���
=��
���
���
���
���
���
=��
���
���
=��
���
���
=��
>8Q켣�
���
���
���
���
���
=��
>�\)>8Q켣�
���
=��
���
���
���
���
���
���
=��
>8Q켣�
���
���
=��
���
���
���
���
���
=��
=��
���
=��
>�\)>8Q켣�
���
���
���
>8Q켣�
>8Q�>8Q켣�
���
>8Q�>8Q켣�
���
=��
=��
=��
���
���
=��
>8Q켣�
���
=��
���
���
���
=��
>�\)=��
���
���
=��
���
���
=��
=��
���
���
���
=��
>8Q켣�
���
���
=��
>8Q�>8Q켣�
���
=��
>8Q�>8Q켣�
���
���
���
���
���
���
=��
=��
���
���
=��
>8Q켣�
���
>8Q�=��
���
���
���
���
>8Q�=��
���
=��
>8Q�>8Q켣�
���
=��
>�\)>�\)���
���
=��
=��
���
���
���
���
=��
=��
���
���
=��
>8Q�>8Q켣�
���
���
=��
=��
���
���
���
���
>�\)>�\)>8Q켣�
=��
=��
=��
���
���
=��
>8Q�>8Q�>8Q켣�
���
=��
>8Q�=��
���
=��
=��
=��
���
>8Q�>8Q�=��
���
���
>8Q�=��
=��
=��
=��
>8Q�=��
=��
=��
>8Q�=��
=��
=��
=��
>�\)=��
>8Q�=��
>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>\>�\)>�\)>�\)>�\)>�\)>�\)>8Q�>8Q�>8Q�>�\)>8Q�>�\)>�\)>�\)>8Q�>\>�\)>�\)>8Q�>�\)>�\)>8Q�>�\)>�\)>�\)>�\)>�\)>8Q�=��
>8Q�>�\)>�\)=��
>�\)>8Q�=��
>8Q�=��
>�\)>�\)>�\)>�\)>�\)>�\)>�\)>\>\>\>�\)>8Q�>�\)>8Q�>8Q�>8Q�>�\)>8Q�=��
>8Q�>\>8Q�>8Q�>\>�\)>\>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>�\)>�\)>�\)>�\)>�\)>�\)=��
>�\)>8Q�>8Q�>8Q�>�\)>8Q�>�\)=��
>�\)>8Q�>8Q�>8Q�=��
>8Q�>�\)>�\)>�\)>�\)>\>�\)>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>�\)>\>�\)>8Q�>�\)>�\)>�\)>\>�\)>�\)>�\)>8Q�>8Q�>�\)>�\)>�\)>\>�\)>\>8Q�>�\)>�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>�\)>8Q�>8Q�>�\)>\>�\)>8Q�>�\)>8Q�>�\)>�\)>8Q�>�\)>�\)>8Q�>8Q�>�\)>�\)>�\)>8Q�>�\)>8Q�>�\)>�\)>8Q�>8Q�>�\)>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>8Q�>8Q�>\>8Q�>8Q�>�\)>�\)>�\)>�\)=��
>8Q�>8Q�>�\)>�\)>�\)>�\)>�\)>8Q�>�\)>\>\>8Q�>�\)>\>\=��
>�\)>8Q�>�\)>8Q�>\>�\)>8Q�>�\)>8Q�>8Q�>�\)>�\)>�\)>8Q�>�\)=��
>8Q�>�\)=��
>8Q�>�\)>8Q�>8Q�>\>�\)>8Q�>\>�\)>8Q�>�\)>8Q�>�\)>8Q�>�\)>�\)=��
>8Q�>8Q�>�\)>�\)>8Q�>8Q�>�\)>8Q�>�\)>8Q�=��
>�\)>�\)>�\)>�\)>\>8Q�>\>\>�\)>\>8Q�>8Q�>8Q�>�\)=��
>8Q�>8Q�>8Q�>\>\>\>�?z�?.{?.{?G�?aG�?z�H?z�H?z�H?�
>?��
?��
?��
?���?�p�?�=q?�=q?�
>?��
?��
?��?�p�@�@�@�@�@Q�@Q�@�R@�R@%�@+�@1�@1�@8Q�@>�R@E�@K�@Q�@XQ�@^�R@e�@k�@xQ�@~�R@~�R@�@���@�(�@�\)@��\@���@�(�@�(�@��\@�@���@�(�@�\)@��\@�@���@�(�@\@�@���@�(�@�\)@ҏ\@�@�(�@�\)@�\@�@���@�(�@�\)@�\@���@�(�@�\)A�GA�GAz�A�A	G�A
�GAz�AzA�AG�Az�Az�A�AG�A�GAz�AzA�A!G�A"�GA$z�A'�A)G�A*�GA,z�A.zA/�A1G�A2�GA4z�A6zA7�A:�GA:�GA>zA?�AAG�AB�GADz�AFzAG�AIG�ALz�ANzAO�AQG�AR�GATz�AW�AYG�AZ�GA\z�A_�AaG�Ab�GAdz�Ag�AiG�Aj�GAlz�Ao�AqG�Ar�GAvzAw�AyG�Az�GA~zA�A�p�A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
A���A�p�A�
=A��
A���A�p�A�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
Aģ�A�p�A�
=A��
Aȣ�A�=pA�
=A��
A�p�A�=pA�
=AУ�A�p�A�=pA��
Aԣ�A�p�A�
=A��
Aأ�A�=pA�
=A��
A�p�A�=pA�
=Dq^�DqeDqk�Dqq�Dq~�Dq�Dq��Dq��Dq�RDq�Dq��Dq��Dq�RDq�Dq˅Dq��Dq�RDq�Dq�Dq��Dq�RDrDr�Dr�DrRDr�Dr+�Dr1�Dr8RDr>�DrK�DrQ�DrXRDr^�Drk�Drq�DrxRDr~�Dr��Dr��Dr�RDr��Dr��Dr��Dr�RDr��Dr�Dr��Dr�RDr޸Dr�Dr��Dr�RDr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsEDsK�DsXRDs^�DseDsk�DsxRDs~�Ds�Ds��Ds��Ds��Ds�Ds��Ds��Ds�RDs�Ds˅Ds��Ds�RDs�Ds�Ds��Ds�RDtDt�Dt�DtRDt%Dt+�Dt1�Dt>�DtEDtK�DtQ�Dt^�DteDtk�Dtq�Dt~�Dt�Dt��Dt�RDt��Dt�Dt��Dt��Dt��Dt�Dt˅Dt�RDt޸Dt�Dt�Dt�RDt��DuDu�@E�@K�@Q�@XQ�@^�R@e�@k�@xQ�@~�R@~�R@�@���@�(�@�\)@��\@���@�(�@�(�@��\@�@���@�(�@�\)@��\@�@���@�(�@\@�@���@�(�@�\)@ҏ\@�@�(�@�\)@�\@�@���@�(�@�\)@�\@���@�(�@�\)A�GA�GAz�A�A	G�A
�GAz�AzA�AG�Az�Az�A�AG�A�GAz�AzA�A!G�A"�GA$z�A'�A)G�A*�GA,z�A.zA/�A1G�A2�GA4z�A6zA7�A:�GA:�GA>zA?�AAG�AB�GADz�AFzAG�AIG�ALz�ANzAO�AQG�AR�GATz�AW�AYG�AZ�GA\z�A_�AaG�Ab�GAdz�Ag�AiG�Aj�GAlz�Ao�AqG�Ar�GAvzAw�AyG�Az�GA~zA�A�p�A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
A���A�p�A�
=A��
A���A�p�A�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�=pA��
Aģ�A�p�A�
=A��
Aȣ�A�=pA�
=A��
A�p�A�=pA�
=AУ�A�p�A�=pA��
Aԣ�A�p�A�
=A��
Aأ�A�=pA�
=A��
A�p�A�=pA�
=Dq^�DqeDqk�Dqq�Dq~�Dq�Dq��Dq��Dq�RDq�Dq��Dq��Dq�RDq�Dq˅Dq��Dq�RDq�Dq�Dq��Dq�RDrDr�Dr�DrRDr�Dr+�Dr1�Dr8RDr>�DrK�DrQ�DrXRDr^�Drk�Drq�DrxRDr~�Dr��Dr��Dr�RDr��Dr��Dr��Dr�RDr��Dr�Dr��Dr�RDr޸Dr�Dr��Dr�RDr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsEDsK�DsXRDs^�DseDsk�DsxRDs~�Ds�Ds��Ds��Ds��Ds�Ds��Ds��Ds�RDs�Ds˅Ds��Ds�RDs�Ds�Ds��Ds�RDtDt�Dt�DtRDt%Dt+�Dt1�Dt>�DtEDtK�DtQ�Dt^�DteDtk�Dtq�Dt~�Dt�Dt��Dt�RDt��Dt�Dt��Dt��Dt��Dt�Dt˅Dt�RDt޸Dt�Dt�Dt�RDt��DuDu�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aɡ�Aɟ�Aɣ�Aɣ�AɑhAɍPA�S�A�E�A�;dA�/A�1'A�-A�&�A�"�A�"�A��A��A�{A�bA��AȬA��A�hsA���A�%Aţ�AŋDA�Q�A���Aė�A�z�A��TA�$�A�oA��;A�~�A�A��#Aú^AÓuA�9XA�VA���A���A¶FA7A�n�A�O�A�(�A�VA��A��
A���A��A�l�A�XA�I�A�C�A�7LA��mA�p�A�%A�z�A�9XA��^A�jA���A�ffA�{A�^5A���A���A���A�r�A��A��uA�dZA�;dA���A���A��A���A���A�
=A��^A��A�S�A��A��A��RA��TA�oA��/A�;dA�1'A�&�A�p�A��wA�O�A�ffA�/A���A���A��RA��A���A�VA���A�bNA�33A��FA��#A�33A��yA���A���A�r�A��DA�$�A�1'A�JA��A�VA���A�K�A�\)A���A��A��A�M�A���A�1A��yA�v�A�~�A�VA~=qA|��A|$�Ay��AwoAu��As�hAqXAohsAm�#AmG�AkS�AiAg�AfQ�Ac�A_��A]A]7LA\�AZ��AX��AXjAW%AV �ATAQ�AO�PAN�!AN  AMO�AKƨAJ�`AJn�AIG�AG��AEoAChsABn�A@�jA=�7A<n�A<A�A<��A;��A:-A9��A7��A4z�A3�mA333A2�!A2I�A1��A1G�A0�A/ƨA.�+A-�FA,�\A+hsA*9XA(A&�A%��A%O�A$��A#��A"r�A!�wA Q�Ap�A��A-A33A1'A�+A33A�AƨA|�AAE�A�wAK�A��A-A��AVA�A5?A7LA
��A	��A	%A�A��A\)A&�A�AK�A��A�uA-A|�A   @�v�@��9@��@�V@�?}@�j@��@�=q@�/@���@�j@���@��@���@��@��@�^5@�;d@ꗍ@땁@�w@��#@�\)@�J@�z�@���@߾w@ݑh@�z�@��H@�  @�?}@�1'@���@ԋD@�  @�o@мj@Ь@ѡ�@�?}@��#@��/@��@���@̣�@�dZ@���@Ȭ@�Q�@�\)@�hs@��@�v�@�/@���@���@��D@��@��D@���@�Ĝ@��@��/@�Z@���@�l�@���@���@�\)@���@��@�hs@�V@�7L@�@���@�A�@�1'@�b@��;@�|�@��@�1'@�t�@�S�@��T@��h@��@���@���@�Ĝ@���@�Z@��P@��!@�=q@�p�@�x�@�@�(�@�33@���@���@�p�@�K�@��w@���@�E�@��#@��@���@�/@��@���@�E�@���@�Z@��@���@�G�@�%@��@�9X@��
@���@�|�@�"�@�S�@�33@�=q@�-@�ȴ@��y@��y@���@��\@�E�@�-@�E�@���@��j@��D@�bN@�A�@�9X@�I�@�j@�Z@�Z@�r�@�z�@�9X@��@��w@��@�|�@�K�@�;d@�33@�C�@�C�@�K�@�l�@���@�|�@�C�@�+@�o@��y@��@��R@��+@�E�@��@���@��#@��7@�V@�Ĝ@��j@��9@���@�j@�A�@�b@��@��m@���@��P@��@���@��\@�ff@�^5@�^5@�-@�=q@���@�x�@�X@�&�@��j@���@�z�@�j@�bN@�Z@�I�@�1@���@�;d@�o@���@���@��+@�^5@�5?@�J@���@��@���@���@��h@�hs@�O�@�7L@�%@�Ĝ@���@�j@�I�@� �@��@���@��w@���@��P@�;d@���@���@�V@�E�@�5?@�-@���@��7@�?�@�x@s�K@j�'@d]d@^�!@Wn/@N?@Gn/@?]�@8�5@2
�@-Y�@)@%o @ ��@C�@�!@w2@�|@z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��+A�jA�dZA���A�n�A�7LA��A�&�A�VA�t�AƋDA�A�5?A�5?A�
=A��RA�(�A���Aɕ�A�dZA��A�XA���A�=qA�
=A���A�=qA�-A�ZA��+A���AǁA���A���A�K�A�bA�$�A���A�p�A�XA��A��DA��Aɴ9A���A���A��#A�jA�%A�p�A�33A�bNA��;A�=qA�x�A��/A���A��hA�(�A�1A���A�;dA�A��7A�JA�dZA�~�A�bAɺ^A�z�A�9XA��hA��RA�1'A��DA��A���AɾwA��A��
A�x�AɬA�7LA���A�$�A�dZA�33A���A��^A�K�Aɲ-A��mA�VA�x�A�x�A���A�1'APA�VA�33A��A�S�A��DA��A�|�A��A�E�A�t�A��A���A���A��TA���A��wA��FA��Aɟ�A�|�A�bNA��TA�;dA��Aɩ�A��PA��9A��A��FA�"�A�t�A��uA���A��mA���A��A��mA�K�AōPA�l�A�ZA�1'A���A��wA��PA�p�A�;dAǧ�A�7LA���Aǩ�AɸRA��`A��A�/AɾwAɺ^A�
=A���Aȴ9A�^5A�bNA��\A�9XA���A���A��#A��A�33A�n�A�p�A�VAȴ9A�dZA�v�A�bNA��FA�
=A��yA�ZA�ĜA�K�A�A�ȴA���A���A��A��!A�-A���A���A�A�dZA��A��hA���A���A��A�?}A��^A�  A���A�A���Aĉ7A��HAǺ^A�+A�ȴA��
AɸRA���A��yA�C�A�?}AɸRA���A�%A��yA\A��7A���A��
A�+AɶFA��
A²-A��#A���A���Aɥ�AɶFA�^5A���A��A���A�/Aȏ\A���A��
A���A���A�G�A���A�ȴA���A��A�ƨAɶFA��A���A���A���A���A��A���A���A��A���A���A� �A�ƨAɑhA���A���A���A���A�ȴA�A�A���AɅA���A��TAɗ�A���A�p�A���A�;dAȇ+A���A���A��
A���Aȩ�A���A��A���A���A���A�K�AǼjA���A���A���A�x�A���A�~�A�XA���A���AǸRA���A���A���A�O�A��`AƟ�A��A���A�Aȏ\A���Aƙ�A���A���A���A���A�33A�%A���AąA�=qA�ĜAȝ�A���A�oA�S�A���A�hsA���A��mA�oA�v�A���A��/A��HA��HA��
A���A��TA�z�A��#A��/A��HA��`A��TA�t�A�z�A��;A��TA��;A��
Aƥ�A��mA��TA��TA��A��;A��;A��`A��#A�1A��TA��`A��A��#A��TA��HA��#A�ĜA��;A��#AĮA�z�A��HA�oA��mA��;Aɲ-Aɴ9A�VA��A��A�hsA���A��HA�|�A��A��;A��
A��#A��#A�C�A��#A���A��A�  A�ȴA��A��`A��;A��A×�Aɡ�A��HA���A��HAƺ^A�t�A��/A�O�A��yA��`A��A�oA���A�~�A��
A��/A���A���A���A�O�A���A�ȴAɡ�A�Q�A���AȰ!A��TA��/A���A��#A��#A���A�A�ȴA���A��#A���A�-A�=qA�x�A���A�ȴA���A�ĜA��A���A���A�C�A�ĜA���A�33A���A���Aə�A��A���A�ĜAƝ�A���A���Aɉ7A���A���A�ĜA���A���Aɏ\A��#A���AŶFA��
A�ĜA���A�O�AɑhA���A���A���A��
A��A��#A�x�A��Aĺ^Aƛ�Aə�A��A��;A��HA��A�ĜAȶFA��
A��/A��;A��HA�ƨA��;A���AɾwAǼjA�%AɓuA���A��HA��;A��TA��
A��#A��/A��A���AɸRAɼjA��
A���Aɲ-AɮAɴ9Aɲ-AɬAɥ�Aɩ�Aɧ�Aɩ�Aɰ!AɬAɩ�Aɧ�AɮAɶFAɰ!AɬAɴ9AɶFAɶFAɧ�Aɰ!Aɰ!AɮAɴ9Aɲ-Aɲ-Aɴ9Aɲ-AɬAɮAɮAɲ-Aɴ9AɶFAɴ9AɬAɬAɧ�AɬAɧ�AɮAɩ�Aɲ-Aɩ�Aɲ-AɬAɩ�AɮAɬAɩ�AɮAɩ�AɮAɮAɰ!AɶFAɲ-Aɴ9Aɲ-Aɲ-Aɰ!Aɲ-Aɴ9Aɴ9Aɰ!Aɴ9Aɴ9Aɴ9Aɴ9Aɰ!Aɰ!AɶFAɴ9Aɴ9Aɺ^AɶFAɸRAɶFAɴ9Aɲ-Aɲ-AɮAɰ!AɶFAɺ^Aɩ�Aɴ9AɸRAɶFAɸRAɸRAɶFAɸRAɸRAɶFAɰ!Aɰ!AɮAɣ�Aɝ�Aɛ�Aɗ�Aɛ�Aɛ�Aɗ�Aɝ�Aɥ�Aə�Aɕ�Aɗ�Aɩ�Aɥ�Aɥ�Aɣ�Aɣ�Aɥ�Aɣ�AɬAɡ�Aɝ�Aɟ�Aɟ�Aɕ�Aɕ�Aɕ�Aɛ�Aɛ�Aɗ�AɋDAɏ\AɓuAɉ7A�jA�n�A�n�A�hsA�ffA�bNA�\)A�XA�ZA�XA�VA�VA�XA�ZA�VA�VA�Q�A�Q�A�S�A�Q�A�S�A�Q�A�G�A�I�A�G�A�K�A�Q�A�O�A�E�A�G�A�C�A�E�A�E�A�C�A�G�A�G�A�E�A�E�A�C�A�I�A�I�A�I�A�E�A�E�A�G�A�=qA�5?A�9XA�7LA�5?A�9XA�7LA�7LA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�9XA�7LA�7LA�7LA�;dA�7LA�7LA�7LA�7LA�5?A�7LA�5?A�5?A�7LA�5?A�5?A�7LA�7LA�7LA�7LA�7LA�7LA�5?A�/A�1'A�33A�/A�1'A�1'A�/A�1'A�/A�-A�-A�-A�-A�-A�-@��@��@���@�ȴ@�ȴ@�ȴ@��!@��R@���@���@��!@��!@��!@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��+@�~�@�~�@�v�@�v�@�v�@�ff@�^5@�V@�V@�V@�V@�M�@�M�@�E�@�M�@�M�@�M�@�M�@�M�@�E�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�E�@�=q@�=q@�E�@�=q@�=q@�=q@�E�@�=q@�5?@�5?@�5?@�5?@�5?@�=q@�=q@�5?@�5?@�5?@�=q@�=q@�5?@�-@�-@�5?@�-@�5?@�5?@�5?@�$�@�$�@�-@�-@�-@�$�@�$�@�$�@�$�@�$�@�$�@��@�$�@��@��@�{@�J@��@��@�@��T@��@��#@���@�@���@��-@���@���@��h@��@��@��7@��7@��@��7Aɡ�Aɥ�Aɣ�Aɥ�Aɥ�Aɣ�Aɣ�Aɧ�Aɣ�Aɡ�Aɣ�Aɝ�Aɝ�Aɟ�Aɡ�Aɝ�Aɝ�Aɟ�Aɟ�Aɟ�Aɝ�Aɝ�Aɛ�Aɛ�Aɟ�Aɡ�Aɟ�Aɡ�Aɣ�Aɡ�Aɣ�Aɣ�Aɣ�Aɡ�Aɣ�Aɡ�Aɡ�Aɣ�Aɟ�Aɡ�Aɡ�Aɣ�Aɡ�Aɣ�Aɥ�Aɥ�Aɥ�Aɧ�Aɥ�Aɥ�Aɧ�Aɧ�Aɡ�Aɥ�Aɣ�Aɟ�Aɝ�Aɛ�Aɝ�Aɝ�Aɥ�Aɩ�Aɧ�Aɥ�Aɥ�Aɧ�Aɩ�Aɧ�Aɥ�Aɥ�Aɛ�Aə�AɍPAɉ7Aɇ+Aɉ7AɋDAɏ\AɍPAɋDAɅAɉ7AɋDAɕ�Aə�Aɛ�Aə�Aɟ�Aɟ�Aɗ�Aɝ�Aə�Aɕ�AɓuAɕ�Aɏ\Aɏ\Aɉ7Aɇ+Aɏ\Aɇ+AɋDAɁAɇ+Aɇ+A�t�A�`BA�bNA�`BA�\)A�\)A�S�A�M�A�M�A�M�A�K�A�K�A�K�A�O�A�M�A�I�A�K�A�I�A�I�A�I�A�I�A�G�A�G�A�E�A�?}A�=qA�?}A�G�A�A�A�A�A�;dA�;dA�;dA�=qA�=qA�=qA�=qA�;dA�;dA�=qA�;dA�=qA�?}A�=qA�;dA�5?A�1'A�-A�-A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�33A�/A�/A�/A�/A�/A�1'A�1'A�/A�/A�1'A�/A�1'A�/A�/A�+A�(�A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�$�A�&�A�&�A�&�A�&�A�(�@��H@��@�ȴ@�ȴ@�ȴ@���@��!@��!@���@��!@��!@��!@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��+@��+@�~�@�~�@�v�@�n�@�n�@�V@�V@�V@�V@�M�@�M�@�M�@�E�@�M�@�M�@�M�@�M�@�M�@�E�@�M�@�E�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�E�@�=q@�E�@�E�@�E�@�=q@�=q@�=q@�5?@�-@�-@�5?@�=q@�=q@�5?@�5?@�5?@�5?@�5?@�-@�-@�-@�-@�-@�5?@�5?@�5?@�5?@�-@�$�@�-@�-@�$�@�$�@�$�@�-@�-@�$�@��@�$�@��@�$�@��@�J@�{@��@�J@��@��T@��@���@�@��^@���@���@���@��7@��7@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999Aɡ�Aɟ�Aɣ�Aɣ�AɑhAɍPA�S�A�E�A�;dA�/A�1'A�-A�&�A�"�A�"�A��A��A�{A�bA��AȬA��A�hsA���A�%Aţ�AŋDA�Q�A���Aė�A�z�A��TA�$�A�oA��;A�~�A�A��#Aú^AÓuA�9XA�VA���A���A¶FA7A�n�A�O�A�(�A�VA��A��
A���A��A�l�A�XA�I�A�C�A�7LA��mA�p�A�%A�z�A�9XA��^A�jA���A�ffA�{A�^5A���A���A���A�r�A��A��uA�dZA�;dA���A���A��A���A���A�
=A��^A��A�S�A��A��A��RA��TA�oA��/A�;dA�1'A�&�A�p�A��wA�O�A�ffA�/A���A���A��RA��A���A�VA���A�bNA�33A��FA��#A�33A��yA���A���A�r�A��DA�$�A�1'A�JA��A�VA���A�K�A�\)A���A��A��A�M�A���A�1A��yA�v�A�~�A�VA~=qA|��A|$�Ay��AwoAu��As�hAqXAohsAm�#AmG�AkS�AiAg�AfQ�Ac�A_��A]A]7LA\�AZ��AX��AXjAW%AV �ATAQ�AO�PAN�!AN  AMO�AKƨAJ�`AJn�AIG�AG��AEoAChsABn�A@�jA=�7A<n�A<A�A<��A;��A:-A9��A7��A4z�A3�mA333A2�!A2I�A1��A1G�A0�A/ƨA.�+A-�FA,�\A+hsA*9XA(A&�A%��A%O�A$��A#��A"r�A!�wA Q�Ap�A��A-A33A1'A�+A33A�AƨA|�AAE�A�wAK�A��A-A��AVA�A5?A7LA
��A	��A	%A�A��A\)A&�A�AK�A��A�uA-A|�A   @�v�@��9@��@�V@�?}@�j@��@�=q@�/@���@�j@���@��@���@��@��@�^5@�;d@ꗍ@땁@�w@��#@�\)@�J@�z�@���@߾w@ݑh@�z�@��H@�  @�?}@�1'@���@ԋD@�  @�o@мj@Ь@ѡ�@�?}@��#@��/@��@���@̣�@�dZ@���@Ȭ@�Q�@�\)@�hs@��@�v�@�/@���@���@��D@��@��D@���@�Ĝ@��@��/@�Z@���@�l�@���@���@�\)@���@��@�hs@�V@�7L@�@���@�A�@�1'@�b@��;@�|�@��@�1'@�t�@�S�@��T@��h@��@���@���@�Ĝ@���@�Z@��P@��!@�=q@�p�@�x�@�@�(�@�33@���@���@�p�@�K�@��w@���@�E�@��#@��@���@�/@��@���@�E�@���@�Z@��@���@�G�@�%@��@�9X@��
@���@�|�@�"�@�S�@�33@�=q@�-@�ȴ@��y@��y@���@��\@�E�@�-@�E�@���@��j@��D@�bN@�A�@�9X@�I�@�j@�Z@�Z@�r�@�z�@�9X@��@��w@��@�|�@�K�@�;d@�33@�C�@�C�@�K�@�l�@���@�|�@�C�@�+@�o@��y@��@��R@��+@�E�@��@���@��#@��7@�V@�Ĝ@��j@��9@���@�j@�A�@�b@��@��m@���@��P@��@���@��\@�ff@�^5@�^5@�-@�=q@���@�x�@�X@�&�@��j@���@�z�@�j@�bN@�Z@�I�@�1@���@�;d@�o@���@���@��+@�^5@�5?@�J@���@��@���@���@��h@�hs@�O�@�7L@�%@�Ĝ@���@�j@�I�@� �@��@���@��w@���@��P@�;d@���@���@�V@�E�@�5?@�-@���G�O�@�?�@�x@s�K@j�'@d]d@^�!@Wn/@N?@Gn/@?]�@8�5@2
�@-Y�@)@%o @ ��@C�@�!@w2@�|@z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��+A�jA�dZA���A�n�A�7LA��A�&�A�VA�t�AƋDA�A�5?A�5?A�
=A��RA�(�A���Aɕ�A�dZA��A�XA���A�=qA�
=A���A�=qA�-A�ZA��+A���AǁA���A���A�K�A�bA�$�A���A�p�A�XA��A��DA��Aɴ9A���A���A��#A�jA�%A�p�A�33A�bNA��;A�=qA�x�A��/A���A��hA�(�A�1A���A�;dA�A��7A�JA�dZA�~�A�bAɺ^A�z�A�9XA��hA��RA�1'A��DA��A���AɾwA��A��
A�x�AɬA�7LA���A�$�A�dZA�33A���A��^A�K�Aɲ-A��mA�VA�x�A�x�A���A�1'APA�VA�33A��A�S�A��DA��A�|�A��A�E�A�t�A��A���A���A��TA���A��wA��FA��Aɟ�A�|�A�bNA��TA�;dA��Aɩ�A��PA��9A��A��FA�"�A�t�A��uA���A��mA���A��A��mA�K�AōPA�l�A�ZA�1'A���A��wA��PA�p�A�;dAǧ�A�7LA���Aǩ�AɸRA��`A��A�/AɾwAɺ^A�
=A���Aȴ9A�^5A�bNA��\A�9XA���A���A��#A��A�33A�n�A�p�A�VAȴ9A�dZA�v�A�bNA��FA�
=A��yA�ZA�ĜA�K�A�A�ȴA���A���A��A��!A�-A���A���A�A�dZA��A��hA���A���A��A�?}A��^A�  A���A�A���Aĉ7A��HAǺ^A�+A�ȴA��
AɸRA���A��yA�C�A�?}AɸRA���A�%A��yA\A��7A���A��
A�+AɶFA��
A²-A��#A���A���Aɥ�AɶFA�^5A���A��A���A�/Aȏ\A���A��
A���A���A�G�A���A�ȴA���A��A�ƨAɶFA��A���A���A���A���A��A���A���A��A���A���A� �A�ƨAɑhA���A���A���A���A�ȴA�A�A���AɅA���A��TAɗ�A���A�p�A���A�;dAȇ+A���A���A��
A���Aȩ�A���A��A���A���A���A�K�AǼjA���A���A���A�x�A���A�~�A�XA���A���AǸRA���A���A���A�O�A��`AƟ�A��A���A�Aȏ\A���Aƙ�A���A���A���A���A�33A�%A���AąA�=qA�ĜAȝ�A���A�oA�S�A���A�hsA���A��mA�oA�v�A���A��/A��HA��HA��
A���A��TA�z�A��#A��/A��HA��`A��TA�t�A�z�A��;A��TA��;A��
Aƥ�A��mA��TA��TA��A��;A��;A��`A��#A�1A��TA��`A��A��#A��TA��HA��#A�ĜA��;A��#AĮA�z�A��HA�oA��mA��;Aɲ-Aɴ9A�VA��A��A�hsA���A��HA�|�A��A��;A��
A��#A��#A�C�A��#A���A��A�  A�ȴA��A��`A��;A��A×�Aɡ�A��HA���A��HAƺ^A�t�A��/A�O�A��yA��`A��A�oA���A�~�A��
A��/A���A���A���A�O�A���A�ȴAɡ�A�Q�A���AȰ!A��TA��/A���A��#A��#A���A�A�ȴA���A��#A���A�-A�=qA�x�A���A�ȴA���A�ĜA��A���A���A�C�A�ĜA���A�33A���A���Aə�A��A���A�ĜAƝ�A���A���Aɉ7A���A���A�ĜA���A���Aɏ\A��#A���AŶFA��
A�ĜA���A�O�AɑhA���A���A���A��
A��A��#A�x�A��Aĺ^Aƛ�Aə�A��A��;A��HA��A�ĜAȶFA��
A��/A��;A��HA�ƨA��;A���AɾwAǼjA�%AɓuA���A��HA��;A��TA��
A��#A��/A��A���AɸRAɼjA��
A���Aɲ-AɮAɴ9Aɲ-AɬAɥ�Aɩ�Aɧ�Aɩ�Aɰ!AɬAɩ�Aɧ�AɮAɶFAɰ!AɬAɴ9AɶFAɶFAɧ�Aɰ!Aɰ!AɮAɴ9Aɲ-Aɲ-Aɡ�Aɥ�Aɣ�Aɥ�Aɥ�Aɣ�Aɣ�Aɧ�Aɣ�Aɡ�Aɣ�Aɝ�Aɝ�Aɟ�Aɡ�Aɝ�Aɝ�Aɟ�Aɟ�Aɟ�Aɝ�Aɝ�Aɛ�Aɛ�Aɟ�Aɡ�Aɟ�Aɡ�Aɣ�Aɡ�Aɣ�Aɣ�Aɣ�Aɡ�Aɣ�Aɡ�Aɡ�Aɣ�Aɟ�Aɡ�Aɡ�Aɣ�Aɡ�Aɣ�Aɥ�Aɥ�Aɥ�Aɧ�Aɥ�Aɥ�Aɧ�Aɧ�Aɡ�Aɥ�Aɣ�Aɟ�Aɝ�Aɛ�Aɝ�Aɝ�Aɥ�Aɩ�Aɧ�Aɥ�Aɥ�Aɧ�Aɩ�Aɧ�Aɥ�Aɥ�Aɛ�Aə�AɍPAɉ7Aɇ+Aɉ7AɋDAɏ\AɍPAɋDAɅAɉ7AɋDAɕ�Aə�Aɛ�Aə�Aɟ�Aɟ�Aɗ�Aɝ�Aə�Aɕ�AɓuAɕ�Aɏ\Aɏ\Aɉ7Aɇ+Aɏ\Aɇ+AɋDAɁAɇ+Aɇ+A�t�A�`BA�bNA�`BA�\)A�\)A�S�A�M�A�M�A�M�A�K�A�K�A�K�A�O�A�M�A�I�A�K�A�I�A�I�A�I�A�I�A�G�A�G�A�E�A�?}A�=qA�?}A�G�A�A�A�A�A�;dA�;dA�;dA�=qA�=qA�=qA�=qA�;dA�;dA�=qA�;dA�=qA�?}A�=qA�;dA�5?A�1'A�-A�-A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�33A�/A�/A�/A�/A�/A�1'A�1'A�/A�/A�1'A�/A�1'A�/A�/A�+A�(�A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�$�A�&�A�&�A�&�A�&�A�(�@��H@��@�ȴ@�ȴ@�ȴ@���@��!@��!@���@��!@��!@��!@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��+@��+@�~�@�~�@�v�@�n�@�n�@�V@�V@�V@�V@�M�@�M�@�M�@�E�@�M�@�M�@�M�@�M�@�M�@�E�@�M�@�E�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�E�@�=q@�E�@�E�@�E�@�=q@�=q@�=q@�5?@�-@�-@�5?@�=q@�=q@�5?@�5?@�5?@�5?@�5?@�-@�-@�-@�-@�-@�5?@�5?@�5?@�5?@�-@�$�@�-@�-@�$�@�$�@�$�@�-@�-@�$�@��@�$�@��@�$�@��@�J@�{@��@�J@��@��T@��@���@�@��^@���@���@���@��7@��7@��@��@��@��@��@��Aɡ�Aɥ�Aɣ�Aɥ�Aɥ�Aɣ�Aɣ�Aɧ�Aɣ�Aɡ�Aɣ�Aɝ�Aɝ�Aɟ�Aɡ�Aɝ�Aɝ�Aɟ�Aɟ�Aɟ�Aɝ�Aɝ�Aɛ�Aɛ�Aɟ�Aɡ�Aɟ�Aɡ�Aɣ�Aɡ�Aɣ�Aɣ�Aɣ�Aɡ�Aɣ�Aɡ�Aɡ�Aɣ�Aɟ�Aɡ�Aɡ�Aɣ�Aɡ�Aɣ�Aɥ�Aɥ�Aɥ�Aɧ�Aɥ�Aɥ�Aɧ�Aɧ�Aɡ�Aɥ�Aɣ�Aɟ�Aɝ�Aɛ�Aɝ�Aɝ�Aɥ�Aɩ�Aɧ�Aɥ�Aɥ�Aɧ�Aɩ�Aɧ�Aɥ�Aɥ�Aɛ�Aə�AɍPAɉ7Aɇ+Aɉ7AɋDAɏ\AɍPAɋDAɅAɉ7AɋDAɕ�Aə�Aɛ�Aə�Aɟ�Aɟ�Aɗ�Aɝ�Aə�Aɕ�AɓuAɕ�Aɏ\Aɏ\Aɉ7Aɇ+Aɏ\Aɇ+AɋDAɁAɇ+Aɇ+A�t�A�`BA�bNA�`BA�\)A�\)A�S�A�M�A�M�A�M�A�K�A�K�A�K�A�O�A�M�A�I�A�K�A�I�A�I�A�I�A�I�A�G�A�G�A�E�A�?}A�=qA�?}A�G�A�A�A�A�A�;dA�;dA�;dA�=qA�=qA�=qA�=qA�;dA�;dA�=qA�;dA�=qA�?}A�=qA�;dA�5?A�1'A�-A�-A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�33A�/A�/A�/A�/A�/A�1'A�1'A�/A�/A�1'A�/A�1'A�/A�/A�+A�(�A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�$�A�&�A�&�A�&�A�&�A�(�@��H@��@�ȴ@�ȴ@�ȴ@���@��!@��!@���@��!@��!@��!@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��+@��+@�~�@�~�@�v�@�n�@�n�@�V@�V@�V@�V@�M�@�M�@�M�@�E�@�M�@�M�@�M�@�M�@�M�@�E�@�M�@�E�@�M�@�M�@�M�@�E�@�E�@�E�@�E�@�=q@�E�@�=q@�E�@�E�@�E�@�=q@�=q@�=q@�5?@�-@�-@�5?@�=q@�=q@�5?@�5?@�5?@�5?@�5?@�-@�-@�-@�-@�-@�5?@�5?@�5?@�5?@�-@�$�@�-@�-@�$�@�$�@�$�@�-@�-@�$�@��@�$�@��@�$�@��@�J@�{@��@�J@��@��T@��@���@�@��^@���@���@���@��7@��7@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=&k�=2�=Y_�=n��=��j=��=�>�ί=��L=�(9>e�@�|[@�3=�K4=���=�O>�>0r@r�@�")=T!�=UqL=M=d�=�0@=�;%=�s.?C�@=���=���>
�@��s=��h=��>bo�@��u=Q�=b#�=���=��n=�I�>��?�@��=��=�Ɇ>/�C@��=���=��J=�v�=��z=��>��@+�>��	=B�?=X�=~�_>=�=A�=F�l=^N=hH�=�Jw=�%�=��g>ȟ@�v!@��=��0=Qc5=ag#=��'?)`�?K-M>2��@�!�?{��=��>i�-@�T=�=�0�>4��@��_=��=�W?=�\S> �@��=�8�=��o>�_@�J�=�b$=�u>	�?�+,@�3=M�=w�U=���?Z��=��x>�@���=���=�B�=��U=��?`ٔ?뛻=�Ft=��@>�]@�Z�@,��@]�=��=�{�>"}@�_>�,�= {5=-�=(��=,�e=<K
=;�=LD|=��=���=�K�=�Z�?�lv@��>��>ӟ�@�!=M�=� *=��=Ы�>�4�@�Ԫ=��\=�ł>���@��=�e=�C>1��@��@��@��>�?�=��w>��.=FT=>�>=\�=y�#>#D==�x=�g#>g�@b�?Ahs@�P]=O=M��=b��=��E=�v!=���=�5+=�n/>�,@�%1@��f@��=��9=�Cl=�Z2=��P=�Se>	�
?ӌ ?�;y@�sm=��=�m3>s�@G�A@/��=��=�[�=��D?	J�=�D�>aT�@��@w�=�]%=⡶>(K^@�%�=�;=�ӄ>_�?��u@�+=ɥe>	��?��q@B�4>a�?|K
=�s>[5+@�,|>>L�?�c >0:�@�-#@��[@�,�@�*�>S��@�,|?�5@�,�@�-8??>@�,�@�.4@�,@�-�?��m@�*�@�+@�*?+��@�&�@�)�?U2a@�,(@�+@�*Z@�,|@�*�@�+@�)�?T��@�+k@�9�@�+�@�+@��@�*�@�)_@�)_@�,@�+>*�@���@�@�*Z@�+,@k\�@�,?��?.s@�.?~V�@�-�@�-#@�,�@�+k@/��@�+�@�.�@�-�@�-�@�,?#�)?�/0@�.4@�.4@�-#?I�@�,�@!?�C�@�-�@�,�@�'g@�.�@�+�@�*�@�,|@o?
�8@�/�@�/E@�/�?r�@�.4?4��@�-�@�/�@�-�@�-�@�,�@�[@�.�>�n@�-�@�Mj@�b@�-#@�-8>|&W@�-#@�-�@�.4@�R~@M��?�:~@�,(@�&�@�/0@�1Q@�-�@�/E@�2a@�b�@�3r@�2a@�4n@�4n@�4�@�B�@�2�@�2M@�2@�0�@�+k?Se@�4@�3�@�5~@�4@�3@�3�@�3�@�3r>�l�@�5i@�5+@�1Q@�2M@�2@�4@�3�>w�k@�2@�3]?N��?��j@�2�>�cs@�3r@�1�@�2@���@Z$�@�-�@�.�@�1�?,�@�3	?�^J@�/E@�.�@�0@@�.4@�0�@�3]@�.�@�0�@�0�@2��@�1Q@�/E@�4@�3@�0U@.��@��/@�.�@�1Q@�3r@Sm@�Y@�0U>Bb�@�2M@�/�@�.�@��@�/E@vX�@�.�@�-�@�-�@�*@�,�?BXO@�*@���@c�@6�X@�)�@�0@�,(@�,(@�-#@�,|@�,|@�*@�'�@�,@�,|@�,@�+k@�,�?Ŕ�@���@�+@�)_@�+@�)�>Ռ�@�-#@�+@��@��	@�*@��@�,(@�,(@�,(>B;y@�.4@�+�>l%@�,�@�,@�*�@�+�@�+k@�+k@q�@�+�@�,(@�.4@�+>f�Q@�,�@�-�@�,�?���@�xW@�)�@�+k@�0@@�-�@8zc@�0�@Nz:@�/�?�s�?�p@� �@�0�@�1�@�.�@�/0@�.4?�э@�,(@�-�@�0�@�0�@�/�?���@�-�@�-#?�7�>��n@���@�/�@�0U@�3]@�1�@�0�@�1f@�.4@�-�@�$�@�")@�$5@�#�@��@��@�@��@��@��@��@�@�:@�@��@�:@��@��@�i@��@��@�!@��@��@��@� \@��@�!l@��@��@� @��@��@��@� �@� @�!�@� �@�!l@�!�@�!l@�K@�u@��@�&@� @�!@�!-@�"}@�")@�#�@�!l@�"�@�"}@�!l@�!B@�")@��@��@�#:@�">@�$�@�#O@�#�@�#�@�#�@�#�@�#�@�%[@�$5@�#O@�$_@�$5@�%@�$�@�$�@�&@�'@�&l@�&@�&@�&@�&W@�&@�%[@�%�@�$�@�%[@�%�@�%@�&W@�$_@�%�@�&W@�&l@�%�@�&@�&l@�%�@�&@�$J@�#�@�"�@� �@�.@�X@��@�@��@��@�.@� �@�~@�@��@�6@� �@� �@� �@�u@�6@�!@�K@�@�T@��@��@�@�@��@��@�+@��@�+@�@�<@�@�F@�5@��@�5@��@��@�e@� �@� i@���@��@���@���@� @���@���@��3@��]@��3@���@���@���@��@@���@���@���@���@��P@��@��3@��	@��3@��]@���@���@��@��D@��@��@���@��n@���@��@���@���@��@��E@��@���@���@��@��E@���@���@��E@��0@���@��0@���@���@���@���@��+@��@��@��@��@��U@��@���@��@���@���@��U@���@��@���@���@��@���@��@���@��U@��@��I@���@��@���@��@���@��Z@��@���@��I@���@���@��4@���@���@Qw\@Qud@Qt�@Qt�@Qti@Qr�@Qrq@Qr@Qq�@Qq�@Qq�@Qq�@Qq"@Qp�@Qp&@Qo�@Qp�@Qp&@QoT@Qo @QnY@Qn@Qm]@Qm]@Qm	@Ql�@Ql@Qkf@Qj�@Qjj@Qin@Qh�@Qf�@Qf�@Qf{@Qf{@Qf{@Qe�@Qe�@Qe�@Qe�@Qe�@Qe�@Qe�@Qe�@Qe�@Qe�@Qf'@Qe�@Qe�@Qe�@QeV@Qe@Qe@Qe@Qe@Qe@QeV@Qe�@QeV@Qe�@QeV@Qe@Qd�@Qc�@Qd@Qc�@QdZ@Qd�@Qe�@Qd�@Qd�@Qe@Qd�@Qe@Qd�@Qd@Qc�@Qd@Qc�@Qc^@Qd�@Qd�@Qc�@Qb�@Qc@Qb�@Qc@Qc@Qbc@Qbc@Qbc@Qb@Qa�@Qag@Q`�@Q`�@Q`@Q_p@Q^ @Q^t@Q]y@QZ�@QX@QV�@QV@QSz@QQ�@QO�@QN�@QN<@QL�@QJ#@QJ#@QJ#@QJ#@QJw@QJ#@QI�@QI�@��@��@��@��@��@��@��@�*@��@�C@��@��@��@��@��@��@��@��@��@�@��@��@��@��@�H@��@�3@�@� @�?@��@��@��@�i@��@� @�?@��@�?@�@�T@�O@�:@�O@�`@��@�!@��@� @��@� \@� 2@�&@��@��@� @�~@��@��@��@� 2@�!W@� �@� @��@� \@� �@� �@� @��@�C@�C@��@��@��@�@@��@��@��@��@��@��@��@��@��@�\@��@�@�?@��@��@�\@�;@��@��@�n@��@�@�@�@��@�#@��@��@�<@�@�6@�2@��@� �@��@��"@��7@��@���@��@��@��D@��@��@��]@���@��@��3@��r@��r@���@���@��@��@@��0@���@��b@��@���@���@���@��^@���@���@��E@��Z@��E@��^@���@��s@��o@��U@���@���@��M@��@��_@��@��t@��t@���@���@���@��@��E@��@��0@��@���@���@���@��,@���@��k@��,@��,@��A@��k@��@��@���@��o@��,@��@��k@��@���@���@��@��@��k@��@��0@�� @��_@��@��J@��_@�� @��J@��_@��_@��@��c@��N@��c@��x@���@Qn�@Qn@Qk�@Qk<@Qk�@Qj�@Qhs@Qh@Qgw@Qg�@Qh�@Qh�@Qg�@Qg#@Qf�@QfQ@Qg#@Qg#@Qe�@QeV@Qe@QdZ@Qc�@Qd@Qd@Qc�@Qb�@Qb@Qa�@Qa@Q`k@Q_�@Q\�@Q\S@Q[�@Q\S@Q[�@Q[@Q[@QZ�@QZ�@QZ�@Q[-@Q[-@Q[W@QZ�@Q[-@Q[�@Q[�@Q[�@Q[�@QZ�@QZ�@QZ2@QZ�@QZ\@QZ�@QZ�@QZ�@QZ�@QZ�@QZ�@QZ�@QY�@QY�@QX�@QW�@QX�@QZ�@QZ�@QZ@QY�@QZ@QY�@QY�@QY`@QX@QY�@QX�@QX�@QY�@QY�@QZ\@QZ\@QX�@QX�@QX�@QY�@QX�@QX�@QX�@QX�@QX�@QX�@QW�@QW�@QW�@QW�@QWi@QU@QV�@QW�@QUG@QO@QMj@QN�@QJ�@QJ#@QH�@QD�@QE9@QC�@QAJ@Q?�@Q>�@Q>�@Q>�@Q?}@Q?S@Q?)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    44444444444334444433444444444443444344444443444444444444444444444444334444444344434443444434443444434444443444444444344444344444444444443443444443444344433344444444444444344444444433444444444344444444444434443444434444444434443333434334333343334334333333343333333333433333344343333433333443334344333333344333434333334343343343333443333333333333333333433333333433333334334434333333334343333333334333334333333343333333333343334333333333333334333334333333333433433333333333433343333343334433333343333343344333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�|Z@�2G�O�G�O�G�O�G�O�G�O�@r�@�")G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��sG�O�G�O�G�O�@��vG�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�v#@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�!�G�O�G�O�G�O�@�XG�O�G�O�G�O�@��bG�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@�J�G�O�G�O�G�O�G�O�@�4G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Z�G�O�G�O�G�O�G�O�G�O�@�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�@�!G�O�G�O�G�O�G�O�G�O�@�ԮG�O�G�O�G�O�@��G�O�G�O�G�O�@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�P]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�%2@��dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�snG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@w�G�O�G�O�G�O�@�%�G�O�G�O�G�O�G�O�@�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�,zG�O�G�O�G�O�@�-(@��]@�,�@�*�G�O�@�,zG�O�@�,�@�-;G�O�@�,�@�.7@�,@�-�G�O�@�*�@�+@�*G�O�@�&�@�)�G�O�@�,&@�+@�*[@�,x@�*�@�+@�)�G�O�@�+h@�9�@�+}@�+@��@�*�@�)b@�)_@�,@�+G�O�@���@�@�*W@�+(@k\�@�,G�O�G�O�@�.#G�O�@�-�@�-"@�,�@�+lG�O�@�+�@�.�@�-�@�-�@�,G�O�G�O�@�.4@�.6@�-"G�O�@�,�G�O�G�O�@�-�@�,�@�'f@�.�@�+�@�*�@�,}G�O�G�O�@�/�@�/B@�/�G�O�@�.4G�O�@�-�@�/�@�-�@�-�@�,�G�O�@�.�G�O�@�-�@�MlG�O�@�-(@�-8G�O�@�-"@�-�@�.6@�R�G�O�G�O�@�,(@�&�@�/1@�1R@�-�@�/E@�2a@�b�@�3x@�2c@�4n@�4n@�4�@�B�@�2�@�2N@�2@�0�@�+nG�O�@�4@�3�@�5@�4@�3"@�3�@�3�@�3sG�O�@�5i@�5*@�1R@�2N@�2@�4@�3�G�O�@�2@�3]G�O�G�O�@�2�G�O�@�3u@�1�@�2@���@Z$�@�-�@�.�@�1�G�O�@�3G�O�@�/B@�.�@�0>@�.4@�0�@�3_@�.�@�0�@�0�G�O�@�1R@�/B@�4@�3@�0PG�O�@��1@�.�@�1W@�3s@Sh@�Z@�0WG�O�@�2N@�/�@�.�@��
@�/G@vX�@�.�@�-�@�-�@�*@�,�G�O�@�*@���@c�G�O�@�)�@�/�@�,(@�,)@�-!@�,~@�,}@�*@�'�@�,@�,{@�,@�+i@�,�G�O�@���@�+@�)]@�+@�)�G�O�@�-(@�+@��@��
@�*@��@�,(@�,)@�,%G�O�@�.4@�+�G�O�@�,�@�,@�*�@�+�@�+l@�+m@q�@�+�@�,+@�.2@�+G�O�@�,�@�-�@�,�G�O�@�xW@�)�@�+n@�0>@�-�G�O�@�0�@Nz>@�/�G�O�G�O�@� �@�0�@�1�@�.�@�/+@�.4G�O�@�,&@�-�@�0�@�0�@�/�G�O�@�-�@�-$G�O�G�O�@���@�/�@�0Z@�3`@�1�@�0�@�1g@�.6@�-�@�$�@�"(@�$5@�#�@��@��@�@��@��@��@��@�@�7@�@��@�=@��@��@�f@��@��@�&@��@��@��@� Z@��@�!k@��@��@� !@��@��@��@��@��@��@��@��@�)@��@�F@��@��@��@��@��@��@��@��@��@�@��@��@��@��@�D@�@�0@�@�@�?@��@��@��@�j@��@��@�>@��@�>@�@�S@�Q@�:@�Q@�^@��@� @��@� @��@� ^@� 2@�%@��@��@��@�|@��@��@��@� 3@�!Z@� �@� 	@��@� Z@� �@� �@� @��@�B@�?@��@��@��@�@@��@��@��@��@��@��@��@��@��@�]@��@�@�A@��@��@�`@�<@��@��@�o@��@�@��@�@��@�%@��@��@�@@�@�6@�1@��@� �@��@��"@��;@��@���@��@��@��E@��@��~@��_@���@��@��3@��r@��v@���@���@��"@��A@��2@���@��_@���@�� @���@���@��\@���@���@��B@��\@��F@��_@���@��v@��o@��Z@���@���@��N@��@��^@��@��v@��s@���@���@���@��@��B@��@��/@��
@���@���@���@��,@���@��m@��,@��+@��D@��n@��@��@���@��k@��+@��@��j@��@���@���@��@��@��j@��@��2@��#@��`@��@��L@��b@�� @��L@��]@��\@��@��f@��K@��c@��z@���@Qn�@Qn @Qk�@Qk;@Qk�@Qj�@Qhp@Qh@Qg}@Qg�@Qh�@Qh�@Qg�@Qg&@Qf�@QfN@Qg"@Qg"@Qe�@QeS@Qe@Qd[@Qc�@Qd
@Qd@Qc�@Qb�@Qb@Qa�@Qa@Q`k@Q_�@Q\�@Q\P@Q[�@Q\U@Q[�@Q[@Q[@QZ�@QZ�@QZ�@Q[.@Q[+@Q[Z@QZ�@Q[0@Q[�@Q[�@Q[~@Q[�@QZ�@QZ�@QZ0@QZ�@QZ[@QZ�@QZ�@QZ�@QZ�@QZ�@QZ�@QZ�@QY�@QY�@QX�@QW�@QX�@QZ�@QZ�@QZ@QY�@QZ@QY�@QY�@QYb@QX@QY�@QX�@QX�@QY�@QY�@QZ^@QZ]@QX�@QX�@QX�@QY�@QX�@QX�@QX�@QX�@QX�@QX�@QW�@QW�@QW�@QW�@QWj@QU@QV�@QW�@QUB@QO@QMk@QN�@QJ�@QJ#@QH�@QD�@QE;@QC�@QAJ@Q?�@Q? @Q>�@Q>�@Q?�@Q?R@Q?(@��@��@��@��@��@��@��@�)@��@�F@��@��@��@��@��@��@��@��@��@�@��@��@��@��@�D@�@�0@�@�@�?@��@��@��@�j@��@��@�>@��@�>@�@�S@�Q@�:@�Q@�^@��@� @��@� @��@� ^@� 2@�%@��@��@��@�|@��@��@��@� 3@�!Z@� �@� 	@��@� Z@� �@� �@� @��@�B@�?@��@��@��@�@@��@��@��@��@��@��@��@��@��@�]@��@�@�A@��@��@�`@�<@��@��@�o@��@�@��@�@��@�%@��@��@�@@�@�6@�1@��@� �@��@��"@��;@��@���@��@��@��E@��@��~@��_@���@��@��3@��r@��v@���@���@��"@��A@��2@���@��_@���@�� @���@���@��\@���@���@��B@��\@��F@��_@���@��v@��o@��Z@���@���@��N@��@��^@��@��v@��s@���@���@���@��@��B@��@��/@��
@���@���@���@��,@���@��m@��,@��+@��D@��n@��@��@���@��k@��+@��@��j@��@���@���@��@��@��j@��@��2@��#@��`@��@��L@��b@�� @��L@��]@��\@��@��f@��K@��c@��z@���@Qn�@Qn @Qk�@Qk;@Qk�@Qj�@Qhp@Qh@Qg}@Qg�@Qh�@Qh�@Qg�@Qg&@Qf�@QfN@Qg"@Qg"@Qe�@QeS@Qe@Qd[@Qc�@Qd
@Qd@Qc�@Qb�@Qb@Qa�@Qa@Q`k@Q_�@Q\�@Q\P@Q[�@Q\U@Q[�@Q[@Q[@QZ�@QZ�@QZ�@Q[.@Q[+@Q[Z@QZ�@Q[0@Q[�@Q[�@Q[~@Q[�@QZ�@QZ�@QZ0@QZ�@QZ[@QZ�@QZ�@QZ�@QZ�@QZ�@QZ�@QZ�@QY�@QY�@QX�@QW�@QX�@QZ�@QZ�@QZ@QY�@QZ@QY�@QY�@QYb@QX@QY�@QX�@QX�@QY�@QY�@QZ^@QZ]@QX�@QX�@QX�@QY�@QX�@QX�@QX�@QX�@QX�@QX�@QW�@QW�@QW�@QW�@QWj@QU@QV�@QW�@QUB@QO@QMk@QN�@QJ�@QJ#@QH�@QD�@QE;@QC�@QAJ@Q?�@Q? @Q>�@Q>�@Q?�@Q?R@Q?(G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    44444444444334444433444444444443444344444443444444444444444444444444334444444344434443444434443444434444443444444444344444344444444444443443444443444344433344444444444444344444444433444444444344444444444434443444434444444434443333434334333343334334333333343333333333433333344343333433333443334344333333344333434333334343343343333443333333333333333333433333333433333334334434333333334343333333334333334333333343333333333343334333333333333334333334333333333433433333333333433343333343334433333343333343344333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�\p9�^|9�]r9�]w9�^z9�]�9�]r9�]�9�]�9�]'9�\�9�[�9�[�9�[�9�]�9�[�9�Z�9�[�9�\r9�\9�[�9�[�9�Z�9�Z�9�\19�]]9�\9�\�9�]�9�^9�^�9�^�9�^�9�^=9�^d9�]�9�^9�^|9�^9�]�9�^'9�_9�_9�_9�`9�`H9�_�9�`Z9�`�9�`�9�a9�`�9�^�9�_�9�_n9�]�9�^O9�]�9�\�9�[�9�`�9�a�9�a�9�`�9�`�9�a9�a�9�a\9�`�9�`�9�]#9�] 9�W�9�V9�T%9�T�9�V9�W	9�V�9�W�9�T:9�V�9�U�9�Y�9�\r9�\H9�\p9�^�9�^9�[�9�]`9�\K9�Z@9�Z�9�Z�9�X�9�W�9�UR9�T�9�X:9�U@9�VY9�QZ9�S�9�U9�Q}9�DH9�E89�E�9�C9�C�9�?p9�?�9�=�9�=-9�<�9�<y9�<�9�>m9�=�9�;�9�<;9�<�9�;�9�;�9�;�9�<:9�<(9�;�9�8�9�7�9�8@9�:�9�9 9�9�9�7c9�6�9�79�7�9�7�9�7�9�89�7�9�79�8�9�7*9�89�8�9�7�9�7�9�69�3�9�2M9�2x9�2d9�2a9�2�9�2�9�2�9�2�9�3'9�2�9�39�2�9�3g9�3�9�3�9�49�3�9�4D9�49�49�49�4E9�3�9�3�9�3i9�3N9�49�3�9�4A9�3�9�3g9�4�9�49�4U9�4A9�3�9�39�29�2O9�2�9�2<9�2Q9�29�2<9�2L9�2L9�1�9�1a9�1G9�1^9�1t9�1�9G��9G�9G��9G�l9G��9G��9G��9G�r9G��9G�'9G�9G��9G��9Gφ9G�29Gθ9Gσ9Gσ9G�H9G��9G�~9G��9G�e9G̏9G̈9G�9G�"9Gʪ9Gʄ9Gɻ9G�9Gȣ9G��9G�09G��9G�49G�k9G��9G��9G�{9G�|9G��9G�9G�9G�E9G��9G�9G�n9G�k9G�g9GĒ9G��9G�{9G�(9G�{9G�R9G�{9G�z9GÞ9G��9G��9Gç9G��9G9G±9G��9G��9G��9Gá9G��9G� 9G´9G�9G±9G´9G�d9G�#9G9G��9G��9G9G±9G�T9G�S9G��9G��9G��9G9G��9G��9G��9G��9G��9G��9G��9G��9G��9G��9G��9G�O9G��9G��9G�t9G��9G��9G�9G�O9G��9G�n9G��9G�)9G��9G�f9G�'9G�79G�9G��9G��9G��9G�]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�3B
�9B
�9B
�9B
�3B
�3B
�-B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�9B
��B
��B
��B �B?}BaHBl�Bm�Bm�Bv�B~�B�bB�wB�#B�fB��B  BB%B%B+B1B
=BDBPBPB\BbBhB{B�B�B�B#�B)�B0!B49B7LB9XB?}BL�B]/BffBx�B�1B��B��B��B�B�9B�}BƨB��B�/B�B�
B��B��B��BǮB�-B��B��B��B��B��B��B��B�'B�3B�9B�-B��B�oB�\B�VB�DB}�BT�B8RB:^B@�BN�BYBXBB�B5?B,B%�B�B\BB��B�B��B�B��B�7Bt�BiyBVB33B�BDB
��B
�B
�B
��B
��B
��B
��B
��B
ɺB
�qB
�B
��B
�hB
�B
w�B
p�B
_;B
I�B
>wB
)�B
�B
\B
B	��B	�B	�B	��B	ȴB	�FB	��B	�PB	�=B	�B	s�B	dZB	]/B	VB	N�B	A�B	49B	)�B	#�B	!�B	�B	oB	JB	
=B	B��B�yB�BB�B��B�}B�dB�qB��B��BȴBŢB�wB�9B�9B�?B�?B�9B�-B�B�B��B��B��B��B��B�bB�=B�1B�B�B}�B{�Bz�Bw�Bt�Br�Bp�Bn�Bk�BgmBaHB^5B\)B[#BZBYBW
BVBT�BS�BQ�BP�BM�BN�BM�BM�BK�BI�BH�BF�BE�BE�BF�BE�BB�BA�BB�BB�BA�BB�BA�BB�BB�BA�B@�BA�B@�B@�BB�BC�BH�BS�BYBYBT�BS�BS�BVBbNBn�Bt�By�Bt�Bv�Bs�Bo�BjBffBl�Bn�BiyBffBffBm�Bn�Bm�Bl�BdZBo�B{�B�B�7B�JB�DB�7B�%B�%B�%B�%B�B�B~�B{�B|�B~�B�B�%B�JB�{B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B�B�^B�jB�wB��BBB��B�wB�dB�^B�^B�jB�jB�jB��B��BBÖBŢBȴB��B��B��B�B�B�
B�B�#B�NB�HB�)B�NB�ZB�sB�yB�B	B	%B��B��B	B�B�5B�;B�mB�B��B��B	  B	
=B	DB	JB	PB	bB	{B	uB	�B	�B	!�B	"�B	#�B	&�B	.B	/B	1'B	2-B	7LB	:^B	=qB	A�B	F�B	K�B	L�B	P�B	Q�B	S�B	ZB	_;B	e`B	hsB	iyB	m�B	s�B	y�B	|�B	~�B	� B	�B	�B	�+B	�7B	�JB	�PB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�3B	�?B	�FB	�FB	�FB	�LB	�LB	�XB	�^B	�dB	�dB	�qB	�wB	�}B	�}B	�}B	�}B	��B	��B	ÖB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�B	��B
B
�B
�B
#nB
+�B
4TB
:�B
A�B
H�B
P�B
TFB
W�B
\]B
`B
e�B
jKB
o�B
uB
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>P��>_�O>�Q�>�u�>��o>ڦ�?b�@�>�>%?!�?���B
FBe>�7>��]>��?+Ct?f4vA�#B
�4>�7;>��>�R/>���>�j>�Ah?!�_@��u>�|t>�??1SiA�h�>�{�>�B�?��Aѹ�>��>��x>��>��3?��?-�cA�yB
��>�
_?�|?ax�Ae��>���>� K>��s>מ�>��$?A\�A�QA@
��>t�c>�ZO>���?wC�>r}�>x��>�s�>���>�N�>�9�?L?=��A���B{=>�jr>�%�>�x}>�&�@o@_@�oA?d#B
�Z@�$F?[K?��B
�S>�i�?��?g��A�iw>�ɴ>Œ�?c�?L|�B��>ƥ�>�?5�Bj�>��b>�1�?.�|A(b�BA>�X>���>�"%@�К?��?C��A�~J>���>ب�?.0?��@�xA1�>��O>��D?C�kA��A��AN��>�d9>�Ύ?5��B
��?�U�>H.>Yi�>S�>X&g>j�H>k#>��>��+>�G>�d�?M�@�CBD�?)�)@v�BG;>��>�:>��?W?��B�c>ؘ�?~@�EB��? ?ם?fZB
��B
�{Bx�?*��AC�>���?�tq>z(�>pp3>�%�>��n?UD�>���?�k??8?A_�@�:�Aڅ�>�`N>�u)>��,>�tK?\[>��>��?>�@�WB
��B
i�Ah�>�+�>�	�>��">���>ފP?/�8A�BA7	(Aڧj>ѭ�>��?)}�A���A�2K>��>��.>�4@@=�e?	[?�@�AS�wA���>�D?��?U&B
��>�p>��Z?5�HA��B
�Q>�\�?.��A�A���?Hz}@�X?��?�|BB
�Z?s�@ߒ�?`=�B
�^A�xRB
��B
��?� GB
�iA5�B
��B
��@�s�B
�wB
�nB
��B
��A� B
��B
�B
��@p[�B
��B
��@��nB
��B
��B
��B
��B
�qB
�{B
��@��B
�sB�/B
��B
��A�0
B
��B
�B
��B
��B
�?<$>B��A�B
�3B
A�-"B
�[A1��@J�BOm@��B
�B
�3B
�B
��A�{�B
�|B
�LB
��B
�kB
��@b��@�u�B
�/B
�7B
�@���B
�@AE1@�^�B
��B
��B{�B
�B
�tB
��B
�AM�{@;=SB
��B
�*B
��@�܁B
�/@y#B
��B
��B
�]B
��B
��Apm�B
��?�� B
�B	�EA_�SB
��B
��?�� B
�3B
�{B
�7B�A��AgB
�nB
�gB
�cB
�ZB
��B
��B
��AωoB
��B
��B
�AB
��B
��B&$B
��B
�B
�@B
�uB
��@W�xB
��B
��B
�lB
��B
��B
�gB
��B
��?�KB
�aB
�VB
�oB
��B
�7B
��B
��?�MfB
��B
��@�P�@�P�B
��?��OB
��B
�pB
�B��A���B
�B
��B
��@m }B
��A��B
��B
��B
�RB
��B
�A��'B
�+B
�B
�/A���B�B
��B
�OB
��B
��A���A�� B
�B
�B
�YA��A��VB
�?w�*B
�(B
��B
��B
��B
�2A�p,B
�	B
�jB
��B
�%B
��@���B
��B�A���A�<�B
��B$1B
��B
�)B
��B
�?B
�GB
��B
�vB
��B
��B
��B
��B
��A��B	N�B
��B
�nB
�{B
�D@��B
��B
��B�_B�B
��B
ڛB
�B
�vB
�?zFB
� B
�0?��UB
�B
�B
�B
�B
��B
��A���B
��B
��B
��B
�r?�c0B
�B
��B
�	ARA�g�B
��B
��B
��B
��A�4\B
�A��lB
��A;�]A.ӎA��EB
��B
��B
�)B
��B
�lA=P:B
�{B
�jB
��B
��B
�IA��B
�{B
��@�+�?��HB
,�B
�B
�jB
��B
��B
��B
��B
��B
��B
�TB
��B
�%B
��B
��B
��B
��B
�]B
��B
�xB
��B
�WB
�FB
�NB
��B
��B
��B
��B
�B
�OB
��B
�XB
��B
�B
��B
��B
�EB
��B
��B
��B
��B
�DB
��B
�3B
�FB
�B
�UB
��B
��B
�B
��B
��B
�B
��B
��B
�B
��B
�.B
�9B
�B
�7B
�B
��B
��B
�fB
� B
�;B
��B
��B
�B
�]B
�yB
�B
�IB
��B
��B
��B
��B
�`B
�IB
��B
�`B
�1B
��B
��B
�B
�BB
��B
�B
��B
�.B
��B
�3B
��B
��B
��B
��B
�B
��B
��B
�!B
��B
��B
��B
��B
�%B
�kB
��B
�B
�]B
�tB
�+B
�:B
�B
��B
�^B
��B
��B
�`B
�vB
�hB
�MB
�	B
�XB
��B
�B
�oB
��B
��B
�`B
�B
�9B
� B
��B
�B
�B
�5B
��B
��B
�%B
�?B
�UB
� B
��B
��B
�?B
��B
�xB
�6B
�7B
��B
��B
��B
��B
��B
��B
�fB
�RB
��B
��B
�-B
��B
��B
�B
��B
��B
�qB
��B
�B
��B
�B
��B
�B
�fB
�xB
�oB
�4B
�<B
��B
�:B
�B
�zB
��B
�B
��B
��B
�B
�RB
�hB
��B
�B
�$B
��B
�B
�eB
�B
��B
��B
�YB
��B
�KB
�8B
��B
�=B
��B
�%B
�	B
�B
��B
��B
�LB
��B
��B
��B
�pB
�B
��B
��B
��B
�dB
��B
��B
�B
�uB
�fB
�B
�MB
�B
�5B
��B
��B
��B
��B
��B
�YB
��B
�B
��B
��B
�'B
�XB
��B
��B
�$B
��B
�pB	�iB	�B	��B	�B	�OB	�B	��B	�B	�B	�tB	�VB	�B	�B	�<B	��B	�B	�RB	�B	�B	��B	�EB	��B	�vB	�iB	�B	��B	�NB	��B	�OB	�B	�BB	�B	�\B	�_B	�B	�B	��B	�uB	�kB	�!B	�$B	�DB	�)B	�B	�B	��B	�B	�B	��B	��B	��B	�MB	�B	��B	��B	��B	��B	�B	�!B	��B	�
B	�B	�dB	�*B	�gB	�B	�MB	�B	��B	�yB	�B	�B	��B	�B	�B	�dB	��B	�B	��B	�{B	�@B	�B	��B	�9B	�B	��B	�\B	�B	�B	�B	��B	��B	�B	�>B	��B	�{B	�^B	��B	�QB	�^B	�B	�B	�B	��B	��B	�eB	�B	�/B	��B	�]B	��B	�B	��B	��B	��B	��B	��B	�B	�eB	�GB
��B
�B
��B
��B
��B
��B
��B
��B
��B
�
B
��B
�MB
�B
�tB
�MB
�B
�B
�SB
��B
�aB
��B
��B
��B
��B
�^B
��B
�;B
�#B
�:B
�5B
��B
��B
��B
�;B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�%B
��B
�gB
��B
�eB
��B
��B
�4B
�HB
��B
��B
�	B
�B
�GB
��B
�<B
��B
��B
��B
��B
�qB
�B
��B
��B
��B
�,B
��B
�uB
�AB
�1B
��B
�sB
��B
�nB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�cB
��B
�!B
�wB
��B
��B
��B
��B
�B
�IB
��B
��B
��B
�ZB
�|B
��B
��B
��B
��B
�yB
�WB
��B
�B
�WB
�}B
�aB
��B
��B
��B
�fB
��B
��B
�B
�HB
�?B
�MB
�*B
�cB
�B
��B
�[B
��B
�OB
��B
��B
�%B
��B
�?B
�$B
�tB
�B
�,B
�GB
��B
�JB
�YB
�]B
��B
��B
�[B
��B
��B
��B
��B
��B
�ZB
�>B
�IB
�LB
�~B
�<B
�RB
�#B
��B
��B
�B
�	B
��B
�bB
�B
�B
��B
�pB
�BB
�:B
�B
��B
��B
��B
��B
�FB
��B
�MB
��B
��B
�mB
�B
��B
��B
��B
��B
��B
��B
�WB
�mB
�xB
�pB
�/B
�8B
�B
�B
�*B
��B	�MB	ݶB	��B	ݲB	��B	� B	ޤB	�YB	��B	��B	ޟB	�sB	޾B	�HB	�B	ޤB	� B	�B	�#B	޺B	�oB	��B	ށB	ޓB	ޅB	�,B	�ZB	�B	��B	�>B	߹B	�PB	�JB	��B	�kB	ߛB	�B	ߛB	߁B	�'B	�
B	�:B	�]B	�PB	�bB	�	B	�)B	�\B	�?B	�2B	�6B	ߟB	�UB	�
B	�;B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�BB	ߺB	�B	�|B	ߎB	��B	߭B	��B	߅B	�xB	�>B	�;B	�BB	߭B	�bB	��B	�B	�pB	�bB	�2B	�5B	��B	ߗB	��B	�B	��B	޼B	��B	߆B	��B	��B	߶B	ުB	�pB	��B	��B	�ZB	߿B	�SB	�&B	��B	�2B	��B	��B	��B	�;B	�B	�sB	�pB	�B	��B	�VB	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444444334444433444444444443444344444443444444444444444444444444334444444344434443444434443444434444443444444444344444344444444444443443444443444344433344444444444444344444444433444444444344444444444434443444434444444434443333434334333343334334333333343333333333433333344343333433333443334344333333344333434333334343343343333443333333333333333333433333333433333334334434333333334343333333334333334333333343333333333343334333333333333334333334333333333433433333333333433343333343334433333343333343344333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999B
�6B
�;B
�9B
�9B
�3B
�6B
�0B
�(B
�'B
�(B
�*B
�*B
�+B
�(B
�)B
�)B
�(B
�.B
�;B
��B
��B
��B �B?}BaHBl�Bm�Bm�Bv�B~�B�fB�yB�&B�jB��B B B&B(B,B2B
?BDBPBPB\BfBjB~B�B�B�B#�B)�B0"B4<B7MB9WB?}BL�B]/BfhBx�B�6B��B��B��B�B�>B�|BƨB��B�2B�B�B��B��B��BǭB�.B��B��B��B��B��B��B��B�)B�6B�=B�2B��B�sB�_B�XB�CB}�BT�B8RB:`B@�BN�BYBXBB�B5AB,
B%�B�B^BB��B�B� B�B��B�:Bt�BiwBVB36B�BDB
��B
�B
�B
��B
��B
��B
��B
��B
ɽB
�qB
�B
��B
�jB
�B
w�B
p�B
_=B
I�B
>xB
)�B
�B
^B
B	��B	�B	�B	��B	ȵB	�HB	��B	�SB	�?B	�B	s�B	dZB	]0B	VB	N�B	A�B	4<B	)�B	#�B	!�B	�B	tB	MB	
@B	B��B�xB�DB�B��B�~B�hB�sB��B��BȹBţB�|B�:B�;B�BB�BB�>B�/B�B�	B��B��B��B��B��B�eB�@B�4B�!B�B}�B{�Bz�Bw�Bt�Br�Bp�Bn�Bk�BgpBaIB^7B\+B[%BZBYBWBVBUBS�BQ�BP�BM�BN�BM�BM�BK�BI�BH�BF�BE�BE�BF�BE�BB�BA�BB�BB�BA�BB�BA�BB�BB�BA�B@�BA�B@�B@�BB�BC�BH�BS�BYBYBU BS�BS�BVBbRBn�Bt�By�Bt�Bv�Bs�Bo�Bj�BfhBl�Bn�Bi{BfgBfgBm�Bn�Bm�Bl�BdZBo�B{�B�B�9B�KB�GB�:B�'B�(B�&B�*B�!B�B~�B{�B|�B~�B�B�'B�LB�B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B�B�`B�lB�{B��BBB��B�yB�hB�cB�bB�nB�lB�oB��B��BBÙBŤBȷB��B��B��B�B�B�B�B�&B�QB�JB�+B�PB�\B�wB�zB�B	B	)B��B��B	
B�B�7B�<B�pB�B��B��B	 B	
AB	HB	MB	TB	cB	�B	wB	�B	�B	!�B	"�B	#�B	&�B	.B	/B	1*B	20B	7LB	:cB	=sB	A�B	F�B	K�B	L�B	P�B	Q�B	S�B	Z B	_?B	ebB	hvB	i}B	m�B	s�B	y�B	|�B	~�B	�B	�B	�B	�,B	�:B	�MB	�SB	�^B	�kB	�qB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�/B	�7B	�@B	�KB	�HB	�HB	�PB	�MB	�YB	�bB	�fB	�hB	�uB	�yB	�~B	��B	�~B	�~B	��B	��B	ÙB	ŤB	ƩB	ƬB	ȷB	ɽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�:B	�8B	�=B	�=B	�?B	�?B	�>G�O�B	�B	��B
B
�B
�B
#pB
+�B
4VB
:�B
A�B
H�B
P�B
THB
W�B
\_B
`B
e�B
jLB
o�B
uB
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
FBgG�O�G�O�G�O�G�O�G�O�A�%B
�4G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�h�G�O�G�O�G�O�Aѹ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���B{?G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�ZG�O�G�O�G�O�B
�XG�O�G�O�G�O�A�i|G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�Bj�G�O�G�O�G�O�G�O�BAG�O�G�O�G�O�G�O�G�O�G�O�A�~MG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BD�G�O�G�O�BG<G�O�G�O�G�O�G�O�G�O�B�fG�O�G�O�G�O�B��G�O�G�O�G�O�B
��B
�{Bx�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aڅ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
i�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AڧlG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�B
��G�O�G�O�G�O�G�O�B
�QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�[G�O�G�O�G�O�B
�aA�xUB
��B
��G�O�B
�iG�O�B
��B
��G�O�B
�{B
�qB
��B
��G�O�B
��B
�B
��G�O�B
��B
��G�O�B
��B
��B
��B
��B
�qB
�}B
��G�O�B
�tB�0B
��B
��A�0B
��B
�B
��B
��B
�G�O�B��A� B
�3B	A�-"B
�\G�O�G�O�BOqG�O�B
�B
�5B
�!B
��G�O�B
�}B
�MB
��B
�nB
��G�O�G�O�B
�1B
�9B
�G�O�B
�AG�O�G�O�B
��B
��B{�B
�B
�tB
��B
�G�O�G�O�B
��B
�)B
��G�O�B
�1G�O�B
��B
��B
�`B
��B
��G�O�B
��G�O�B
�B	�GG�O�B
��B
��G�O�B
�5B
�|B
�9B�G�O�G�O�B
�oB
�gB
�eB
�]B
��B
��B
��AωqB
��B
��B
�AB
��B
��B&$B
��B
�B
�@B
�wB
��G�O�B
��B
��B
�mB
��B
��B
�gB
��B
��G�O�B
�aB
�WB
�pB
��B
�9B
��B
��G�O�B
��B
��G�O�G�O�B
��G�O�B
�B
�oB
�B��A���B
�B
��B
��G�O�B
��G�O�B
��B
��B
�SB
��B
�
A��*B
�,B
�B
�-G�O�B�B
��B
�PB
��B
��G�O�A��%B
�B
�B
�ZA��A��YB
�G�O�B
�*B
��B
��B
��B
�5A�p,B
�
B
�kB
��B
�%B
��G�O�B
��B�A���G�O�B
��B$2B
��B
�*B
��B
�AB
�IB
��B
�zB
��B
��B
��B
��B
��G�O�B	N�B
��B
�lB
�}B
�GG�O�B
��B
��B�aB�B
��B
ڛB
�B
�xB
�G�O�B
�B
�2G�O�B
��B
�B
�B
�B
��B
��A���B
��B
��B
��B
�tG�O�B
�!B
��B
�
G�O�A�g�B
��B
��B
��B
��G�O�B
�A��pB
��G�O�G�O�A��HB
��B
��B
�)B
��B
�mG�O�B
�|B
�kB
��B
��B
�JG�O�B
�{B
��G�O�G�O�B
-B
�B
�lB
�B
��B
��B
��B
��B
��B
�TB
��B
�%B
��B
��B
��B
��B
�_B
��B
�{B
��B
�WB
�GB
�PB
��B
��B
��B
��B
��B
�SB
��B
�[B
��B
�B
��B
��B
�GB
��B
��B
��B
��B
�FB
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�OB
�B
�vB
�MB
�B
�B
�RB
��B
�bB
��B
��B
��B
��B
�_B
��B
�:B
�%B
�<B
�7B
��B
��B
��B
�=B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�(B
��B
�hB
��B
�gB
��B
��B
�3B
�IB
��B
��B
�
B
�B
�IB
��B
�<B
��B
��B
��B
��B
�qB
� B
��B
��B
��B
�,B
��B
�uB
�EB
�2B
��B
�uB
��B
�oB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�gB
��B
�"B
�{B
��B
��B
��B
��B
�B
�JB
��B
��B
��B
�\B
�}B
��B
��B
��B
��B
�zB
�WB
��B
�B
�YB
�}B
�dB
��B
��B
��B
�hB
��B
��B
�B
�IB
�AB
�OB
�*B
�eB
�B
��B
�\B
��B
�SB
��B
��B
�(B
��B
�AB
�%B
�uB
��B
�.B
�GB
��B
�MB
�ZB
�`B
��B
��B
�]B
��B
��B
��B
� B
��B
�[B
�=B
�KB
�NB
�|B
�BB
�TB
�%B
��B
��B
�B
�B
��B
�cB
� B
�B
��B
�qB
�FB
�=B
�B
��B
��B
��B
��B
�HB
��B
�OB
��B
��B
�nB
�B
��B
��B
��B
��B
��B
��B
�XB
�pB
�wB
�pB
�1B
�;B
�B
�!B
�.B
��B	�QB	ݷB	�B	ݴB	��B	�!B	ަB	�[B	��B	��B	ޡB	�vB	޾B	�LB	�B	ަB	�!B	�B	�(B	޼B	�sB	��B	ކB	ޘB	ކB	�/B	�\B	�B	��B	�AB	߼B	�TB	�OB	��B	�mB	ߝB	�B	ߝB	߄B	�,B	�B	�=B	�aB	�QB	�eB	�B	�,B	�`B	�?B	�4B	�9B	ߤB	�XB	�B	�=B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�EB	߻B	�B	߀B	ߏB	��B	߱B	��B	߈B	�|B	�AB	�?B	�EB	߯B	�dB	�B	�B	�tB	�eB	�5B	�8B	��B	ߛB	��B	�B	��B	��B	��B	߉B	��B	��B	߷B	ޭB	�sB	��B	��B	�ZB	��B	�SB	�*B	��B	�3B	��B	��B	��B	�>B	�B	�uB	�rB	�B	��B	�XB	��B	�B	��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�OB
�B
�vB
�MB
�B
�B
�RB
��B
�bB
��B
��B
��B
��B
�_B
��B
�:B
�%B
�<B
�7B
��B
��B
��B
�=B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�(B
��B
�hB
��B
�gB
��B
��B
�3B
�IB
��B
��B
�
B
�B
�IB
��B
�<B
��B
��B
��B
��B
�qB
� B
��B
��B
��B
�,B
��B
�uB
�EB
�2B
��B
�uB
��B
�oB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�gB
��B
�"B
�{B
��B
��B
��B
��B
�B
�JB
��B
��B
��B
�\B
�}B
��B
��B
��B
��B
�zB
�WB
��B
�B
�YB
�}B
�dB
��B
��B
��B
�hB
��B
��B
�B
�IB
�AB
�OB
�*B
�eB
�B
��B
�\B
��B
�SB
��B
��B
�(B
��B
�AB
�%B
�uB
��B
�.B
�GB
��B
�MB
�ZB
�`B
��B
��B
�]B
��B
��B
��B
� B
��B
�[B
�=B
�KB
�NB
�|B
�BB
�TB
�%B
��B
��B
�B
�B
��B
�cB
� B
�B
��B
�qB
�FB
�=B
�B
��B
��B
��B
��B
�HB
��B
�OB
��B
��B
�nB
�B
��B
��B
��B
��B
��B
��B
�XB
�pB
�wB
�pB
�1B
�;B
�B
�!B
�.B
��B	�QB	ݷB	�B	ݴB	��B	�!B	ަB	�[B	��B	��B	ޡB	�vB	޾B	�LB	�B	ަB	�!B	�B	�(B	޼B	�sB	��B	ކB	ޘB	ކB	�/B	�\B	�B	��B	�AB	߼B	�TB	�OB	��B	�mB	ߝB	�B	ߝB	߄B	�,B	�B	�=B	�aB	�QB	�eB	�B	�,B	�`B	�?B	�4B	�9B	ߤB	�XB	�B	�=B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�EB	߻B	�B	߀B	ߏB	��B	߱B	��B	߈B	�|B	�AB	�?B	�EB	߯B	�dB	�B	�B	�tB	�eB	�5B	�8B	��B	ߛB	��B	�B	��B	��B	��B	߉B	��B	��B	߷B	ޭB	�sB	��B	��B	�ZB	��B	�SB	�*B	��B	�3B	��B	��B	��B	�>B	�B	�uB	�rB	�B	��B	�XB	��B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444444334444433444444444443444344444443444444444444444444444444334444444344434443444434443444434444443444444444344444344444444444443443444443444344433344444444444444344444444433444444444344444444444434443444434444444434443333434334333343334334333333343333333333433333344343333433333443334344333333344333434333334343343343333443333333333333333333433333333433333334334434333333334343333333334333334333333343333333333343334333333333333334333334333333333433433333333333433343333343334433333343333343344333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455082020082814550820200828145508202008281455082020082814550820200828145508202008281455082020082814550820200828145508202008281455082020082814550820200828145508AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730382019021417303820190214173038    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730382019021417303820190214173038  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730382019021417303820190214173038  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455082020082814550820200828145508  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                