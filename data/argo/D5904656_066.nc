CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ^   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:44Z creation      
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
resolution        =���   axis      Z        (h  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  mx   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (h  w�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (h  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (h  Ҁ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (h    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 -l   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (h 7�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (h _�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �X   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (h �t   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (h ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (h �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (h �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 HL   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (h Rh   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � z�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   {�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173044  20200828145521  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               B   B   BAAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @���G�\@���G�\@���G�\111 @��K�l@��K�l@��K�l@66E����@66E����@66E�����cj~��#�cj~��#�cj~��#111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    B   B   BADA BDA  DA BDA @333@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A���A�  B   B��B��B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  BxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�=D���D�Y�D���D��qD��D�\)D�� D��
D���D�)HD�~�DǹHD��D�H�Dڇ\D��D���D�?\D�}�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    =���        =���        >���>L��            =���            >���>L��    =���>L��            =���        >L��>L��    =���>���>L��        >L��>���                                                =���=���        =���            >���>L��        =���>���>L��        >L��>L��        >L��            >���=���    =���>L��                =���                        >L��>L��        =���>���>���        =���=���        >L��                        >L��        >L��>L��    =���        =���    >L��=���                                            >L��        =���                            =���=���    >L��        =���>L��    >L��    =���>L��            =���>L��        =���                =���=���=���                >L��>L��=���        =���>L��>���=���            >���>���=���        =���=���>L��>L��=���            >L��>L��        =���>L��=���        =���=���=���    =���=���        >L��>���=���=���        >L��>L��=���    >L��=���=���=���>L��>L��    >���>L��>L��>L��=���>L��>���>L��=���>���>L��>L��>L��>L��>L��=���>L��>���>���>L��>L��>���>���>���>L��>L��=���>L��=���>L��>L��>���=���>L��>���>���>���>L��=���>L��>L��=���>���>���>L��>���>���>L��>L��>L��>L��>L��=���=���>L��>���>L��=���>L��>L��>L��>���>���>L��>L��>���>L��>L��>���>���>���>���>���>L��>L��=���>L��>���>L��>���>L��>L��>���>���>L��>L��>L��>L��>L��>L��>���>L��>L��>L��>���>L��=���>L��>���>L��>L��>L��>���>���>���>L��>L��>L��>L��>���>���>L��=���>���>���>L��>L��>L��>L��>L��>L��>L��>L��>���>L��>L��    >L��>���>���>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>���>���>���>���>���>���>���>���>���=���=���>L��>L��>L��>L��>L��>L��=���>���>L��>L��>���>���>���=���=���=���>L��>L��>���>���>���>���>L��>L��>L��>L��=���>���>L��=���=���>���>���>L��=���=���>L��=���>���>���>���=���>L��=���>L��>L��>L��>L��=���>L��>L��>���>L��>L��>L��>L��>L��>L��>���>���>L��>���>L��>���=���>L��>L��>���>���>���>L��=���>L��>L��>L��>L��>���>L��>���>���>L��>���>���>L��>L��>L��>L��>L��>L��>L��=���>���>���=���>L��>���>L��=���>L��>L��>L��>L��>L��>L��=���>L��=���>���>���>���>���?   ?��?��?333?333?L��?fff?�  ?���?���?���?�ff?�  ?�  ?�33?�  ?�  ?���?ٙ�?�ff?�ff?�ff?�33@   @ff@ff@ff@��@33@��@��@   @&ff@,��@,��@333@9��@@  @Fff@L��@S33@Y��@fff@fff@l��@y��@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@���@���@�  @�ff@���@���@�  @�33@ə�@���@�  @�ff@ٙ�@���@�33@�ff@陚@�  @�33@�ff@���A   A��A��AffA  A33A��A  A��A��AffA��A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA1��A333A4��A8  A9��A<��A>ffAA��AC33AD��AFffAI��AK33ANffAP  AQ��AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��At��AvffAx  A{33A|��A~ffA���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A���A�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A���A͙�A͙�A�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�33A���Aٙ�Aٙ�A�ffA�33A�  A���Aݙ�A�ffDqS3Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrL�DrS3Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr�fDr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt�Dt&fDt,�Dt33Dt9�DtFfDtL�DtS3DtY�DtffDtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3DtٚDt�fDt��Dt�3Dt��DufDu�Du3@333@9��@@  @Fff@L��@S33@Y��@fff@fff@l��@y��@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@���@���@�  @�ff@���@���@�  @�33@ə�@���@�  @�ff@ٙ�@���@�33@�ff@陚@�  @�33@�ff@���A   A��A��AffA  A33A��A  A��A��AffA��A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA1��A333A4��A8  A9��A<��A>ffAA��AC33AD��AFffAI��AK33ANffAP  AQ��AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��At��AvffAx  A{33A|��A~ffA���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A���A���A���A�33A�  A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A���A͙�A͙�A�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�33A���Aٙ�Aٙ�A�ffA�33A�  A���Aݙ�A�ffDqS3Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrL�DrS3Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr�fDr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt�Dt&fDt,�Dt33Dt9�DtFfDtL�DtS3DtY�DtffDtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3DtٚDt�fDt��Dt�3Dt��DufDu�Du3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @/\)@|(�@�{@�{A
=A?
=A]p�A
=A��A��A��A��AυA�Q�A�A��B\)B\)BBB'B/B8(�B?BGBOBWB_BgBoBx(�B\)B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3��D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt��Dy�fD���D�X D���D�˅D�
D�Z=D�~D��D���D�'\D�|�DǷ\D�
D�G
DڅpD��D���D�=pD�{�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��u=#��u�u=#��u�u>u>\)�u�u�u=#��u�u�u>u>\)�u=#�>\)�u�u�u=#��u�u>\)>\)�u=#�>u>\)�u�u>\)>u�u�u�u�u�u�u�u�u�u�u�u�u=#�=#��u�u=#��u�u�u>u>\)�u�u=#�>u>\)�u�u>\)>\)�u�u>\)�u�u�u>u=#��u=#�>\)�u�u�u�u=#��u�u�u�u�u�u>\)>\)�u�u=#�>u>u�u�u=#�=#��u�u>\)�u�u�u�u�u�u>\)�u�u>\)>\)�u=#��u�u=#��u>\)=#��u�u�u�u�u�u�u�u�u�u�u>\)�u�u=#��u�u�u�u�u�u�u=#�=#��u>\)�u�u=#�>\)�u>\)�u=#�>\)�u�u�u=#�>\)�u�u=#��u�u�u�u=#�=#�=#��u�u�u�u>\)>\)=#��u�u=#�>\)>u=#��u�u�u>�{>u=#��u�u=#�=#�>\)>\)=#��u�u�u>\)>\)�u�u=#�>\)=#��u�u=#�=#�=#��u=#�=#��u�u>\)>u=#�=#��u�u>\)>\)=#��u>\)=#�=#�=#�>\)>\)�u>u>\)>\)>\)=#�>\)>u>\)=#�>u>\)>\)>\)>\)>\)=#�>\)>u>�{>\)>\)>u>u>u>\)>\)=#�>\)=#�>\)>\)>u=#�>\)>u>�{>u>\)=#�>\)>\)=#�>u>u>\)>u>u>\)>\)>\)>\)>\)=#�=#�>\)>u>\)=#�>\)>\)>\)>u>u>\)>\)>u>\)>\)>u>u>u>u>u>\)>\)=#�>\)>u>\)>u>\)>\)>�{>�{>\)>\)>\)>\)>\)>\)>u>\)>\)>\)>u>\)=#�>\)>u>\)>\)>\)>u>�{>u>\)>\)>\)>\)>u>�{>\)=#�>u>�{>\)>\)>\)>\)>\)>\)>\)>\)>u>\)>\)�u>\)>u>u>\)=#�>\)>\)>\)>\)>\)>\)>\)>\)>u>�{>u>u>u>u>u>u>u=#�=#�>\)>\)>\)>\)>\)>\)=#�>u>\)>\)>u>u>u=#�=#�=#�>\)>\)>u>u>u>u>\)>\)>\)>\)=#�>u>\)=#�=#�>u>�{>\)=#�=#�>\)=#�>u>u>u=#�>\)=#�>\)>\)>\)>\)=#�>\)>\)>u>\)>\)>\)>\)>\)>\)>u>u>\)>�{>\)>u=#�>\)>\)>u>�{>u>\)=#�>\)>\)>\)>\)>u>\)>u>u>\)>u>�{>\)>\)>\)>\)>\)>\)>\)=#�>u>u=#�>\)>u>\)=#�>\)>\)>\)>\)>\)>\)=#�>\)=#�>u>u>u>�{>�G�?
=q?
=q?#�
?#�
?=p�?W
=?p��?��?��?��?��R?�Q�?�Q�?��?�Q�?�Q�?��?��?޸R?޸R?޸R?�?�Q�@�\@�\@�\@��@\)@@@(�@"�\@(��@(��@/\)@5@<(�@B�\@H��@O\)@U@b�\@b�\@h��@u@|(�@�G�@�z�@��@��H@�{@�G�@��@��H@�{@�G�@��@��H@�{@�z�@��@��H@�{@�G�@Ǯ@��H@�{@�z�@׮@��H@�G�@�z�@�@�{@�G�@�z�@��@�{A ��A�
Ap�A
=A
=pA�
A
=A��A�
Ap�A��A=pA�
A
=A ��A"=pA%p�A'
=A*=pA+�
A-p�A0��A2=pA3�
A7
=A8��A;�
A=p�A@��AB=pAC�
AEp�AH��AJ=pAMp�AO
=AP��AS�
AUp�AW
=AZ=pA[�
A]p�A`��Ab=pAc�
Ag
=Ah��Aj=pAmp�Ao
=Ap��As�
Aup�Aw
=Az=pA{�
A}p�A�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��A��RA��A��A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��A�Q�A��A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��RA�Q�A��A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��RA��A�Q�A��A��A��RA�Q�A��A��A��RA��A�Q�A��A��A¸RAÅA�Q�A��A��AƸRAǅA�Q�A��A��AʸRA�Q�A��A��AθRAυA�Q�A��A��AҸRAӅA�Q�A��A��AָRAָRA�Q�A��A��A��AڸRAۅA�Q�A��A��DqO\Dq\)Dqb�Dqh�Dqu�Dq|)Dq��Dq��Dq��Dq�)Dq��Dq��Dq��Dq�)DqDq��Dq��Dq�)Dq�Dq��Dq��Dq�)Dr�Dr�Dr�Dr)Dr"�Dr(�Dr5�Dr<)DrB�DrH�DrO\Dr\)Drb�Drh�Dru�Dr|)Dr��Dr��Dr��Dr�)Dr��Dr�\Dr��Dr�)DrDr�\Dr��Dr�)Dr�Dr�\Dr��Dr�)Ds�Ds\Ds�Ds)Ds(�Ds/\Ds5�Ds<)DsH�DsO\DsU�Dsb�Dsh�Dso\Dsu�Ds��Ds��Ds�\Ds��Ds��Ds��Ds�\Ds��DsDs��Ds�\Ds��Ds�Ds��Ds�\Ds��Dt�Dt�Dt\Dt�Dt"�Dt(�Dt/\Dt5�DtB�DtH�DtO\DtU�Dtb�Dth�Dto\Dtu�Dt|)Dt��Dt�\Dt��Dt�)Dt��Dt�\Dt��DtDt��Dt�\Dt��Dt�Dt��Dt�\Dt��Du�Du�Du\@/\)@5@<(�@B�\@H��@O\)@U@b�\@b�\@h��@u@|(�@�G�@�z�@��@��H@�{@�G�@��@��H@�{@�G�@��@��H@�{@�z�@��@��H@�{@�G�@Ǯ@��H@�{@�z�@׮@��H@�G�@�z�@�@�{@�G�@�z�@��@�{A ��A�
Ap�A
=A
=pA�
A
=A��A�
Ap�A��A=pA�
A
=A ��A"=pA%p�A'
=A*=pA+�
A-p�A0��A2=pA3�
A7
=A8��A;�
A=p�A@��AB=pAC�
AEp�AH��AJ=pAMp�AO
=AP��AS�
AUp�AW
=AZ=pA[�
A]p�A`��Ab=pAc�
Ag
=Ah��Aj=pAmp�Ao
=Ap��As�
Aup�Aw
=Az=pA{�
A}p�A�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��A��A�Q�A��A��A��RA��A��A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��A�Q�A��A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��RA�Q�A��A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��RA��A�Q�A��A��A��RA�Q�A��A��A��RA��A�Q�A��A��A¸RAÅA�Q�A��A��AƸRAǅA�Q�A��A��AʸRA�Q�A��A��AθRAυA�Q�A��A��AҸRAӅA�Q�A��A��AָRAָRA�Q�A��A��A��AڸRAۅA�Q�A��A��DqO\Dq\)Dqb�Dqh�Dqu�Dq|)Dq��Dq��Dq��Dq�)Dq��Dq��Dq��Dq�)DqDq��Dq��Dq�)Dq�Dq��Dq��Dq�)Dr�Dr�Dr�Dr)Dr"�Dr(�Dr5�Dr<)DrB�DrH�DrO\Dr\)Drb�Drh�Dru�Dr|)Dr��Dr��Dr��Dr�)Dr��Dr�\Dr��Dr�)DrDr�\Dr��Dr�)Dr�Dr�\Dr��Dr�)Ds�Ds\Ds�Ds)Ds(�Ds/\Ds5�Ds<)DsH�DsO\DsU�Dsb�Dsh�Dso\Dsu�Ds��Ds��Ds�\Ds��Ds��Ds��Ds�\Ds��DsDs��Ds�\Ds��Ds�Ds��Ds�\Ds��Dt�Dt�Dt\Dt�Dt"�Dt(�Dt/\Dt5�DtB�DtH�DtO\DtU�Dtb�Dth�Dto\Dtu�Dt|)Dt��Dt�\Dt��Dt�)Dt��Dt�\Dt��DtDt��Dt�\Dt��Dt�Dt��Dt�\Dt��Du�Du�Du\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AН�AН�AП�AП�AХ�AХ�AЧ�AЧ�AЩ�AЬAа!Aд9AжFAиRAжFAа!AЕ�A� �A�S�A�`BAɰ!A�Aȣ�A���A���A��A�O�A��`A��A���A� �A���A��+A���A�5?A�7LA��\A��7A�ƨA��A��7A��9A�S�A�ȴA��HA�$�A�M�A��DA��9A�VA��A���A�p�A��A���A��mA��A�5?A�p�A���A���A�/A�bNA���A��-A��hA�bNA��A�ƨA�&�A��A��A���A�XA��A�~�A�C�A��A�
=A�M�A�M�A���A���A���A�
=A���A��jA���A���A��A���A�A�dZA�=qA�"�A���A�l�A���A�~�A"�A{dZAxM�Aw;dAtZApĜAm�PAk;dAi��Agl�Ae�TAb�\A_�A\1AYG�AV$�AR9XAQ;dAP�AN�ALv�AL-AK�FAKO�AJĜAI��AH1AF��AE�AD��ADI�ABM�A@I�A=�wA;��A:n�A9�A8��A6��A5��A4��A3�TA3hsA21'A0��A.�!A-��A+oA)��A(��A(=qA&��A%�TA%?}A$VA#K�A"��A"1A!XA ffA"�A��A�+A�FA&�A�A��A�AVAG�A��AA�A/A�\A�A�AȴA�;AȴA��AhsA��A
VA	�A�uAbNA�A��A&�A�TA%A�AVA{A�hA��A��A ��A $�@�t�@��H@�^5@��7@�hs@�&�@�Ĝ@��F@�-@�V@�j@�@�{@�b@��@�E�@�\@��@�t�@�t�@�1@�@@���@��@�1@�1'@��@��@�Z@�\@�@�"�@�\@�Z@޸R@�-@�V@��@��;@ٺ^@�l�@�=q@�$�@�@�Ĝ@�@�=q@���@��/@�r�@���@�ȴ@�=q@Ͳ-@��`@�A�@�Q�@�1@�|�@�S�@��H@�^5@�M�@�$�@�l�@��@���@��@�~�@�V@�hs@�A�@�\)@��y@���@�33@��H@�$�@���@�7L@�/@�x�@���@���@�=q@�V@�5?@�5?@���@�7L@��9@�j@�Q�@�ƨ@�33@���@�=q@�/@�1@�V@�/@���@���@���@��@��@�^5@��7@�1'@�@�S�@��y@���@��m@�5?@��7@�O�@�?}@�O�@���@�;d@���@���@�ff@�-@���@���@��
@�ȴ@��y@��@�?}@�A�@�dZ@�\)@�33@��^@�p�@�x�@�ff@�$�@�?}@�V@�j@���@�o@�
=@��R@���@�E�@���@��@��T@���@�dZ@�;d@�=q@��@�dZ@�=q@�@��R@��@�j@�I�@�\)@�~�@�M�@���@�p�@��@�J@�hs@��`@�Z@��
@�|�@�;d@���@�\)@�S�@��!@��+@�n�@�v�@���@�ȴ@��\@��\@��
@�C�@��-@��^@�=q@���@��`@���@���@�&�@�hs@�v�@��\@�=q@���@��-@�J@��@��+@�{@�v�@�$�@�E�@��h@��@�K�@�
=@�-@��T@���@��/@��9@�Z@�b@���@��w@�ƨ@�  @��w@��@�dZ@�33@��@�v�@���@�~�@�@���@��j@�G�@�`B@�O�@�7L@�&�@���@���@��@�j@�bN@��@���@�33@�dZ@�o@�
=@��@���@��\@�J@�@�@�O�@���@���@��u@��9@��`@�Ĝ@�9X@��@�l�@�o@�ȴ@��!@���@���@��+@�E�@�-@��@���@���@�@��^@��-@��-@��@��9@��@��@��@��@.I@y@@pG@h�.@aA @ZJ�@Q�N@Iu�@C��@=@6�@0]d@*��@%�S@ g8@�'@O@�@�@�@�kG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A¥�A�\)A�|�A�ĜA��A���A�JA�\)A�AËDA��A��A�ƨA�ĜA���A���A�~�Aŧ�A�C�A�C�A�  A�ȴAȋDA͑hA��A��TAͲ-A�hsA�jA�+AЃAͰ!A�p�AƅA���A�~�A��mA�%A���A�&�A�p�A���A���A�;dA���A�~�A���A�7LA�E�A�ƨA�+A���Aȥ�A���AĴ9AƮA��/A�z�A�r�Aǥ�A� �A�bNA�t�A�1'A���A�?}A��A�A�JA�33A�&�A�?}AɋDA�jA��A���A���A�M�Aď\A�;dA�A��#Aß�A��A�$�A�Q�A���A�=qA�JA̸RA�dZA�XA�I�A�hsA�~�A�t�A���Aá�A�9XA�M�A�x�A�A�z�A�l�A�z�A�v�A�C�A�VAāA�S�AͬA��/A�dZA�n�A�AʃA��mA�M�A�1A�ĜA�v�A·+AÛ�A���A�/A�oAÃA�^5Aá�AA���A��PA�n�A��AȲ-Aƥ�A�jA��A�
=A�~�Aº^A���Aò-AŁAΕ�A�jAŕ�A� �AƏ\A�r�AǛ�A�hsA˶FA�M�A��A�VAϼjAκ^A�A�A�bNA�JA�t�A���A¸RAģ�AčPA�l�A�G�A�A�^5A�ƨAź^AŋDA��A�ȴAÙ�A��`AЍPA�"�A�dZAʛ�A���AЁA�oA�p�A�\)A���A�9XA�{AЃA�z�AhA�n�AɃA��A�v�A�Q�A�bNA�O�A��Aǟ�A�oA�~�A���A��A�oA�ffAΙ�A�%A×�A�A�-A�"�A���A� �A�/AļjAƏ\A�`BAЉ7A�z�A���AƶFA�~�A�hsAЏ\AϼjAʹ9Ḁ�AЇ+Aʙ�A�p�A�Q�AЋDA�z�Aϛ�AЋDA�?}AЋDA�jA�bAЋDAЉ7AЇ+A�
=AЋDA�?}AЇ+AЉ7A�G�A���A΅A�;dAЏ\AЏ\A�;dA�
=AЏ\AЇ+A�Q�AЉ7A�ffA�z�A�hsA͍PAЏ\AЋDAЁAύPAЁAЋDAЉ7AЁA�n�A�1A�dZA˙�A� �AЅAЁA�p�AЉ7A�A�AЉ7A�t�A�^5A�~�A̡�A�z�A�XAЅAЋDA�E�A�I�AЅAЃAϰ!AЍPAЍPA��AЏ\AГuAЍPAЇ+AЏ\AЋDAЏ\AБhAμjAЁA���A��AЍPAБhAБhA�I�A���AЅAЕ�AБhAЏ\A�AЏ\AϼjA���AЏ\AЋDA�p�A�bNAЋDAЅA�E�A̬AЋDAЉ7A�~�AЍPAЏ\AГuAЋDAЁA�t�A���AЏ\AЏ\AБhAБhA�ȴA�|�AЋDAБhAЍPAГuȂhAЕ�A��AЇ+Aϕ�AБhA�{AЉ7A��A�1'AЉ7AЇ+A�\)A�dZAϗ�AЅAЃA�A�&�A�`BA�t�AЋDAЋDAЍPAЍPAЋDAЋDAЇ+AЉ7AЉ7AЇ+A��A��A���AЏ\AЏ\AБhA���A�AЋDAЏ\AБhAЇ+AЍPAЍPAБhA�v�A��HA��A�7LAГuAБhAЋDAГuAГuAЏ\A�JAЏ\AЉ7A�  A�?}A�G�A�ZA�
=A�z�AЕ�AЕ�A�n�A��A��/AЅA�I�AЕ�AГuAЇ+A�(�A�XA��AБhAБhAμjAЅA�VA���AБhAГuA�v�AБhAЅA���AЕ�AГuAЕ�AЙ�AЗ�AБhA�=qAϣ�AЋDAЏ\AЙ�AЗ�AЛ�AЕ�A̟�A�AЗ�AΟ�AГuAБhAЙ�AЙ�AЕ�A�5?AЗ�AЙ�AЙ�AЋDAϲ-A˰!A�;dAЕ�A�t�A�A�Aϥ�AЗ�A�%AГuA��AЙ�A�z�A��AЉ7AЗ�A�v�AЕ�Aϕ�A�\)AϸRA�AЁAЕ�AЛ�AЛ�AН�AЛ�AН�AЙ�AЛ�AЛ�AН�AН�AЙ�AН�AН�AН�AЕ�AЕ�AЕ�AГuAЙ�AЕ�AЕ�AЕ�AБhAЍPAБhAЏ\AЏ\AГuAЕ�AБhAС�AН�AУ�AУ�AП�AС�AН�AЛ�AН�AН�AЛ�AЛ�AЛ�AЛ�AЛ�AЛ�AЙ�AН�AП�AП�AН�AН�AС�AП�AП�AЛ�AП�AН�AП�AЛ�AС�AС�AН�AП�AП�AС�AП�AП�AН�AП�AЛ�AЛ�AН�AЛ�AН�AН�AЛ�AЛ�AЙ�AЙ�AЙ�AЛ�AС�AН�AН�AХ�AС�AХ�AХ�AХ�AХ�AХ�AХ�AУ�AХ�AУ�AХ�AХ�AХ�AХ�AХ�AУ�AХ�AХ�AУ�AУ�AУ�AХ�AУ�AУ�AХ�AУ�AХ�AУ�AХ�AЧ�AХ�AХ�AХ�AХ�AХ�AХ�AЧ�AХ�AЩ�AЩ�AЩ�AЩ�AЧ�AЧ�AХ�AЧ�AХ�AЧ�AХ�AЧ�AХ�AУ�AХ�AЧ�AЧ�AЧ�AХ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AХ�AЧ�AЩ�AЧ�AЩ�AЬAЧ�AЩ�AЩ�AЬAЩ�AЬAЬAЩ�AЮAЬAЬAЬAЬAЬAЬAЩ�AЩ�AЩ�AЬAЩ�AЩ�AЩ�AЩ�AЩ�AЮAЮAЮAЬAЮAа!Aа!Aа!AЮAЮAЬAа!Aа!Aа!AЮAа!Aа!Aа!Aа!Aв-Aд9Aд9Aд9AжFAд9Aд9AжFAд9Aд9Aд9Aв-Aв-Aа!Aа!Aа!Aд9Aв-Aд9AжFAжFAд9Aд9AжFAжFAд9AжFAжFAд9Aд9Aд9AиRAжFAд9AжFAжFAиRAд9Aв-Aд9AжFAжFAжFAд9AиRAжFAиRAжF@��^@��-@��-@��-@���@���@���@���@���@���@���@���@��-@��^@��^@��^@��-@��-@��-@��-@��-@���@���@��-@��-@��-@��^@��^@��^@��^@��-@��-@��-@��-@���@���@��h@��h@��7@�x�@�hs@�`B@�`B@�O�@�7L@�/@�V@�%@��@�Ĝ@��j@���@���@���@���@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��D@��@�z�@�z�@�r�@�r�@�bN@�bN@�Z@�Z@�I�@�A�@�9X@�1'@�1'@�1'@�(�@��@�1@�  @�  @�@�@�;@�;@�;@�;@��@�w@�w@�w@�@��@�P@�P@�P@|�@|�@|�@|�@�P@|�@�P@�P@��@��@�@�@�@��@��@�AН�AН�AН�AН�AЛ�AЛ�AЛ�AЛ�AП�AП�AП�AН�AП�AП�AП�AН�AН�AН�AН�AН�AН�AН�AП�AП�AС�AС�AП�AП�AН�AН�AН�AН�AЛ�AН�AН�AН�AН�AН�AЛ�AЛ�AЛ�AЛ�AЛ�AН�AН�AП�AУ�AХ�AЧ�AЧ�AХ�AХ�AЧ�AХ�AХ�AХ�AХ�AХ�AЧ�AХ�AЩ�AЧ�AХ�AХ�AХ�AУ�AХ�AХ�AУ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AУ�AХ�AХ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЩ�AЩ�AЧ�AЩ�AЩ�AЩ�AХ�AЧ�AЧ�AЧ�AЩ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЩ�AЩ�AЩ�AЬAЬAЬAЬAЮAЮAЬAЬAЬAЮAЬAЬAЬAЮAЬAЬAЬAЬAЬAЬAЮAЮAа!AЮAв-Aа!AЬAа!AЮAЮAЮAЮAа!Aа!Aа!Aа!Aа!Aа!Aв-Aд9AжFAд9Aд9Aд9AжFAжFAд9AжFAд9Aд9Aв-Aв-Aв-Aв-Aв-Aд9Aд9Aд9AжFAд9Aд9AжFAжFAжFAжFAжFAжFAжFAд9AжFAжFAжFAжFAжFAжFAиRAжFAжFAжFAиRAжFAжFAиRAиRAиRAиRAжFAиR@��^@��-@��-@��-@���@���@���@���@���@���@���@��-@��^@�@��^@��-@��-@��-@��^@��-@���@���@���@��-@��-@��^@�@��^@��^@��-@��-@��-@��-@��-@���@���@��h@��7@��@�p�@�hs@�`B@�X@�?}@�/@�&�@�V@���@�Ĝ@��j@��9@���@���@���@���@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��D@��D@��@�z�@�z�@�r�@�j@�bN@�Z@�Z@�Q�@�A�@�A�@�9X@�1'@�1'@�(�@� �@�b@�  @�  @�@�;@�;@�;@�;@�;@��@��@�w@�w@�@�@��@�P@�P@|�@|�@|�@|�@�P@|�@�P@�P@�P@�@�@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  AН�AН�AП�AП�AХ�AХ�AЧ�AЧ�AЩ�AЬAа!Aд9AжFAиRAжFAа!AЕ�A� �A�S�A�`BAɰ!A�Aȣ�A���A���A��A�O�A��`A��A���A� �A���A��+A���A�5?A�7LA��\A��7A�ƨA��A��7A��9A�S�A�ȴA��HA�$�A�M�A��DA��9A�VA��A���A�p�A��A���A��mA��A�5?A�p�A���A���A�/A�bNA���A��-A��hA�bNA��A�ƨA�&�A��A��A���A�XA��A�~�A�C�A��A�
=A�M�A�M�A���A���A���A�
=A���A��jA���A���A��A���A�A�dZA�=qA�"�A���A�l�A���A�~�A"�A{dZAxM�Aw;dAtZApĜAm�PAk;dAi��Agl�Ae�TAb�\A_�A\1AYG�AV$�AR9XAQ;dAP�AN�ALv�AL-AK�FAKO�AJĜAI��AH1AF��AE�AD��ADI�ABM�A@I�A=�wA;��A:n�A9�A8��A6��A5��A4��A3�TA3hsA21'A0��A.�!A-��A+oA)��A(��A(=qA&��A%�TA%?}A$VA#K�A"��A"1A!XA ffA"�A��A�+A�FA&�A�A��A�AVAG�A��AA�A/A�\A�A�AȴA�;AȴA��AhsA��A
VA	�A�uAbNA�A��A&�A�TA%A�AVA{A�hA��A��A ��A $�@�t�@��H@�^5@��7@�hs@�&�@�Ĝ@��F@�-@�V@�j@�@�{@�b@��@�E�@�\@��@�t�@�t�@�1@�@@���@��@�1@�1'@��@��@�Z@�\@�@�"�@�\@�Z@޸R@�-@�V@��@��;@ٺ^@�l�@�=q@�$�@�@�Ĝ@�@�=q@���@��/@�r�@���@�ȴ@�=q@Ͳ-@��`@�A�@�Q�@�1@�|�@�S�@��H@�^5@�M�@�$�@�l�@��@���@��@�~�@�V@�hs@�A�@�\)@��y@���@�33@��H@�$�@���@�7L@�/@�x�@���@���@�=q@�V@�5?@�5?@���@�7L@��9@�j@�Q�@�ƨ@�33@���@�=q@�/@�1@�V@�/@���@���@���@��@��@�^5@��7@�1'@�@�S�@��y@���@��m@�5?@��7@�O�@�?}@�O�@���@�;d@���@���@�ff@�-@���@���@��
@�ȴ@��y@��@�?}@�A�@�dZ@�\)@�33@��^@�p�@�x�@�ff@�$�@�?}@�V@�j@���@�o@�
=@��R@���@�E�@���@��@��T@���@�dZ@�;d@�=q@��@�dZ@�=q@�@��R@��@�j@�I�@�\)@�~�@�M�@���@�p�@��@�J@�hs@��`@�Z@��
@�|�@�;d@���@�\)@�S�@��!@��+@�n�@�v�@���@�ȴ@��\@��\@��
@�C�@��-@��^@�=q@���@��`@���@���@�&�@�hs@�v�@��\@�=q@���@��-@�J@��@��+@�{@�v�@�$�@�E�@��h@��@�K�@�
=@�-@��T@���@��/@��9@�Z@�b@���@��w@�ƨ@�  @��w@��@�dZ@�33@��@�v�@���@�~�@�@���@��j@�G�@�`B@�O�@�7L@�&�@���@���@��@�j@�bN@��@���@�33@�dZ@�o@�
=@��@���@��\@�J@�@�@�O�@���@���@��u@��9@��`@�Ĝ@�9X@��@�l�@�o@�ȴ@��!@���@���@��+@�E�@�-@��@���@���@�@��^@��-@��-@��@��9@��@��@��G�O�@.I@y@@pG@h�.@aA @ZJ�@Q�N@Iu�@C��@=@6�@0]d@*��@%�S@ g8@�'@O@�@�@�@�kG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A¥�A�\)A�|�A�ĜA��A���A�JA�\)A�AËDA��A��A�ƨA�ĜA���A���A�~�Aŧ�A�C�A�C�A�  A�ȴAȋDA͑hA��A��TAͲ-A�hsA�jA�+AЃAͰ!A�p�AƅA���A�~�A��mA�%A���A�&�A�p�A���A���A�;dA���A�~�A���A�7LA�E�A�ƨA�+A���Aȥ�A���AĴ9AƮA��/A�z�A�r�Aǥ�A� �A�bNA�t�A�1'A���A�?}A��A�A�JA�33A�&�A�?}AɋDA�jA��A���A���A�M�Aď\A�;dA�A��#Aß�A��A�$�A�Q�A���A�=qA�JA̸RA�dZA�XA�I�A�hsA�~�A�t�A���Aá�A�9XA�M�A�x�A�A�z�A�l�A�z�A�v�A�C�A�VAāA�S�AͬA��/A�dZA�n�A�AʃA��mA�M�A�1A�ĜA�v�A·+AÛ�A���A�/A�oAÃA�^5Aá�AA���A��PA�n�A��AȲ-Aƥ�A�jA��A�
=A�~�Aº^A���Aò-AŁAΕ�A�jAŕ�A� �AƏ\A�r�AǛ�A�hsA˶FA�M�A��A�VAϼjAκ^A�A�A�bNA�JA�t�A���A¸RAģ�AčPA�l�A�G�A�A�^5A�ƨAź^AŋDA��A�ȴAÙ�A��`AЍPA�"�A�dZAʛ�A���AЁA�oA�p�A�\)A���A�9XA�{AЃA�z�AhA�n�AɃA��A�v�A�Q�A�bNA�O�A��Aǟ�A�oA�~�A���A��A�oA�ffAΙ�A�%A×�A�A�-A�"�A���A� �A�/AļjAƏ\A�`BAЉ7A�z�A���AƶFA�~�A�hsAЏ\AϼjAʹ9Ḁ�AЇ+Aʙ�A�p�A�Q�AЋDA�z�Aϛ�AЋDA�?}AЋDA�jA�bAЋDAЉ7AЇ+A�
=AЋDA�?}AЇ+AЉ7A�G�A���A΅A�;dAЏ\AЏ\A�;dA�
=AЏ\AЇ+A�Q�AЉ7A�ffA�z�A�hsA͍PAЏ\AЋDAЁAύPAЁAЋDAЉ7AЁA�n�A�1A�dZA˙�A� �AЅAЁA�p�AЉ7A�A�AЉ7A�t�A�^5A�~�A̡�A�z�A�XAЅAЋDA�E�A�I�AЅAЃAϰ!AЍPAЍPA��AЏ\AГuAЍPAЇ+AЏ\AЋDAЏ\AБhAμjAЁA���A��AЍPAБhAБhA�I�A���AЅAЕ�AБhAЏ\A�AЏ\AϼjA���AЏ\AЋDA�p�A�bNAЋDAЅA�E�A̬AЋDAЉ7A�~�AЍPAЏ\AГuAЋDAЁA�t�A���AЏ\AЏ\AБhAБhA�ȴA�|�AЋDAБhAЍPAГuȂhAЕ�A��AЇ+Aϕ�AБhA�{AЉ7A��A�1'AЉ7AЇ+A�\)A�dZAϗ�AЅAЃA�A�&�A�`BA�t�AЋDAЋDAЍPAЍPAЋDAЋDAЇ+AЉ7AЉ7AЇ+A��A��A���AЏ\AЏ\AБhA���A�AЋDAЏ\AБhAЇ+AЍPAЍPAБhA�v�A��HA��A�7LAГuAБhAЋDAГuAГuAЏ\A�JAЏ\AЉ7A�  A�?}A�G�A�ZA�
=A�z�AЕ�AЕ�A�n�A��A��/AЅA�I�AЕ�AГuAЇ+A�(�A�XA��AБhAБhAμjAЅA�VA���AБhAГuA�v�AБhAЅA���AЕ�AГuAЕ�AЙ�AЗ�AБhA�=qAϣ�AЋDAЏ\AЙ�AЗ�AЛ�AЕ�A̟�A�AЗ�AΟ�AГuAБhAЙ�AЙ�AЕ�A�5?AЗ�AЙ�AЙ�AЋDAϲ-A˰!A�;dAЕ�A�t�A�A�Aϥ�AЗ�A�%AГuA��AЙ�A�z�A��AЉ7AЗ�A�v�AЕ�Aϕ�A�\)AϸRA�AЁAЕ�AЛ�AЛ�AН�AЛ�AН�AЙ�AЛ�AЛ�AН�AН�AЙ�AН�AН�AН�AЕ�AЕ�AЕ�AГuAЙ�AЕ�AЕ�AЕ�AБhAЍPAБhAЏ\AЏ\AГuAЕ�AБhAС�AН�AУ�AУ�AП�AС�AН�AЛ�AН�AН�AН�AН�AЛ�AЛ�AЛ�AЛ�AП�AП�AП�AН�AП�AП�AП�AН�AН�AН�AН�AН�AН�AН�AП�AП�AС�AС�AП�AП�AН�AН�AН�AН�AЛ�AН�AН�AН�AН�AН�AЛ�AЛ�AЛ�AЛ�AЛ�AН�AН�AП�AУ�AХ�AЧ�AЧ�AХ�AХ�AЧ�AХ�AХ�AХ�AХ�AХ�AЧ�AХ�AЩ�AЧ�AХ�AХ�AХ�AУ�AХ�AХ�AУ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AУ�AХ�AХ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЩ�AЩ�AЧ�AЩ�AЩ�AЩ�AХ�AЧ�AЧ�AЧ�AЩ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЩ�AЩ�AЩ�AЬAЬAЬAЬAЮAЮAЬAЬAЬAЮAЬAЬAЬAЮAЬAЬAЬAЬAЬAЬAЮAЮAа!AЮAв-Aа!AЬAа!AЮAЮAЮAЮAа!Aа!Aа!Aа!Aа!Aа!Aв-Aд9AжFAд9Aд9Aд9AжFAжFAд9AжFAд9Aд9Aв-Aв-Aв-Aв-Aв-Aд9Aд9Aд9AжFAд9Aд9AжFAжFAжFAжFAжFAжFAжFAд9AжFAжFAжFAжFAжFAжFAиRAжFAжFAжFAиRAжFAжFAиRAиRAиRAиRAжFAиR@��^@��-@��-@��-@���@���@���@���@���@���@���@��-@��^@�@��^@��-@��-@��-@��^@��-@���@���@���@��-@��-@��^@�@��^@��^@��-@��-@��-@��-@��-@���@���@��h@��7@��@�p�@�hs@�`B@�X@�?}@�/@�&�@�V@���@�Ĝ@��j@��9@���@���@���@���@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��D@��D@��@�z�@�z�@�r�@�j@�bN@�Z@�Z@�Q�@�A�@�A�@�9X@�1'@�1'@�(�@� �@�b@�  @�  @�@�;@�;@�;@�;@�;@��@��@�w@�w@�@�@��@�P@�P@|�@|�@|�@|�@�P@|�@�P@�P@�P@�@�@��@��@��@��@��@��AН�AН�AН�AН�AЛ�AЛ�AЛ�AЛ�AП�AП�AП�AН�AП�AП�AП�AН�AН�AН�AН�AН�AН�AН�AП�AП�AС�AС�AП�AП�AН�AН�AН�AН�AЛ�AН�AН�AН�AН�AН�AЛ�AЛ�AЛ�AЛ�AЛ�AН�AН�AП�AУ�AХ�AЧ�AЧ�AХ�AХ�AЧ�AХ�AХ�AХ�AХ�AХ�AЧ�AХ�AЩ�AЧ�AХ�AХ�AХ�AУ�AХ�AХ�AУ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AХ�AУ�AХ�AХ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЩ�AЩ�AЧ�AЩ�AЩ�AЩ�AХ�AЧ�AЧ�AЧ�AЩ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЧ�AЩ�AЩ�AЧ�AЧ�AЩ�AЩ�AЩ�AЬAЬAЬAЬAЮAЮAЬAЬAЬAЮAЬAЬAЬAЮAЬAЬAЬAЬAЬAЬAЮAЮAа!AЮAв-Aа!AЬAа!AЮAЮAЮAЮAа!Aа!Aа!Aа!Aа!Aа!Aв-Aд9AжFAд9Aд9Aд9AжFAжFAд9AжFAд9Aд9Aв-Aв-Aв-Aв-Aв-Aд9Aд9Aд9AжFAд9Aд9AжFAжFAжFAжFAжFAжFAжFAд9AжFAжFAжFAжFAжFAжFAиRAжFAжFAжFAиRAжFAжFAиRAиRAиRAиRAжFAиR@��^@��-@��-@��-@���@���@���@���@���@���@���@��-@��^@�@��^@��-@��-@��-@��^@��-@���@���@���@��-@��-@��^@�@��^@��^@��-@��-@��-@��-@��-@���@���@��h@��7@��@�p�@�hs@�`B@�X@�?}@�/@�&�@�V@���@�Ĝ@��j@��9@���@���@���@���@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��u@��D@��D@��@�z�@�z�@�r�@�j@�bN@�Z@�Z@�Q�@�A�@�A�@�9X@�1'@�1'@�(�@� �@�b@�  @�  @�@�;@�;@�;@�;@�;@��@��@�w@�w@�@�@��@�P@�P@|�@|�@|�@|�@�P@|�@�P@�P@�P@�@�@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=��i=�!�=�=�2=�?��K@�:�?er�=�Uq=��|?0M�=�Ȋ>
y�>.�>��@�|1=�Q/=�w2@XĜ?��=敖=�}�>%��?wp=�z�>l�@�*E=�S�>�@�3	?�E�>�1Q=�>.�k@�;�@ۡ=1�3=2=A�7=[a�=gw=i�4=j+=��m=���=�Q�=�H>���@��=�o=��> >�=��=�S;=���>Zu@�B=��=�e�>�@�I�@�9=�AJ=��">@N�@�0j@�0�=�k�>:u�@��w=ؽf>*A@vnn@_��=��8>��.@�7L=T�
=O�J=J82=o� =���=Se=gb=}��=�,==�t�>g?S�P@�>�@��.=�z�>��@�4@�1�>�9=�7a?0M�=�D=�P�=�s�?�NQ@�&-=���=���=��,=ȅ
>=,|?��Y?��+=���?-Y@�1<?�^>7h�>G��=௣?��=�iD>nP�@��=eә==��=H�H=Kۡ=Z��=���=� �=��=�ڥ=�[�=�a|>;/�@S"�=�?+C=l,�=S&=Y!=u�=��t=�$=��j?��?�֌=���?P&�?(S;=�T>��?���@�l>_>@g�>�e@F�@�@d=��>#��>)9�@�5+=�)_=Ō�>�=M5�=bl=l�D=�p�=���=���=�#�?�&>�?�^�=��>C�-@�I�?��r=�;y>{J@d�|@�Bp@@ߤ@�Go=�+�=���=��&>�f'@�JM@�Ft=���=ښ�>L�!@�G�>]�3@M��@�==��+=���>�/?��@@�JM>^��=�9?�?ϢI@��Y?�n�=ż>#9�@��?ҍ=�x�=��S>�X@|�$=�L�?��@�Ln@�M�>�ʂ=��=��>��(@�J�@�J#@��>��@�N'>$_@�|p@g�@�N<@��?�zx@�N<@Y��@�Ln@�L@M��@�M@�N<@�CW?H�@�N�@5�j@�M+@�J8@O��?�g8?�_@��D@�N�@�M�>�#�@,�k@�N<@�M+@aK�@�L@�Ln@��?�CW?L$_@�N�@�N'@�LY?��8@�L�@�K�@�Ln@�I�@�L�>���@+ �@�$t@���@�L�@�Go@�Ln@�M+@�Mj@�M�@�K^@&9�@�G�@@�@�j@?�t?@�Ln@�K
@�I=@��4@�K�@�I�?���@�N�@�L�?�X:@�O�@�Q@�M�@�N<@�M�@�N<@�Oa@�P@U��@�J�@�G�>B��@�N<@�O�@�O�@�N�@��K@��p@�Q�@�Q�@�P@�e�@�N�@�G?Q�k@�Oa@�N�@�N�@�J�@�L�@�M+?��>T�@�N�@�L�@�M@�N<@�N�@�M�@�M@�N�>�'�>��y@�P@�P@�N�@�P]@��@�L�@�N�@�O�@�P]@�P]>���@�O�@S�t@�M�@���@�O�@�N<@�O�?��<@G@�M@�N�@�M+@�@��@�M+@�L@�M�@�Ks>���@�L�@�N�@�O7@�P@�Oa@�M�@�N�@�M+@�N<@�M@�M?�I�?��@
U�@�P@�N�@�Oa@�h@n��@�N�@�P]@�P@�L@�M�@�O�@�P�@�N�>��@���@�N�@�P]@�Q/@�P�@�Q/@�Q�@�O�>\��@�Oa@�P?��$@���@\��@cd�>1�@>�[�@�Q�@�R*@�O�?Zs.@Y�a@�N�@��w@�Q�@�R�@�R*>�ƽ@M�F?��@�Q�@�Q�@Xs�@�O�>q�
>�#d@�Q�@�Q�@�Q�@�Q�@�R?@/	@�Q�@�R�@�R�@�R�@�R?@�S�@���@�R?@��F@�S�@�Ta@�S�@�R�@�SP@H2�?���@�Ta@&D�@�S�@�S�@�S�@�S�@�S�@��@�S�@�S�@�SP@�N�@��@��@k��@�R�@�SP@�R�?�,|@�S�@I=@�SP@4�1@�R�@�Q/?#}�@�S;@�S�@?�@�R�@Z�(@�R�@��@z?�@qu�@�S�@�U@�Uq@�U�@�U�@�U�@�T�@�U@�Uq@�V.@�U�@�T�@�Uq@�Uq@�T@�SP@�R�@�R�@�R*@�S;@�R�@�R�@�R?@�Q�@�Q�@�Q�@�R�@�R�@�T�@�Uq@�Uq@�W?@�Xd@�Xd@�X�@�Y!@�V�@�V.@�V�@�V�@�V.@�V.@�V.@�V.@�V.@�V.@�V�@�V�@�W�@�W�@�W�@�W?@�W�@�W�@�X@�WT@�WT@�WT@�W�@�W�@�W�@�W�@�X�@�Y!@�Y!@�X�@�Xd@�Xd@�Xd@�X@�Xd@�X@�X@�X�@�Xd@�Xd@�X�@�X@�Xd@�Xd@�Xd@�Xd@�Yu@�Z2@�Y�@�[�@�\�@�\@�]y@�]@�]%@�]@�]%@�\�@�\h@�]%@�]y@�]�@�]y@�]y@�]�@�]y@�]%@�\�@�]%@�]y@�]y@�]y@�]�@�]�@�]�@�]y@�]%@�]y@�]�@�^5@�]�@�^�@�^�@�^�@�_@�_F@�_F@�_�@�`@�`@�_�@�`@�_�@�_�@�`k@�`@�`@�_�@�_�@�_�@�`@�`-@�`@�`@�`k@�`�@�`k@�`k@�`�@�`k@�a(@�`�@�a(@�a�@�a(@�a�@�a�@�a�@�b�@�b�@�b9@�bc@�bN@�b�@�c@�d@�c�@�d@�do@�d@�do@�d@�c�@�d@�c�@�d�@�c^@�c�@�c�@�d@�d@�d@�d0@�do@�e,@�eV@�d�@�e�@�e�@�e�@�f<@�f�@�ff@�f<@�f<@�f<@�f�@�f�@�f�@�g#@�gM@�g�@�g�@�g�@�h
@�hs@�i@�i/@�i/@�i@�i/@�i/@�i/@�h�@�hs@�h�@�h�@�i/@�i/@�i�@�i�@�i�@�j@@�j@�j@@�j@@�j�@�j@�j�@�j�@�j@�j@�j�@�kf@�k�@�k�@�kf@�kf@�k�@�l@�l@�k�@�l@�k�@�k�@�k�@�lv@�l�@�mr@�m�@�m�@�m�@P�)@P� @P��@P��@P��@P��@P��@P�X@P��@P�}@P��@P�p@P��@P��@P�l@P��@P��@P��@P��@P�l@P��@P��@P�B@P��@P�h@P�c@P��@P�@P��@P��@P�l@P��@P�@P�p@P�O@P�X@P�@P�@P�i@P�"@P��@P�@P�@P��@P�V@P�c@P�y@P��@P��@P�+@P�^@P�b@P��@P��@P��@P��@P��@P�o@P��@P��@P�@P�@P��@P�o@P��@P��@P�o@P�@P$@P~|@P},@P|�@P|@Pzc@Px�@Pw�@Pvu@Pu�@Pt�@Pr�@Pq�@Ppe@Pp@Po@Pmr@Pj�@Pi�@Ph�@Ph
@Pgb@Pg@Pg@Pf�@Pff@Pf@PeA@Pdo@Pc�@Pc @Pb�@Pa�@PaR@PaR@Pa(@P`�@P`�@Pa�@Pa�@PbN@Pb�@Pc�@Pd�@Pe�@Pf�@Pg@Pf�@Pf�@Pf�@Pg@Pg�@�T�@�T�@�T@�T@�S�@�T@�S�@�T@�U�@�U�@�U�@�UG@�Uq@�U�@�U�@�T�@�U@�U2@�U2@�UG@�Uq@�U�@�V.@�U�@�W?@�W*@�W*@�Vm@�VC@�U�@�U�@�V@�Uq@�U�@�V@�U�@�U�@�UG@�U2@�U\@�U�@�U�@�U�@�Uq@�W @�V�@�W�@�Y�@�Y`@�Z\@�Z�@�Z�@�Z�@�Z2@�Z\@�Zq@�Z�@�Z2@�Z\@�[-@�[-@�[l@�[B@�Z�@�Z�@�Z\@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�[@�[�@�[�@�[�@�[�@�[�@�\@�\S@�\S@�\�@�\�@�]O@�]O@�]y@�]y@�]@�]%@�^�@�^@�\�@�\�@�\�@�\�@�]@�]O@�]%@�]O@�]�@�]O@�]�@�]�@�]�@�]�@�^ @�]�@�]�@�^5@�^J@�^J@�^t@�_@�^J@�^J@�^�@�_@�_@�^�@�`@�`-@�`k@�`k@�a@�`�@�`�@�`�@�`�@�`�@�`@�`�@�`k@�_�@�`W@�`�@�`�@�`�@�`W@�a@�b$@�b@�bc@�bc@�b�@�bN@�b�@�b�@�bc@�b�@�bN@�c^@�cI@�cI@�c^@�c^@�c�@�c�@�d�@�ek@�ek@�e�@�e�@�f@�e�@�e�@�e�@�e�@�ek@�e�@�e@�eA@�e@�eV@�eA@�e�@�e�@�f<@�ff@�f�@�f�@�f�@�f{@�f�@�gM@�ff@�f�@�f�@�g@�g#@�gM@�gM@�gM@�g�@�g�@�g�@�gb@�gM@�h
@�g�@�g�@�g�@�hI@�hs@�hI@�h�@�h@P��@P�@P��@P��@P�/@P��@P�@P�Y@P��@P��@P��@P��@P�m@P��@P�i@P�D@P��@P��@P��@P�i@P��@P�r@P�D@P��@P�;@P��@P�@P�.@P�\@P��@P�6@P��@P��@P��@P��@P�H@P�"@P��@P�U@P�8@P��@P�I@P�x@P��@P��@P�K@P�e@P�@P�@P}�@P}�@P{�@Pzc@Pzc@Px�@PxB@PwG@Pw�@PxB@Pxl@Px�@Px�@Px�@Px�@Px�@Pyh@Py@Px�@Pw@PvK@Pu�@Pt�@Ps�@Pr�@Pq@Pp@PnD@Pm�@Pl"@Pj�@PiY@Ph4@Pg�@Pg8@Pek@Pc @P`�@P_@P^�@P^@P]:@P]@P\�@P\�@P\>@P[l@PZq@PY�@PX�@PX�@PW*@PV�@PVX@PV@PU�@PU�@PV@PV�@PV�@PW @PW~@PX�@PZ�@P[�@P[�@P[B@PZ�@P[@P[B@P[�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              44444443444444443443444444434434444334444444444443444444434443344433443443344344444444444433443344444443444444444344444443444444444444344444444444444444443443444344444444444444434443343444433444344344443444434443444434433444433343433334333343334343344433344333333443334333334433333333343434333333433433333333333433333333333343333334433333333443333333333433333334433344333343333333333344433343333333334333333334334333443334333333444333344333334333333333333334434333333333334333343434334334333433333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�|2G�O�G�O�@XěG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�*GG�O�G�O�@�3G�O�G�O�G�O�G�O�@�;�@۫G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�BG�O�G�O�G�O�@�I�@�9G�O�G�O�G�O�@�0k@�0�G�O�G�O�@��xG�O�G�O�@vnk@_��G�O�G�O�@�7JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�>�@��.G�O�G�O�@�4@�1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�1>G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@S"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@g�"G�O�G�O�@�@fG�O�G�O�G�O�@�5)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�I�G�O�G�O�G�O�@d�v@�BrG�O�@�GqG�O�G�O�G�O�G�O�@�JL@�FqG�O�G�O�G�O�@�G�G�O�G�O�@�=G�O�G�O�G�O�G�O�@�JOG�O�G�O�G�O�G�O�@��YG�O�G�O�G�O�@��	G�O�G�O�G�O�G�O�@|� G�O�G�O�@�Lp@�M�G�O�G�O�G�O�G�O�@�J�@�J @��G�O�@�N$G�O�@�|l@g�@�N=@��G�O�@�N:@Y��@�Ll@�LG�O�@�M�@�N:@�CXG�O�@�N�G�O�@�M.@�J;G�O�G�O�G�O�@��C@�N�@�M�G�O�G�O�@�N=@�M,@aK�@�L@�Lm@��G�O�G�O�@�N�@�N%@�L[G�O�@�L�@�K�@�Lp@�I�@�L�G�O�G�O�@�$r@���@�L�@�Go@�Lr@�M-@�Mj@�M�@�K]G�O�@�G�G�O�@�jBG�O�@�Lr@�K@�I;@��6@�K�@�I�G�O�@�N�@�L�G�O�@�O�@�Q@�M�@�N=@�M�@�N:@�O`@�P!@U��@�J�@�G�G�O�@�N;@�O�@�O�@�N�@��J@��q@�Q�@�Q�@�P@�e�@�N�@�GG�O�@�Oc@�N�@�N�@�J�@�L�@�M.G�O�G�O�@�N�@�L�@�M�@�N;@�N�@�M�@�M�@�N�G�O�G�O�@�P@�P@�N�@�P^@��@�L�@�N�@�O�@�P]@�PcG�O�@�O�@S�z@�M�@���@�O�@�N=@�O�G�O�G�O�@�M~@�N�@�M)G�O�G�O�@�M(@�L@�M�@�KqG�O�@�L�@�N�@�O9@�P@�Ob@�M�@�N�@�M,@�N:@�M~@�MG�O�G�O�G�O�@�P@�N�@�ObG�O�@n��@�N�@�PZ@�P!@�L@�M�@�O�@�P�@�N�G�O�@���@�N�@�Pc@�Q1@�P�@�Q.@�Q�@�O�G�O�@�Oc@�PG�O�@���@\��@cd�G�O�G�O�@�Q�@�R/@�O�G�O�@Y�Z@�N�@��x@�Q�@�R�@�R0G�O�G�O�G�O�@�Q@�Q@Xs�@�O�G�O�G�O�@�Q�@�Q�@�Q�@�Q�@�RBG�O�@�Q�@�R�@�R�@�R�@�R=@�S�@���@�RB@��F@�S�@�Tb@�S�@�R�@�SSG�O�G�O�@�T]G�O�@�S�@�S�@�S�@�S�@�S�@��@�S�@�S�@�SQ@�N�@��G�O�@k��@�R�@�SS@�R�G�O�@�S�G�O�@�SNG�O�@�R�@�Q.G�O�@�S:@�S�G�O�@�R�@Z�%@�R�G�O�@z?�@qu�@�S�@�U@�Un@�U�@�U�@�U�@�T�@�U @�Uq@�V-@�U�@�T�@�Us@�Uq@�T@�SR@�R�@�R�@�R+@�S;@�R�@�R�@�RB@�Q�@�Q�@�Q�@�R�@�R�@�T�@�Us@�Ut@�WB@�Xa@�Xf@�X�@�Y$@�V�@�V.@�V�@�T�@�T�@�T@�T@�S�@�T@�S�@�T@�U�@�U�@�U�@�UK@�Ut@�U�@�U�@�T�@�U@�U1@�U2@�UD@�Uq@�U�@�V.@�U�@�WB@�W,@�W/@�Vm@�VA@�U�@�U�@�V@�Un@�U�@�V@�U�@�U�@�UH@�U2@�UZ@�U�@�U�@�U�@�Up@�W @�V�@�W�@�Y�@�Yb@�Z\@�Z�@�Z�@�Z�@�Z3@�Z[@�Zq@�Z�@�Z2@�Z^@�[*@�[-@�[j@�[A@�Z�@�Z�@�Z^@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�[@�[�@�[�@�[�@�[�@�\@�\@�\V@�\U@�\�@�\�@�]N@�]K@�]z@�]x@�]@�]%@�^�@�^@�\�@�\�@�\�@�\�@�]@�]Q@�]"@�]T@�]�@�]N@�]�@�]�@�]�@�]�@�^%@�]�@�]�@�^9@�^G@�^N@�^u@�_@�^N@�^M@�^�@�_@�_@�^�@�`@�`*@�`j@�`l@�a@�`�@�`�@�`�@�`�@�`�@�`@�`�@�`l@�_�@�`W@�`�@�`�@�`�@�`T@�a@�b&@�b@�be@�be@�b�@�bO@�b�@�b�@�be@�b�@�bT@�cb@�cM@�cJ@�ca@�cf@�c�@�c�@�d�@�em@�en@�e�@�e�@�f@�e�@�e�@�e�@�e�@�ej@�e�@�e@�eB@�e@�eU@�eB@�e�@�e�@�f>@�fj@�f�@�f�@�f�@�f|@�f�@�gQ@�fi@�f�@�f�@�g@�g(@�gT@�gR@�gR@�g�@�g�@�g�@�gd@�gO@�h@�g�@�g�@�g�@�hJ@�hs@�hK@�h�@�h@P��@P�~@P��@P��@P�3@P��@P�@P�]@P��@P��@P��@P��@P�n@P��@P�j@P�B@P��@P��@P��@P�j@P��@P�r@P�C@P��@P�:@P��@P�@P�.@P�`@P��@P�8@P��@P��@P��@P��@P�K@P�"@P��@P�V@P�:@P��@P�F@P�x@P��@P��@P�H@P�h@P�@P�@P}�@P}�@P{�@Pze@Pzf@Px�@PxB@PwK@Pw�@Px>@Pxk@Px�@Px�@Px�@Px�@Px�@Pyj@Py@Px�@Pw@PvM@Pu�@Pt�@Ps�@Pr�@Pq
@Pp@PnB@Pm�@Pl%@Pj�@PiX@Ph3@Pg�@Pg:@Pek@Pc@P`�@P_@P^�@P^
@P]8@P]@P\�@P\�@P\;@P[k@PZp@PY�@PX�@PX�@PW*@PV�@PVV@PV@PU�@PU�@PV@PV�@PV�@PV�@PW~@PX�@PZ�@P[�@P[�@P[B@PZ�@P[@P[@@P[�@�T�@�T�@�T@�T@�S�@�T@�S�@�T@�U�@�U�@�U�@�UK@�Ut@�U�@�U�@�T�@�U@�U1@�U2@�UD@�Uq@�U�@�V.@�U�@�WB@�W,@�W/@�Vm@�VA@�U�@�U�@�V@�Un@�U�@�V@�U�@�U�@�UH@�U2@�UZ@�U�@�U�@�U�@�Up@�W @�V�@�W�@�Y�@�Yb@�Z\@�Z�@�Z�@�Z�@�Z3@�Z[@�Zq@�Z�@�Z2@�Z^@�[*@�[-@�[j@�[A@�Z�@�Z�@�Z^@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�Z�@�[@�[�@�[�@�[�@�[�@�\@�\@�\V@�\U@�\�@�\�@�]N@�]K@�]z@�]x@�]@�]%@�^�@�^@�\�@�\�@�\�@�\�@�]@�]Q@�]"@�]T@�]�@�]N@�]�@�]�@�]�@�]�@�^%@�]�@�]�@�^9@�^G@�^N@�^u@�_@�^N@�^M@�^�@�_@�_@�^�@�`@�`*@�`j@�`l@�a@�`�@�`�@�`�@�`�@�`�@�`@�`�@�`l@�_�@�`W@�`�@�`�@�`�@�`T@�a@�b&@�b@�be@�be@�b�@�bO@�b�@�b�@�be@�b�@�bT@�cb@�cM@�cJ@�ca@�cf@�c�@�c�@�d�@�em@�en@�e�@�e�@�f@�e�@�e�@�e�@�e�@�ej@�e�@�e@�eB@�e@�eU@�eB@�e�@�e�@�f>@�fj@�f�@�f�@�f�@�f|@�f�@�gQ@�fi@�f�@�f�@�g@�g(@�gT@�gR@�gR@�g�@�g�@�g�@�gd@�gO@�h@�g�@�g�@�g�@�hJ@�hs@�hK@�h�@�h@P��@P�~@P��@P��@P�3@P��@P�@P�]@P��@P��@P��@P��@P�n@P��@P�j@P�B@P��@P��@P��@P�j@P��@P�r@P�C@P��@P�:@P��@P�@P�.@P�`@P��@P�8@P��@P��@P��@P��@P�K@P�"@P��@P�V@P�:@P��@P�F@P�x@P��@P��@P�H@P�h@P�@P�@P}�@P}�@P{�@Pze@Pzf@Px�@PxB@PwK@Pw�@Px>@Pxk@Px�@Px�@Px�@Px�@Px�@Pyj@Py@Px�@Pw@PvM@Pu�@Pt�@Ps�@Pr�@Pq
@Pp@PnB@Pm�@Pl%@Pj�@PiX@Ph3@Pg�@Pg:@Pek@Pc@P`�@P_@P^�@P^
@P]8@P]@P\�@P\�@P\;@P[k@PZp@PY�@PX�@PX�@PW*@PV�@PVV@PV@PU�@PU�@PV@PV�@PV�@PV�@PW~@PX�@PZ�@P[�@P[�@P[B@PZ�@P[@P[@@P[�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              44444443444444443443444444434434444334444444444443444444434443344433443443344344444444444433443344444443444444444344444443444444444444344444444444444444443443444344444444444444434443343444433444344344443444434443444434433444433343433334333343334343344433344333333443334333334433333333343434333333433433333333333433333333333343333334433333333443333333333433333334433344333343333333333344433343333333334333333334334333443334333333444333344333334333333333333334434333333333334333343434334334333433333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9xU[9xU9xTN9xTN9xT19xTN9xS�9xTQ9xV�9xVn9xV�9xV9xVO9xV�9xVk9xU�9xU�9xU�9xU�9xV
9xVJ9xVh9xWY9xV�9xX�9xX�9xX�9xW�9xWt9xW9xV�9xW 9xVF9xVk9xW 9xV�9xV�9xV9xU�9xV)9xVh9xVh9xV�9xVI9xX�9xX09xY�9x\Q9x[�9x]V9x]�9x]�9x]�9x]9x]U9x]t9x]�9x]9x]Y9x^}9x^�9x^�9x^�9x]�9x]�9x]Y9x]�9x^$9x^)9x^	9x^)9x]�9x]�9x]�9x^&9x^h9x_9x_9x_9x_�9x_�9x_�9x`+9x`*9x`�9x`�9xa�9xa�9xa�9xa�9xa39xaT9xcW9xb�9x`�9x`�9x`�9x`�9xa69xa�9xaO9xa�9xa�9xa�9xb9xb�9xb�9xb)9xb�9xbg9xb�9xb�9xb�9xb�9xc59xd9xb�9xb�9xc�9xd!9xd9xc�9xe�9xe�9xf9xf9xf�9xf�9xfB9xf`9xfa9xf�9xe�9xf�9xf9xeQ9xe�9xf`9xf�9xfd9xe�9xf�9xh9xh]9xh�9xh�9xi9xh�9xi�9xi9xh�9xi9xh�9xjD9xj&9xj!9xjB9xjI9xjz9xj�9xl9xm19xm29xmQ9xmO9xn 9xm�9xm�9xm�9xmm9xm-9xml9xl�9xl�9xl�9xm9xl�9xml9xme9xn\9xn�9xn�9xoO9xn�9xn�9xn�9xo�9xn�9xo9xo9xo�9xo�9xo�9xo�9xo�9xpA9xp�9xp>9xp9xo�9xp�9xp�9xp>9xp�9xqK9xq�9xqM9xq�9xq9vs9vQ9u�9u�9ud9u9uD9u�9u�9u�9v�9w�9y#9z9y�9y9x�9y}9z9y�9yC9xo9y9z39zm9{�9|m9|�9{�9{�9{#9{^9{z9{}9z9xS9w~9w#9v49s�9r69q%9p�9m*9jW9i�9e�9d9\�9Z�9Z�9X�9X)9X*9V�9V�9U�9V)9V�9V�9V�9V�9V�9V�9V�9Wu9W89V�9U�9U:9T�9T9SW9R�9Qu9P�9Ow9O!9M�9L�9K�9K 9J�9Jn9I"9G{9E�9D�9Dp9C�9CB9C&9C	9C9B�9A�9AD9@�9@89?�9>�9>r9>T9>9=�9=�9>9>t9>�9>�9?(9@9Ae9B59B9A�9A9A�9A�9BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�B�B��B`BBq�B~�B�7B�\B��B��B�B�dB�}B��B{B)�B1'B?}B0!B-B �BuB�B	7B+B	7BuBhBbBVB
=BB��B�B�ZB��B��B��BǮBǮBB�FB�B�bB�Bt�BffBZBN�BA�B>wB:^B0!B&�B!�B�B�B{B�B�BB�B�B�)B�}B��B��B�Bk�Bm�BaHBP�B=qB33B�B
�yB
�`B
�B
�5B
ƨB
��B
�B
�JB
z�B
gmB
H�B
,B
�B
B	�sB	��B	�dB	�!B	��B	�1B	iyB	I�B	(�B	uB��B�fB�BB�)B��B�
B�B��B�B�B�B�NB�B��B��B�#B��B�wB�!B��B��B��B��B��B�bB�VB�PB�DB�=B�+B�B{�Bz�Bx�Bu�Bs�Bq�Bo�Bn�Bk�BhsBe`BbNB_;B\)BYBW
BR�BP�BM�BK�BI�BH�BG�BD�B>wB:^B8RB7LB6FB49B2-B1'B1'B/B.B,B+B+B+B+B+B+B(�B(�B(�B(�B(�B(�B(�B(�B'�B'�B'�B'�B'�B'�B'�B'�B&�B&�B%�B%�B&�B&�B)�B(�B'�B(�B+B0!B1'B5?B7LB=qBH�BN�BN�BQ�BS�BXBXBW
BXBXBXBYBW
BXBXB^5BdZBffBe`BbNBaHBffBiyBhsBffBiyBo�Bs�Bu�Bw�B{�B�B�%B�7B�JB�PB�VB�VB�PB�PB�PB�bB�oB�hB�\B�uB�{B��B��B��B��B��B��B��B��B�B�wB��B��BBǮB��B��B�B�#B�ZB�B��B��B��B��B��B��B��B	B	B	B	B��B��B��B	  B	B	+B	uB	uB	uB	uB	{B	uB	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	&�B	)�B	)�B	(�B	(�B	'�B	$�B	"�B	$�B	!�B	�B	"�B	$�B	&�B	(�B	'�B	'�B	(�B	2-B	7LB	5?B	6FB	:^B	<jB	?}B	C�B	H�B	K�B	M�B	P�B	T�B	ZB	aHB	gmB	gmB	dZB	^5B	ZB	VB	ZB	aHB	ZB	ZB	ZB	XB	W
B	VB	W
B	[#B	hsB	e`B	dZB	dZB	cTB	dZB	hsB	l�B	n�B	s�B	w�B	x�B	z�B	|�B	}�B	�%B	�DB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�?B	�LB	�^B	ŢB	ŢB	ŢB	ǮB	ȴB	��B	ȴB	ŢB	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	��B	��B	�B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�HB	�BB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�yB	�yB	�sB	�fB	�fB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
JB
�B
!�B
'�B
1[B
7�B
=�B
E�B
IB
PB
U�B
[=B
`vB
e`B
hsB
k�B
n�B
q�B
v�B
zxG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�+>�xp?��>��J?]?�<A�B�}@��n>�i�>�D=@s[�>�@O?/?�?24D@��B	]�>�0?�A�{�A?�?j�?M0j@L�? 	-?�i#B�>��?=0{B��A-��@"�?�s?Z��B�!A�ڡ>Y�
>Y�9>l�>��b>��s>��>��o>�>�ژ?�M>��?��A�(H>Ո<>�m�?H�Z>��>��?�?�3�B�>ӻA>�r?DKA��VB��>��?�^?p�B��Bcg?�W?h�A�U?)c?UT"A�B�A���?��?�*�B�>��H>~X`>w`>�i�>���>��>�Ec>�(�>��$>ڐ?:J'@� �B�DB	G�? =W?<u�B�cB��?�z�>�z�@s-{?4;? �?�%@�$�B1�>ӆ�>�
>��>�I?qė@��pA�\?5H@h��B��@.�?fP�?��D?��A �?��?�շBqn>��#>g��>ur�>zK�>���>��T>�~�>���>�Љ>�
�?�g?j�A��X?+�@f]�>��1>�o>���>�N#>���>�� ?�^@���A�6?�j@�ͯ@f�>ܑ?.�R@���Ag��?��A��!?(��AY��B��>�?N��?R�9B�B>���>��?'F�>za>���>���>�w�>�fk>���?�@Ңw?B=�A :!?	��?ykB�@�c�>�K�?"��A�uB�A�uB�]>�K�>��?�@&ruB��BIu>�G�?�f?�{�B�?���A��BM�>���>�5?%�)@�K�B�?�}g?��@<èAЮB$@�Nl>�T�?N�nB��A5g>�H>��?El�A�,?9@>��B�B�
?ݿ�>԰:?1<?���B�B
a�A��?��PB��?8' B3�A��B�kA�4@��8B�bA���B��B��A���B�B�*B�@T-�B��A�r�B�B�A��1@�5@P�RBc5B�~B�~@#�EA�RB��B��A�<KB�0B��A��A1d@�}B�:B�WB�A*#�B��B� B�B�B�?��A��A���A�`B�vB�B��B�3B�B��B�hAyF5B�&AS9�B
7>@Ж�B�B�{B�tA��B�B�_AO}B�FB�V@�}�B�1B��B�CB��B�B�bB��B��A�V�B�B(�?tl3B�B�aB�B�A��B��B�mB��B�B2'B�B0�@���B��B�B��B��B�'B��A8�,?�_5B�B��B�oB�B�B��B�B�?�g@&qeB�B�B�B��A�p�B�B��B�FB�B�:?ղ_B��A�>&B�B��B�B�B�@ԯAg��B�B�RB�eAJl�AjvB��B�BsB3?�b B�B��B�OB�QB�B�B��B��B�*B�B��A��@3}JAK
;B�B�:B�Ac�-A���B��B��B��B�B�CB��B�YB�??NB ںB	-B�:B��B�B��B�=B�b?�ܠB��B��A;��A��A�c�A��D?^�Y@>%B�uB�B��@�k�A���B�nBX�B��B�4B�@�A�L	A*��B�B�A�tB�?�O�?���B�YB�vB�6B�YB�hA�z#B��B�=B�lB�)B�`B��A�k�BD�B�B�B�B�B�YB�A���@�1UB�PAy,�B�B�IB� B�)B�B
�B��B� B�B�B��AE�=A�.'B��B��Bj�A%XB��Ab�B��A�dB�)B�Y@Z��B��B�A��GB��A��B��Ad
�A�lpA��3B�dB�cB�B�+B��B�.B�B�BB�B�jB�B�B�B�B�ZB��B�B�B�|B�B�WB�OB�B��B�zB��B�BB�:B�B�uB�B�`B��B�B��B��B�B��B��B�'B�B�B�wB�oB�gB�_B�B�vB��B��B��B�VB�B�aB�4B�B�B�hB�tB��B�+B�B�B�B��B�mB�>B��B��B�aB��B�B�	B��B�EB�lB��B��B�B��B��B��B��B�.B�bB��B��B�B�`B��B��B��B��B�B��B��B��B�	B��B��B��B��B�9B�B�XB�dB�\B�LB��B�B�?B�SB��B�CB�uB��B�B�0B�aB�B�\B�B�B�B�B�B�#B�{B�B��B�nB��B�
B�jB�B�RB��B�B�XB�B�B�B��B�B�FB��B�oB�.B�WB�B�GB�fB��B�B�uB�B�mB�B�8B�|B�B��B�B��B��B�
B�B��B�yB�B�UB�B�B��B��B�qB�1B�(B�3B�dB� B�B�B�B�B��B�oB�B�}B�B�B��B�B�B�}B�B�B�\B�TB�LB��B�WB��B��B�"B�B��B�
B��B�mB�B�%B�B�)B�!B�B�B��B�hB�rB�B�PB�B�B��B�B�B�B�B�B�UB�B�1B��B�mB�B��B�VB�1B�B�B�<B��B��B��B�B�;B��B	�xB	�OB	��B	��B	�TB	�GB	��B	��B	��B	�9B	�"B	�IB	��B	��B	��B	�#B	�B	��B	��B	�mB	��B	�"B	�*B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	�\B	��B	�9B	��B	��B	�B	��B	��B	��B	�RB	�B	��B	�B	��B	��B	�EB	�<B	��B	�HB	��B	�B	�B	�B	��B	��B	�AB	�FB	�WB	��B	�{B	�#B	��B	�B	��B	��B	��B	��B	�eB	�rB	�'B	��B	�AB	�"B	�=B	�hB	��B	�.B	��B	��B	��B	��B	��B	��B	��B	��B	�KB	��B	�>B	��B	��B	��B	�TB	�	B	�rB	��B	�FB	��B	��B	��B	�vB	�[B	�/B	�B	�B	�hB	�zB	��B	��B	��B	�3B	��B	��B	��B	�`B	�RB	�IB	�zB	��B�B�BB��B��B�lB�wB�6B�_B�LB�B�4B�B��B�.B��B�'B�2B�PB�@B�KB�iB�tB�4B��B�TB�0B��B�=B��B�B�LB�}B�B��B�\B�B�1B�B�=B�SB�qB�iB�tB�uB��B��B�B��B��B��B�B�B��B�5B�KB�VB�`B�B�ZB��B�8B�1B��B�5B�B�B��B�FB��B�B�B��B��B��B��B�B�uB�lB�\B�{B�B�B�B�B�B�B��B��B�B�B�iB�B��B�eB�B�B��B��B�PB�AB�B�1B�ZB�B�\B��B��B�OB�B�]B�hB�B�B�B��B�gB�{B�kB��B�QB�6B�SB�UB�XB�B�B�IB��B�B�B��B�B��B�B�`B�B�B�IB�fB�8B��B�B�B��B��B�8B�B�lB�QB��B�B�B�tB�B�B�pB�{B�rB�B�B��B��B�B�B�B�OB�B��B��B�|B�MB�3B�B��B�B��B��B�;B�2B��B��B��B�{B�&B�B�B�B��B�B��B�XB�cB�B�yB�qB�B�B�B�cB�PB�#B�B�qB��B�DB�bB�4B�@B��B	�|B	�SB	��B	��B	�9B	��B	�B	�0B	�4B	�eB	�B	�FB	�mB	�FB	�B	�8B	��B	��B	�B	��B	�*B	�EB	��B	��B	�B	�!B	��B	��B	�IB	��B	�gB	��B	��B	��B	�2B	�gB	��B	�-B	�:B	�B	�-B	�B	��B	�B	�PB	��B	��B	�/B	�/B	��B	��B	�%B	�@B	�3B	��B	��B	��B	�B	�gB	�yB	��B	��B	�cB	�VB	�gB	��B	�}B	�$B	�B	�^B	��B	� B	�kB	��B	�wB	��B	�bB	��B	��B	��B	��B	��B	�\B	�"B	��B	�*B	�QB	�.B	�B	�pB	��B	��B	�pB	�UB	��B	�6B	��B	��B	�TB	�	B	��B	��B	�GB	�B	��B	��B	��B	�B	�9B	�KB	��B	��B	��B	�fB	�KB	� B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444443444444443443444444434434444334444444444443444444434443344433443443344344444444444433443344444443444444444344444443444444444444344444444444444444443443444344444444444444434443343444433444344344443444434443444434433444433343433334333343334343344433344333333443334333334433333333343434333333433433333333333433333333333343333334433333333443333333333433333334433344333343333333333344433343333333334333333334334333443334333333444333344333334333333333333334434333333333334333343434334334333433333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B�lB�nB�jB�jB�jB�jB�nB�lB�jB�lB�lB�lB�lB�nB�jB�B�B��B`DBq�BB�=B�^B��B��B�B�jB��B��B�B* B1-B?�B0&B-B �BzB�B	@B/B	=B|BlBgB\B
BBB��B�B�_B��B��B��BǴBǴBB�LB�B�fB�Bt�BflBZ$BN�BA�B>zB:aB0$B&�B!�B�B�BB�B�B'B�B�B�/B��B��B��B�Bk�Bm�BaMBP�B=yB3:B�B
�B
�hB
�B
�8B
ƱB
��B
�B
�PB
z�B
gqB
H�B
,B
�B
B	�yB	��B	�iB	�'B	��B	�8B	i�B	I�B	(�B	zB�B�oB�GB�1B��B�B�B��B�B�B�B�WB�!B��B�B�*B��B�~B�'B��B��B��B��B��B�iB�]B�WB�JB�DB�3B�B{�Bz�Bx�Bu�Bs�Bq�Bo�Bn�Bk�BhyBeiBbVB_CB\1BYBWBR�BP�BM�BK�BI�BH�BG�BD�B>B:hB8XB7TB6LB4BB25B1/B1/B/B.B,B+
B+	B+B+
B+	B+B(�B(�B(�B) B(�B) B(�B(�B'�B'�B'�B'�B'�B'�B'�B'�B&�B&�B%�B%�B&�B&�B*B(�B'�B(�B+B0)B1/B5GB7SB=vBH�BN�BN�BQ�BS�BXBXBWBXBXBXBYBWBXBXB^;BdaBfnBeiBbTBaQBflBi�BhxBfoBi�Bo�Bs�Bu�Bw�B{�B�B�.B�>B�RB�ZB�]B�^B�XB�YB�VB�hB�uB�rB�cB�{B��B��B��B��B��B��B��B��B��B�!B��B��B��BBǶB��B��B�B�*B�bB�B��B��B��B�B��B��B�B	B	B	B	B��B��B�B	 B	!B	2B	{B	}B	}B	{B	�B	}B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	&�B	*B	*B	(�B	(�B	'�B	$�B	"�B	$�B	!�B	�B	"�B	$�B	&�B	(�B	'�B	'�B	(�B	24B	7TB	5HB	6NB	:fB	<rB	?�B	C�B	H�B	K�B	M�B	P�B	UB	Z$B	aQB	gtB	gsB	d`B	^>B	Z%B	V
B	Z'B	aOB	Z$B	Z&B	Z%B	XB	WB	VB	WB	[-B	hzB	eiB	dbB	daB	cZB	dbB	h{B	l�B	n�B	s�B	w�B	x�B	z�B	|�B	}�B	�-B	�KB	�_B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�:B	�IB	�TB	�fB	ūB	ŪB	ũB	ǵB	ȻB	��B	ȻB	ŪB	��B	��B	B	ĢB	ŪB	ưB	ǷB	ǶB	ǷB	ǶB	ȽB	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	��B	�B	�1B	�7B	�9B	�5B	�8B	�6B	�<B	�EB	�KB	�OB	�RB	�QB	�JB	�]B	�^B	�[B	�ZB	�aB	�jB	�gB	�bB	�bB	�dB	�cB	�iB	�oB	�zB	�B	�B	�|B	�mB	�mB	�oB	�|B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
RB
�B
!�B
'�B
1dB
7�B
=�B
E�B
IB
PB
U�B
[FB
`B
eiB
hyB
k�B
n�B
q�B
v�B
zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	]�G�O�G�O�A�{�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�B��G�O�G�O�G�O�G�O�B�&A�ڬG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�(SG�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�A��_B��G�O�G�O�G�O�B��BclG�O�G�O�A�`G�O�G�O�A�B�A���G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�GB	G�G�O�G�O�B�iB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BqqG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��+G�O�G�O�B��G�O�G�O�G�O�B�DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�A�u"B�G�O�B�aG�O�G�O�G�O�G�O�B��BIyG�O�G�O�G�O�B�G�O�G�O�BM�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�B)G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�A�,G�O�G�O�B�B�G�O�G�O�G�O�G�O�B�B
a�A��G�O�B��G�O�B3�A��B�oA�4G�O�B�fA���B��B��G�O�B��B�+B�G�O�B��G�O�B�	B�G�O�G�O�G�O�Bc8B�B�G�O�G�O�B��B��A�<NB�7B��A��G�O�G�O�B�@B�[B�G�O�B��B�B�B�$B�G�O�G�O�A���A�iB�zB�!B��B�8B�B��B�lG�O�B�+G�O�B
7BG�O�B�$B�B�xA��B�B�cG�O�B�LB�ZG�O�B�5B��B�JB� B�B�fB��B��A�V�B�#B(�G�O�B�B�gB�BA��B��B�tB�B�B2*B�B0�G�O�B��B�B��B�B�,B��G�O�G�O�B�B��B�sB�B�B��B�B�G�O�G�O�B�B�B�B��A�p�B�B��B�JB�B�@G�O�B��A�>/B�B��B�B�B�G�O�G�O�B�B�VB�iG�O�G�O�B��B�BzB8G�O�B�B��B�VB�TB�B�B��B��B�+B�B��G�O�G�O�G�O�B�B�@B�"G�O�A���B��B��B��B�B�JB��B�^B�G�O�B ڿB	2B�@B��B�B��B�CB�gG�O�B��B��G�O�A��A�c�A��IG�O�G�O�B�zB�B��G�O�A���B�sBX�B��B�9B�G�O�G�O�G�O�B�B�A�tB�G�O�G�O�B�]B�|B�<B�]B�nG�O�B��B�@B�oB�.B�dB��A�k�BD�B�B�B�B�B�^B�G�O�G�O�B�RG�O�B�B�QB�&B�0B�B
�B��B�&B�B�B��G�O�A�.0B��B��Bj�G�O�B��G�O�B��G�O�B�.B�^G�O�B��B�G�O�B��A��B��G�O�A�lxA��;B�hB�gB�B�3B��B�3B�B�GB�B�oB�B�B�B�B�^B��B�B�B�B�B�[B�RB�B��B�~B��B�GB�>B�B�xB�B�gB��B�B��B��B�B��B��B�B�HB��B��B�sB�|B�;B�eB�QB�$B�9B�B��B�2B��B�,B�7B�VB�FB�MB�nB�wB�7B��B�ZB�7B��B�BB��B�B�OB�B�B��B�aB� B�4B�B�BB�XB�uB�nB�{B�yB��B��B�B� B��B��B�B�B��B�:B�OB�ZB�eB�	B�`B��B�>B�4B��B�:B�B�B��B�HB�B�B� B��B��B��B��B�B�wB�sB�cB�~B�B�B�B�B�B�B��B��B�B�B�jB�B��B�jB��B�B��B��B�SB�HB�B�7B�^B�B�`B��B��B�VB�B�cB�nB�B�B�B��B�lB�B�pB� B�VB�:B�YB�ZB�]B�B�B�MB��B�B�B��B�B��B�B�dB�B�B�MB�nB�?B��B�B�B��B��B�>B�!B�rB�XB��B�B�B�{B�B�B�uB�B�{B�B�B��B��B�B�B�B�SB� B��B� B�B�QB�8B��B��B�B��B��B�BB�4B� B��B�B�B�+B�B�B�B��B�!B��B�ZB�hB�B�B�wB�B�"B�B�hB�SB�)B�B�wB��B�JB�gB�;B�DB�B	�B	�[B	��B	��B	�BB	��B	�B	�9B	�<B	�kB	�B	�MB	�uB	�NB	�B	�>B	��B	��B	�B	��B	�2B	�NB	��B	��B	�B	�)B	��B	�B	�RB	��B	�oB	��B	��B	��B	�<B	�oB	��B	�3B	�@B	�B	�5B	�!B	��B	�'B	�VB	��B	��B	�5B	�6B	��B	��B	�.B	�HB	�<B	��B	��B	��B	�B	�mB	�B	��B	��B	�kB	�^B	�oB	��B	��B	�-B	�	B	�gB	��B	�*B	�rB	��B	�~B	��B	�iB	�B	��B	��B	��B	��B	�eB	�*B	��B	�0B	�ZB	�7B	�B	�wB	��B	��B	�zB	�]B	��B	�=B	��B	��B	�]B	�B	��B	��B	�MB	�B	��B	��B	��B	�B	�@B	�SB	��B	��B	��B	�nB	�RB	�	B	��B	��B	��B	�B�B�HB��B��B�sB�|B�;B�eB�QB�$B�9B�B��B�2B��B�,B�7B�VB�FB�MB�nB�wB�7B��B�ZB�7B��B�BB��B�B�OB�B�B��B�aB� B�4B�B�BB�XB�uB�nB�{B�yB��B��B�B� B��B��B�B�B��B�:B�OB�ZB�eB�	B�`B��B�>B�4B��B�:B�B�B��B�HB�B�B� B��B��B��B��B�B�wB�sB�cB�~B�B�B�B�B�B�B��B��B�B�B�jB�B��B�jB��B�B��B��B�SB�HB�B�7B�^B�B�`B��B��B�VB�B�cB�nB�B�B�B��B�lB�B�pB� B�VB�:B�YB�ZB�]B�B�B�MB��B�B�B��B�B��B�B�dB�B�B�MB�nB�?B��B�B�B��B��B�>B�!B�rB�XB��B�B�B�{B�B�B�uB�B�{B�B�B��B��B�B�B�B�SB� B��B� B�B�QB�8B��B��B�B��B��B�BB�4B� B��B�B�B�+B�B�B�B��B�!B��B�ZB�hB�B�B�wB�B�"B�B�hB�SB�)B�B�wB��B�JB�gB�;B�DB�B	�B	�[B	��B	��B	�BB	��B	�B	�9B	�<B	�kB	�B	�MB	�uB	�NB	�B	�>B	��B	��B	�B	��B	�2B	�NB	��B	��B	�B	�)B	��B	�B	�RB	��B	�oB	��B	��B	��B	�<B	�oB	��B	�3B	�@B	�B	�5B	�!B	��B	�'B	�VB	��B	��B	�5B	�6B	��B	��B	�.B	�HB	�<B	��B	��B	��B	�B	�mB	�B	��B	��B	�kB	�^B	�oB	��B	��B	�-B	�	B	�gB	��B	�*B	�rB	��B	�~B	��B	�iB	�B	��B	��B	��B	��B	�eB	�*B	��B	�0B	�ZB	�7B	�B	�wB	��B	��B	�zB	�]B	��B	�=B	��B	��B	�]B	�B	��B	��B	�MB	�B	��B	��B	��B	�B	�@B	�SB	��B	��B	��B	�nB	�RB	�	B	��B	��B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444443444444443443444444434434444334444444444443444444434443344433443443344344444444444433443344444443444444444344444443444444444444344444444444444444443443444344444444444444434443343444433444344344443444434443444434433444433343433334333343334343344433344333333443334333334433333333343434333333433433333333333433333333333343333334433333333443333333333433333334433344333343333333333344433343333333334333333334334333443334333333444333344333334333333333333334434333333333334333343434334334333433333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455222020082814552220200828145522202008281455222020082814552220200828145522202008281455222020082814552220200828145522202008281455222020082814552220200828145522AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730442019021417304420190214173044    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730442019021417304420190214173044  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730442019021417304420190214173044  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455222020082814552220200828145522  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                