CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  Z   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:43Z creation      
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
resolution        =���   axis      Z        (8  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  mH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (8  wX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (8  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (8  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (8     TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ,X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (8 6h   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (8 ^�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (8 ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �    CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (8 �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (8 �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (8 �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 E�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (8 O�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � x0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   x�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �Argo profile    3.1 1.2 19500101000000  20190214173043  20200828145518  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               >   >   >AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @������D@������D@������D111 @����v`@����v`@����v`@6BM���@6BM���@6BM����c8bM���c8bM���c8bM��111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    >   >   >ADA BDA  DA BDA @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Dy�qD��D�B�D���D��\D��D�P�D��RD��=D���D�T�D���DǹHD�D�@�Dڄ)D�{D�� D�2�D�}D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���        >L��=���                =���>L��        =���>L��        >���>���>���    =���>L��                            =���>L��>L��    =���=���    =���        =���                =���    =���=���    >L��>L��    >L��=���=���=���    =���    =���        =���    =���        =���    =���    =���        =���>L��        >L��=���        =���>L��        =���>L��=���    =���                =���            =���>L��=���    =���>L��        >L��                =���            >L��            =���>���>L��        =���>L��=���        =���>L��                =���=���        >L��>L��=���        =���>���>���            =���        =���            =���    =���    =���>L��    =���            =���=���    =���            =���>L��>���=���    =���=���=���        =���=���        >L��=���    >L��>L��=���=���    =���>L��=���>L��=���        >L��>L��>L��=���>L��=���=���=���>L��=���>L��=���=���=���=���>���=���>L��=���>���>L��=���>L��>L��>L��>L��>L��>���=���>���>���>���>���=���>���>L��>L��>L��>���>���>L��>L��>L��>���>���>���>L��>���>L��>���>L��>L��>���>L��>���>���>���>���>���>���>���>���>���>L��=���>L��>L��>���>L��>���>���>���>���>���>���>���>���>���>L��>���>L��>L��>���>L��=���>���>���>���>���>���>���>���>L��>���>L��>���>L��>L��>���>���>���>���>���>L��>���>���>���>���>���>���>L��>L��?   >L��>���>���>���>���>L��>���>L��>���>���>���>���>L��>L��>���>���>���>���>���>���>L��=���>���>���>���>���>���>L��>���>���>���>���>���>���>L��>���>���>���>���>L��>L��>���>���>���>���>���>L��>���>���>L��>���>���>L��>���>L��>���>���>���>L��>���>L��>���>���>���>L��>L��>���>���>���>���>���>L��>���>���>L��>���>���>L��>���>���>���>���>���>L��>L��>���>���>���>���>L��>���>���>���>���>���=���>L��>L��>���>L��>���>���>L��>���>���>���>���>���>L��>L��>���>���>���>���>L��>���>L��>���>L��>���>���>���>���>���>���>L��>���>���>���>���>���>L��>���>���>���>���>���>���>L��>���>���>���>L��>L��=���>���>L��>L��>���>L��>���>���>���>���>���?   >���?   ?   ?��?��?��?333?��?L��?333?L��?fff?L��?L��?fff?fff?�  ?�  ?�  ?�  ?fff?���?���?���?���?���?���?���?�ff?�ff?�33?�33?�  ?�  ?�  ?���?���?���?�ff?�ff?�ff?�ff?�33@   @   @ff@ff@ff@��@33@33@��@��@   @   @   @&ff@333@333@333@9��@@  @@  @Fff@S33@Y��@Y��@`  @fff@l��@s33@y��@�  @�33@�ff@���@�  @�  @�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@���@���@�  @�33@陚@���@�  @�33@���@���A   A��A��AffA  A	��A��AffA  A��A��AffA  A33A��AffA   A#33A$��A&ffA(  A+33A,��A0  A1��A333A4��A6ffA9��A;33A<��A@  AA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A^ffA`  Aa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A͙�A�ffA�33A���Aљ�A�ffA�  A���Aՙ�A�33A�  A���Aٙ�A�33A�  A���A�ffDq@ DqFfDqL�DqY�Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Dr��Ds  DsfDs3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt�3@333@9��@@  @@  @Fff@S33@Y��@Y��@`  @fff@l��@s33@y��@�  @�33@�ff@���@�  @�  @�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@ə�@���@�  @�33@�ff@���@���@�  @�33@陚@���@�  @�33@���@���A   A��A��AffA  A	��A��AffA  A��A��AffA  A33A��AffA   A#33A$��A&ffA(  A+33A,��A0  A1��A333A4��A6ffA9��A;33A<��A@  AA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A^ffA`  Aa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A͙�A�ffA�33A���Aљ�A�ffA�  A���Aՙ�A�33A�  A���Aٙ�A�33A�  A���A�ffDq@ DqFfDqL�DqY�Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Dr��Ds  DsfDs3Ds�Ds  Ds&fDs33Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @(Q�@hQ�@�\)@�\)A�A;�A[�A{�A��
A��
A��
A��
A��
Aޣ�A��
A��
B�B�B�B�B'Q�B.�B6�B>�BF�BO�RBV�B^�Bf�Bn�Bv�B~�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD n�D �Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D	n�D	�D
n�D
�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D n�D �D!n�D!�D"n�D"�D#n�D#�D$n�D$�D%n�D%�D&n�D&�D'n�D'�D(n�D(�D)n�D)�D*n�D*�D+n�D+�D,n�D,�D-n�D-�D.n�D.�D/n�D/�D0n�D0�D1n�D1�D2n�D2�D3n�D3�RD4n�D4�D5n�D5�D6n�D6�D7n�D7�D8n�D8�D9n�D9�D:n�D:�D;n�D;�D<n�D<�D=n�D=�D>n�D>�D?n�D?�D@n�D@�DAn�DA�DBn�DB�DCn�DC�DDn�DD�DEn�DE�DFn�DF�DGn�DG�DHn�DH�DIn�DI�DJn�DJ�DKn�DK�DLn�DL�DMn�DM�DNn�DN�DOn�DO�DPn�DP�DQn�DQ�DRn�DR�DSn�DS�DTn�DT�DUn�DU�DVn�DV�DWn�DW�DXn�DX�DYn�DY�DZn�DZ�D[n�D[�D\n�D\�D]n�D]�D^n�D^�D_n�D_�D`n�D`�Dan�Da�Dbn�Db�Dcn�Dc�Ddn�Dd�Den�De�Dfn�Df�Dgn�Dg�Dhn�Dh�Din�Di�Djn�Dj�Dkn�Dk�Dln�Dl�Dmn�Dm�Dnn�Dn�Don�Do�Dpn�Dp�Dqn�Dq�Drn�Dr�Dsn�Ds�RDthRDy�)D�
=D�9�D�|)D���D��D�H D�w�D�əD���D�L)D�x Dǰ�D��{D�8RD�{�D��D��\D�)�D�t{D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��.{��=q��=q��\)�.{��=q��=q��=q��=q�.{��\)��=q��=q�.{��\)��=q��=q<�>�<���=q�.{��\)��=q��=q��=q��=q��=q��=q��=q�.{��\)��\)��=q�.{�.{��=q�.{��=q��=q�.{��=q��=q��=q��=q�.{��=q�.{�.{��=q��\)��\)��=q��\)�.{�.{�.{��=q�.{��=q�.{��=q��=q�.{��=q�.{��=q��=q�.{��=q�.{��=q�.{��=q��=q�.{��\)��=q��=q��\)�.{��=q��=q�.{��\)��=q��=q�.{��\)�.{��=q�.{��=q��=q��=q��=q�.{��=q��=q��=q�.{��\)�.{��=q�.{��\)��=q��=q��\)��=q��=q��=q��=q�.{��=q��=q��=q��\)��=q��=q��=q�.{<���\)��=q��=q�.{��\)�.{��=q��=q�.{��\)��=q��=q��=q��=q�.{�.{��=q��=q��\)��\)�.{��=q��=q�.{<�<���=q��=q��=q�.{��=q��=q�.{��=q��=q��=q�.{��=q�.{��=q�.{��\)��=q�.{��=q��=q��=q�.{�.{��=q�.{��=q��=q��=q�.{��\)<��.{��=q�.{�.{�.{��=q��=q�.{�.{��=q��=q��\)�.{��=q��\)��\)�.{�.{��=q�.{��\)�.{��\)�.{��=q��=q��\)��\)��\)�.{��\)�.{�.{�.{��\)�.{��\)�.{�.{�.{�.{<��.{��\)�.{<���\)�.{��\)��\)��\)��\)��\)<��.{<�<�<�<��.{<���\)��\)��\)<�<���\)��\)��\)>�>�<���\)<���\)<���\)��\)<���\)<�<�<�>�<�>�>�>�<���\)�.{��\)��\)<���\)<�<�<�>�>�>�<�<�<���\)<���\)��\)<���\)�.{>�<�<�>�<�<�<���\)<���\)<���\)��\)<�<�<�<�<���\)<�<�>�<�<�>���\)��\)>k���\)>�<�<�<���\)<���\)<�<�<�<���\)��\)<�<�<�<�<�<���\)�.{<�<�<�<�<���\)<�>�>�<�<�<���\)<�<�<�<���\)��\)<�<�<�<�<���\)<�<���\)>�<���\)<���\)<�<�<���\)<���\)<�<�<���\)��\)<�<�<�>�<���\)<�<���\)>�<���\)<�<�<�<�<���\)��\)<�<�>�<���\)<�>�<�<�<��.{��\)��\)<���\)<�<���\)<�>�<�>�<���\)��\)<�<�<�<���\)<���\)<���\)<�>�<�>�<�>���\)<�<�<�>�<���\)<�<�<�>�>�<���\)<�<�<���\)��\)�.{<���\)��\)<���\)<�<�<�>�<�>k�>�>k�>k�>���>���>���>�(�>���?�>�(�?�?!G�?�?�?!G�?!G�?:�H?:�H?:�H?:�H?!G�?Tz�?Tz�?Tz�?Tz�?n|?n|?n|?��
?��
?���?���?�p�?�p�?�p�?�=q?�=q?�=q?��
?��
?��
?��
?У�?�p�?�p�?�=p?�=p?�=p?�
>@�@�@Q�@Q�@�R@�R@�R@�@!�@!�@!�@(Q�@.�R@.�R@5�@A�@HQ�@HQ�@N�R@U�@[�@a�@hQ�@n�R@u�@{�@�(�@�\)@�\)@�@���@�(�@�\)@��\@�@���@�(�@�\)@��\@�@���@�(�@�\)@��\@���@�(�@�\)@ʏ\@�@�(�@�(�@�\)@ڏ\@���@�(�@�\)@�\@���@�(�@�\)@��]A z�AzA�AG�Az�A
zA�AG�Az�AzA�A�GAz�AzA�A�GA z�A"zA#�A&�GA(z�A+�A-G�A.�GA0z�A2zA5G�A6�GA8z�A;�A=G�A>�GA@z�AC�AEG�AF�GAHz�AK�AMG�AN�GAPz�AS�AUG�AV�GAZzA[�A]G�A^�GA`z�Ac�AeG�Af�GAjzAk�AmG�An�GArzAs�AuG�Av�GAzzA{�A}G�A~�HA�
=A��
A���A�p�A�
=A��
A���A�=pA�
=A��
A���A�=pA�
=A��
A���A�=pA�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA��
A���A�p�A�=pA��
A���A�p�A�=pA��
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
A£�A�=pA�
=A��
Aƣ�A�=pA�
=Aʣ�A�p�A�=pA�
=AΣ�A�p�A�=pA��
Aң�A�p�A�
=A��
A֣�A�p�A�
=A��
Aڣ�A�=pDq.�Dq5Dq;�DqHRDqN�DqUDq[�DqhRDqn�DquDq{�Dq�RDq��Dq�Dq��Dq��Dq��Dq�Dq��Dq��DqθDq�DqۅDq��Dq�Dq�Dq��Dr�DrRDrDr�Dr!�Dr(RDr5Dr;�DrA�DrHRDrN�Dr[�Dra�DrhRDrn�Dr{�Dr��Dr�RDr��Dr��Dr��Dr�RDr��Dr�Dr��Dr�RDrθDrۅDr��Dr�RDr�Dr�Ds�DsRDs�DsDs!�Ds(RDs.�Ds5DsA�DsHRDsN�DsUDsa�DshRDsn�DsuDs��Ds�RDs��Ds�Ds��Ds�RDs��Ds�Ds��Ds�RDsθDs�Ds��Ds�RDs�Ds�Ds��DtRDt�DtDt!�Dt(RDt.�Dt5DtA�DtHRDtN�DtUDta�DthRDtn�DtuDt��Dt�RDt��Dt�Dt��Dt�RDt��Dt�Dt��Dt�RDtθDt�Dt��@!�@(Q�@.�R@.�R@5�@A�@HQ�@HQ�@N�R@U�@[�@a�@hQ�@n�R@u�@{�@�(�@�\)@�\)@�@���@�(�@�\)@��\@�@���@�(�@�\)@��\@�@���@�(�@�\)@��\@���@�(�@�\)@ʏ\@�@�(�@�(�@�\)@ڏ\@���@�(�@�\)@�\@���@�(�@�\)@��]A z�AzA�AG�Az�A
zA�AG�Az�AzA�A�GAz�AzA�A�GA z�A"zA#�A&�GA(z�A+�A-G�A.�GA0z�A2zA5G�A6�GA8z�A;�A=G�A>�GA@z�AC�AEG�AF�GAHz�AK�AMG�AN�GAPz�AS�AUG�AV�GAZzA[�A]G�A^�GA`z�Ac�AeG�Af�GAjzAk�AmG�An�GArzAs�AuG�Av�GAzzA{�A}G�A~�HA�
=A��
A���A�p�A�
=A��
A���A�=pA�
=A��
A���A�=pA�
=A��
A���A�=pA�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA��
A���A�p�A�=pA��
A���A�p�A�=pA��
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
A£�A�=pA�
=A��
Aƣ�A�=pA�
=Aʣ�A�p�A�=pA�
=AΣ�A�p�A�=pA��
Aң�A�p�A�
=A��
A֣�A�p�A�
=A��
Aڣ�A�=pDq.�Dq5Dq;�DqHRDqN�DqUDq[�DqhRDqn�DquDq{�Dq�RDq��Dq�Dq��Dq��Dq��Dq�Dq��Dq��DqθDq�DqۅDq��Dq�Dq�Dq��Dr�DrRDrDr�Dr!�Dr(RDr5Dr;�DrA�DrHRDrN�Dr[�Dra�DrhRDrn�Dr{�Dr��Dr�RDr��Dr��Dr��Dr�RDr��Dr�Dr��Dr�RDrθDrۅDr��Dr�RDr�Dr�Ds�DsRDs�DsDs!�Ds(RDs.�Ds5DsA�DsHRDsN�DsUDsa�DshRDsn�DsuDs��Ds�RDs��Ds�Ds��Ds�RDs��Ds�Ds��Ds�RDsθDs�Ds��Ds�RDs�Ds�Ds��DtRDt�DtDt!�Dt(RDt.�Dt5DtA�DtHRDtN�DtUDta�DthRDtn�DtuDt��Dt�RDt��Dt�Dt��Dt�RDt��Dt�Dt��Dt�RDtθDt�Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aћ�Aя\Aя\A�p�A�jA�\)A�S�A�M�A�I�A�I�A�A�A�;dA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�5?A�"�A���A�dZA�ƨA��A���A�x�Aȴ9A�hsA��A�7LAę�AđhA�`BA�VA�v�A�&�A�  A�JA�p�A�hsA�I�A�A��wA��wA��^A�p�A�A���A���A��A�bA��`A��HA��DA�7LA�A��A��/A��A��A���A�S�A�v�A�&�A��9A�v�A��A���A�1'A�/A��TA���A�?}A���A�5?A�  A��
A���A�VA�9XA��9A��+A���A��7A�%A��A�XA�  A�dZA�JA�l�A�M�A�{A��A��A��RA��A�/A��!A�A�K�A�C�A��A��HA�K�A�r�A�A���A�{A}��A{
=AxI�Av��At�!Ap��Am�mAh��Ae�mAbE�A^^5A[`BAZ=qAXz�AWK�AW�AU�wAT5?ASG�ARn�AP~�AN�AJ�!AF(�ADA�ACdZAC`BACC�AC&�AA�A?G�A>I�A<�!A;C�A:�A:ĜA:-A9l�A8z�A6�A4��A1�-A1��A1O�A.��A,�`A,�uA+�A*jA)��A'`BA&�DA%XA$ffA#��A"�A"ZA!�PA!�A ��A��A��AdZA�+A�mAp�AVA�HA�A$�A�^A��AA��A�#A|�A+A��A��A�A$�A�9A=qA�A%A��A�A9XA  A�-At�A�A
ȴA
�A
~�A
A	��A��A�yA�jA��Ar�A��A	�^A
�A
bA	�A	�A�Ax�A��AĜAz�A~�At�@��@���@�t�@�t�@�C�@���@�ff@��T@�33@��@�M�@�$�@� �@��@���@��@���@�&�@�@�^5@�r�@�v�@���@�l�@ݡ�@�Ĝ@��
@���@�=q@���@ؓu@ם�@֗�@���@�r�@�\)@�n�@�O�@��@Ͼw@�;d@�o@�+@�33@�E�@̛�@�33@ʧ�@�n�@�=q@�@�j@Ƨ�@��@�7L@öF@Å@�C�@\@���@��`@���@�1@�^5@��u@���@��@���@�K�@�@�/@��D@��u@��@�X@��-@���@�J@���@��D@���@���@���@�j@�9X@�r�@��@�Ĝ@��@�V@�%@��@��@�I�@� �@� �@��@��9@�K�@��#@���@�@��@��@��w@���@�"�@�O�@���@��h@��@���@�{@��@�b@�Ĝ@��@��R@�V@�5?@�`B@�E�@�1@��`@�O�@�$�@���@��u@�9X@��F@��h@���@�A�@��;@�o@���@�A�@��^@�$�@�hs@���@�j@�j@��u@��`@�?}@��@��j@�A�@�9X@���@�S�@�V@��7@�p�@�X@�/@�V@�%@��`@���@��j@���@�9X@��@�  @��
@�ƨ@���@�t�@�S�@���@���@���@�dZ@��y@�ȴ@�n�@�-@�$�@��-@�X@�?}@���@���@���@��u@���@�j@�b@��m@���@��F@��@�S�@��@��H@���@��\@�V@�E�@�5?@��@���@���@���@��@�G�@�ƨ@���@���@���@��j@��@�&�@��@�r�@�9X@�(�@�1'@�9X@�9X@�Z@�j@�r�@�Q�@�b@���@��
@��@�S�@�C�@�@��@���@�ff@��#@�@��-@��@�`B@�/@���@�bN@� �@���@�t�@�l�@�dZ@�o@���@�M�@���@���@�p�@�&�@��u@�z�@�bN@�I�@�9X@���@{A�@p�@p�@d��@]�H@V�@QJ�@I5�@C�$@>�H@7��@3>�@-��@%�j@ ��@��@��@i�@��@	�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�C�A�5?A�XA�bNA� �A�M�A���A�JA���Aş�AǾwA���A�C�A�  A�VA��
A��/A�A�XAƮA�^5A�ffA�33A�/A�oAƬA���A�+A��/A�$�A�ƨA�S�A��A�A�x�A�(�A�bNA���A���Aá�A���A�^5Aç�AȁA���A�A�\)A�C�A��AѾwA���A�"�Aѣ�A�bA��A�z�A�hsA�^5A�I�A��AđhA��mA�ffAŇ+Aĝ�AƲ-A�oA�33AǃA�E�A��mA��AƅA���A�oA�hsA��AǺ^A�9XA�;dA�bNA˰!AЅA���A��A�t�A�(�AüjA�dZA�oAÃA�9XA��yA�A�VA�hsA�A�G�AżjA�\)AѲ-A�&�A�r�A��A¶FA�~�A�bNA�/A�O�A�l�A�  A�t�A�Q�A��`A�t�A���A�
=A���A�t�A�x�Ȧ+A�ĜAŅA�n�A�M�A�A�AƲ-AŸRAʅA��`A���A�?}AƝ�A�ZA�?}A�1'A�7LA�5?AŋDA�A�A�AѲ-A���A��A�9XẠ�A���A�9XA���A���AĴ9A���AŲ-Aũ�AăA�  A���A��mA��;AǙ�AǇ+A���AѲ-A�9XAŝ�Aŏ\A���A�5?AȮAЅA�-A�Q�A�7LA�G�A��A�5?A��HAѸRA��A��AȲ-A�ZAэPAƑhA��A��A��A�bNA���A��Aѥ�A��mA΁AͼjA�A�=qA�S�A���A�VA��A���A�9XA��Aɺ^A�  A�|�A��Aя\A�Q�A�A�A��A���AΝ�A�{A�5?A�/A��;A��TA���A�ȴA�33A�$�A�A��;A���A�+A�S�AЧ�A���A�hsA�ƨAѸRA�A�{AѼjA���A�ȴA�9XAξwA�Aѕ�A�1A�ĜA�ƨA���AѸRA� �A�ĜA���AѾwAѡ�A�ȴA��/A�I�AѼjA�ƨA�A�ƨA�ZAѺ^AѼjA���A�ĜA�ȴA�ȴAѾwAѺ^AѾwA͕�A�VA���AѾwAѥ�AѾwA�A���A���A�ȴA���AѼjAѺ^AѺ^AϼjA�ffAэPA���AѼjA�ĜA̙�A���AѸRAѺ^AѺ^AѸRAѴ9AѸRA��AёhAѼjAΧ�A�ƨA�
=AѾwAѾwA�AѲ-AѰ!A��HAѺ^AѲ-AѸRA���AѸRAѶFAї�Aɧ�AѮAѴ9AѰ!AѺ^A�S�A���Aч+AѼjAэPAѸRAѴ9AѶFAѾwAѴ9A�ȴAѺ^AѴ9AѼjAѸRA���AѶFAѴ9A�ƨA��AѴ9AѾwAѸRAѶFAѺ^AѶFAѾwAѺ^AѼjAѶFAѴ9AѰ!AѶFAѺ^AѼjAѾwAѴ9A�=qAѰ!AѴ9AѲ-A�n�AѴ9AѴ9AѸRAѕ�A���A��;AѾwAѶFA�Q�AѺ^AѴ9AѲ-AѮAѣ�A�"�AѴ9A� �AѲ-AѺ^AѰ!Aћ�A���AѲ-AѺ^AѸRA�ĜAѶFA�ƨA�ĜA�^5AуA�AѺ^A�dZA���A���A�AѼjA�ȴA���A�=qA�ȴA�A���A� �A�`BA�ȴA���A��
AѺ^AύPAЛ�A���A�ĜAя\A���AѾwA�ȴA�ĜAѸRA�ƨA�A�ƨA�ȴAϡ�A�ȴA�ĜA�ĜA���AѾwA�ĜAѾwA�Q�A�ȴAѼjA�ƨA�ȴAсA�A�ƨAѸRAμjAѼjA�ĜA�ȴA���AѶFA���AѼjA���A�ƨAѺ^A�A�|�AѺ^A�ĜAѾwAѶFAЧ�AѴ9A�
=AυA���AѶFAѶFA���AѮAѓuA�AѼjAѴ9AѼjAѼjAѶFAѺ^AѾwAѾwA���AѸRAѶFAѼjAѶFAѥ�AѬAѥ�AѲ-AѮAѮAѮAѸRAѸRAѩ�AѶFAѰ!AѸRAѼjAѲ-AѸRA�ĜAѲ-AѶFAѮAѶFAѮAѶFAѼjAѶFAѾwAѬAѴ9AѸRAѰ!AѲ-AѸRAѼjAѰ!Aѣ�Aѧ�AѮAѩ�Aѥ�AѰ!AѬAѩ�AѰ!Aѧ�Aѥ�Aѣ�Aѩ�Aѡ�Aѣ�Aѥ�Aѡ�AѰ!Aѥ�Aѥ�Aѡ�Aѕ�Aя\Aћ�Aї�Aљ�Aџ�Aї�Aѥ�Aћ�Aї�Aѝ�Aћ�Aѝ�Aљ�Aћ�Aѝ�Aѣ�Aѣ�AѬAѥ�Aљ�Aљ�Aї�Aї�Aѕ�Aї�Aљ�Aї�AѓuAћ�Aћ�Aѡ�Aћ�Aѕ�Aћ�Aѡ�Aї�AѓuAч+AхA�|�AсA�t�A�v�A�t�A�p�A�r�A�p�A�l�A�v�A�z�A�~�A�v�A�x�A�v�A�x�A�t�A�r�A�x�A�v�A�z�A�v�A�v�A�v�A�z�A�~�A�r�A�jA�jA�n�A�r�A�r�A�hsA�jA�hsA�l�A�dZA�ffA�hsA�ffA�bNA�n�A�ffA�ffA�ffA�dZA�bNA�bNA�hsA�dZA�dZA�ffA�dZA�bNA�`BA�dZA�dZA�ZA�^5A�ZA�bNA�ZA�\)A�ZA�\)A�XA�ZA�VA�S�A�VA�VA�ZA�\)A�VA�\)A�\)A�XA�Q�A�O�A�S�A�VA�S�A�VA�Q�A�S�A�S�A�Q�A�S�A�Q�A�S�A�VA�S�A�VA�S�A�VA�S�A�S�A�S�A�Q�A�Q�A�Q�A�O�A�VA�VA�Q�A�S�A�O�A�S�A�S�A�XA�S�A�VA�Q�A�G�A�E�A�G�A�E�A�E�A�E�A�C�A�C�A�E�A�E�A�G�A�G�A�C�A�C�A�A�A�C�A�C�A�E�A�C�A�E�A�C�A�A�A�E�A�C�A�E�A�E�A�G�A�E�A�I�A�E�A�G�A�C�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�A�A�C�A�C�A�E�A�E�A�E�@��7@��7@��@��7@��@��@��@��@�x�@�p�@�p�@�hs@�hs@�`B@�X@�X@�O�@�O�@�G�@�?}@�?}@�?}@�7L@�/@�&�@�&�@�&�@�&�@��@��@��@�V@���@�Ĝ@��@���@���@��u@��D@��u@��D@��u@��D@��D@��D@��@��@��@�z�@��@��@�z�@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�bN@�Z@�Z@�Z@�Z@�Z@�Z@�Z@�Q�@�Q�@�Q�@�Q�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�1'@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�@�(�@�(�Aћ�Aѝ�Aѝ�Aѡ�Aї�AѓuAэPAэPAэPAэPAщ7Aщ7AэPAэPAыDAэPAщ7Aщ7AыDAѕ�Aя\AыDAя\Aѕ�Aї�Aљ�AѓuAёhAыDAэPAыDAыDAэPAыDAэPAя\Aя\AэPAя\AэPAщ7Aя\Aч+AсA�|�A�|�A�t�A�l�A�jA�jA�hsA�ffA�bNA�`BA�`BA�n�A�l�A�p�A�l�A�jA�jA�hsA�hsA�jA�dZA�jA�hsA�hsA�l�A�l�A�p�A�n�A�`BA�^5A�`BA�^5A�bNA�dZA�^5A�ZA�XA�ZA�XA�XA�ZA�XA�\)A�\)A�XA�VA�VA�VA�VA�VA�XA�XA�XA�VA�VA�S�A�XA�Q�A�Q�A�M�A�M�A�O�A�O�A�M�A�O�A�M�A�M�A�I�A�K�A�I�A�I�A�I�A�M�A�M�A�O�A�M�A�M�A�M�A�E�A�G�A�K�A�I�A�M�A�I�A�I�A�I�A�G�A�I�A�G�A�G�A�G�A�G�A�I�A�K�A�K�A�K�A�I�A�K�A�I�A�K�A�K�A�G�A�E�A�G�A�I�A�K�A�I�A�G�A�G�A�G�A�I�A�E�A�C�A�I�A�C�A�=qA�=qA�;dA�;dA�=qA�;dA�;dA�=qA�=qA�;dA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�A�A�=qA�=qA�;dA�=qA�;dA�;dA�=qA�;dA�=qA�;dA�=qA�;dA�=qA�=qA�=qA�=qA�=q@��7@��7@��7@��@��@��@��@��@�x�@�p�@�hs@�hs@�`B@�X@�X@�X@�O�@�G�@�G�@�?}@�?}@�?}@�7L@�/@�/@�&�@�&�@�&�@��@��@�V@�%@��/@��9@���@���@���@��u@��u@��u@��D@��D@��D@��D@��D@��@�z�@��@��@��@�z�@�z�@��@��@�z�@�z�@�z�@�z�@�z�@�r�@�z�@�r�@�r�@�r�@�r�@�j@�r�@�r�@�j@�j@�j@�j@�bN@�Z@�bN@�Z@�Z@�Z@�Z@�Z@�Q�@�Q�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�1'@�9X@�9X@�9X@�9X@�1'@�9X@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�1'@�(�@�(�@�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  Aћ�Aя\Aя\A�p�A�jA�\)A�S�A�M�A�I�A�I�A�A�A�;dA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�5?A�"�A���A�dZA�ƨA��A���A�x�Aȴ9A�hsA��A�7LAę�AđhA�`BA�VA�v�A�&�A�  A�JA�p�A�hsA�I�A�A��wA��wA��^A�p�A�A���A���A��A�bA��`A��HA��DA�7LA�A��A��/A��A��A���A�S�A�v�A�&�A��9A�v�A��A���A�1'A�/A��TA���A�?}A���A�5?A�  A��
A���A�VA�9XA��9A��+A���A��7A�%A��A�XA�  A�dZA�JA�l�A�M�A�{A��A��A��RA��A�/A��!A�A�K�A�C�A��A��HA�K�A�r�A�A���A�{A}��A{
=AxI�Av��At�!Ap��Am�mAh��Ae�mAbE�A^^5A[`BAZ=qAXz�AWK�AW�AU�wAT5?ASG�ARn�AP~�AN�AJ�!AF(�ADA�ACdZAC`BACC�AC&�AA�A?G�A>I�A<�!A;C�A:�A:ĜA:-A9l�A8z�A6�A4��A1�-A1��A1O�A.��A,�`A,�uA+�A*jA)��A'`BA&�DA%XA$ffA#��A"�A"ZA!�PA!�A ��A��A��AdZA�+A�mAp�AVA�HA�A$�A�^A��AA��A�#A|�A+A��A��A�A$�A�9A=qA�A%A��A�A9XA  A�-At�A�A
ȴA
�A
~�A
A	��A��A�yA�jA��Ar�A��A	�^A
�A
bA	�A	�A�Ax�A��AĜAz�A~�At�@��@���@�t�@�t�@�C�@���@�ff@��T@�33@��@�M�@�$�@� �@��@���@��@���@�&�@�@�^5@�r�@�v�@���@�l�@ݡ�@�Ĝ@��
@���@�=q@���@ؓu@ם�@֗�@���@�r�@�\)@�n�@�O�@��@Ͼw@�;d@�o@�+@�33@�E�@̛�@�33@ʧ�@�n�@�=q@�@�j@Ƨ�@��@�7L@öF@Å@�C�@\@���@��`@���@�1@�^5@��u@���@��@���@�K�@�@�/@��D@��u@��@�X@��-@���@�J@���@��D@���@���@���@�j@�9X@�r�@��@�Ĝ@��@�V@�%@��@��@�I�@� �@� �@��@��9@�K�@��#@���@�@��@��@��w@���@�"�@�O�@���@��h@��@���@�{@��@�b@�Ĝ@��@��R@�V@�5?@�`B@�E�@�1@��`@�O�@�$�@���@��u@�9X@��F@��h@���@�A�@��;@�o@���@�A�@��^@�$�@�hs@���@�j@�j@��u@��`@�?}@��@��j@�A�@�9X@���@�S�@�V@��7@�p�@�X@�/@�V@�%@��`@���@��j@���@�9X@��@�  @��
@�ƨ@���@�t�@�S�@���@���@���@�dZ@��y@�ȴ@�n�@�-@�$�@��-@�X@�?}@���@���@���@��u@���@�j@�b@��m@���@��F@��@�S�@��@��H@���@��\@�V@�E�@�5?@��@���@���@���@��@�G�@�ƨ@���@���@���@��j@��@�&�@��@�r�@�9X@�(�@�1'@�9X@�9X@�Z@�j@�r�@�Q�@�b@���@��
@��@�S�@�C�@�@��@���@�ff@��#@�@��-@��@�`B@�/@���@�bN@� �@���@�t�@�l�@�dZ@�o@���@�M�@���@���@�p�@�&�@��u@�z�@�bN@�I�G�O�@���@{A�@p�@p�@d��@]�H@V�@QJ�@I5�@C�$@>�H@7��@3>�@-��@%�j@ ��@��@��@i�@��@	�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�C�A�5?A�XA�bNA� �A�M�A���A�JA���Aş�AǾwA���A�C�A�  A�VA��
A��/A�A�XAƮA�^5A�ffA�33A�/A�oAƬA���A�+A��/A�$�A�ƨA�S�A��A�A�x�A�(�A�bNA���A���Aá�A���A�^5Aç�AȁA���A�A�\)A�C�A��AѾwA���A�"�Aѣ�A�bA��A�z�A�hsA�^5A�I�A��AđhA��mA�ffAŇ+Aĝ�AƲ-A�oA�33AǃA�E�A��mA��AƅA���A�oA�hsA��AǺ^A�9XA�;dA�bNA˰!AЅA���A��A�t�A�(�AüjA�dZA�oAÃA�9XA��yA�A�VA�hsA�A�G�AżjA�\)AѲ-A�&�A�r�A��A¶FA�~�A�bNA�/A�O�A�l�A�  A�t�A�Q�A��`A�t�A���A�
=A���A�t�A�x�Ȧ+A�ĜAŅA�n�A�M�A�A�AƲ-AŸRAʅA��`A���A�?}AƝ�A�ZA�?}A�1'A�7LA�5?AŋDA�A�A�AѲ-A���A��A�9XẠ�A���A�9XA���A���AĴ9A���AŲ-Aũ�AăA�  A���A��mA��;AǙ�AǇ+A���AѲ-A�9XAŝ�Aŏ\A���A�5?AȮAЅA�-A�Q�A�7LA�G�A��A�5?A��HAѸRA��A��AȲ-A�ZAэPAƑhA��A��A��A�bNA���A��Aѥ�A��mA΁AͼjA�A�=qA�S�A���A�VA��A���A�9XA��Aɺ^A�  A�|�A��Aя\A�Q�A�A�A��A���AΝ�A�{A�5?A�/A��;A��TA���A�ȴA�33A�$�A�A��;A���A�+A�S�AЧ�A���A�hsA�ƨAѸRA�A�{AѼjA���A�ȴA�9XAξwA�Aѕ�A�1A�ĜA�ƨA���AѸRA� �A�ĜA���AѾwAѡ�A�ȴA��/A�I�AѼjA�ƨA�A�ƨA�ZAѺ^AѼjA���A�ĜA�ȴA�ȴAѾwAѺ^AѾwA͕�A�VA���AѾwAѥ�AѾwA�A���A���A�ȴA���AѼjAѺ^AѺ^AϼjA�ffAэPA���AѼjA�ĜA̙�A���AѸRAѺ^AѺ^AѸRAѴ9AѸRA��AёhAѼjAΧ�A�ƨA�
=AѾwAѾwA�AѲ-AѰ!A��HAѺ^AѲ-AѸRA���AѸRAѶFAї�Aɧ�AѮAѴ9AѰ!AѺ^A�S�A���Aч+AѼjAэPAѸRAѴ9AѶFAѾwAѴ9A�ȴAѺ^AѴ9AѼjAѸRA���AѶFAѴ9A�ƨA��AѴ9AѾwAѸRAѶFAѺ^AѶFAѾwAѺ^AѼjAѶFAѴ9AѰ!AѶFAѺ^AѼjAѾwAѴ9A�=qAѰ!AѴ9AѲ-A�n�AѴ9AѴ9AѸRAѕ�A���A��;AѾwAѶFA�Q�AѺ^AѴ9AѲ-AѮAѣ�A�"�AѴ9A� �AѲ-AѺ^AѰ!Aћ�A���AѲ-AѺ^AѸRA�ĜAѶFA�ƨA�ĜA�^5AуA�AѺ^A�dZA���A���A�AѼjA�ȴA���A�=qA�ȴA�A���A� �A�`BA�ȴA���A��
AѺ^AύPAЛ�A���A�ĜAя\A���AѾwA�ȴA�ĜAѸRA�ƨA�A�ƨA�ȴAϡ�A�ȴA�ĜA�ĜA���AѾwA�ĜAѾwA�Q�A�ȴAѼjA�ƨA�ȴAсA�A�ƨAѸRAμjAѼjA�ĜA�ȴA���AѶFA���AѼjA���A�ƨAѺ^A�A�|�AѺ^A�ĜAѾwAѶFAЧ�AѴ9A�
=AυA���AѶFAѶFA���AѮAѓuA�AѼjAѴ9AѼjAѼjAѶFAѺ^AѾwAѾwA���AѸRAѶFAѼjAѶFAѥ�AѬAѥ�AѲ-AѮAѮAѮAѸRAѸRAѩ�AѶFAѰ!AѸRAѼjAѲ-AѸRA�ĜAѲ-AѶFAѮAѶFAѮAѶFAѼjAѶFAѾwAѬAѴ9AѸRAѰ!AѲ-AѸRAѼjAѰ!Aѣ�Aѧ�AѮAѩ�Aѥ�AѰ!AѬAѩ�AѰ!Aѧ�Aѥ�Aѣ�Aѩ�Aѡ�Aѣ�Aћ�Aѝ�Aѝ�Aѡ�Aї�AѓuAэPAэPAэPAэPAщ7Aщ7AэPAэPAыDAэPAщ7Aщ7AыDAѕ�Aя\AыDAя\Aѕ�Aї�Aљ�AѓuAёhAыDAэPAыDAыDAэPAыDAэPAя\Aя\AэPAя\AэPAщ7Aя\Aч+AсA�|�A�|�A�t�A�l�A�jA�jA�hsA�ffA�bNA�`BA�`BA�n�A�l�A�p�A�l�A�jA�jA�hsA�hsA�jA�dZA�jA�hsA�hsA�l�A�l�A�p�A�n�A�`BA�^5A�`BA�^5A�bNA�dZA�^5A�ZA�XA�ZA�XA�XA�ZA�XA�\)A�\)A�XA�VA�VA�VA�VA�VA�XA�XA�XA�VA�VA�S�A�XA�Q�A�Q�A�M�A�M�A�O�A�O�A�M�A�O�A�M�A�M�A�I�A�K�A�I�A�I�A�I�A�M�A�M�A�O�A�M�A�M�A�M�A�E�A�G�A�K�A�I�A�M�A�I�A�I�A�I�A�G�A�I�A�G�A�G�A�G�A�G�A�I�A�K�A�K�A�K�A�I�A�K�A�I�A�K�A�K�A�G�A�E�A�G�A�I�A�K�A�I�A�G�A�G�A�G�A�I�A�E�A�C�A�I�A�C�A�=qA�=qA�;dA�;dA�=qA�;dA�;dA�=qA�=qA�;dA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�A�A�=qA�=qA�;dA�=qA�;dA�;dA�=qA�;dA�=qA�;dA�=qA�;dA�=qA�=qA�=qA�=qA�=q@��7@��7@��7@��@��@��@��@��@�x�@�p�@�hs@�hs@�`B@�X@�X@�X@�O�@�G�@�G�@�?}@�?}@�?}@�7L@�/@�/@�&�@�&�@�&�@��@��@�V@�%@��/@��9@���@���@���@��u@��u@��u@��D@��D@��D@��D@��D@��@�z�@��@��@��@�z�@�z�@��@��@�z�@�z�@�z�@�z�@�z�@�r�@�z�@�r�@�r�@�r�@�r�@�j@�r�@�r�@�j@�j@�j@�j@�bN@�Z@�bN@�Z@�Z@�Z@�Z@�Z@�Q�@�Q�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�1'@�9X@�9X@�9X@�9X@�1'@�9X@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�1'@�(�@�(�@�(�Aћ�Aѝ�Aѝ�Aѡ�Aї�AѓuAэPAэPAэPAэPAщ7Aщ7AэPAэPAыDAэPAщ7Aщ7AыDAѕ�Aя\AыDAя\Aѕ�Aї�Aљ�AѓuAёhAыDAэPAыDAыDAэPAыDAэPAя\Aя\AэPAя\AэPAщ7Aя\Aч+AсA�|�A�|�A�t�A�l�A�jA�jA�hsA�ffA�bNA�`BA�`BA�n�A�l�A�p�A�l�A�jA�jA�hsA�hsA�jA�dZA�jA�hsA�hsA�l�A�l�A�p�A�n�A�`BA�^5A�`BA�^5A�bNA�dZA�^5A�ZA�XA�ZA�XA�XA�ZA�XA�\)A�\)A�XA�VA�VA�VA�VA�VA�XA�XA�XA�VA�VA�S�A�XA�Q�A�Q�A�M�A�M�A�O�A�O�A�M�A�O�A�M�A�M�A�I�A�K�A�I�A�I�A�I�A�M�A�M�A�O�A�M�A�M�A�M�A�E�A�G�A�K�A�I�A�M�A�I�A�I�A�I�A�G�A�I�A�G�A�G�A�G�A�G�A�I�A�K�A�K�A�K�A�I�A�K�A�I�A�K�A�K�A�G�A�E�A�G�A�I�A�K�A�I�A�G�A�G�A�G�A�I�A�E�A�C�A�I�A�C�A�=qA�=qA�;dA�;dA�=qA�;dA�;dA�=qA�=qA�;dA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�A�A�=qA�=qA�;dA�=qA�;dA�;dA�=qA�;dA�=qA�;dA�=qA�;dA�=qA�=qA�=qA�=qA�=q@��7@��7@��7@��@��@��@��@��@�x�@�p�@�hs@�hs@�`B@�X@�X@�X@�O�@�G�@�G�@�?}@�?}@�?}@�7L@�/@�/@�&�@�&�@�&�@��@��@�V@�%@��/@��9@���@���@���@��u@��u@��u@��D@��D@��D@��D@��D@��@�z�@��@��@��@�z�@�z�@��@��@�z�@�z�@�z�@�z�@�z�@�r�@�z�@�r�@�r�@�r�@�r�@�j@�r�@�r�@�j@�j@�j@�j@�bN@�Z@�bN@�Z@�Z@�Z@�Z@�Z@�Q�@�Q�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�1'@�9X@�9X@�9X@�9X@�1'@�9X@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�1'@�(�@�(�@�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=��K=�-b@"$=rGE=zcs=���=�>'(@F�=���=��=��@S�>�}=��+>���@�.4@��>�E=H�=��D>q�+=Z0=O��==f'=U�=��2=�y=�z�>+hI@�,=���=�u�=��J>��@�`=��=8�5=8�=d%�=��q=.ƽ=Y��=Ew�=�2�=��X=��=�l�>W`�@�)5>I�>?�a@�+V=
=��=#c�=+��=�=�=0��=3�=2=i�;=��=C��=d��=�#> D(=�M=��X=�7a=��=��>2�@^t=�?h=���>k'@��=�2�=��=���?���=�E�=�<K>\�N@-��=��=�پ?^�="��=/�m=2�I=\��=mq�=�H�=�_=�1{=�P�>'(c@�.4=�CB=���?[=�~�=��'=�A�=q,(=��U=�T"=�#O>%P�=���=a�=p��=�6�?9>�=�G=��z=�7">�F@�3r?xW�=z9�=�)_?+��=��=��=ƭ.>��?]��=n��=�J�>XO=�P3=�[l>.�w=�y=��>+,@�-b@�0�=���=�vu=�e�>�t�@�3�=+�8=D�=i/=C=/n�=:)�=BE�=E�N=q��=lvK=��?;\=��=ě�>9jj@�0j>3E�=�|=�Q=���=���>"�@��W=ѷ>�u�=w�U=�EN=��3=�@�?
�
@��5?��U=�	�=�-�>h�@�E9=���=��_>��@�U�=�C-=�,>Q�	@�4�=ң>[ 2@is?�O�?O*Z=�_1=�j?RZ�?��?bM�?O�)=�=���>�+�?v_�>��@�6e=��=�#�=�3	=��?��@�7�>C�?��m=�6P@_#%>4h�@�,g?���>�o�@(��?wRT@�7a?��X>Ҁ�@o��@�6�@/�@�6�@�9m@�6&>?�@�5�@�5i@�6z@���>�p�@�5�@�8�@_��@�6P@�8@�7@�8?�'|@�7@�7a@�7@�7L@�7�?���?��A@�7@�6�@�5�@�7a>�ϫ@�6�@�6�@�77@�6z@�6&@�7�@�77@�6P@�5??��">�?h@�6P@�6P@�Ov@�6�@�9.@�7�@�9.@�6P@�8@�5�@�5�@�7a@v�8@��@�3�@O�@�4�@�4/@C-�?��@�4�@�4�@�4�@�3�@�6P@�5�@�4�@_��@�3r>Wc^@�4�@���@�4�@�4�@�5�@�3�@�3�?��@�3r@�4/@�5�@�3�@�3@�3r@�2�>t�,@�3r@�2v@�3�@�2v@bO@�3r@�3�@�3�@�2@�3@�2�@�3r@�4�@�U�@�2v@�4/@�4/@�4/@�3�@�3�@�2v@�3r@kKI?�l�@�3@�2�@�4�@�5�@�4�@�5?@�4�@�5�@�4/@�6z@�4�@�4@�5i@�3@�3�@�4�@�2v?���@�3H@�3r@�5�>��W@�4�@�4�@�6P@�4�@�5??�j@�2�@�1�@���@�2�@�3H@�28@�2�@�2@M@@�1�>ܾb@�4Y@�4@�3�@�3@�5?@�5�@�6�@�3r@�6P@�5�@�5?@�3r@�5?@fߤ@�7�@�5�@}U�@�9.@�7@�9�@�7a@�8�@�v�?F��@�9�@�7a@�8q@��?IE�@�8�@�7?��@�9.@���@y@�9�@�7a@h�[@�9�@�7�@�8q@�8@�8�@�7�@�9@�6z@�9?�@�9@�7�@�9.@�6P@�6P@�9.@�7�?�g@�5�@�8�@�9�@�6�@�'@�6�@�5�@�7a?�_�@�5�@�5�@�5�@�5�@�5�@�8@�6P@�5?@�8�@�5�@�5i@O�@�7a@�6z@�5�@�5i@h�_@�4�?x�A@Oy@�5�@�4�@�5�@�A�@�4/@�5�@�4�@�4�@�5?@�3@�5�@�4�@�5?@�4/@�5�@�4Y@�1{@�28@�1'@�/�@�0�@�/�@�.�@�1�@�1@�1@�1@�3@�2@�1f@�5i@�3H@�2�@�2�@�1�@�1{@�2@�3�@�3@�1�@�2@�3�@�1f@�3@�2v@�0U@�4@�5@�5@�3H@�4@�3�@�1@�/E@�/�@�.�@�0�@�0U@�0�@�2v@�2#@�.�@�/@�.I@�.I@�-�@�-�@�/�@�-�@�*Z@�.I@�/E@�-8@�+k@�+�@�(�@�&l@�)J@�)�@�*Z@�*@�)�@�+@�(�@�)�@�*@�,(@�+�@�,|@�-�@�,�@�.I@�,|@�,|@�+@�(�@�*Z@�*o@�)�@�+k@�+,@�*Z@�*�@�*@�,|@�*@�)J@�+@�*@�*@�(�@�'|@�&-@�#:@�#�@�!�@�`@��@�m@��@�\@��@�@��@�O@�@�@��@�:@��@��@��@��@��@��@�d@��@�?@�?@��@��@��@��@�"@��@��@�a@��@�j@�/@��@��@��@�D@�/@��@�@��@�H@��@�r@�H@��@��@��@�/@��@�r@��@��@�b@��@��@��@�'@�8@��@�'@��@�@�+@��@��@�0@�@�@�'@�{@�<@��@��@��@��@��@��@��@�o@�E@�o@�E@�E@��@��@�o@��@��@�o@��@�+@��@��@�+@��@��@��@�@��@�+@�+@��@�+@�U@�+@�+@�U@��@�U@�b@��@��@��@��@��@��@��@��@��@�g@�R@�R@�g@��@�R@��@��@�g@��@�@�|@��@��@�#@�b@��@��@�@��@�@��@�b@�b@��@��@��@��@�@�@�I@�s@��@�s@�0@�Z@QIR@QH�@QHV@QH@QH@QH@QG�@QF�@QE�@QE@QDg@QCl@QC@QA�@Q@�@Q@%@Q?}@Q>�@Q>�@Q=�@Q=2@Q<�@Q;�@Q;@Q:@Q9m@Q9@Q8q@Q7v@Q7"@Q5+@Q2�@Q-w@Q(9@Q&�@Q%�@Q$�@Q#�@Q#�@Q#O@Q#O@Q#O@Q#%@Q"S@Q")@Q!�@Q!�@Q!�@Q!�@Q!�@Q!�@Q!�@Q!�@Q!�@Q!W@Q!-@Q!-@Q!-@Q!�@Q!-@Q �@Q �@Q �@Q �@Q �@Q�@Q �@Q 2@Q�@Q�@Q�@Q6@Q�@Q�@Q?@Q�@Q�@Q�@Q?@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Qq@Q@Q@Q@Q�@Qv@Q�@Q"@Q�@Q"@Q"@Q"@Q"@Q"@Q"@Qv@Q�@Qv@Qv@Qv@Qv@Q"@Q"@Q"@Q"@Q�@Q"@Q�@�)�@�*�@�,�@�-w@�+@�)�@�%p@�$�@�%F@�$�@�$�@�#�@�%@�%�@�$�@�$_@�$�@�$5@�$�@�(9@�'@�%@�'g@�(x@�(x@�-w@�'=@�(N@�%1@�%[@�%1@�%p@�%�@�%�@�%F@�'@�'(@�'@�&�@�%�@�%@�&@�#�@� �@� G@��@� @�&@��@�@�r@��@�@��@��@��@��@�	@�+@��@�@�/@�n@�@@��@�@��@�D@�&@�+@�"@��@�+@��@�<@�@@��@�H@�b@��@�s@�0@�b@��@��@�@�o@��@��@�^@�4@�4@�4@�^@��@�@��@�0@�4@��@�@��@��@�@��@��@�(@��@��@��@�k@�o@�0@��@�Z@��@��@�=@��@��@�g@�(@��@�_@��@��@��@�o@��@�Z@�0@�@�@��@��@��@��@��@�@�V@��@�,@�k@��@��@��@�@��@�,@��@�,@��@��@�,@�o@��@��@��@��@�	�@�	�@�	W@�	�@�	W@�	�@�	�@�	�@�	�@�	�@�
@�	�@�	W@�	�@�	�@�	�@�	�@�	B@�
=@�
R@�
=@�
=@�
|@�
�@�
�@�c@�c@�x@�9@�x@�
�@�
�@�
�@�
�@�@�
�@�
�@�
�@�@�N@�N@�x@�x@�x@��@QA @Q?�@Q@%@Q?)@Q?S@Q?)@Q?S@Q>�@Q>@Q<�@Q<6@Q;�@Q:�@Q9�@Q8�@Q8@Q7v@Q6�@Q6P@Q5�@Q5~@Q4�@Q4@Q3	@Q2�@Q28@Q1�@Q1f@Q0�@Q0j@Q/E@Q-�@Q(9@Q �@Q@QC@Q�@Q�@Q�@Q�@Qz@Qz@QP@Q�@Q�@Q�@QY@Q�@Q�@Q+@QY@QY@Q�@Q�@Q/@Q/@Q�@QY@Q/@Q@Q@Q�@Q�@Q]@Q]@Q3@Q	@Q]@Q3@Q3@Q3@Q�@Q�@Qj@Q@@Q@@Q�@Q@@Q�@Q�@Qo@Qo@Q�@Q�@Q�@Q�@Qs@Q�@Q�@Q�@Q�@Q�@Qw@Q�@Q�@Q�@QM@Q�@Q|@QR@QR@Q(@Q(@QR@QR@QR@QR@Q|@Q�@Q�@Q�@QR@Q(@Q(@QR@Q(@Q�@Q�@Q�@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          444444444444434443344444444444434444444444444444443443444444444444444444444444434444444444444444444443444444444444444444443444444444444444444334444344444444444444434444443444444434444344434443443444444444444434444434443434444344334333433334333333343333344333343333333334433333333333333343344333333333433333334333333343333333333333333333333343333333333333333343334333334333333334343333333333333333333333343334433433433333333333343333333433333333433333333333433333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@S�G�O�G�O�G�O�@�.2@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�,G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�)6G�O�G�O�@�+RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�.5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�3oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�-c@�1G�O�G�O�G�O�G�O�@�3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�0jG�O�G�O�G�O�G�O�G�O�G�O�@��ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�@��6G�O�G�O�G�O�G�O�@�E<G�O�G�O�G�O�@�U�G�O�G�O�G�O�@�4�G�O�G�O�@isG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�6eG�O�G�O�G�O�G�O�G�O�@�7�G�O�G�O�G�O�@_#%G�O�@�,iG�O�G�O�G�O�G�O�@�7bG�O�G�O�@o��@�6�G�O�@�6�@�9p@�6&G�O�@�5�@�5l@�6~@���G�O�@�5�@�8�@_��@�6R@�8@�7@�8G�O�@�7@�7c@�7	@�7J@�7�G�O�G�O�@�7@�6�@�5�@�7aG�O�@�6�@�6�@�78@�6~@�6&@�7�@�77@�6R@�5>G�O�G�O�@�6R@�6T@�Or@�6�@�9.@�7�@�9,@�6R@�8@�5�@�5�@�7`@v�8@��@�3�G�O�@�4�@�44G�O�G�O�@�4�@�4�@�4�@�3�@�6R@�5�@�4�@_��@�3rG�O�@�4�@���@�4�@�4�@�5�@�3�@�3�G�O�@�3r@�4,@�5�@�3�@�3@�3t@�2�G�O�@�3w@�2u@�3�@�2x@bR@�3u@�3�@�3�@�2@�3 @�2�@�3t@�4�@�U�@�2{@�4.@�42@�4.@�3�@�3�@�2x@�3r@kKNG�O�@�3 @�2�@�4�@�5�@�4�@�5?@�4�@�5�@�41@�6z@�4�@�4@�5j@�3"@�3�@�4�@�2xG�O�@�3H@�3r@�5�G�O�@�4�@�4�@�6O@�4�@�5AG�O�@�2�@�1�@���@�2�@�3K@�29@�2�@�2G�O�@�1�G�O�@�4W@�4@�3�@�3 @�5C@�5�@�6�@�3r@�6R@�5�@�5>@�3o@�5?@fߞ@�7�@�5�@}U�@�9,@�7@�9�@�7a@�8�@�v�G�O�@�9�@�7b@�8tG�O�G�O�@�8�@�7G�O�@�91@���G�O�@�9�@�7b@h�^@�9�@�7�@�8r@�8#@�8�@�7�@�9@�6z@�9G�O�@�9@�7�@�91@�6Q@�6N@�9/@�7�G�O�@�5�@�8�@�9�@�6�@�'@�6�@�5�@�7bG�O�@�5�@�6@�5�@�5�@�5�@�8@�6R@�5A@�8�@�5�@�5hG�O�@�7d@�6z@�5�@�5j@h�^@�4�G�O�G�O�@�5�@�4�@�5�@�A�@�40@�5�@�4�@�4�@�5?@�3@�5�@�4�@�5>@�40@�5�@�4Z@�1}@�29@�1*@�/�@�0�@�/�@�.�@�1�@�1@�1@�1@�3@�2@�1g@�5i@�3B@�2�@�2�@�1�@�1}@�2@�3�@�3@�1�@�2@�3�@�1g@�3@�2v@�0V@�4@�5@�5@�3H@�4@�3�@�1@�/E@�/�@�.�@�0�@�0X@�0�@�2x@�2#@�.�@�/@�.L@�.I@�-�@�-�@�/�@�-�@�)�@�*�@�,�@�-z@�+@�)�@�%n@�$�@�%F@�$�@�$�@�#�@�%
@�%�@�$�@�$^@�$�@�$7@�$�@�(:@�'@�%@�'i@�(x@�(z@�-x@�'>@�(O@�%2@�%^@�%2@�%r@�%�@�%�@�%J@�'@�'+@�'@�&�@�%�@�%
@�&@�#�@� �@� F@��@��@�*@��@�@�u@��@�@��@��@��@��@�
@�(@��@��@�1@�p@�@@��@�@��@�G@�)@�(@�"@��@�.@��@�;@�A@��@�H@�_@��@�r@�2@�_@��@��@�@�n@��@��@�b@�6@�5@�2@�b@��@�@��@�2@�2@��@�@��@��@�@��@��@�&@��@��@��@�l@�n@�1@��@�Z@��@��@�>@��@��@�f@�(@��@�[@��@��@��@�q@��@�\@�2@�@�@��@��@��@��@��@�@�Y@��@�-@�o@��@��@��@�@��@�-@��@�-@��@��@�-@�p@��@��@��@��@�	�@�	�@�	V@�	~@�	Z@�	�@�	�@�	�@�	�@�	�@�
@�	�@�	[@�	�@�	@�	�@�	�@�	B@�
B@�
P@�
@@�
@@�
|@�
�@�
�@�b@�b@�y@�:@�z@�
�@�
�@�
�@�
�@�@�
�@�
�@�
�@�@�O@�O@�z@�{@�z@��@QA @Q?�@Q@(@Q?*@Q?R@Q?*@Q?R@Q>�@Q>@Q<�@Q<5@Q;�@Q:�@Q9�@Q8�@Q8@Q7v@Q6�@Q6P@Q5�@Q5�@Q4�@Q4@Q3@Q2�@Q25@Q1�@Q1h@Q0�@Q0k@Q/F@Q-�@Q(8@Q �@Q@Q@@Q�@Q�@Q�@Q�@Qz@Q{@QR@Q�@Q�@Q�@QZ@Q�@Q�@Q*@QX@QZ@Q�@Q�@Q0@Q-@Q�@QX@Q.@Q@Q@Q�@Q�@Q`@Q]@Q.@Q@Q`@Q6@Q5@Q6@Q�@Q�@Qk@Q>@QB@Q�@Q>@Q�@Q�@Qp@Qm@Q�@Q�@Q�@Q�@Qr@Q�@Q�@Q�@Q�@Q�@Qv@Q�@Q�@Q�@QN@Q�@Q~@QU@QP@Q&@Q%@QU@QS@QR@QP@Qx@Q�@Q�@Q�@QP@Q#@Q*@QP@Q*@Q�@Q�@Q�@Q�@�)�@�*�@�,�@�-z@�+@�)�@�%n@�$�@�%F@�$�@�$�@�#�@�%
@�%�@�$�@�$^@�$�@�$7@�$�@�(:@�'@�%@�'i@�(x@�(z@�-x@�'>@�(O@�%2@�%^@�%2@�%r@�%�@�%�@�%J@�'@�'+@�'@�&�@�%�@�%
@�&@�#�@� �@� F@��@��@�*@��@�@�u@��@�@��@��@��@��@�
@�(@��@��@�1@�p@�@@��@�@��@�G@�)@�(@�"@��@�.@��@�;@�A@��@�H@�_@��@�r@�2@�_@��@��@�@�n@��@��@�b@�6@�5@�2@�b@��@�@��@�2@�2@��@�@��@��@�@��@��@�&@��@��@��@�l@�n@�1@��@�Z@��@��@�>@��@��@�f@�(@��@�[@��@��@��@�q@��@�\@�2@�@�@��@��@��@��@��@�@�Y@��@�-@�o@��@��@��@�@��@�-@��@�-@��@��@�-@�p@��@��@��@��@�	�@�	�@�	V@�	~@�	Z@�	�@�	�@�	�@�	�@�	�@�
@�	�@�	[@�	�@�	@�	�@�	�@�	B@�
B@�
P@�
@@�
@@�
|@�
�@�
�@�b@�b@�y@�:@�z@�
�@�
�@�
�@�
�@�@�
�@�
�@�
�@�@�O@�O@�z@�{@�z@��@QA @Q?�@Q@(@Q?*@Q?R@Q?*@Q?R@Q>�@Q>@Q<�@Q<5@Q;�@Q:�@Q9�@Q8�@Q8@Q7v@Q6�@Q6P@Q5�@Q5�@Q4�@Q4@Q3@Q2�@Q25@Q1�@Q1h@Q0�@Q0k@Q/F@Q-�@Q(8@Q �@Q@Q@@Q�@Q�@Q�@Q�@Qz@Q{@QR@Q�@Q�@Q�@QZ@Q�@Q�@Q*@QX@QZ@Q�@Q�@Q0@Q-@Q�@QX@Q.@Q@Q@Q�@Q�@Q`@Q]@Q.@Q@Q`@Q6@Q5@Q6@Q�@Q�@Qk@Q>@QB@Q�@Q>@Q�@Q�@Qp@Qm@Q�@Q�@Q�@Q�@Qr@Q�@Q�@Q�@Q�@Q�@Qv@Q�@Q�@Q�@QN@Q�@Q~@QU@QP@Q&@Q%@QU@QS@QR@QP@Qx@Q�@Q�@Q�@QP@Q#@Q*@QP@Q*@Q�@Q�@Q�@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          444444444444434443344444444444434444444444444444443443444444444444444444444444434444444444444444444443444444444444444444443444444444444444444334444344444444444444434444443444444434444344434443443444444444444434444434443434444344334333433334333333343333344333343333333334433333333333333343344333333333433333334333333343333333333333333333333343333333333333333343334333334333333334343333333333333333333333343334433433433333333333343333333433333333433333333333433333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9y��9y�r9y�9y�^9y��9y� 9y��9yf9y�z9y�9y�9y~V9y�#9y��9yj9y*9yn9y~�9yh9y��9y�9y�;9y��9y�9y�9y�[9y�U9y��9y�]9y��9y�]9y��9y�o9y�79y��9y�9y�99y�9y��9y�9y�#9y��9y~�9yy�9yy;9yx+9yt{9yn�9yk9yk�9yk9yj9yh�9yg09yf�9yn�9yo�9yq�9ymw9ym 9ym:9yl9ylm9ym�9yj-9ym?9ym 9yl19yn�9ymw9ypU9yrW9yfA9ye�9yg�9yf\9yh�9yj�9yin9ye�9yc�9yd�9yb/9yd9ydV9yd�9ye+9ye�9yeH9yc�9ycf9yce9yca9yc�9yc�9yd�9ydw9yd�9yca9yc9ycE9yc9ya$9y^�9y_�9y_�9y`i9y_�9yaA9y_�9y_\9y]�9y]�9y^H9y]�9y\�9y_�9y`�9ya9y_�9y`�9y`l9y[�9y\]9y^H9y\�9y`9y]�9y^.9y]�9y]�9y]r9y]S9y^09y^+9y^Q9y^E9y^�9y^�9y_@9y^�9y_ 9y_`9y_�9y`9y^H9y]r9y^�9y_ 9y_�9y_ 9y^�9y^�9y_ 9y]�9y^9y[�9y_�9y\�9yX�9yXX9yW�9yX69yX9yX<9yX�9yXS9yXX9yX<9yY9yXY9yX9yX<9yX89yX[9yX[9yW�9yYS9yYg9yYP9yYP9yY�9yY�9yZ9yZ�9yZ�9y[9yZ�9y[9yZ#9yZb9yY�9yZ9yZ}9yZ#9yZ#9yZ_9yZ�9yZ�9yZ�9y[9y[9y[9y[s9��9��9��9�29�O9�29�O9�9�^9��9�9��9��9�d9��9�9��9�C9��9�p9�19��9�9�i9�9��9�p9�:9��9��9��9�~9��9��9�49��9�'9�<9�9�<9��9��9��9�m9�L9��9�X9��9��9��9�W9�X9�u9��9�:9�89��9�W9�89�9�9��9��9��9��9�9�e9��9��9��9��9�(9��9�~9�^9�a9��9�^9�#9�$9��9��9�n9�m9�29�29�9�09�29�49�09�09�Z9�x9�z9�z9�=9��9��9��9��9�f9�f9��9��9��9��9��9��9��9��9��9�d9�i9��9�i9�9�9�9�,G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
w�B
x�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
|�B
�+B
ĜB)�B]/BXB[#B^5BP�BYBjBw�B�=B��B��B�LB��B��B�}BǮB�/B�sBJBk�B��B�B�LB�jB�wB��B�9B�3B��B�ZB�mB�B�B�B�B�B�B��B��B�B�B�B�bB=qB+B��BBDB\BbBhBhBhBbB\BDB
=B%B�B�;BŢB�9B��B��B�\B�BaHBE�BC�B>wB33B&�B\B
�B
�B
�sB
��B
��B
�VB
~�B
x�B
o�B
C�B
B	��B	�BB	�B	��B	{�B	m�B	O�B	1'B	�B�B�/BȴB�?B��B��B�?B��B�B�B��B��BɺB�jB�LB��B��B��B�uB�oB�hB�\B�=B�B�B� B�B�B�B�B�B�B�%B�%B�1B�1B�%B~�Bz�B}�B�B~�B{�B}�B~�B~�B}�B}�B}�B� B�B�B� B~�B}�Bz�Bw�Bs�Bq�Bq�Bp�Bp�Bn�Bk�BgmBk�Bm�Bl�Bn�Bq�Bt�Br�Bq�Bs�Bt�Bs�Bs�Bq�Bs�Bq�Bp�Bs�By�B�B�B�1B�7B�JB�bB��B��B��B��B�!B�FBŢB�)B�sB�B�mB�`B�HB��B��B�B�B��B�qB��B�+B� B�B�B}�B� B��B�{B�oB��B��B�oB�1B�B{�B|�Bw�Bu�Bp�BjBhsBiyBcTB^5B]/B^5B^5BdZBn�Bs�Bt�Bu�Bx�Bx�B{�B|�B~�B~�B�B�1B�DB�PB�PB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�XB��B��B�B��B	B	B	+B	
=B	PB	oB	�B	�B	�B	�B	 �B	!�B	�B	�B	�B	�B	#�B	,B	,B	.B	1'B	5?B	8RB	7LB	8RB	5?B	33B	=qB	J�B	R�B	K�B	<jB	/B	$�B	 �B	"�B	+B	7LB	6FB	33B	)�B	'�B	0!B	B�B	H�B	M�B	S�B	[#B	ZB	]/B	ZB	T�B	S�B	Q�B	Q�B	Q�B	VB	^5B	[#B	bNB	`BB	`BB	cTB	ffB	l�B	n�B	s�B	v�B	v�B	w�B	w�B	x�B	x�B	w�B	v�B	w�B	w�B	y�B	{�B	� B	�B	�%B	�1B	�=B	�JB	�PB	�JB	�VB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�-B	�3B	�9B	�FB	�FB	�FB	�LB	�LB	�XB	�^B	�jB	�qB	�}B	��B	��B	��B	�wB	�jB	�^B	�XB	�XB	�jB	�wB	�}B	�}B	�}B	��B	B	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�5B	�BB	�BB	�NB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�5B
 �B
�B
$�B
WB
)�B
2�B
3B
:�B
A B
G_B
OvB
R�B
XyB
\xB
^5B
c�B
i�B
mB
r�B
utG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�7�>ִ ?4�AyLA>�$�>�(|>�m�>��7?@?�A@� >��>ۗ�?� A���@�:?�?�4�B
c;B
`?��o>r��>��?�K>�NM>}��>e�>���>�m�>�"�>�i�?V��B
i�>�Q>�+�>��?*AW�A>>��>b�3>`�[>���>��f>Sʸ>�)V>n]>�;�>�rX>�)�>��Z?��B
jN?}}?n�GB
vB>'�->8��>E��>Oܜ>(ͧ>:��>V��>Z�>Wv>>��_>��j>n�H>� X>�|�?�>��,>���?��>��0>�%_? YdAi�>���>��?9��B��?�>�Y?�/@�{>�+�?8L?�0�A��>�?�@By>F�B>T��>Y)$>��1>��>���>��3?�r?��?R/�B
st>��C?&�@FQ�>�kj>��?Q>��>�M�>���?g�?N�>�eM>��>��z>��@��>�c�>��>��?BG�B
q9@�r&>�$�>�3�@kw�>�K�>��>��?$��@��V>��I>�֦?"�]>�w?�?[΅>��>���?0��B
l�B
u�>�a�>�R;?��@"��B
nJ>O�a>1�>)�W>=�W>V�{>b$%>l' >p�>��s>���>մg@���>�>�U�?k*�B
u}?eM�>ƷE>���>�<�>�*?K�A���? P�@�>���>ф�>�m�?�@8�cA�Q0@̲�?	kU>��l?CL�B	��>С�>�]F??�A�X>�4/?_?��B
~)??�W�A�r�A0W�@�
�?�>�#�@��u@���@���@���>�G�? Y!?�q�@���@� B
��>�W�>��>�:C?��A�GB
��?s��@�4�?�=A�#�?b��B
i_AU?��#A{�;@�e�B
p�A"�h@�(A�>�B
pJANюB
s�B
{KB
tq?l�B
v�B
t�B
rlB/|?�?�B
tKB
��A�o�B
s�B
t�B
r,B
z@�J�B
t�B
vKB
v�B
��B
s�@���A�yB
w�B
s`B
tSB
t?���B
w�B
w3B
v-B
s�B
r B
snB
v�B
w�B
u+A
�L?��ZB
udB
v*B
�&B
vvB
w/B
v�B
w�B
r>B
v�B
vGB
wB
x�A�ȺA��B
�HA���B
u�B
q�A�a�A
�B
wB
vjB
vB
v1B
y�B
x&B�A�$jB
tO?�ӦB
q�A�"�B
t�B
t�B
tKB
x�B
yO@��&B
uB
x�B
w�B
sB
u�B
v�B
��?�D�B
y�B
vqB
yWB
t)A�vUB
r�B
�sB
t�B
��B
u�B
v�B
v�B
t�B�lB
n�B
u�B
xB
t�B
v9B
s#B
u�B
weA���@�>bB
wB
r�B
w0B
x�B
vB
xIB
t�B
wYB
t�B
y^B
x�B
yuB
xoB
t�B
t�B
t�B
v�A4!�B
x�B
weB
z+?��_B
xoB
xoB
xzB
�NB
tf@�L�B
sB
uA��rB
tDB
wGB
wB
x�B
|OAO',B
u�@?B
x�B
u�B
yB
�dB
tnB
z+B
w�B
u�B
s�B
x�B
rB
q9B
�tA�iaB
u�B
wAı8B
w�B
vB
w{B
w�B
t�BA5@��B
u*B
u�B
wBAi-�@�T*B
t�B
u�@�8B
zEB	E�A?fB
s.B
t�A���B
tmB
wdB
t4B
u{B
z�B
t3B
w	B
s*B
t�A �B
t�B
uB
viB
p�B
v"B
vqB
wdALB
q�B
y;B
u�B
r�B
~B
t�B
r�B
ya@�)B
vOB
s�B
q�B
t�B
x�B
wB
v�B
tfB
uXB
w B
s�A���B	�0B
s�B
u�B
xgA�{�B
xw@��AA�#�B
uB
w�B
x�A�J�B
z`B
�
B
sTB
uQB
yB
s�B
v�B
w�B
v�B
tB
urB
nB
s�B
ukB
rB
sB
z1B
v�B
x�B
vtB
w_B
w_B
wWB
uVB
t_B
y.B
x7B
x�B
t�B
sQB
v�B
s�B
o�B
w�B
vB
w�B
uB
ypB
teB
s�B
u\B
pPB
z�B
xxB
v�B
xRB
x7B
u�B
q�B
t�B
y�B
wbB
v�B
w�B
y�B
wlB
x�B
v%B
tGB
v�B
woB
w�B
u�B
z%B
w�B
s�B
x�B
tPB
vQB
t�B
vnB
x�B
x�B
vuB
x�B
x!B
u}B
x,B
tB
u�B
x`B
vB
x�B
w�B
y�B
y�B
x6B
wlB
u�B
r�B
s�B
vVB
w�B
xaB
w�B
y�B
x�B
wiB
x�B
yUB
xpB
v/B
s,B
wB
x`B
vB
rtB
u>B
u�B
wwB
x�B
y�B
v+B
x�B
wLB
w�B
x�B
w;B
xDB
z_B
x�B
w�B
vDB
xB
w�B
w�B
vB
w�B
ycB
vB
v�B
v�B
w�B
wIB
wAB
v�B
t�B
u�B
w�B
x�B
wB
uSB
u�B
x3B
wB
v�B
t�B
v�B
v�B
v�B
wHB
y=B
t�B
wsB
vOB
v�B
w*B
w�B
x6B
u�B
w B
w�B
vJB
v�B
v�B
w�B
u�B
tGB
x&B
v�B
xbB
v:B
w�B
w|B
xB
v�B
xB
v�B
xZB
x�B
w�B
x�B
w�B
whB
yoB
v�B
v�B
v=B
x�B
yCB
w�B
w�B
xCB
wOB
x�B
w�B
w�B
x-B
x1B
x�B
x?B
w�B
w�B
wYB
xuB
wIB
x$B
xUB
w�B
w�B
w�B
w�B
yoB
wOB
wGB
x~B
w�B
y�B
w�B
w�B
vgB
wyB
wB
u�B
xkB
x�B
xB
x�B
x�B
x�B
y�B
y�B
x�B
yBB
xaB
xYB
y�B
y"B
z�B
y/B
y`B
x�B
y�B
yqB
y�B
z�B
x�B
z"B
y�B
y�B
yB
zB
xDB
z B
x�B
zB
y�B
zB
zUB
ytB
zEB
zmB
zeB
{IB
z�B
z�B
y�B
zfB
z|B	�B	�rB	��B	�B	�B	�B	�>B	�kB	��B	�>B	��B	��B	��B	��B	�B	�yB	��B	�lB	�1B	�B	�$B	�B	�B	�B	��B	�CB	��B	�qB	��B	�tB	��B	�B	�hB	��B	�B	�$B	�B	�B	�B	�TB	�WB	�:B	�B	�jB	�>B	�B	��B	�B	�B	��B	�xB	�nB	��B	�SB	�B	��B	��B	��B	�B	�B	�bB	�TB	�B	� B	��B	�jB	��B	�B	�FB	�9B	�,B	�B	�B	�iB	�B	�BB	�5B	�'B	��B	��B	�KB	�=B	�B	�B	�B	�B	�FB	�,B	�B	�B	�B	��B	�B	�B	�wB	�\B	�1B	��B	�OB	�rB	�(B	�XB	�KB	�1B	�$B	�B	�	B	�=B	�]B	�"B	�B	�B	��B	�B	�B	�{B	�B	�4B	�dB	�B
v�B
w5B
x�B
xB
y�B
zB
xhB
w�B
x:B
w�B
y0B
xWB
w�B
xKB
x$B
w1B
x�B
x~B
xB
woB
x�B
xiB
x�B
w�B
v�B
zrB
wB
x�B
xDB
w�B
x4B
xeB
x	B
x�B
wYB
x-B
x8B
x�B
w�B
w�B
x{B
wB
x:B
w]B
x�B
w�B
x�B
x@B
v�B
wB
wAB
w^B
x6B
w�B
w�B
wB
xGB
x	B
v�B
wpB
w{B
w{B
w�B
w�B
w�B
wKB
w�B
wNB
woB
v�B
v�B
x�B
vvB
v�B
w]B
w7B
wB
w�B
yB
xNB
w�B
w�B
v�B
w�B
wB
xB
v�B
w%B
xTB
xB
w�B
w�B
w�B
w�B
wKB
w�B
wB
xvB
w�B
xB
v�B
x�B
w�B
w�B
xB
wEB
w�B
w�B
xB
w�B
w�B
x9B
w3B
xbB
xB
wfB
w�B
xB
w�B
wzB
xB
w�B
w�B
w�B
w,B
wB
wkB
w�B
w�B
wuB
xB
w$B
w�B
xLB
x<B
xGB
wyB
v�B
v�B
w,B
wwB
v�B
w�B
w<B
w�B
w�B
xB
w�B
wkB
wB
w[B
w�B
w�B
x B
v�B
xB
wXB
w�B
xB
w�B
wmB
w�B
xB
wB
w�B
x B
w,B
w$B
w�B
w~B
w�B
v�B
w�B
w�B
w�B
w�B
vxB
wLB
xB
w<B
w,B
w]B
whB
w�B
wHB
w@B
v}B
w�B
w�B
xB
wnB
w�B
w�B
waB
w�B
wB
w�B
w9B
x0B
wbB
wxB
wpB
whB
w�B	�B	�B	��B	�'B	�9B	�B	�B	��B	�B	�=B	��B	�0B	�B	�B	�B	�B	�B	�B	�1B	��B	�B	��B	�aB	�B	�5B	��B	�B	�GB	��B	�B	�B	�B	��B	�dB	�B	�[B	�{B	�B	�jB	�{B	�"B	�B	��B	�sB	�GB	�B	�JB	�jB	�]B	�B	�B	��B	��B	�,B	�B	�B	��B	�B	�sB	�JB	�,B	��B	��B	�B	�B	�dB	�(B	�KB	�0B	�"B	�B	�B	�B	��B	�B	�B	��B	�B	�KB	�1B	��B	��B	�rB	�WB	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�aB	��B	�B	�B	�wB	�\B	�>B	�CB	�5B	�(B	�+B	�B	�1B	�SB	�FB	��B	�B	�B	�B	�B	�B	�B	��B	� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444444444444434443344444444444434444444444444444443443444444444444444444444444434444444444444444444443444444444444444444443444444444444444444334444344444444444444434444443444444434444344434443443444444444444434444434443434444344334333433334333333343333344333343333333334433333333333333343344333333333433333334333333343333333333333333333333343333333333333333343334333334333333334343333333333333333333333343334433433433333333333343333333433333333433333333333433333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B
w�B
x�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
}B
�@B
İB*B]DBX$B[9B^KBP�BY/Bj�Bw�B�SB��B�B�eB��B��B��B��B�FB�B`Bk�B��B�B�cB��B��B��B�OB�KB�B�qB�B�B�B��B�B�B�B��B��B��B�B�0B�{B=�BBB��B1B\BuB|B�B�BByBqB\B
TB>B�B�VBŻB�QB��B��B�uB�+BabBE�BC�B>�B3JB'BuB
��B
�B
�B
�B
�	B
�pB
B
x�B
o�B
C�B
"B	�B	�^B	�B	��B	| B	m�B	O�B	1CB	�B�B�JB��B�\B�B�B�\B��B�B�9B�B��B��B��B�jB� B��B��B��B��B��B�{B�ZB�3B�"B�B�*B�0B�0B�8B�/B�0B�DB�CB�NB�NB�CBB{ B~B�-BB|B~BBB~B~B~B�#B�%B�,B� BB~B{ Bw�Bs�Bq�Bq�Bp�Bp�Bn�Bk�Bg�Bk�Bm�Bl�Bn�Bq�Bt�Br�Bq�Bs�Bt�Bs�Bs�Bq�Bs�Bq�Bp�Bs�By�B�%B�AB�QB�XB�jB��B��B��B��B�B�AB�fB��B�KB�B�B�B�B�iB�B�B�/B�$B�B��B��B�KB�!B�&B�'B~B� B��B��B��B��B��B��B�SB�,B|B}Bw�Bu�Bp�Bj�Bh�Bi�BcuB^YB]RB^VB^VBdyBn�Bs�Bt�Bu�Bx�Bx�B|B}BBB�,B�SB�fB�sB�rB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�<B�{B��B�B��B�	B	)B	;B	MB	
_B	qB	�B	�B	�B	�B	�B	 �B	!�B	�B	�B	�B	�B	#�B	,*B	,,B	.4B	1KB	5aB	8sB	7mB	8uB	5bB	3UB	=�B	J�B	SB	K�B	<�B	/=B	%B	 �B	"�B	+'B	7oB	6iB	3XB	*B	(B	0DB	B�B	H�B	M�B	TB	[GB	ZBB	]TB	Z@B	UB	TB	RB	RB	RB	V(B	^YB	[FB	bqB	`fB	`eB	cuB	f�B	l�B	n�B	s�B	v�B	v�B	w�B	w�B	x�B	x�B	w�B	v�B	w�B	w�B	y�B	|	B	�"B	�6B	�IB	�TB	�aB	�nB	�sB	�lB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�7B	�JB	�RB	�QB	�QB	�WB	�]B	�kB	�kB	�kB	�pB	�qB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�yB	��B	��B	��B	��B	��B	��B	²B	´B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	� B	�#B	�;B	�DB	�PB	�[B	�dB	�dB	�pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�YB
 �B
B
%B
{B
)�B
3$B
3<B
:�B
ADB
G�B
O�B
R�B
X�B
\�B
^VB
c�B
j!B
m1B
s!B
u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�B
cOB
`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
i�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
jdG�O�G�O�B
vVG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
qMG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
l�B
vG�O�G�O�G�O�G�O�B
n_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
u�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�QVG�O�G�O�G�O�G�O�B	��G�O�G�O�G�O�A�X9G�O�G�O�G�O�B
~>G�O�G�O�A�r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�B
��G�O�G�O�G�O�A�$G�O�B
iuG�O�G�O�G�O�G�O�B
qG�O�G�O�A�>�B
p_G�O�B
s�B
{aB
t�G�O�B
v�B
t�B
r�B/�G�O�B
t`B
��A�o�B
s�B
t�B
r@B
z0G�O�B
t�B
vbB
v�B
��B
s�G�O�G�O�B
w�B
swB
tgB
tG�O�B
xB
wHB
vBB
tB
r5B
s�B
v�B
w�B
u@G�O�G�O�B
uyB
v@B
�:B
v�B
wDB
v�B
x
B
rUB
w
B
v]B
w)B
x�A���A�B
�]G�O�B
u�B
q�G�O�G�O�B
wB
v�B
v3B
vGB
zB
x<B�A�$�B
teG�O�B
q�A�"�B
t�B
t�B
t`B
x�B
ycG�O�B
u3B
x�B
w�B
s1B
u�B
v�B
��G�O�B
y�B
v�B
ykB
tAA�vsB
r�B
��B
t�B
��B
u�B
v�B
v�B
t�BǁB
n�B
u�B
x'B
uB
vPB
s9B
u�B
wyA���G�O�B
w/B
sB
wEB
x�B
v3B
x^B
t�B
woB
uB
yqB
x�B
y�B
x�B
t�B
t�B
t�B
v�G�O�B
x�B
wyB
zCG�O�B
x�B
x�B
x�B
�bB
t{G�O�B
s(B
u&A�ȜB
tYB
w_B
w$B
x�B
|eG�O�B
u�G�O�B
yB
u�B
y*B
�{B
t�B
zCB
xB
u�B
s�B
x�B
r3B
qMB
��A�iyB
u�B
w)AıYB
x
B
vB
w�B
w�B
t�BAHG�O�B
uBB
u�B
wYG�O�G�O�B
t�B
vG�O�B
z\B	E�G�O�B
sBB
t�A��B
t�B
wzB
tHB
u�B
z�B
tJB
wB
s@B
t�G�O�B
t�B
uB
v�B
p�B
v5B
v�B
wzG�O�B
q�B
yQB
vB
r�B
�B
t�B
r�B
ywG�O�B
vfB
s�B
q�B
t�B
x�B
wB
v�B
t{B
unB
w4B
s�G�O�B	�FB
tB
u�B
x|A�{�B
x�G�O�G�O�B
u-B
w�B
y A�KB
ztB
�B
siB
uhB
yB
tB
v�B
w�B
v�B
t1B
u�B
n,B
tB
u�B
r*B
s-B
zEB
v�B
x�B
v�B
wvB
wvB
wlB
ukB
tuB
yEB
xMB
x�B
t�B
seB
v�B
s�B
o�B
xB
vB
w�B
uB
y�B
t|B
s�B
uqB
pfB
z�B
x�B
v�B
xeB
xLB
u�B
q�B
t�B
y�B
wwB
v�B
w�B
y�B
w�B
x�B
v:B
tZB
v�B
w�B
w�B
u�B
z:B
w�B
wB
wMB
x�B
x+B
y�B
z"B
x}B
w�B
xOB
w�B
yDB
xkB
w�B
xbB
x;B
wEB
x�B
x�B
xB
w�B
x�B
x}B
x�B
w�B
v�B
z�B
w.B
x�B
xZB
w�B
xJB
xzB
xB
x�B
wqB
xAB
xOB
x�B
w�B
w�B
x�B
wB
xNB
wuB
x�B
xB
x�B
xWB
v�B
wB
wWB
wqB
xMB
w�B
w�B
wB
x]B
x B
w	B
w�B
w�B
w�B
w�B
w�B
w�B
waB
xB
wdB
w�B
v�B
v�B
x�B
v�B
v�B
wrB
wNB
w3B
w�B
yB
xcB
w�B
w�B
v�B
w�B
w3B
x)B
v�B
w<B
xhB
x B
w�B
w�B
w�B
w�B
wbB
w�B
w�B
x�B
w�B
x#B
v�B
x�B
w�B
w�B
x%B
w[B
w�B
xB
x2B
w�B
w�B
xOB
wGB
xwB
x"B
w|B
w�B
x-B
w�B
w�B
x2B
w�B
xB
w�B
wCB
wB
w�B
w�B
w�B
w�B
xB
w:B
w�B
xcB
xSB
x^B
w�B
v�B
wB
wCB
w�B
wB
w�B
wUB
w�B
w�B
x,B
xB
wB
w$B
wrB
w�B
w�B
xB
v�B
x,B
wlB
w�B
x%B
w�B
w�B
xB
xB
w+B
xB
x5B
w?B
w7B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
v�B
wcB
x0B
wSB
wBB
wrB
w~B
w�B
w\B
wVB
v�B
w�B
xB
x4B
w�B
w�B
w�B
wuB
w�B
w0B
x
B
wOB
xEB
wwB
w�B
w�B
w~B
w�B	��B	��B	�B	�KB	�[B	�0B	�BB	��B	�5B	�^B	��B	�TB	�B	�%B	�@B	�B	�6B	��B	�TB	��B	�B	�B	�B	��B	�ZB	�B	�B	�kB	�B	�B	��B	�B	��B	�B	��B	�|B	�B	��B	�B	�B	�DB	�8B	�B	�B	�lB	�B	�mB	�B	�B	��B	�9B	�B	�B	�RB	��B	�B	�
B	��B	�B	�mB	�MB	�B	��B	�B	�B	�B	�KB	�qB	�UB	�GB	�:B	��B	�-B	�B	��B	��B	�B	�B	�oB	�VB	��B	��B	�B	�{B	�3B	�$B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�}B	�`B	�gB	�YB	�LB	�MB	�BB	�VB	�wB	�jB	��B	��B	�B	��B	�B	�)B	�+B	�B	�$B
wB
wMB
x�B
x+B
y�B
z"B
x}B
w�B
xOB
w�B
yDB
xkB
w�B
xbB
x;B
wEB
x�B
x�B
xB
w�B
x�B
x}B
x�B
w�B
v�B
z�B
w.B
x�B
xZB
w�B
xJB
xzB
xB
x�B
wqB
xAB
xOB
x�B
w�B
w�B
x�B
wB
xNB
wuB
x�B
xB
x�B
xWB
v�B
wB
wWB
wqB
xMB
w�B
w�B
wB
x]B
x B
w	B
w�B
w�B
w�B
w�B
w�B
w�B
waB
xB
wdB
w�B
v�B
v�B
x�B
v�B
v�B
wrB
wNB
w3B
w�B
yB
xcB
w�B
w�B
v�B
w�B
w3B
x)B
v�B
w<B
xhB
x B
w�B
w�B
w�B
w�B
wbB
w�B
w�B
x�B
w�B
x#B
v�B
x�B
w�B
w�B
x%B
w[B
w�B
xB
x2B
w�B
w�B
xOB
wGB
xwB
x"B
w|B
w�B
x-B
w�B
w�B
x2B
w�B
xB
w�B
wCB
wB
w�B
w�B
w�B
w�B
xB
w:B
w�B
xcB
xSB
x^B
w�B
v�B
wB
wCB
w�B
wB
w�B
wUB
w�B
w�B
x,B
xB
wB
w$B
wrB
w�B
w�B
xB
v�B
x,B
wlB
w�B
x%B
w�B
w�B
xB
xB
w+B
xB
x5B
w?B
w7B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
v�B
wcB
x0B
wSB
wBB
wrB
w~B
w�B
w\B
wVB
v�B
w�B
xB
x4B
w�B
w�B
w�B
wuB
w�B
w0B
x
B
wOB
xEB
wwB
w�B
w�B
w~B
w�B	��B	��B	�B	�KB	�[B	�0B	�BB	��B	�5B	�^B	��B	�TB	�B	�%B	�@B	�B	�6B	��B	�TB	��B	�B	�B	�B	��B	�ZB	�B	�B	�kB	�B	�B	��B	�B	��B	�B	��B	�|B	�B	��B	�B	�B	�DB	�8B	�B	�B	�lB	�B	�mB	�B	�B	��B	�9B	�B	�B	�RB	��B	�B	�
B	��B	�B	�mB	�MB	�B	��B	�B	�B	�B	�KB	�qB	�UB	�GB	�:B	��B	�-B	�B	��B	��B	�B	�B	�oB	�VB	��B	��B	�B	�{B	�3B	�$B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�}B	�`B	�gB	�YB	�LB	�MB	�BB	�VB	�wB	�jB	��B	��B	�B	��B	�B	�)B	�+B	�B	�$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444444444444434443344444444444434444444444444444443443444444444444444444444444434444444444444444444443444444444444444444443444444444444444444334444344444444444444434444443444444434444344434443443444444444444434444434443434444344334333433334333333343333344333343333333334433333333333333343344333333333433333334333333343333333333333333333333343333333333333333343334333334333333334343333333333333333333333343334433433433333333333343333333433333333433333333333433333344333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.27 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.27 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.27 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455182020082814551820200828145518202008281455182020082814551820200828145518202008281455182020082814551820200828145518202008281455182020082814551820200828145518AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730432019021417304320190214173043    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730432019021417304320190214173043  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730432019021417304320190214173043  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455182020082814551820200828145518  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                