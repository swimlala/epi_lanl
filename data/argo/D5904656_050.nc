CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  h   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:39Z creation      
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
8  m�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (�  x(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
8  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (�  �@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (�  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
8  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� 8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
8 0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� :P   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (� c0   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
8 �   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (� �H   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
8 �(   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (� �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
8     PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� %X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
8 N8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (� Xp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �4Argo profile    3.1 1.2 19500101000000  20190214173039  20200828145509  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               2   2   2AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @ױ��~�P@ױ��~�P@ױ��~�P111 @ױ����@ױ����@ױ����@5��j~��@5��j~��@5��j~���c�x����c�x����c�x���111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    2   2   2ADA BDA  DB BDB @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Ds��Dty�Dt��Dy��D��D�PRD��fD�� D��D�NfD�{�D�ǮD��D�G�D�{3D��fD��D�?
D�y�DමD� �D�AHD�e�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    ����            =���                                                                                        ����                                                                                                                                                                    =���            =���                                                                                                                                                                                                                                                                            =���                            ����                ����                                        =���                                    =���        =���=���                        =���=���            =���=���                        =���            =���    =���        =���=���=���=���=���=���    =���=���>L��=���>L��=���=���    =���>L��>L��=���=���=���=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>���>L��>L��=���=���>L��>L��>L��>L��>���>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>���>L��=���>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>���>���>���>L��>L��=���>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>���>���>L��>L��>L��>L��=���=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>L��=���>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��>L��=���>L��>���>���>���>���>L��>L��>L��=���=���=���>L��>���>���>���>���?   ?��?333?333?L��?fff?fff?�  ?���?���?���?�ff?�33?�33?�  ?���?ٙ�?ٙ�?�ff?�33@   @ff@ff@��@33@��@   @&ff@&ff@,��@333@@  @Fff@S33@Y��@`  @fff@s33@y��@�  @�33@���@���@�  @�ff@���@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ə�@���@�33@�ff@ٙ�@�  @�33@陚@���@�  @�ff@���A   A��A33AffA  A	��A��AffA  A33A��AffA��A33A��AffA!��A#33A$��A&ffA(  A+33A,��A.ffA0  A1��A4��A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A^ffA`  Ac33Ad��AfffAh  Ai��Al��AnffAp  Aq��At��AvffAx  Ay��A{33A~ffA�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�  A���A͙�A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�  A���Aٙ�A�ffA�33A�  A���Aݙ�A�ffA�33Dql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq� Dq�fDq�3Dq��Dr  DrfDr3Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrL�DrY�Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt�3Dt��Du  DufDu3Du�@333@@  @Fff@S33@Y��@`  @fff@s33@y��@�  @�33@���@���@�  @�ff@���@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ə�@���@�33@�ff@ٙ�@�  @�33@陚@���@�  @�ff@���A   A��A33AffA  A	��A��AffA  A33A��AffA��A33A��AffA!��A#33A$��A&ffA(  A+33A,��A.ffA0  A1��A4��A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A^ffA`  Ac33Ad��AfffAh  Ai��Al��AnffAp  Aq��At��AvffAx  Ay��A{33A~ffA�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�  A���A͙�A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�  A���Aٙ�A�ffA�33A�  A���Aݙ�A�ffA�33Dql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq� Dq�fDq�3Dq��Dr  DrfDr3Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrL�DrY�Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt�3Dt��Du  DufDu3Du�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@6fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�A�fgAA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��DqvgDq��Dr|�Dr��Ds|�Ds�gDtvgDt�Dy��D�\D�N�D���D��fD���D�L�D�z=D��D�\D�FD�y�D���D�
D�=pD�xRD�D��\D�?�D�d)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�;���L�ͽL�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�;���L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�;���L�ͽL�ͽL�ͽL�;���L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL��=L��=L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL��=L��=L�ͽL�ͽL�ͽL��=L��=L�ͽL�ͽL�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL��=L�ͽL��=L�ͽL�ͽL��=L��=L��=L��=L��=L��=L�ͽL��=L��=L��>��=L��>��=L��=L�ͽL��=L��>��>��=L��=L��=L��=L��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��=L��>��>��>��>��>�  >��>��=L��=L��>��>��>��>��>�  >��>��>��>��>��=L��>��>��>��>��>��>��>��>��>�  >��=L��>��>��>��>��>��>��>��=L��>��>��>��>�  >��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��=L��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��=L��>��>��>��>��>�  >�  >�  >��>��=L��>��>��>��>��=L��>��>��>��>��>��>��>��>��>��=L��>��>��>��>��>��>��>��>��>��>��=L��>��>��>��>�  >�  >��>��>��>��=L��=L��>��>��>��>��>��>��>��>��>��>��>��>��=L��>��>��>��>��>��>��>��>��>��>��>��>��=L��>��>��>��>��>��>��>��>��=L��>��=L��>��>��>��>��>��>��>��>��>��>��>��>��>��=L��>��>��>��>��>��=L��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��>��=L��>��>�  >�  >�  >�  >��>��>��=L��=L��=L��>��>�  >�  >�33>�33>�ff?��?&ff?&ff?@  ?Y��?Y��?s33?�fg?�34?�34?�  ?���?���?���?�fg?�34?�34?�  ?���?���@33@33@	��@  @fg@��@#33@#33@)��@0  @<��@C33@P  @Vfg@\��@c33@p  @vfg@|��@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@љ�@���@�  @�ff@ᙙ@�  @�33@�ff@���@�  @�ffA ��AffA��A33A��A  A��A33AffA  A��A��AffA  A��A ��A"ffA$  A%��A'33A*ffA,  A-��A/33A0��A4  A5��A733A8��A<  A=��A?33A@��ABffAE��AG33AH��AJffAL  AO33AP��ARffAT  AW33AX��AZffA\  A]��A_33AbffAd  Ae��Ag33Ah��Al  Am��Ao33Ap��At  Au��Aw33Ax��AzffA}��A33A�fgA�34A�  A���A�fgA�34A�  A���A���A�34A�  A���A���A�34A�  A���A���A�fgA�  A���A���A�fgA�34A���A���A�fgA�34A���A���A�fgA�34A�  A���A�fgA�34A�  A���A���A�34A�  A���A���A�fgA�34A���A���A�fgA�34A�  A���A���A�fgA�34A�  A���A���A�fgA�34A�  A�  A���A���A�fgA�34A�  A���A���A�fgA�34A�  A���A���A�fgA�fgA�34A�  A���AÙ�A�fgA�34A�  A���AǙ�A�fgA�34A�  A���A˙�A�fgA�34A�34A�  A���Aϙ�A�fgA�34A�  A���Aә�A�fgA�34A�  A���Aי�A�fgA�34A�  A���Aۙ�A�fgA�34A�  A���Dqi�Dqp Dq|�Dq�3Dq��Dq� Dq�gDq�3Dq��Dq� Dq�gDq��DqɚDq� Dq�gDq��Dq�3Dq� Dq�gDq��Dr3Dr DrgDr�Dr#3Dr)�Dr6gDr<�DrC3DrI�DrVgDr\�Drc3Dri�Drp Dr|�Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�gDr�3DrɚDr� Dr�gDr��Dr�Dr� Dr�gDr��Ds	�Ds DsgDs�Ds)�Ds0 Ds6gDs<�DsI�DsP DsVgDs\�Dsi�Dsp DsvgDs|�Ds�3Ds� Ds�gDs��Ds�3Ds� Ds�gDs��Ds�3Ds� Ds�gDs��Ds�3Ds�Ds�gDs��Dt3Dt	�DtgDt�Dt#3Dt)�Dt6gDt<�DtC3DtP DtVgDt\�Dtc3Dtp DtvgDt|�Dt�3Dt� Dt�gDt��Dt�3Dt� Dt�gDt��Dt�3Dt� Dt�gDt��Dt�3Dt� Dt�gDt��Du3Du Dug@0  @<��@C33@P  @Vfg@\��@c33@p  @vfg@|��@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@љ�@���@�  @�ff@ᙙ@�  @�33@�ff@���@�  @�ffA ��AffA��A33A��A  A��A33AffA  A��A��AffA  A��A ��A"ffA$  A%��A'33A*ffA,  A-��A/33A0��A4  A5��A733A8��A<  A=��A?33A@��ABffAE��AG33AH��AJffAL  AO33AP��ARffAT  AW33AX��AZffA\  A]��A_33AbffAd  Ae��Ag33Ah��Al  Am��Ao33Ap��At  Au��Aw33Ax��AzffA}��A33A�fgA�34A�  A���A�fgA�34A�  A���A���A�34A�  A���A���A�34A�  A���A���A�fgA�  A���A���A�fgA�34A���A���A�fgA�34A���A���A�fgA�34A�  A���A�fgA�34A�  A���A���A�34A�  A���A���A�fgA�34A���A���A�fgA�34A�  A���A���A�fgA�34A�  A���A���A�fgA�34A�  A�  A���A���A�fgA�34A�  A���A���A�fgA�34A�  A���A���A�fgA�fgA�34A�  A���AÙ�A�fgA�34A�  A���AǙ�A�fgA�34A�  A���A˙�A�fgA�34A�34A�  A���Aϙ�A�fgA�34A�  A���Aә�A�fgA�34A�  A���Aי�A�fgA�34A�  A���Aۙ�A�fgA�34A�  A���Dqi�Dqp Dq|�Dq�3Dq��Dq� Dq�gDq�3Dq��Dq� Dq�gDq��DqɚDq� Dq�gDq��Dq�3Dq� Dq�gDq��Dr3Dr DrgDr�Dr#3Dr)�Dr6gDr<�DrC3DrI�DrVgDr\�Drc3Dri�Drp Dr|�Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr�gDr�3DrɚDr� Dr�gDr��Dr�Dr� Dr�gDr��Ds	�Ds DsgDs�Ds)�Ds0 Ds6gDs<�DsI�DsP DsVgDs\�Dsi�Dsp DsvgDs|�Ds�3Ds� Ds�gDs��Ds�3Ds� Ds�gDs��Ds�3Ds� Ds�gDs��Ds�3Ds�Ds�gDs��Dt3Dt	�DtgDt�Dt#3Dt)�Dt6gDt<�DtC3DtP DtVgDt\�Dtc3Dtp DtvgDt|�Dt�3Dt� Dt�gDt��Dt�3Dt� Dt�gDt��Dt�3Dt� Dt�gDt��Dt�3Dt� Dt�gDt��Du3Du DugG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA��AˑhA��A��mA���AʍPA�r�A�ZA�9XA�$�A� �A�
=A��Aɺ^AɋDA�ffA�ZA�9XA��A�A��A��Aȗ�A�K�A�%A���AǺ^AǕ�A�ZA��Aư!A�jAĲ-A�t�A��HA���A�ffA�?}A�(�A��A���A�jA�O�A��wA���A���A��A��A���A��\A��TA�1'A���A�$�A��^A��A�%A���A�?}A�9XA��yA�n�A��jA���A��7A���A��A�\)A�~�A���A�(�A�1'A�x�A��;A���A�1'A���A��A�VA�9XA�x�A���A���A�  A���A�\)A��A��A�ffA�9XA�+A���A�=qA��#A��!A�
=A�ĜA�r�A��A�1A��
A�bA�oA��7A�ffA��A�dZA��mA���A��`A���A`BA|~�AzQ�AxbNAv�RAs��As�At�!At�/As�#Aq\)Ap1'Ao&�Am�;Am�Aj�+Ai�AgO�Af�DAeS�Ac�
Aa|�A^=qA];dAX�DATĜAS��AR��AQoAP��AP�\AP �AN�AN^5AN  AMAMC�AL�`AL(�AK��AK;dAJ$�AHAFI�AE33AC�AB��ABz�AA�AA�A@�9A@VA?�;A?+A>jA=XA<�\A;O�A9�A8�DA7;dA5��A3�TA25?A1K�A0M�A//A-�PA,  A+t�A+VA*�A*1'A)�PA(�A'�-A&�HA%�
A$��A$v�A#��A#XA"�!A!��A ��A A�A
=AJA�A�TAI�A7LA  A~�A33AVA{A��A��A�7A%A�A��A
�A	�7A	33AZAƨAC�A�jAXA�#A��A�hA ��A 9X@��
@�K�@��R@��^@��9@�1@���@�"�@��+@�-@�x�@�b@��y@�G�@�;d@��H@�V@�p�@�1@���@���@��H@�@���@���@�p�@�{@��u@�j@�Q�@ߕ�@�M�@�x�@�X@ܼj@ۥ�@ؼj@�ƨ@�dZ@�n�@�Ĝ@ӝ�@��@���@ѡ�@�?}@�V@���@Ь@�ƨ@��#@�O�@��@�9X@�S�@ʰ!@���@�X@�Ĝ@�r�@�A�@��@��m@ǥ�@ǅ@�C�@�"�@�
=@Ƈ+@ř�@�7L@Ĭ@ÍP@�|�@�t�@�\)@�x�@�  @���@��P@��@�(�@���@�hs@�(�@���@�\)@�S�@��P@��P@�dZ@��!@�ff@�{@��#@��h@��^@��^@���@��h@�`B@�;d@��#@�V@�Ĝ@�j@��@���@��@�"�@��#@�Q�@���@�-@���@�x�@�7L@���@��\@�@��@�hs@��@�ƨ@�V@��m@���@�S�@�
=@�^5@���@�hs@���@���@�r�@�Q�@�Z@�Q�@�Q�@�Q�@�I�@�A�@�A�@�(�@��@��@�K�@�ff@��@�{@��\@�~�@���@�@��#@��@��7@���@��^@���@���@�J@���@��#@���@���@���@��7@�?}@�7L@�?}@�hs@�?}@�X@��@���@�/@��`@�Ĝ@���@��D@�Ĝ@���@�%@�V@��@�V@�Z@���@�l�@�C�@�o@��@��R@�~�@�~�@�V@���@���@���@��^@�x�@�?}@�&�@��@��@��D@��@�|�@��
@��m@�ƨ@��w@��@���@�t�@�\)@�\)@�;d@���@���@���@�~�@�E�@���@��^@���@���@�hs@��@��
@�;d@�o@�+@��@��+@�@��-@�X@�7L@��@�%@���@��/@�Ĝ@���@��@���@��@�|�@�l�@�\)@��@�
=@��R@�^5@�E�@�{@���@���@���@��h@~͟@w� @p�4@ga@^�]@X�@Pu�@Fxl@?��@9%F@2�s@.5?@*0U@%�#@!�~@�@"h@�@�v@(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�l�A��`A��\A�VA��RA���A��wA�/A�G�A�&�A��-A�ffA�
=A�dZA��A�E�A��!A�
=A�|�A��7A��A�{A�~�A��A��`A���A�5?A�^5A��/A��A��jA��#A�-A���A��HA���A�ȴA�1'A��
A��/A�ffA�  A��DA�ffA�C�A��yA�~�A� �A�XA�hsA��A�/A��A���A���A�|�A��A��A�bA�JA���A�/A�"�A��PA��A��9A�1A�oA��^A��FA�~�A��7A�S�A�-A���A�A�VA��A��-A��A�(�A�ĜA���A���A� �A��TA��A�r�A��A�{A�JA�ȴA��wA�-A� �A��jA��RA��jA���A��#A�A�1'A�?}A���A���A� �A��jA�1A��#A��DA��!A��FA���A�?}A���A�|�A�C�A�A�ZA�1'A���A���A���A�-A�JA���A��A�ĜA��^A��-A���A���A��A��hA�/A�n�A�1'A�=qA��^A��A��mA��A��-A��FA�9XA�A�5?A���A�&�A���A���A�bNA�  A��PA�|�A�9XA�Q�A�?}A�I�A��A�I�A���A��#A��PA�$�A��PA�A��A��uA�A�A�A�+A�
=A���A���A�v�A�O�A���A�M�A�9XA�O�A�M�A���A�\)A�ȴA���A�S�A���A��9A��;A�\)A�r�A�33A���A���A��uA�K�A���A�/A��A��DA�ȴA�t�A�  A���A�=qA��RA��A�;dA�9XA�ffA���A�5?A�7LA� �A�7LA�M�A��/Aş�A��yA�v�Aô9AƃA�K�Aʏ\A�^5A��A�jA��!Aé�Aʏ\A�`BAȰ!AʋDA°!Aʕ�Aʗ�Aʕ�Aʕ�AʁA�x�Aɣ�AʓuAʕ�Aʇ+Aʕ�AʋDAʓuAʓuA��Aʕ�AʓuAʕ�A��TAʕ�Aʕ�A�hsAɰ!Aʕ�Aʏ\AʓuAʙ�Aʕ�A� �A�\)AʋDAʏ\AʍPA�K�A�ffAʉ7Aʕ�AʍPAʑhAʉ7Aʏ\A�~�A�z�A�-Aɥ�Aʕ�AʍPAʗ�AʓuAʑhA�`BA�z�A�AʍPAʃAʍPAʍPAʕ�AʋDAʁA�VA�~�A�~�A�oAʃAʇ+A�~�AʋDAʁA�C�Aʇ+Aʏ\Aʏ\A��;A��AʓuAʉ7AʑhA�~�Aʗ�Aʕ�Aɰ!A�oAʏ\A�z�Aʗ�AʓuA��#Aʕ�AʓuAʉ7AƩ�AʑhAʓuAʑhAʇ+Aʏ\AʁAʑhAʓuA��A�|�A�33AʑhAʓuAʍPAʏ\AʓuA�~�AȸRAʉ7A�l�Aʏ\AʋDAʃAʍPAʃAʋDA�|�Aʏ\AʋDAʍPA� �AʑhAʓuAʑhAʕ�A�oA��AȶFAʋDAʍPAʋDAʑhAʏ\Aʏ\AʋDAʍPA��
A���AʑhAʑhA�~�AʁA�{Aʗ�Aʕ�AʅAʍPA�I�AɅAʇ+Aʇ+A�ĜA��AʋDAʅAʓuAʅA�~�AʅAʉ7AʃAʁAʉ7A�|�Aʇ+Aʉ7A�|�Aʉ7Aʇ+Aʉ7AʋDAʉ7A�jAȝ�A�-AʃA�~�A�z�Aʇ+Aɕ�AʅAʃAʉ7A�;dAʉ7Aʇ+A�5?AɁAʉ7A�r�AʓuAʕ�AʓuA�ƨAʕ�AʑhA�t�Aʙ�AʑhA®Aʗ�Aʕ�Aʗ�Aʏ\Aʗ�Aʛ�Aʛ�Aʗ�A�?}A�Aʏ\Aʗ�Aʗ�Aʗ�Aʕ�Aʕ�Aʙ�AʍPAʍPAʍPAʍPAʓuAʕ�A���Aʕ�Aʙ�Aʗ�Aʝ�Aʕ�Aʛ�Aʗ�A���Aʟ�Aʛ�Aʇ+AʑhAʃAʛ�Aʏ\AʓuAʛ�Aʝ�A�K�Aʝ�Aʟ�Aʛ�A��HAʟ�Aʛ�A��Aʝ�Aʟ�Aʟ�Aʟ�Aʛ�Aʛ�A���Aʕ�A�;dA��FA��
A��Aʗ�Aʡ�Aʟ�Aʡ�Aʣ�Aʝ�Aʟ�Aʟ�Aʡ�Aʟ�Aʟ�Aʝ�Aʝ�Aʟ�Aʟ�Aʟ�Aʟ�Aʟ�Aʡ�Aʥ�Aʧ�Aʰ!AʶFAʶFAʼjA�AʾwAʾwAʾwAʺ^A�ƨA��yA��;A��HA���A��A˰!AˮA˧�Aˣ�A˃A��yA��A��yA��A��A��yA�1A�%A�oA��A� �A�/A�A�A�A�A�=qA�5?A���A���A��/A�-A�
=A���A���A�  A���A���A���A���A���A���A��A��A���A��A��A��A��mA��mA��mA��A��A��`A��mA��yA��`A��yA��mA��mA��mA��mA��mA��yA��mA��mA��TA��HA��TA��`A��`A��`A��TA��`A��HA��TA��`A��`A��TA��HA�ĜAʸRAʥ�Aʝ�Aʙ�Aʛ�Aʕ�AʓuAʓuAʏ\AʓuAʏ\AʑhAʏ\Aʏ\Aʉ7AʋDAʅAʅAʅA�|�A�z�A�|�A�x�A�x�A�x�A�x�A�t�A�t�A�r�A�r�A�r�A�p�A�l�A�l�A�hsA�ffA�dZA�ffA�dZA�`BA�bNA�^5A�^5A�^5A�\)A�\)A�ZA�XA�XA�XA�Q�A�Q�A�O�A�I�A�I�A�E�A�E�A�E�A�A�A�=qA�?}A�=qA�;dA�7LA�5?A�5?A�1'A�-A�(�A�&�A�$�A�$�A�$�A�&�A�&�A�&�A�(�A�&�A�(�A�&�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A� �A�"�A�"�A� �A� �A� �A� �A��A� �A�$�A� �A�"�A�$�A�"�A�"�A�"�A�$�A�$�A�&�A�&�A�"�A� �A��A��A��A��A��A�oA�{A�oA�bA�bA�VA�JA�VA�JA�
=A�
=A�
=A�
=A�A�  A�A���A���A���A���A��A��A��A��mA��;A��;A��#A��
@���@���@���@��R@���@���@���@�v�@�v�@�ff@�^5@�V@�^5@�V@�V@�V@�^5@�^5@�V@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�V@�E�@�E�@�M�@�E�@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�5?@�$�@�$�@��@�{@�{@�{@�{@�{@�{@�J@�{@�{@�{@�{@�{@�@���@�@���@���@��@���@���@���@���@��@��@���@��@��@�@�J@�J@�J@�@���@�@�@�@�@�@�@���@�@���@���@��@��#@���@��^@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@��h@��h@��7@��h@��7@��7A�%A�ƨA˶FA˕�A˙�A��A���A��A��yA��A��A��`A�%A�bA�{A��A�$�A�5?A�A�A�?}A�9XA��A���A��TA˩�A�A�A���A���A���A���A���A���A���A���A��A��A��A���A��A��A��yA��mA��mA��A��A��yA��mA��mA��`A��mA��mA��`A��mA��mA��yA��yA��mA��yA��`A��TA��TA��`A��`A��`A��`A��TA��HA��TA��TA��`A��TA��HA��
AʾwAʮAʡ�Aʛ�Aʗ�Aʗ�Aʕ�AʓuAʑhAʑhAʑhAʓuAʏ\Aʏ\AʋDAʉ7Aʇ+AʅAʃAʁA�|�A�|�A�z�A�x�A�v�A�x�A�v�A�t�A�t�A�r�A�r�A�r�A�p�A�jA�ffA�ffA�dZA�dZA�ffA�dZA�`BA�`BA�`BA�`BA�\)A�\)A�ZA�ZA�XA�XA�VA�S�A�O�A�M�A�I�A�G�A�G�A�E�A�C�A�A�A�?}A�=qA�;dA�9XA�9XA�7LA�33A�1'A�-A�(�A�&�A�$�A�&�A�$�A�$�A�&�A�$�A�&�A�&�A�(�A�$�A�"�A�"�A�$�A� �A�"�A�"�A�"�A�"�A� �A� �A� �A�"�A��A�"�A� �A�"�A� �A�"�A�"�A�"�A�"�A�$�A�"�A�$�A�$�A�&�A�$�A�$�A� �A��A��A��A��A�{A�{A�{A�oA�bA�VA�VA�JA�JA�1A�1A�1A�%A�%A�A�A�  A���A���A���A���A��A��A��yA��TA��HA��/A��#A��@�ȴ@��!@���@���@���@���@���@�v�@�n�@�^5@�V@�V@�V@�V@�^5@�V@�V@�V@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�ff@�V@�M�@�E�@�M�@�M�@�E�@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�-@�$�@��@��@�{@�{@�{@�{@�{@�{@�J@�{@�{@�J@�{@�@�@�@�@���@���@���@���@���@��@��@��@��@��@��@���@�J@�J@�@�@�@�@�J@�@�@�@�@�@�@�@���@���@��@���@�@��-@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@���@��h@��h@��h@��h@��7@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999A�ffA��AˑhA��A��mA���AʍPA�r�A�ZA�9XA�$�A� �A�
=A��Aɺ^AɋDA�ffA�ZA�9XA��A�A��A��Aȗ�A�K�A�%A���AǺ^AǕ�A�ZA��Aư!A�jAĲ-A�t�A��HA���A�ffA�?}A�(�A��A���A�jA�O�A��wA���A���A��A��A���A��\A��TA�1'A���A�$�A��^A��A�%A���A�?}A�9XA��yA�n�A��jA���A��7A���A��A�\)A�~�A���A�(�A�1'A�x�A��;A���A�1'A���A��A�VA�9XA�x�A���A���A�  A���A�\)A��A��A�ffA�9XA�+A���A�=qA��#A��!A�
=A�ĜA�r�A��A�1A��
A�bA�oA��7A�ffA��A�dZA��mA���A��`A���A`BA|~�AzQ�AxbNAv�RAs��As�At�!At�/As�#Aq\)Ap1'Ao&�Am�;Am�Aj�+Ai�AgO�Af�DAeS�Ac�
Aa|�A^=qA];dAX�DATĜAS��AR��AQoAP��AP�\AP �AN�AN^5AN  AMAMC�AL�`AL(�AK��AK;dAJ$�AHAFI�AE33AC�AB��ABz�AA�AA�A@�9A@VA?�;A?+A>jA=XA<�\A;O�A9�A8�DA7;dA5��A3�TA25?A1K�A0M�A//A-�PA,  A+t�A+VA*�A*1'A)�PA(�A'�-A&�HA%�
A$��A$v�A#��A#XA"�!A!��A ��A A�A
=AJA�A�TAI�A7LA  A~�A33AVA{A��A��A�7A%A�A��A
�A	�7A	33AZAƨAC�A�jAXA�#A��A�hA ��A 9X@��
@�K�@��R@��^@��9@�1@���@�"�@��+@�-@�x�@�b@��y@�G�@�;d@��H@�V@�p�@�1@���@���@��H@�@���@���@�p�@�{@��u@�j@�Q�@ߕ�@�M�@�x�@�X@ܼj@ۥ�@ؼj@�ƨ@�dZ@�n�@�Ĝ@ӝ�@��@���@ѡ�@�?}@�V@���@Ь@�ƨ@��#@�O�@��@�9X@�S�@ʰ!@���@�X@�Ĝ@�r�@�A�@��@��m@ǥ�@ǅ@�C�@�"�@�
=@Ƈ+@ř�@�7L@Ĭ@ÍP@�|�@�t�@�\)@�x�@�  @���@��P@��@�(�@���@�hs@�(�@���@�\)@�S�@��P@��P@�dZ@��!@�ff@�{@��#@��h@��^@��^@���@��h@�`B@�;d@��#@�V@�Ĝ@�j@��@���@��@�"�@��#@�Q�@���@�-@���@�x�@�7L@���@��\@�@��@�hs@��@�ƨ@�V@��m@���@�S�@�
=@�^5@���@�hs@���@���@�r�@�Q�@�Z@�Q�@�Q�@�Q�@�I�@�A�@�A�@�(�@��@��@�K�@�ff@��@�{@��\@�~�@���@�@��#@��@��7@���@��^@���@���@�J@���@��#@���@���@���@��7@�?}@�7L@�?}@�hs@�?}@�X@��@���@�/@��`@�Ĝ@���@��D@�Ĝ@���@�%@�V@��@�V@�Z@���@�l�@�C�@�o@��@��R@�~�@�~�@�V@���@���@���@��^@�x�@�?}@�&�@��@��@��D@��@�|�@��
@��m@�ƨ@��w@��@���@�t�@�\)@�\)@�;d@���@���@���@�~�@�E�@���@��^@���@���@�hs@��@��
@�;d@�o@�+@��@��+@�@��-@�X@�7L@��@�%@���@��/@�Ĝ@���@��@���@��@�|�@�l�@�\)@��@�
=@��R@�^5@�E�@�{@���@���@���G�O�@~͟@w� @p�4@ga@^�]@X�@Pu�@Fxl@?��@9%F@2�s@.5?@*0U@%�#@!�~@�@"h@�@�v@(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�l�A��`A��\A�VA��RA���A��wA�/A�G�A�&�A��-A�ffA�
=A�dZA��A�E�A��!A�
=A�|�A��7A��A�{A�~�A��A��`A���A�5?A�^5A��/A��A��jA��#A�-A���A��HA���A�ȴA�1'A��
A��/A�ffA�  A��DA�ffA�C�A��yA�~�A� �A�XA�hsA��A�/A��A���A���A�|�A��A��A�bA�JA���A�/A�"�A��PA��A��9A�1A�oA��^A��FA�~�A��7A�S�A�-A���A�A�VA��A��-A��A�(�A�ĜA���A���A� �A��TA��A�r�A��A�{A�JA�ȴA��wA�-A� �A��jA��RA��jA���A��#A�A�1'A�?}A���A���A� �A��jA�1A��#A��DA��!A��FA���A�?}A���A�|�A�C�A�A�ZA�1'A���A���A���A�-A�JA���A��A�ĜA��^A��-A���A���A��A��hA�/A�n�A�1'A�=qA��^A��A��mA��A��-A��FA�9XA�A�5?A���A�&�A���A���A�bNA�  A��PA�|�A�9XA�Q�A�?}A�I�A��A�I�A���A��#A��PA�$�A��PA�A��A��uA�A�A�A�+A�
=A���A���A�v�A�O�A���A�M�A�9XA�O�A�M�A���A�\)A�ȴA���A�S�A���A��9A��;A�\)A�r�A�33A���A���A��uA�K�A���A�/A��A��DA�ȴA�t�A�  A���A�=qA��RA��A�;dA�9XA�ffA���A�5?A�7LA� �A�7LA�M�A��/Aş�A��yA�v�Aô9AƃA�K�Aʏ\A�^5A��A�jA��!Aé�Aʏ\A�`BAȰ!AʋDA°!Aʕ�Aʗ�Aʕ�Aʕ�AʁA�x�Aɣ�AʓuAʕ�Aʇ+Aʕ�AʋDAʓuAʓuA��Aʕ�AʓuAʕ�A��TAʕ�Aʕ�A�hsAɰ!Aʕ�Aʏ\AʓuAʙ�Aʕ�A� �A�\)AʋDAʏ\AʍPA�K�A�ffAʉ7Aʕ�AʍPAʑhAʉ7Aʏ\A�~�A�z�A�-Aɥ�Aʕ�AʍPAʗ�AʓuAʑhA�`BA�z�A�AʍPAʃAʍPAʍPAʕ�AʋDAʁA�VA�~�A�~�A�oAʃAʇ+A�~�AʋDAʁA�C�Aʇ+Aʏ\Aʏ\A��;A��AʓuAʉ7AʑhA�~�Aʗ�Aʕ�Aɰ!A�oAʏ\A�z�Aʗ�AʓuA��#Aʕ�AʓuAʉ7AƩ�AʑhAʓuAʑhAʇ+Aʏ\AʁAʑhAʓuA��A�|�A�33AʑhAʓuAʍPAʏ\AʓuA�~�AȸRAʉ7A�l�Aʏ\AʋDAʃAʍPAʃAʋDA�|�Aʏ\AʋDAʍPA� �AʑhAʓuAʑhAʕ�A�oA��AȶFAʋDAʍPAʋDAʑhAʏ\Aʏ\AʋDAʍPA��
A���AʑhAʑhA�~�AʁA�{Aʗ�Aʕ�AʅAʍPA�I�AɅAʇ+Aʇ+A�ĜA��AʋDAʅAʓuAʅA�~�AʅAʉ7AʃAʁAʉ7A�|�Aʇ+Aʉ7A�|�Aʉ7Aʇ+Aʉ7AʋDAʉ7A�jAȝ�A�-AʃA�~�A�z�Aʇ+Aɕ�AʅAʃAʉ7A�;dAʉ7Aʇ+A�5?AɁAʉ7A�r�AʓuAʕ�AʓuA�ƨAʕ�AʑhA�t�Aʙ�AʑhA®Aʗ�Aʕ�Aʗ�Aʏ\Aʗ�Aʛ�Aʛ�Aʗ�A�?}A�Aʏ\Aʗ�Aʗ�Aʗ�Aʕ�Aʕ�Aʙ�AʍPAʍPAʍPAʍPAʓuAʕ�A���Aʕ�Aʙ�Aʗ�Aʝ�Aʕ�Aʛ�Aʗ�A���Aʟ�Aʛ�Aʇ+AʑhAʃAʛ�Aʏ\AʓuAʛ�Aʝ�A�K�Aʝ�Aʟ�Aʛ�A��HAʟ�Aʛ�A��Aʝ�Aʟ�Aʟ�Aʟ�Aʛ�Aʛ�A���Aʕ�A�;dA��FA��
A��Aʗ�Aʡ�Aʟ�Aʡ�Aʣ�Aʝ�Aʟ�Aʟ�Aʡ�Aʟ�Aʟ�Aʝ�Aʝ�Aʟ�Aʟ�Aʟ�Aʟ�Aʟ�Aʡ�Aʥ�Aʧ�Aʰ!AʶFAʶFAʼjA�AʾwAʾwAʾwAʺ^A�ƨA��yA��;A��HA���A�%A�ƨA˶FA˕�A˙�A��A���A��A��yA��A��A��`A�%A�bA�{A��A�$�A�5?A�A�A�?}A�9XA��A���A��TA˩�A�A�A���A���A���A���A���A���A���A���A��A��A��A���A��A��A��yA��mA��mA��A��A��yA��mA��mA��`A��mA��mA��`A��mA��mA��yA��yA��mA��yA��`A��TA��TA��`A��`A��`A��`A��TA��HA��TA��TA��`A��TA��HA��
AʾwAʮAʡ�Aʛ�Aʗ�Aʗ�Aʕ�AʓuAʑhAʑhAʑhAʓuAʏ\Aʏ\AʋDAʉ7Aʇ+AʅAʃAʁA�|�A�|�A�z�A�x�A�v�A�x�A�v�A�t�A�t�A�r�A�r�A�r�A�p�A�jA�ffA�ffA�dZA�dZA�ffA�dZA�`BA�`BA�`BA�`BA�\)A�\)A�ZA�ZA�XA�XA�VA�S�A�O�A�M�A�I�A�G�A�G�A�E�A�C�A�A�A�?}A�=qA�;dA�9XA�9XA�7LA�33A�1'A�-A�(�A�&�A�$�A�&�A�$�A�$�A�&�A�$�A�&�A�&�A�(�A�$�A�"�A�"�A�$�A� �A�"�A�"�A�"�A�"�A� �A� �A� �A�"�A��A�"�A� �A�"�A� �A�"�A�"�A�"�A�"�A�$�A�"�A�$�A�$�A�&�A�$�A�$�A� �A��A��A��A��A�{A�{A�{A�oA�bA�VA�VA�JA�JA�1A�1A�1A�%A�%A�A�A�  A���A���A���A���A��A��A��yA��TA��HA��/A��#A��@�ȴ@��!@���@���@���@���@���@�v�@�n�@�^5@�V@�V@�V@�V@�^5@�V@�V@�V@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�ff@�V@�M�@�E�@�M�@�M�@�E�@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�-@�$�@��@��@�{@�{@�{@�{@�{@�{@�J@�{@�{@�J@�{@�@�@�@�@���@���@���@���@���@��@��@��@��@��@��@���@�J@�J@�@�@�@�@�J@�@�@�@�@�@�@�@���@���@��@���@�@��-@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@���@��h@��h@��h@��h@��7@��A�%A�ƨA˶FA˕�A˙�A��A���A��A��yA��A��A��`A�%A�bA�{A��A�$�A�5?A�A�A�?}A�9XA��A���A��TA˩�A�A�A���A���A���A���A���A���A���A���A��A��A��A���A��A��A��yA��mA��mA��A��A��yA��mA��mA��`A��mA��mA��`A��mA��mA��yA��yA��mA��yA��`A��TA��TA��`A��`A��`A��`A��TA��HA��TA��TA��`A��TA��HA��
AʾwAʮAʡ�Aʛ�Aʗ�Aʗ�Aʕ�AʓuAʑhAʑhAʑhAʓuAʏ\Aʏ\AʋDAʉ7Aʇ+AʅAʃAʁA�|�A�|�A�z�A�x�A�v�A�x�A�v�A�t�A�t�A�r�A�r�A�r�A�p�A�jA�ffA�ffA�dZA�dZA�ffA�dZA�`BA�`BA�`BA�`BA�\)A�\)A�ZA�ZA�XA�XA�VA�S�A�O�A�M�A�I�A�G�A�G�A�E�A�C�A�A�A�?}A�=qA�;dA�9XA�9XA�7LA�33A�1'A�-A�(�A�&�A�$�A�&�A�$�A�$�A�&�A�$�A�&�A�&�A�(�A�$�A�"�A�"�A�$�A� �A�"�A�"�A�"�A�"�A� �A� �A� �A�"�A��A�"�A� �A�"�A� �A�"�A�"�A�"�A�"�A�$�A�"�A�$�A�$�A�&�A�$�A�$�A� �A��A��A��A��A�{A�{A�{A�oA�bA�VA�VA�JA�JA�1A�1A�1A�%A�%A�A�A�  A���A���A���A���A��A��A��yA��TA��HA��/A��#A��@�ȴ@��!@���@���@���@���@���@�v�@�n�@�^5@�V@�V@�V@�V@�^5@�V@�V@�V@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�^5@�ff@�V@�M�@�E�@�M�@�M�@�E�@�=q@�=q@�=q@�=q@�5?@�5?@�5?@�-@�$�@��@��@�{@�{@�{@�{@�{@�{@�J@�{@�{@�J@�{@�@�@�@�@���@���@���@���@���@��@��@��@��@��@��@���@�J@�J@�@�@�@�@�J@�@�@�@�@�@�@�@���@���@��@���@�@��-@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@���@��h@��h@��h@��h@��7@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                :Lx�:���;%�[;;n�;O;kF5;}��;���;�g�;��o;�ҳ;��8;��+;�ϖ<��<��< �.<,�<H6e<&��<9#�<"	�<2<3g�<!߹<)�<(C�<8'�<F#<8��<5^�<60�<AT�<D��<T�<Q9C<_zN<c��<UQ�<dd�<c��<kp&<a��<aq�<l��<u��<q��<we�<v�F<we�<��4<�X�<��<��o<��<�.�<���<�4D<�d<���<��g<��@=s�=4xB=�&?]��;��;���<j�<z�<�<<28<6�<�j<t�<��< <K<,\<@�<@�<7,R<:�<Ez<?]y<Ht<Mt�<O��<S�h<\�F<a��<e�<l��<g�X<l��<n��<m�h<p�b<un�<{��<���<�	<���<�	<�i/<�:�<��M<�`�<�6�<��^<�1{<��<���<��4<�_<��}<���<�ڥ<��<��<���<�6z<�-<��<�pP<�s.<��4<���<Ʌ�<͉�<��<ڹ�<�~�<���<�>�<�zN<�h4<�bc<�Q�<�w\<��_=�<��2=�`=�=��=�=R�=*�)=2"�=9�a=OV�=A�=}!�=��x=�nY=�F=�R?> �?�{=O=�u=*�=-l�=E��=K�=bl=~��=�D|=�;=�/o=�$5?�L�=B��=�-=Z�=0U2=DR?=Z0=���? %[=&E=+6z=?�c=��X?%�`=%�<��|<��><�2�=>�=P�=#��=5=*d=L��=f[�=���?�*=H�=W��=�:�=�E�=���@P��=��R?+��=���=�k=�>Z��@��A>e�V@!�&>3��?lL>.��@�v�@�y}?r%1>�@�@H��?B1@�x�?b�@A8@�x�?���@�{5@�{�@�|@�{�@�z�>��W@��@�z�@�{5@�{5@�z�@�zN@�y�@�y}@�=@�{�@�{J@�{�@�z%@�zx@�zx@�y�@�82@�z%@�y�@�zN@�{5@�z�@�z�@�vu@�xl@�y�@�y>?��n@�v@�y}@�z�@�z�@�z�@�y}@�z�@�y�@���@|��@7-M@�y}@�v@�zN@�zN@�x�@�x�@�xl@)�<@�y�@�w�@�z%@�x�@�zN@�x�@�x@yj@�w�@�wp@7�@�w�@�y�@�xB@�xB@�sC@�v�@�w�@�z�@�{�@*�@)�@�{J@�{�@�{5@�{�@�|1@�{�@�@} \@�{�@�{5@�z�@�{J@���@�{J@�{5@�zx>ԁo@�z�@�{5@�{5@�z�@�y}@�y@�y�@�z%@Z�@�xB@]��@�z�@�{J@�y�@�zx@�z�@�w�?�LY@�y@�z�@�z�@�y�@�z%@�z�@�x�@�xl@�y}@�w�@�x@�y�@G��@�zx@�z�@�y�@�y�@�j@�w�@��v@�x�@�y�@�y�@�y@�y}@�y�@�y}@�x�@Jm>���@�zx@�zx@�{�@�z%@yѢ@�{�@�y�@�xB@�x�@�t�@�Z�@�y@�y�@�y}@�2v@�y�@�y@�z%@�xB@�x@�x�@�y>@�v`@�v�@�v�@�v6@�v�@�wp@�v�@�w�@�v@�v�@�w2@�v`@�xl@G<6@��n@�s�@�rq@�q�@�v@�M�@�v�@�u�@�v`@�v`@�x@�x�@�x?��+@�x�@�x�@�z%@�y}@�zN@EVC@�{�@�z%?�Ec@�}@�|�@.�@�q�@�|@�}@�|�@�|�@�|�@�|1@�{�@�{J@�{�@�}A@�|�@�}�@�|�@�|[@�|�@�}�@�zN@�z%@�}A@�|�@�}@�}�@Ц@�}k@�|�@�}�@�}�@�}�@�}�@�~?�s.@�}�@�}�@�|[@�}�@�}�@�}A@�}@�|1@�}�@�~@�~R@�~g@�~g@�|�@/�@�}�@�~g@{Y�@�~@�~�@�~�@�c@��@��@�#@��
@�~�@F�"@[D=@�@��
@��,@��k@��k@��8@��{@���@���@��'@��{@��'@��,@���@��#@���@���@���@���@���@���@��7@���@��Q@���@���@��F@���@���@��'@���@��@���@�ݭ@��@��@�	�@�[�@�]�@�]�@�W*@�hI@���@�{�@�{t@���@��I@�}V@��d@��@��_@���@���@��/@��d@�� @��e@��o@���@�l"@�bc@�'�@� \@��@�q@��@��@��@�@��@��@�!-@� �@�!�@�%�@�)@�(@�$�@�"�@�#:@�$�@�&W@�%F@�#y@�#�@�#�@�$J@�$�@�#�@�$J@�$�@�$�@�%�@�%F@�$�@�#�@�#�@�#�@�#�@�$_@�$5@�#�@�$_@�#�@�$�@�%@�%F@�$_@�#:@��@��@��@��@��@��@��@��@��@�
�@�
�@�
�@�
=@�	�@�	�@�	B@��@��@�!@�!@�%@�@�W@�@��@�q@��@�\@�`@�K@�@��@��@� �@���@���@��3@���@��"@��"@���@��@��@@���@��@���@���@��#@��M@���@���@���@���@��^@��@��@��A@��@��@��J@��@���@���@��-@��p@���@��t@��@��@��X@��@��X@��@��@��@��@��@��@��!@��@���@��-@���@���@��@��@���@���@���@��N@��@��_@���@��@��@��@��@��@��(@��g@��#@��#@���@��#@��@���@���@��E@���@���@���@���@��E@���@���@��s@���@��s@��@��I@��s@��@��s@��E@���@���@��U@���@���@��<@���@��@���@��E@��0@���@���@���@��@��,@��o@��@��_@��@P�`@P�m@P�@P�@P�r@P�v@P�
@P�j@P�@P�@P�$@P�$@P��@P��@P��@P�@P�@P��@P�s@P�@P�@P�@P�o@P��@P�o@P�@P�s@P��@P��@P�s@P��@P�R@P�R@P��@P��@P��@P�V@P�V@P�@P�@P��@P�[@P��@P�@P�h@P�l@P�B@P�l@P�l@P�@P�@P�@P�B@P��@P��@P�l@P�@P��@P�u@P�u@P�u@P�K@P��@P��@P��@P��@P�K@P�K@P�K@P�K@P�K@P��@P��@P�@P��@P��@P��@P��@P�h@P��@P�>@P��@P�>@P��@P��@P�B@P�F@P��@P��@P� @P��@P�@P�H@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�L@P��@P��@P��@P��@P��@P��@P��@P�Q@P�Q@P��@P��@P��@P�U@P�U@P��@P�Y@P�
@���@�e�@�ff@�O7@�Ov@�z�@���@���@�~�@��4@��
@�~�@��@��[@��p@��^@���@��S@���@���@��[@��i@��)@��@�}�@��@��@��@��@�@�\@�	@��@�@�C@��@�!@��@�6@�&W@�!�@� �@� 2@�!-@�#%@�$_@�#�@�"�@�!�@�"@�"�@�#d@�!�@�!�@�"�@�#O@�%F@�$�@�$�@�#�@�"S@�#@�#�@�$@�#�@�#�@�$5@�#%@�%@�%�@�'(@�&l@�%�@�$@�"@�U@��@�J@�c@�
�@�
R@�
g@�	W@�	�@�	�@�	W@�	@��@�@��@�@��@��@��@�G@�@��@�K@��@�@�!@�P@� �@� �@� �@� �@� ?@���@��"@���@��z@��&@��&@���@���@��@���@��@���@��#@��<@��@���@��@���@��4@���@���@���@��@��t@��@��N@��)@��-@��@��@���@��d@���@��@���@��!@��@��;@��i@��@��@��u@��G@��G@���@��@��S@���@���@��`@��t@��@���@��@��@��@��@���@��@��@���@��>@��$@��@�� @��t@���@���@��o@���@��0@��@��@��@��@���@���@���@��M@���@��|@��@���@���@���@��@���@��@��@��@���@���@��@��s@���@���@���@��@���@��@��M@��@��@���@�� @��c@��@��)@��B@��[@P��@P��@P��@P�@P��@P�@P��@P�h@P�l@P�y@P��@P� @P�}@P��@P��@P�%@P�O@P��@P�O@P��@P��@P��@P��@P�p@P��@P��@P��@P��@P�B@P�u@P� @P�.@P��@P�}@P��@P�@P�@P��@P�@P�@P�2@P�@P�6@P��@P�i@P��@P�r@P�@P�H@P�H@P�@P�@P��@P�D@P�m@P��@P��@P�L@P�'@P�Q@P�z@P��@P�@P�U@P�@P�@P��@P��@P��@P�@P�@P��@P�Q@P��@P�?@P�?@P�?@P�@P�@P�@P�?@P��@P��@P��@P��@P��@P��@P��@P�@P�Q@P��@P�k@P��@P��@P�,@P�V@P�V@P�V@P�V@P��@P�V@P��@P��@P��@P��@P�(@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�V@P�,@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444344444434444433444443443433333433333333433333333333333333343333333333433333334333333333343333333334433333333333333334333333333333333334333333333333433333333333333344333333333333333333333333333333333334333333333333343333343343343333333333333333333333343333333433333333333333433333333343343433333333333333333333333333333333331422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@P��G�O�G�O�G�O�G�O�G�O�G�O�@��AG�O�G�O�G�O�G�O�G�O�@�v�@�yzG�O�G�O�G�O�G�O�G�O�@�x�G�O�G�O�@�x�G�O�@�{1@�{�@�|	@�{�@�z�G�O�@��@�z�@�{1@�{6@�z�@�zP@�y�@�y�G�O�@�{�@�{J@�{�@�z#@�z{@�z{@�y�@�82@�z&@�y�@�zP@�{6@�z�@�z�@�vv@�xl@�y�@�y=G�O�@�v@�yz@�z�@�z�@�z�@�yz@�z�@�y�@���@|��G�O�@�yz@�v@�zO@�zP@�x�@�x�@�xjG�O�@�y�@�w�@�z$@�x�@�zR@�x�@�x@yj@�w�@�wvG�O�@�w�@�y�@�xB@�xG@�sA@�v�@�w�@�z�@�{�G�O�G�O�@�{J@�{�@�{5@�{�@�|4@�{�@�@} Z@�{�@�{4@�z�@�{G@���@�{J@�{6@�z{G�O�@�z�@�{6@�{5@�z�@�yz@�y@�y�@�z$@Z�@�xD@]��@�z�@�{J@�y�@�zx@�z�@�w�G�O�@�y@�z�@�z�@�y�@�z"@�z�@�x�@�xl@�y@�w�@�x@�y�G�O�@�zy@�z�@�y�@�y�@�j�@�w�@��v@�x�@�y�@�y�@�y@�y�@�y�@�y~@�x�G�O�G�O�@�zy@�zy@�{�@�z$@yў@�{�@�y�@�xB@�x�@�t�@�Z�@�y@�y�@�y�@�2v@�y�@�y@�z$@�xB@�x@�x�@�y>@�v`@�v�@�v�@�v8@�v�@�wp@�v�@�w�@�v@�v�@�w3@�v`@�xnG�O�@��o@�s�@�rp@�q�@�v@�M�@�v�@�u�@�v`@�vc@�x@�x�@�xG�O�@�x�@�x�@�z$@�yz@�zPG�O�@�{�@�z'G�O�@�}@�|�G�O�@�q�@�|	@�} @�|�@�|�@�|�@�|6@�{�@�{G@�{�@�}>@�|�@�}�@�|�@�|]@�|�@�}�@�zR@�z$@�}C@�|�@�}@�}�G�O�@�}n@�|�@�}�@�}�@�}�@�}�@�~G�O�@�}�@�}�@�|\@�}�@�}�@�}E@�}@�|2@�}�@�~@�~R@�~g@�~g@�|�G�O�@�}�@�~j@{Y�@�~@�~�@�~�@�b@��@��G�O�@��@�~�G�O�@[D;G�O�@��@��.@��j@��j@��6@��~@���@���@��(@��y@��&@��2@���@��%@��~@���@�� @���@���@���@��7@���@��Q@���@���@��C@���@���@��*@���@��@���@�ݮ@��@��G�O�@�e�@�ff@�O8@�Ov@�z�@���@���@�~�@��4@��@�~�@��@��]@��p@��]@���@��R@���@���@��Y@��g@��*@��@�}�@��@��@��@��@�@�`@�@��@�@�@@��@�"@��@�3@�&X@�!�@� �@� 1@�!.@�#&@�$e@�#�@�"�@�!�@�"@�"�@�#g@�!�@�!�@�"�@�#L@�%F@�$�@�$�@�#�@�"Q@�#@�#�@�$@�#�@�#�@�$8@�#$@�%
@�%�@�'%@�&k@�%�@�$@�#@�T@��@�I@�`@�
�@�
R@�
f@�	W@�	�@�	�@�	\@�	@��@�@��@�
@��@��@��@�J@�
@��@�M@��@�@�!@�L@� �@� �@� �@� �@� B@���@��"@���@��z@��&@��&@���@���@��@���@��@���@��(@��>@��@���@�� @���@��7@���@���@���@��@��r@��@��M@��*@��(@��@��	@���@��b@���@��@���@��#@��@��>@��i@��@��@��x@��G@��H@���@��@��U@���@���@��^@��s@��@���@��@��@��@��@���@��@��@���@��=@��'@��@��@��v@���@���@��q@���@��/@��~@��@��@��@���@���@���@��O@���@��y@��@���@�� @���@��@���@���@��@��@���@���@��"@��s@���@���@���@��@���@��@��J@��@��@���@��$@��h@��@��*@��E@��[@P��@P��@P��@P�"@P��@P�@P��@P�f@P�k@P�z@P��@P�@P�z@P��@P��@P�%@P�R@P��@P�N@P��@P��@P��@P��@P�r@P��@P��@P��@P��@P�B@P�s@P�@P�-@P�}@P�}@P��@P�@P�@P��@P�@P�@P�3@P�@P�8@P��@P�h@P��@P�s@P�"@P�H@P�F@P�&@P� @P��@P�@@P�m@P��@P��@P�M@P�*@P�P@P�z@P��@P��@P�S@P�}@P�}@P��@P��@P��@P�~@P��@P��@P�M@P��@P�B@P�C@P�=@P�@P�@P�@P�@@P��@P��@P��@P��@P��@P��@P��@P�@P�R@P��@P�m@P��@P��@P�-@P�V@P�X@P�X@P�X@P��@P�V@P��@P��@P��@P��@P�+@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�U@P�.@P��@P��@P��G�O�@�e�@�ff@�O8@�Ov@�z�@���@���@�~�@��4@��@�~�@��@��]@��p@��]@���@��R@���@���@��Y@��g@��*@��@�}�@��@��@��@��@�@�`@�@��@�@�@@��@�"@��@�3@�&X@�!�@� �@� 1@�!.@�#&@�$e@�#�@�"�@�!�@�"@�"�@�#g@�!�@�!�@�"�@�#L@�%F@�$�@�$�@�#�@�"Q@�#@�#�@�$@�#�@�#�@�$8@�#$@�%
@�%�@�'%@�&k@�%�@�$@�#@�T@��@�I@�`@�
�@�
R@�
f@�	W@�	�@�	�@�	\@�	@��@�@��@�
@��@��@��@�J@�
@��@�M@��@�@�!@�L@� �@� �@� �@� �@� B@���@��"@���@��z@��&@��&@���@���@��@���@��@���@��(@��>@��@���@�� @���@��7@���@���@���@��@��r@��@��M@��*@��(@��@��	@���@��b@���@��@���@��#@��@��>@��i@��@��@��x@��G@��H@���@��@��U@���@���@��^@��s@��@���@��@��@��@��@���@��@��@���@��=@��'@��@��@��v@���@���@��q@���@��/@��~@��@��@��@���@���@���@��O@���@��y@��@���@�� @���@��@���@���@��@��@���@���@��"@��s@���@���@���@��@���@��@��J@��@��@���@��$@��h@��@��*@��E@��[@P��@P��@P��@P�"@P��@P�@P��@P�f@P�k@P�z@P��@P�@P�z@P��@P��@P�%@P�R@P��@P�N@P��@P��@P��@P��@P�r@P��@P��@P��@P��@P�B@P�s@P�@P�-@P�}@P�}@P��@P�@P�@P��@P�@P�@P�3@P�@P�8@P��@P�h@P��@P�s@P�"@P�H@P�F@P�&@P� @P��@P�@@P�m@P��@P��@P�M@P�*@P�P@P�z@P��@P��@P�S@P�}@P�}@P��@P��@P��@P�~@P��@P��@P�M@P��@P�B@P�C@P�=@P�@P�@P�@P�@@P��@P��@P��@P��@P��@P��@P��@P�@P�R@P��@P�m@P��@P��@P�-@P�V@P�X@P�X@P�X@P��@P�V@P��@P��@P��@P��@P�+@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�U@P�.@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444344444434444433444443443433333433333333433333333333333333343333333333433333334333333333343333333334433333333333333334333333333333333334333333333333433333333333333344333333333333333333333333333333333334333333333333343333343343343333333333333333333333343333333433333333333333433333333343343433333333333333333333333333333333331422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��]9���9���9���9��9��9��9��9���9�ћ9�˷9�܇9�߂9��9��9��R9��9�<9� r9��X9��9��%9��9���9�t9�t9�p(9�pO9�p�9�q�9�qh9�nq9�q}9�r�9�s�9�u#9�t�9�u29�{�9�w�9�v{9�v9�v�9�x�9�y�9�y\9�xT9�wp9�w�9�xR9�y9�w�9�w�9�x|9�x�9�z�9�z9�z9�y_9�x9�x�9�y79�y�9�y`9�yo9�y�9�x�9�z9�{)9�|h9�{�9�{?9�y�9�p�9�jm9�f9�d	9�c59�b�9�b@9�bR9�a]9�a�9�a�9�aa9�a9�`�9�`+9�_�9�_F9�^89�^=9�]Z9�[�9�[�9�[W9�Z�9�Z�9�Z�9�Z�9�Z9�Y�9�Y�9�Y�9�Y�9�Y9�W�9�VI9�V9�U�9�Ud9�Ud9�T�9�S�9�Sx9�S9�R�9�R59�Q�9�P�9�P�9�Pk9�O�9�N�9�N19�M9�L9�J�9�Jb9�I�9�I39�H�9�G�9�F�9�Fr9�E�9�D�9�D]9�C�9�B�9�A�9�@�9�?,9�?�9�>�9�@"9�@9�@�9�A�9�A�9�B&9�B�9�Ci9�C�9�D�9�EB9�EU9�E�9�E�9�E�9�Ez9�E}9�E�9�F�9�H&9�G�9�G�9�G�9�H�9�I79�I�9�I�9�J-9�J-9�J�9�J.9�J�9�K�9�K�9�L�9�M&9�M9�M�9�M�9�M_9�L�9�L�9�L?9�L9�L/9�L,9�K�9�L	9�K�9�K�9�K�9�M9�M�9�N9�Nh9�N�9�N�9�N�9�Ny9�M�9�M�9�MZ9�M&9�K�9�K9�I�9�H�9�H)9�G�9�F�9�F(9=k�9=gp9=e�9=e9=f�9=f�9=d�9=^�9=^9=[d9=Y�9=Z9=Z|9=Z�9=Z�9=[9=[@9=[�9=[=9=[�9=\o9=[�9=[�9=].9=\m9=\k9=[�9=[�9=]�9=\F9=Z9=YN9=Y�9=Z9=Y�9=Y(9=Y+9=X�9=Y+9=XD9=Xk9=XB9=W�9=V09=U�9=T=9=T9=S�9=S�9=S�9=S�9=S�9=S�9=T�9=T�9=Tg9=UJ9=S9=R	9=R+9=RR9=Q�9=Qo9=QF9=Ql9=Ql9=Q�9=Q�9=Q�9=Qm9=Qo9=Q�9=R)9=Uu9=U�9=U�9=U�9=T�9=T�9=U�9=U�9=U'9=UM9=U!9=U'9=U&9=U'9=T=9=S�9=R-9=O�9=L�9=J�9=Ig9=H9=H69=H89=H89=H89=H�9=H69=H~9=H�9=H�9=H�9=H�9=H�9=H�9=H�9=H�9=H�9=H�9=H�9=H�9=H^9=H59=H9=G�9=Gv9=E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
	7B
1'B
8RB
6FB
<jB
?}B
D�B
F�B
F�B
F�B
L�B
S�B
_;B
k�B
x�B
�bB
�B
ȴB
��B
�
B
�B
�#B
�/B
�fB
��B
=B �B(�B+B7LBJ�Bt�B��B�B�LB�XB��BiyBgmBu�B�%B�B�)B��B��B��B��B�B%�B�B�B�B�B �B$�B%�B%�B&�B,B)�B%�B�B�B �B�B#�B.B�B-B�B,B+B�B"�B!�B�B/B33B�B��B�yB�B��B��B%B%B��B�B�NB�
B�HB��B��B�B�B�B�;BŢB�hBE�B�BB
�fB
�
B
��B
�3B
��B
�uB
~�B
x�B
k�B
P�B
0!B
�B
B	�B	�mB	��B	�ZB
�B
�B
�B
hB
	7B	��B	�B	�B	�B	��B	��B	�dB	�B	��B	�{B	�%B	~�B	iyB	W
B	P�B	I�B	D�B	C�B	?}B	<jB	5?B	33B	1'B	/B	,B	(�B	#�B	 �B	�B	oB	B��B�B�B�yB�sB�yB�B�B�B�B�B�B�B�B�B�HB�B��B��B��B�LB�'B�B��B��B��B�oB�bB�\B�VB�JB�=B�+B�DB�JB�DB�7B�%B�B~�B|�By�Bw�Bq�BhsB`BB[#BW
BS�BP�BO�BP�BO�BO�BM�BM�BK�BJ�BG�BE�BC�BB�BA�B@�B?}B>wB<jB:^B9XB8RB7LB6FB6FB6FB5?B5?B5?B6FB7LB7LB7LB7LB7LB7LB7LB7LB6FB7LB7LB7LB8RB:^B>wB<jB<jBC�BB�B@�B>wB=qB>wB>wB>wB>wB@�BA�BB�BC�BE�BK�BM�BM�BO�BQ�BR�BT�BYBYBZBZBZBYBZB^5B`BB`BBe`Bl�Bp�Bt�Bv�B|�B|�B|�B|�B|�B~�B� B�B�B�B�+B�JB�PB�\B��B��B��B��B��B��B�hB�%B�B�B�B�1B�VB��B��B��B��B�B�!B�FB�LB�jB�}B��BĜBǮBɺBɺBɺB��B��B��B��B��B��BɺBƨBB�wB�dB�dB��B��B��BƨB��B�ZB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B		7B	
=B	
=B	
=B	DB	\B	uB	{B	�B	�B	�B	�B	"�B	#�B	(�B	/B	2-B	6FB	9XB	>wB	B�B	L�B	M�B	Q�B	Q�B	VB	[#B	bNB	e`B	e`B	hsB	l�B	m�B	r�B	s�B	u�B	w�B	y�B	|�B	~�B	�B	�B	�B	�B	�+B	�PB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�?B	�FB	�LB	�XB	�dB	�qB	�qB	�wB	��B	ĜB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�5B	�BB	�HB	�HB	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	��B
�B
�B
!�B
)*B
3hB
;dB
D�B
J�B
NVB
S�B
Y1B
]~B
bB
f�B
k6B
m�B
r�B
v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<A�<B7H<B��<BT�<B�N<Bv2<Ba9<Bzc<V�<�T7<�f<�%�<�\�<�6�<�k<���<�%=3�<��F=�0=n�='c=2t\=Bg�=T��=c �=��0=\T=q7�=U�T=h�R=j]l=U��=^��=\�=ob�=��=p C=l�=m=z�=~!�=�h4=�j�=���=�L#=��2=�=��V=��t=�dg=���=�a�=��b=�?�=���=�#�=���=�k=��1=���=�vG=��=��=�=�'=�b=܃#> /�>�>.�>a��>���@�%�=-�u=,�3=2p*=9�=7_�=A%�=@2U==X�=F�=Q�=T�@=b�$=zC�=z�L=oVe=t0�=�/0=y"�=���=���=��=��=�dR=�LI=�:=���=��/=��=���=�+�=�<=�]>=��=�VK=�Z�=�u�=��$=��{=��=�a=��=��
=�`=���=�2�=�^�=���=��G=�A-=ϸo=�t=͂�=�W�=���=��i=�_�=�%J=��9=��=�a�=��=�Wy>�>�T>	�+>�0>JQ>�>��>^�><�>_J>��>z�>!�>�9>"�>8N�>G�B>E(�>=m>W�>bDc>mtO>��>t�V>��6>�1�?�>��>�(�?;��A2ѱ>?	S>@�[>V�v>[J>zs�>��n>��M>���>��x>�BW>�v`?�@�!>u|N>%F�>?j�>[vq>u�>���?�-@b��>P��>VҪ>os;>�/#@i��>!,(>>s> />6�{>9ߘ>L��>c�2>U�j>��M>��z>�.@�!>|Z�>��>���>�B�?��A��<>��@q��>��>�6?�?�n�A�Z0?��DAw��?glH@6�)?\�&B	��B	��@���?=łAJ�DA��@9y�B	�G@N�A�P�B	ŮA�B	�B	ûB	��B	�[B	ˇ?��MA�ۭB	�NB	�B	�B	ÇB	�B	ÍB	�@Ao	�B	�[B	��B	�[B
�B	�aB	�aB	ӺB3B	�B	�B	�B	B	��B	�,B	J�B	�aB	�BB	�[@�*B	�0B	�#B	ÇB	��B	�OB	�#B	�)B	�SBDyAȀ�A�p%B	�yB	�kB	�sB	��B	�YB	�B	ˡA��	B	ŻB	��B	�/B	�B	�2B	ŮB	��A��yB	�JB	�$A��B	��B	�7B	��B	�;B	ąB	�B	�.B	�B	��A�z-AqrB	��B	�B	ŜB	�B	�/B	�[B��A�wB	ưB	�)B	��B	��A߽dB	�!B	��B	�
@3vB	�bB	��B	ŜB	��B	��B	��B	�TB	��A���B
,A�BKB	�OB	��B	��B	ŵB	�NB	ɘA+,�B	��B	�MB	�)B	ƩB	�B	ƣB	��B	�aB	��B	�B	�B	��A�	�B	��B	ĈB	�{B	��A���B
 A��JB	ŮB	��B	ƂB	çB	��B	��B	�TB	��A��q?���B	��B	��B	�B	��AŎxB	��B	��B	ǐB	��B	�B
�B	ǉB	�^B
�A�|�B	ƩB	�PB	��B	ǐB	ɾB	�B	��B	ƛB	ǯB	�mB	��B	�ZB	�AB	�dB	�sB	ĸB	�mB	�@B	�FB	��A�x�B �B	�YB	ċB	�B	��B	��B	��B	��B	�FB	��B	��B	�cB	�A)��B	�uB	�)B	��B	�yB	�A�^(B	ĨB	ġA.�B	�(B	�1A�W�B	�lB	��B	��B	��B	��B	�B	¡B	��B	��B	��B	�EB	ĪB	�vB	ďB	�B	�VB	įB	�VB	�/B	�B	�rB	�}B	�=As�B	�B	�B	�vB	�5B	ƊB	��B	��@�.>B	�[B	�6B	ʍB	�B	͉B	ÛB	�B	ŽB	�6B	ÕB	��B	��B	�B	�NA���B	¨B	ĩA��YB	ÝB	âB	ÚB	��B	��B	��Al�B	ȀB
KA��8A��Am��B	ǹB	��B	��B	��B	��B	�uB	��B	�	B	ƊB	ǖB	�AB	� B	ɔB	�B	ƎB	��B	ˑB	ʏB	җB	�bB	�B	�8B	��B	�7B	�-B	�B	�B	�B	�~B	�eB	��B	��B
�B
�B
�B
tB
&B
(�B
*�B
&nB
B�B
5�B
*/B
-B
3IB
3�B
.�B
6QB
6�B
7�B
8�B
<�B
;SB
:�B
9�B
34B
/�B
0�B
&4B
 B
'cB
-�B
-�B
/.B
-�B
.�B
.�B
/�B
0oB
1B
5�B
6�B
7�B
9�B
>QB
<�B
<�B
<pB
<�B
=�B
="B
<B
=�B
=B
<FB
>%B
<�B
<�B
=EB
=oB
=�B
>YB
=<B
=aB
<�B
=�B
>�B
>B
=�B
=�B
=1B
>dB
<�B
?�B
>�B
>HB
=lB
=B
9�B
>B
>^B
A�B
C�B
D6B
B�B
D�B
EzB
DoB
FCB
D^B
E�B
D{B
E;B
D�B
F~B
D�B
FhB
F`B
EhB
G�B
G�B
F�B
G�B
GhB
G�B
GEB
G�B
G�B
HFB
H+B
G�B
G�B
H'B
GBB
HiB
H�B
H�B
HB
HMB
IaB
G�B
H�B
H%B
G�B
H|B
GB
G�B
HBB
GfB
FOB
HeB
GNB
F�B
H8B
GoB
HoB
HLB
GB
G�B
H�B
G
B
G/B
G9B
H&B
H�B
GHB
H�B
H�B
I�B
K B
KsB
LB
L]B
L B
LmB
L\B
L�B
N/B
NB
OB
O�B
PkB
QB
Q�B
R)B
R!B
RB
S&B
R�B
SQB
T�B
T�B
U5B
U�B
V�B
VB
T�B
V�B
VB
U�B
V!B
VgB
V�B
W?B
W�B
V�B
W3B
X�B
Y�B
Y�B
[;B
\OB
\mB
[�B
]�B
\�B
]|B
^B
^ B
^�B
_�B
_�B
a�B
baB
bB
b�B
bHB
eB
f B
d�B
e�B
fOB
elB
g4B
g�B
g�B
i(B
j(B
l�B
l*B
mB
nRB	�IB	�3B	��B	��B	�wB	�B	�uB	��B	�wB	�B	�dB	�gB	��B	�B	�B	��B	��B	�!B	��B	�DB	�7B	�B	�MB	�}B	�2B	��B	�RB	�B	�.B	�*B	��B	�B	�B	��B	�!B	��B	�B	�B	�UB	�KB	��B	�B	�uB	�B	�xB	��B	�B	�B	�B	�TB	�FB	�9B	�\B	�B	�B	�5B	�B	��B	�B	�B	�B	�mB	��B	�&B	�B	��B	�B	�B	�B	�B	��B	�[B	�B	�hB	�B	��B	��B	�NB	�B	�B	�WB	�B	�=B	��B	�B	�`B	�B	�AB	�>B	�B	�B	�B	�_B	�|B	�5B	�fB	�YB	�B	�B	�5B	�B	�B	�=B	��B	��B	��B	�B	�B	�B	�XB	� B	��B	�B	�B	�TB	�	B	�B	�sB	�,B	�(B	�B
&�B
-�B
$�B
#AB
*�B
3NB
3FB
0DB
2!B
3�B
1eB
5�B
4�B
8B
8zB
;
B
>�B
E'B
DB
DKB
B{B
@\B
=(B
F�B
.{B
-�B
,�B
,B
-B
.CB
-�B
,iB
.�B
0uB
3iB
4�B
4B
3�B
;�B
9�B
9�B
9�B
:�B
;B
<B
<SB
;�B
;B
<-B
;�B
<�B
;�B
:�B
;�B
;~B
=DB
=iB
<�B
=WB
<�B
=aB
=B
=qB
=&B
=1B
>>B
>B
>�B
?�B
@B
@B
@NB
B{B
B�B
B�B
B�B
CB
C�B
CB
C�B
DXB
DB
DZB
D>B
C5B
DoB
D@B
D�B
EpB
E�B
EGB
FB
E�B
E�B
E�B
FB
FbB
F�B
FB
F�B
F�B
FeB
GB
G	B
GB
GMB
H-B
H>B
G�B
H[B
HB
G5B
GyB
G�B
GvB
F�B
F�B
G�B
G!B
GB
F�B
G4B
F�B
FVB
FhB
F�B
F�B
F�B
GB
F�B
F�B
F�B
F�B
FxB
F�B
F�B
F�B
FB
FaB
F�B
F�B
F�B
F�B
H"B
H!B
H�B
I2B
I�B
I�B
J�B
J^B
J�B
J�B
L�B
N�B
N�B
N3B
O�B
O8B
OB
OB
N�B
P!B
P�B
RgB
Q$B
R�B
Q:B
R�B
R�B
S�B
S"B
SgB
S_B
S�B
R�B
S�B
TB
TB
T7B
UkB
UOB
W�B
YB
YkB
Y�B
Z*B
Z�B
ZGB
ZeB
[%B
[�B
\~B
\jB
\�B
\�B
_�B
`�B
`�B
a�B
b/B
c�B
c�B
d!B
d3B
d�B
e?B
e�B
f�B
f�B
g�B
i(B
i&B
jNB
j9B
j$B	�kB	��B	�B	� B	�5B	�GB	�B	�#B	�mB	�WB	�'B	�8B	�zB	�B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�AB	�4B	�aB	�B	�#B	��B	�B	�uB	�B	�AB	�B	�%B	�B	��B	��B	�;B	�MB	�!B	�B	�xB	�1B	��B	�B	�kB	�|B	�bB	�6B	�)B	�B	��B	��B	�sB	�B	�FB	�bB	�sB	�B	��B	�B	�B	�B	�B	��B	�B	�B	�\B	�BB	�SB	�B	�(B	�KB	�OB	�AB	�\B	�BB	��B	��B	�wB	�B	�OB	�BB	�5B	�(B	�eB	��B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�B	�sB	�fB	�LB	�]B	�aB	�B	��B	�B	�B	�B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444344444434444433444443443433333433333333433333333333333333343333333333433333334333333333343333333334433333333333333334333333333333333334333333333333433333333333333344333333333333333333333333333333333334333333333333343333343343343333333333333333333333343333333433333333333333433333333343343433333333333333333333333333333333331422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999B
	<B
1+B
8VB
6KB
<nB
?�B
D�B
F�B
F�B
F�B
L�B
S�B
_?B
k�B
x�B
�fB
�B
ȶB
��B
�B
�B
�*B
�4B
�jB
��B
?B �B(�B+B7OBJ�Bt�B��B�B�QB�ZB��Bi}BgsBu�B�'B�B�,B��B�B��B��B�B%�B�B�B�B�B �B$�B%�B%�B&�B,B)�B%�B�B�B �B�B#�B.B�B-B�B,B+B�B"�B!�B�B/"B39B�B��B�}B�#B��B��B*B*B��B�B�PB�B�LB��B��B�B�B�B�>BŦB�lBE�B�BB
�iB
�B
��B
�:B
��B
�xB
~�B
x�B
k�B
P�B
0%B
�B
B	�B	�qB	�B	�_B
�B
�B
�B
nB
	;B	��B	�B	�B	�B	��B	��B	�iB	� B	��B	��B	�*B	 B	i~B	WB	P�B	I�B	D�B	C�B	?�B	<pB	5DB	39B	1*B	/#B	,B	(�B	#�B	 �B	�B	sB	B��B�B�B�|B�xB�~B�B�B�B�B�B�B�B�B�B�MB�B��B��B��B�QB�.B�B��B��B��B�uB�iB�aB�[B�NB�CB�3B�IB�QB�JB�=B�+B� B~�B|�By�Bw�Bq�BhwB`HB[)BWBS�BP�BO�BP�BO�BO�BM�BM�BK�BJ�BG�BE�BC�BB�BA�B@�B?�B>�B<pB:cB9^B8ZB7PB6LB6MB6OB5EB5DB5FB6LB7QB7PB7RB7QB7SB7PB7SB7RB6LB7QB7SB7TB8WB:eB>~B<pB<pBC�BB�B@�B>B=wB>~B>B>}B>|B@�BA�BB�BC�BE�BK�BM�BM�BO�BQ�BR�BUBYBYBZ'BZ#BZ$BYBZ"B^<B`HB`FBeeBl�Bp�Bt�Bv�B|�B|�B|�B|�B|�BB�B�B�B�B�1B�OB�XB�cB��B��B��B��B��B��B�lB�.B�B�B�B�8B�[B��B��B��B��B�B�'B�KB�RB�qB��B��BġBǵBɿB��B��B��B��B��B��B��B��BɿBƪBB�B�kB�lB��B��B��BƮB��B�aB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	!B		@B	
EB	
EB	
CB	LB	dB	{B	�B	�B	�B	�B	�B	"�B	#�B	(�B	/$B	25B	6LB	9\B	>}B	B�B	L�B	M�B	Q�B	Q�B	VB	[)B	bRB	efB	efB	hyB	l�B	m�B	r�B	s�B	u�B	w�B	y�B	|�B	~�B	�B	�B	�B	�!B	�0B	�UB	�mB	�tB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�EB	�LB	�SB	�_B	�jB	�yB	�xB	�{B	��B	ĢB	ƮB	ǵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�+B	�*B	�*B	�.B	�<B	�IB	�OB	�OB	�UB	�aB	�jB	�hB	�lB	�lB	�lB	�sB	�zB	�zB	�G�O�B	�B	��B
�B
�B
!�B
)/B
3pB
;mB
D�B
J�B
N]B
S�B
Y8B
]�B
bB
f�B
k<B
n B
r�B
v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��EG�O�G�O�G�O�G�O�G�O�G�O�A�Z8G�O�G�O�G�O�G�O�G�O�B	��B	��G�O�G�O�G�O�G�O�G�O�B	�MG�O�G�O�B	ųG�O�B	�B	��B	��B	�^B	ˋG�O�A�۶B	�QB	�B	ɂB	ËB	�!B	ÑB	�EG�O�B	�^B	��B	�^B
�B	�gB	�gB	��B7B	�B	�"B	�B	B	��B	�0B	J�B	�dB	�FB	�_G�O�B	�6B	�%B	ËB	��B	�SB	�%B	�-B	�WBD�AȀ�G�O�B	�{B	�pB	�xB	��B	�]B	�B	ˣG�O�B	ſB	��B	�2B	�B	�5B	ųB	��A��B	�OB	�+G�O�B	��B	�;B	��B	�?B	ćB	�B	�2B	�B	��G�O�G�O�B	��B	�B	ŞB	�B	�4B	�^B��A�w#B	ƴB	�+B	��B	��A߽kB	�&B	��B	�G�O�B	�eB	��B	ŞB	��B	��B	��B	�ZB	��A���B
,A�BOB	�SB	��B	��B	ŹB	�QB	ɚG�O�B	��B	�RB	�-B	ƫB	�B	ƪB	��B	�dB	��B	�B	�B	��G�O�B	��B	ċB	āB	��A���B
 A��LB	ųB	��B	ƅB	êB	��B	��B	�ZB	��G�O�G�O�B	��B	��B	�B	��AŎ}B	��B	��B	ǓB	��B	�B
�B	ǍB	�cB
�A�|�B	ƫB	�UB	��B	ǓB	��B	�B	��B	ƠB	ǴB	�rB	��B	�_B	�DB	�iB	�yB	ļB	�rB	�DB	�JB	��G�O�B �B	�]B	ďB	łB	��B	��B	�B	��B	�JB	��B	��B	�iB	��G�O�B	�yB	�.B	��B	�{B	�G�O�B	ĬB	ĥG�O�B	�,B	�6G�O�B	�qB	��B	��B	��B	��B	�B	§B	��B	��B	��B	�HB	ĮB	�zB	ĕB	�"B	�[B	ĴB	�\B	�2B	�B	�uB	ƂB	�CG�O�B	�B	�B	�zB	�8B	ƐB	��B	��G�O�B	�`B	�:B	ʑB	�B	͍B	áB	�B	��B	�:B	ÛB	��B	��B	�B	�SG�O�B	«B	İA��]B	âB	æB	àB	��B	��B	��G�O�B	ȄB
KG�O�A��G�O�B	ǾB	��B	��B	��B	��B	�xB	� B	�B	ƏB	ǘB	�CB	�(B	ɕB	�B	ƑB	��B	˕B	ʓB	ҜB	�eB	�B	�<B	��B	�9B	�/B	�B	�B	�B	��B	�jB	��B	��B
�B
�B
�G�O�B
&�B
-�B
$�B
#EB
*�B
3SB
3HB
0IB
2&B
3�B
1gB
5�B
4�B
8B
8}B
;B
>�B
E-B
DB
DOB
B}B
@aB
=,B
F�B
.}B
-�B
,�B
,#B
-B
.GB
-�B
,lB
.�B
0vB
3lB
4�B
4B
3�B
;�B
:B
9�B
9�B
:�B
;B
<#B
<TB
< B
;B
<2B
;�B
<�B
;�B
:�B
;�B
;�B
=GB
=mB
<�B
=[B
<�B
=gB
=B
=vB
=,B
=4B
>AB
>B
>�B
?�B
@B
@B
@TB
B}B
B�B
B�B
B�B
C!B
C�B
CB
C�B
D[B
DB
D^B
DAB
C;B
DvB
DCB
E B
EsB
E�B
EFB
FB
E�B
E�B
E�B
FB
FhB
F�B
FB
F�B
F�B
FiB
GB
GB
GB
GTB
H1B
HBB
H B
H`B
HB
G8B
G}B
G�B
G{B
F�B
F�B
G�B
G&B
GB
F�B
G8B
F�B
F\B
FlB
F�B
F�B
F�B
GB
F�B
F�B
F�B
F�B
FyB
F�B
F�B
F�B
FB
FeB
F�B
F�B
F�B
F�B
H'B
H%B
H�B
I4B
I�B
I�B
J�B
JeB
J�B
J�B
L�B
N�B
N�B
N7B
O�B
O=B
OB
OB
N�B
P$B
P�B
RjB
Q(B
R�B
Q?B
R�B
R�B
S�B
S&B
SkB
SdB
S�B
R�B
S�B
TB
T$B
T<B
UmB
UUB
W�B
Y#B
YnB
Y�B
Z-B
Z�B
ZLB
ZlB
[(B
[�B
\�B
\mB
\�B
\�B
_�B
`�B
`�B
a�B
b1B
c�B
c�B
d"B
d9B
d�B
eBB
e�B
f�B
f�B
g�B
i.B
i)B
jRB
j?B
j(B	�rB	��B	�B	�B	�?B	�PB	�B	�)B	�sB	�_B	�-B	�@B	�B	�B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�FB	�9B	�gB	�B	�*B	��B	�B	�|B	�B	�FB	�B	�-B	�B	��B	�B	�BB	�RB	�'B	�B	�~B	�6B	��B	�B	�sB	�B	�hB	�?B	�0B	�B	��B	��B	�zB	�B	�LB	�iB	�xB	�B	��B	�B	�B	�B	�B	��B	�B	�B	�cB	�HB	�WB	�B	�/B	�QB	�WB	�HB	�cB	�JB	��B	�B	�~B	�B	�SB	�JB	�<B	�/B	�iB	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�B	�{B	�lB	�SB	�bB	�hB	�B	��B	�B	�B	�!B	��B	�G�O�B
&�B
-�B
$�B
#EB
*�B
3SB
3HB
0IB
2&B
3�B
1gB
5�B
4�B
8B
8}B
;B
>�B
E-B
DB
DOB
B}B
@aB
=,B
F�B
.}B
-�B
,�B
,#B
-B
.GB
-�B
,lB
.�B
0vB
3lB
4�B
4B
3�B
;�B
:B
9�B
9�B
:�B
;B
<#B
<TB
< B
;B
<2B
;�B
<�B
;�B
:�B
;�B
;�B
=GB
=mB
<�B
=[B
<�B
=gB
=B
=vB
=,B
=4B
>AB
>B
>�B
?�B
@B
@B
@TB
B}B
B�B
B�B
B�B
C!B
C�B
CB
C�B
D[B
DB
D^B
DAB
C;B
DvB
DCB
E B
EsB
E�B
EFB
FB
E�B
E�B
E�B
FB
FhB
F�B
FB
F�B
F�B
FiB
GB
GB
GB
GTB
H1B
HBB
H B
H`B
HB
G8B
G}B
G�B
G{B
F�B
F�B
G�B
G&B
GB
F�B
G8B
F�B
F\B
FlB
F�B
F�B
F�B
GB
F�B
F�B
F�B
F�B
FyB
F�B
F�B
F�B
FB
FeB
F�B
F�B
F�B
F�B
H'B
H%B
H�B
I4B
I�B
I�B
J�B
JeB
J�B
J�B
L�B
N�B
N�B
N7B
O�B
O=B
OB
OB
N�B
P$B
P�B
RjB
Q(B
R�B
Q?B
R�B
R�B
S�B
S&B
SkB
SdB
S�B
R�B
S�B
TB
T$B
T<B
UmB
UUB
W�B
Y#B
YnB
Y�B
Z-B
Z�B
ZLB
ZlB
[(B
[�B
\�B
\mB
\�B
\�B
_�B
`�B
`�B
a�B
b1B
c�B
c�B
d"B
d9B
d�B
eBB
e�B
f�B
f�B
g�B
i.B
i)B
jRB
j?B
j(B	�rB	��B	�B	�B	�?B	�PB	�B	�)B	�sB	�_B	�-B	�@B	�B	�B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�FB	�9B	�gB	�B	�*B	��B	�B	�|B	�B	�FB	�B	�-B	�B	��B	�B	�BB	�RB	�'B	�B	�~B	�6B	��B	�B	�sB	�B	�hB	�?B	�0B	�B	��B	��B	�zB	�B	�LB	�iB	�xB	�B	��B	�B	�B	�B	�B	��B	�B	�B	�cB	�HB	�WB	�B	�/B	�QB	�WB	�HB	�cB	�JB	��B	�B	�~B	�B	�SB	�JB	�<B	�/B	�iB	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�B	�{B	�lB	�SB	�bB	�hB	�B	��B	�B	�B	�!B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444344444434444433444443443433333433333333433333333333333333343333333333433333334333333333343333333334433333333333333334333333333333333334333333333333433333333333333344333333333333333333333333333333333334333333333333343333343343343333333333333333333333343333333433333333333333433333333343343433333333333333333333333333333333331422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455092020082814550920200828145509202008281455092020082814550920200828145509202008281455092020082814550920200828145509202008281455092020082814550920200828145509AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730392019021417303920190214173039    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730392019021417303920190214173039  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730392019021417303920190214173039  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455092020082814550920200828145509  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                