CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  G   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:41Z creation      
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
resolution        =���   axis      Z        'T  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ld   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'T  v<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'T  �h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T  μ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� '<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T 1   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'T Xh   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'T ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'T ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 	h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� :�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'T Dl   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � k�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   l�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   x�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173041  20200828145515  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               :   :   :AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @׼Wa�\H@׼Wa�\H@׼Wa�\H111 @׼W�}3�@׼W�}3�@׼W�}3�@6e�Q�@6e�Q�@6e�Q��cb��O�;�cb��O�;�cb��O�;111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    :   :   :ADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp��Bw��B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DynD��fD�UqD�{�D���D��qD�J�D���D��fD��fD�J�D��{DǺ�D��D�R=DڈRD��D�3D�'�D�o\D��3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    =���=���                        =���>���            >L��>L��            =���                =���    ����            >L��            =���                >���>���    ���ͽ��ͽ���    >L��        =���=���    ����    =���>L��        ���ͽ��ͽ���    >L��=���                            =���                                >L��    =���=���=��ͽ���=���=���        =���            >L��>���    ����=���=���    ����                >L��        ���ͽ���>L��>L��    ����    >L��        ���ͽ��ͽ���        =���                        >L��        =���=���        ����=���>���>���=���    =���=���=���                            >L��=���            >L��    ����=���                    =���>���>���    ���ͽ��ͽ���=���=���    =���            =���        =���        =���                    =���=���                        >���>���=���    ����    =���>L��=���=���=���    =���    =���>L��=���>���>L��    =���    >L��>L��>L��=���    >���=���>L��>L��>���>���=���=���>L��=���>L��>L��>L��>���>L��=���>L��>L��>���>���>���>���>L��>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>L��=���>L��>L��>L��>���>L��>���>L��>L��>���>���>���>���>L��>���>L��>���>���>���>���>L��=���>L��>L��=���>L��>L��>���>���>L��>L��>���>���>���>���>���>L��>L��>���>L��=���>L��>���>L��>���>L��>���>L��=���=���>���>L��>���>���>L��>L��=���>L��>L��=���>L��>L��>L��>L��>L��>���>L��=���>L��>L��>L��>���>���>L��>L��>���>���>L��=���>L��>L��>L��>���>L��>L��>L��>L��>���>L��>L��>���>���=���=���>L��=���>���>���>���>���>���>���>���>L��=���>L��>L��=���>���>���>���=���>L��>���=���=���>���=���>L��>L��>L��>L��>L��>���>L��=���>L��>���>L��=���=���>���>���>L��>���>L��>L��=���=���=���>L��>���>���>���>���>L��>L��>���>���>���>���>���>L��>L��>L��>L��=���    >L��>L��>���>���>���>L��>���>���>L��>L��>L��>L��>���>���=���>L��>L��>L��>L��=���>L��>L��>L��>L��>���>L��=���>���>���>���>���>L��=���=���>L��>L��>���>L��=���=���=���>���>L��>L��>���>���>L��>���=���>L��>���>���>���>L��>L��=���>L��>L��>L��=���>���>���>���>���>���>���>���>���>L��=���=���>L��>L��=���>L��>L��>L��>L��>���>���?   ?��?��?��?333?L��?L��?L��?fff?���?���?���?���?���?�ff?�  ?�  ?�  ?���?ٙ�?�ff?�33?�33?�33@ff@33@��@��@33@��@   @&ff@,��@333@@  @Fff@L��@S33@`  @fff@l��@y��@�33@�ff@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@���@�  @�33@ə�@���@�  @�ff@ٙ�@���@�33@�ff@���@�33@�ff@���@���A��A33A��A  A33A��AffA��A33AffA  A��A��A   A!��A#33A&ffA(  A)��A,��A0  A1��A333A6ffA8  A;33A>ffA@  AA��AC33AFffAH  AI��AL��ANffAQ��AS33AT��AX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��As33At��Ax  Ay��A{33A|��A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���Aə�A�33A�  A͙�A�ffA�  A���A�ffA�33A���Aՙ�A�33A�  Aٙ�A�33A�  Aݙ�DqFfDqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs��DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3DtٚDt�fDt��@333@@  @Fff@L��@S33@`  @fff@l��@y��@�33@�ff@���@���@�33@�ff@���@�  @�33@���@���@�  @�ff@���@�  @�33@ə�@���@�  @�ff@ٙ�@���@�33@�ff@���@�33@�ff@���@���A��A33A��A  A33A��AffA��A33AffA  A��A��A   A!��A#33A&ffA(  A)��A,��A0  A1��A333A6ffA8  A;33A>ffA@  AA��AC33AFffAH  AI��AL��ANffAQ��AS33AT��AX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��As33At��Ax  Ay��A{33A|��A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���Aə�A�33A�  A͙�A�ffA�  A���A�ffA�33A���Aՙ�A�33A�  Aٙ�A�33A�  Aݙ�DqFfDqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsL�DsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs��DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3DtٚDt�fDt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @:=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A��HA�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp�
Bw��B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dyn�D���D�U�D�|)D���D���D�K3D��D�ƸD���D�J�D���Dǻ3D��fD�R�Dڈ�D�)D��D�( D�o�D�ÅG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
=�G�=�G�<#�
<#�
<#�
<#�
<#�
<#�
=�G�>��R<#�
<#�
<#�
>W
>>W
><#�
<#�
<#�
=�G�<#�
<#�
<#�
<#�
=�G�<#�
��Q�<#�
<#�
<#�
>W
><#�
<#�
<#�
=�G�<#�
<#�
<#�
<#�
>��R>��<#�
��Q콸Q콸Q�<#�
>W
><#�
<#�
=�G�=�G�<#�
��Q�<#�
=�G�>W
><#�
<#�
��Q콸Q콸Q�<#�
>W
>=�G�<#�
<#�
<#�
<#�
<#�
<#�
<#�
=�G�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
>W
><#�
=�G�=�G�=�G���Q�=�G�=�G�<#�
<#�
=�G�<#�
<#�
<#�
>W
>>��<#�
��Q�=�G�=�G�<#�
��Q�<#�
<#�
<#�
<#�
>W
><#�
<#�
��Q콸Q�>W
>>W
><#�
��Q�<#�
>W
><#�
<#�
��Q콸Q콸Q�<#�
<#�
=�G�<#�
<#�
<#�
<#�
<#�
<#�
>W
><#�
<#�
=�G�=�G�<#�
<#�
��Q�=�G�>��>��=�G�<#�
=�G�=�G�=�G�<#�
<#�
<#�
<#�
<#�
<#�
<#�
>W
>=�G�<#�
<#�
<#�
>W
><#�
��Q�=�G�<#�
<#�
<#�
<#�
<#�
=�G�>��R>��R<#�
��Q콸Q콸Q�=�G�=�G�<#�
=�G�<#�
<#�
<#�
=�G�<#�
<#�
=�G�<#�
<#�
=�G�<#�
<#�
<#�
<#�
<#�
=�G�=�G�<#�
<#�
<#�
<#�
<#�
<#�
>��R>��R=�G�<#�
��Q�<#�
=�G�>W
>=�G�=�G�=�G�<#�
=�G�<#�
=�G�>W
>=�G�>��R>W
><#�
=�G�<#�
>W
>>W
>>W
>=�G�<#�
>��R=�G�>W
>>W
>>��>��R=�G�=�G�>W
>=�G�>W
>>W
>>W
>>��R>W
>=�G�>W
>>W
>>��R>��R>��R>��R>W
>>��R>��R>��R>��>��>��R>W
>>��R>��R>��R>��R>��R>��R>��R>��R>��R>W
>=�G�>W
>>W
>>W
>>��R>W
>>��R>W
>>W
>>��R>��R>��R>��>W
>>��R>W
>>��R>��R>��R>��>W
>=�G�>W
>>W
>=�G�>W
>>W
>>��R>��R>W
>>W
>>��R>��R>��R>��>��R>W
>>W
>>��>W
>=�G�>W
>>��R>W
>>��R>W
>>��R>W
>=�G�=�G�>��R>W
>>��R>��R>W
>>W
>=�G�>W
>>W
>=�G�>W
>>W
>>W
>>W
>>W
>>��>W
>=�G�>W
>>W
>>W
>>��R>��>W
>>W
>>��>��R>W
>=�G�>W
>>W
>>W
>>��R>W
>>W
>>W
>>W
>>��>W
>>W
>>��R>��R=�G�=�G�>W
>=�G�>��R>��R>��R>��R>��R>��R>��>W
>=�G�>W
>>W
>=�G�>��R>��R>��R=�G�>W
>>��R=�G�=�G�>��R=�G�>W
>>W
>>W
>>W
>>W
>>��R>W
>=�G�>W
>>��R>W
>=�G�=�G�>��R>��R>W
>>��R>W
>>W
>=�G�=�G�=�G�>W
>>��R>��R>��R>��R>W
>>W
>>��R>��R>��R>��>��R>W
>>W
>>W
>>W
>=�G�<#�
>W
>>W
>>��R>��R>��R>W
>>��>��>W
>>W
>>W
>>W
>>��R>��R=�G�>W
>>W
>>W
>>W
>=�G�>W
>>W
>>W
>>W
>>��R>W
>=�G�>��>��>��R>��R>W
>=�G�=�G�>W
>>W
>>��R>W
>=�G�=�G�=�G�>��R>W
>>W
>>��>��R>W
>>��R=�G�>W
>>��>��>��R>W
>>W
>=�G�>W
>>W
>>W
>=�G�>��R>��R>��R>��R>��R>��R>��R>��R>W
>=�G�=�G�>W
>>W
>=�G�>W
>>W
>>W
>>W
>>��R>��?�\?(�?(�?(�?5?O\)?O\)?O\)?h��?�{?�{?�{?��H?��H?��?�G�?�G�?�G�?�{?��H?�?�z�?�z�?�z�@
=@�
@=q@=q@�
@=q@ ��@'
=@-p�@3�
@@��@G
=@Mp�@S�
@`��@g
=@mp�@z=q@��@��R@��@��@��@��R@��@�Q�@��@��@��@�Q�@��R@��@�Q�@Å@��@��@�Q�@ָR@��@��@�@�R@��@�@��R@��@��AA\)A��A(�A\)A��A�\AA\)A�\A(�AA��A (�A!A#\)A&�\A((�A)A,��A0(�A1A3\)A6�\A8(�A;\)A>�\A@(�AAAC\)AF�\AH(�AIAL��AN�\AQAS\)AT��AX(�AYA\��A^�\A`(�Ac\)Ad��Ah(�AiAl��An�\AqAs\)At��Ax(�AyA{\)A|��A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA��A�z�A�{A��HA�z�A�G�A��HA��A�z�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A��HA��A�z�A�{A��HA��A�G�A�{A��A�z�A�G�A��HA��A�z�A�{A��HA��A�G�A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA��A�G�A�{A��A�z�A�G�A��HA��A�G�A�{A��HA�z�A�G�A��HAɮA�G�A�{AͮA�z�A�{A��HA�z�A�G�A��HAծA�G�A�{AٮA�G�A�{AݮDqG
DqS�DqZ>Dq`�Dqg
Dqs�Dqz>Dq��Dq�
Dq�qDq�>Dq��Dq�
Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�>Dr �DrqDr�Dr>Dr �Dr'
Dr3�Dr:>Dr@�DrG
DrS�DrZ>Dr`�Drg
DrmqDrz>Dr��Dr�
Dr�qDr��Dr��Dr�
Dr�qDr��Dr�>Dr�
Dr�qDr��Dr�>Dr�
Dr�qDr��Dr�>Ds
DsqDs�Ds>Ds �Ds-qDs3�Ds:>Ds@�DsMqDsS�DsZ>Ds`�Dsg
Dss�Dsz>Ds��Ds�
Ds��Ds�>Ds��Ds�
Ds��Ds�>Ds��Ds�
Ds�qDs�>Ds�Ds�
Ds�qDs�>Dt �Dt
DtqDt�Dt �Dt'
Dt-qDt3�Dt@�DtG
DtMqDtS�Dt`�Dtg
DtmqDts�Dt��Dt�
Dt�qDt��Dt��Dt�
Dt�qDt��Dt��Dt�
Dt�qDt��Dt�>Dt�
Dt�q@3�
@@��@G
=@Mp�@S�
@`��@g
=@mp�@z=q@��@��R@��@��@��@��R@��@�Q�@��@��@��@�Q�@��R@��@�Q�@Å@��@��@�Q�@ָR@��@��@�@�R@��@�@��R@��@��AA\)A��A(�A\)A��A�\AA\)A�\A(�AA��A (�A!A#\)A&�\A((�A)A,��A0(�A1A3\)A6�\A8(�A;\)A>�\A@(�AAAC\)AF�\AH(�AIAL��AN�\AQAS\)AT��AX(�AYA\��A^�\A`(�Ac\)Ad��Ah(�AiAl��An�\AqAs\)At��Ax(�AyA{\)A|��A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA��A�z�A�{A��HA�z�A�G�A��HA��A�z�A�{A��HA��A�z�A�G�A�{A��A�z�A�G�A��HA��A�z�A�{A��HA��A�G�A�{A��A�z�A�G�A��HA��A�z�A�{A��HA��A�G�A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA��A�G�A�{A��A�z�A�G�A��HA��A�G�A�{A��HA�z�A�G�A��HAɮA�G�A�{AͮA�z�A�{A��HA�z�A�G�A��HAծA�G�A�{AٮA�G�A�{AݮDqG
DqS�DqZ>Dq`�Dqg
Dqs�Dqz>Dq��Dq�
Dq�qDq�>Dq��Dq�
Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�>Dr �DrqDr�Dr>Dr �Dr'
Dr3�Dr:>Dr@�DrG
DrS�DrZ>Dr`�Drg
DrmqDrz>Dr��Dr�
Dr�qDr��Dr��Dr�
Dr�qDr��Dr�>Dr�
Dr�qDr��Dr�>Dr�
Dr�qDr��Dr�>Ds
DsqDs�Ds>Ds �Ds-qDs3�Ds:>Ds@�DsMqDsS�DsZ>Ds`�Dsg
Dss�Dsz>Ds��Ds�
Ds��Ds�>Ds��Ds�
Ds��Ds�>Ds��Ds�
Ds�qDs�>Ds�Ds�
Ds�qDs�>Dt �Dt
DtqDt�Dt �Dt'
Dt-qDt3�Dt@�DtG
DtMqDtS�Dt`�Dtg
DtmqDts�Dt��Dt�
Dt�qDt��Dt��Dt�
Dt�qDt��Dt��Dt�
Dt�qDt��Dt�>Dt�
Dt�qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�;dA�;dA�;dA�;dA�=qA�?}A�A�A�A�A�?}A�?}A�;dA�A�A�I�A�K�A�I�A�?}A�9XA�(�A�&�A�(�A��A��A�ĜAҺ^A�z�A�A��HAэPA�E�A���A�r�A˧�A�jAȇ+AǕ�A�dZA�1A�x�A�VA�ĜA�1'Aġ�A�I�A�t�A¾wA�C�A�7LA�M�A�1A�ȴA�l�A�1'A��A��jA��A���A�1'A���A��^A�n�A�~�A���A�(�A���A�O�A�A�1A�bA�x�A��A�$�A��`A�$�A�"�A�jA��hA�-A���A��A�?}A���A��;A�|�A�{A�A�A�ƨA�ĜA�
=A�A�A�=qA��RA�XA��A��TA�ĜA��-A�G�A���A�`BA���A�JA�  A��FA�E�A���A�ĜA�E�A��A��-A�Q�A�ffA��A�VA���A�/A��^A�-A{/AyC�Aw��At�uAr~�Ap�`An�DAk�Ah��AfbNAdI�Aa�-A\�A[
=AY33AV��AT��AS�hAQdZAP��AN�9AL�+AIK�AG��AF��AFVAD5?AB�\A@E�A>(�A;��A:�!A9A8�DA8jA8�A7S�A6��A6  A533A4E�A37LA2  A/�A.�A-;dA*  A'�PA&��A&��A& �A#�FA!�FA7LA�A�PA�9Av�A��AK�A��AG�A|�A�#A�#A�A�A�Al�AĜA�PA�#A �A��A`BA��A��A$�A5?A�!A�hAS�Al�A(�A`BA	�A��Az�AE�A5?AƨA7LA^5A��A~�AAp�AȴA��A~�A�Ax�A �A �!@��F@��h@���@��@��^@���@�?}@�Q�@�{@�V@��u@�j@�A�@�?}@�Q�@��@���@��@�~�@�"�@�C�@噚@��@��@���@�z�@�"�@�E�@�`B@�r�@�
=@��T@׾w@�V@�&�@��
@Гu@υ@ΰ!@�@͑h@�x�@Ͳ-@��@Η�@���@ˮ@�
=@��@ʰ!@�5?@�/@�;d@Ų-@�&�@�z�@��m@���@�X@� �@��H@�j@�b@�ƨ@�ƨ@��@�{@��@�J@���@�7L@��@��D@��m@��@�K�@��@���@���@�~�@�E�@��@�@��7@�bN@�+@��T@��-@�x�@�/@��@��\@���@�@��h@�p�@�V@�1@���@��@�ƨ@��w@��w@�;d@�ff@�$�@��@��-@��T@�K�@��@��j@�Z@��m@���@�\)@�"�@���@�^5@���@���@�7L@��!@�ƨ@��T@�l�@��@��7@�p�@���@���@�^5@��j@��
@��w@��@�I�@���@��9@��D@��@�@��9@��+@�V@��/@�?}@�%@��@���@��@�bN@��D@��^@�x�@�7L@��j@��@��@���@��!@���@��@�I�@��`@��9@�I�@�  @��@�(�@�1@��w@���@���@���@�(�@���@�9X@���@�-@��@�E�@���@�bN@�9X@�bN@���@�`B@�t�@�Q�@���@�{@�Ĝ@��@��@�Q�@�b@��m@��
@�ƨ@��F@�|�@�;d@��@��y@���@�E�@�x�@�?}@�&�@���@���@�1'@���@���@�33@�S�@�K�@�+@�33@�K�@�\)@�S�@���@���@��@���@��@�33@��H@�1@�b@���@�Z@�bN@�Q�@�b@��w@��m@��F@�l�@�dZ@�\)@�o@�v�@�$�@�@���@��T@���@�x�@�hs@��@��D@�I�@�  @��@�E�@��@��@�  @���@�"�@���@���@��@�ȴ@��@|@sX�@nd�@dѷ@`��@V��@Oo�@G i@A�o@>�B@7�$@1f�@,oi@&��@!��@}�@�9@ �@��@/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��AŶFA���A�~�A��A���A�bA��A�r�A��mA�ĜA�p�A��A��A��AҼjAİ!A�$�A���A�
=Aò-A��TA�%A�ƨA�l�A�$�AƅA�(�A�{AёhA�(�A��yA��#A���A�x�A�ZAũ�AȮA�+A��A�O�A���A�I�AōPA�t�A��A�1'A��`A�ƨA��A�XA��/A�(�Aϗ�A�AǅA�v�A�jA���A°!A�-A�ffA��A��A�^5A���A�`BA�K�A���A�n�A���AÛ�A�Q�A��A��TAÙ�A�-A�I�AʼjA��A̕�A�O�Aɗ�A��
A�ȴA�|�A��HA��A�{A�9XA�JA�+A���A·+A��A�ZAƉ7A�;dAˏ\AѬA��A�oAď\Aə�A�/A���Aå�A�hsA�&�AǕ�A��
A�`BAŰ!A�ƨA�XA��yA��A��A��PA���A�`BA+A�9XAĺ^A+A�JA�9XA�-A�9XA�S�A�v�A��HA���A�&�A�$�A�x�A�bA��A�VA�1A�oAѮA�ȴA�A�hsA�n�A�+A��TA��7A��A��A��/A�%A�{A�VA�A�jA�=qA���AҋDA��TAʑhA�JA�ȴAğ�A�M�A�"�ÃA��A�JAғuAÅA���AĶFA� �A�r�AĮA�S�A�K�AȸRA�ZAҲ-Aͩ�A�
=A�$�A��`A� �Aƙ�A�(�A�33AōPA���A�~�Aͧ�A�ƨA��
AƋDAőhAƗ�AŇ+A�VA�
=A�
=Aѧ�A�jA�S�AǕ�A�;dA�
=A�%A��yA��AɁA�Q�A���A�{A�ĜA�A���A�%A�n�A�oA��TA�(�A�1A�
=A��mA�^5A�G�A�{Aβ-A��A�{A�VA�n�A���A�jA���A�-A�%A���A�bA�VA�C�A�jA�A�JA�VA�1A�JA���A�1A�JA�{A�bA�oA�{A�JA�bA��A�{A��TA�oA�bA�oA�{A�oA�bA�&�AѬA���A� �A�bA�oA�VA�JA�oA�oA�VA��A��A��A�`BA��A�$�A� �A� �A�$�A�oA��#A΃A�"�A�7LAϾwA��A��A�"�A��A�K�A�bA�&�A�&�A�(�A�&�A�$�AҸRA�&�A��A�A͏\A�&�A�&�A�(�A�&�Aҡ�A�&�A�1A�VA�$�A� �A�$�A�&�A�&�A�(�A���A҇+A� �A���A���A�"�A�;dA� �A��A�&�A� �A��A�r�A�$�A�+A�$�A�&�A� �A��HA� �A�(�A�oA��A�&�A�(�A�E�A�(�A�"�A��/A��A�&�A�+A�-A��A�5?A�&�A̼jA�bNA��A��A�jA�+A�-A�(�A�(�A�(�A�(�A��A�t�A�ZA�&�A���AҋDA�$�A�$�A�{Aҕ�A�"�AҬA��A�dZA�$�A��A��#A�+A� �A�&�A�(�A�&�A�|�A�VA�$�A�"�A�JA�O�AЧ�A�(�A�+A�$�A�+A�oA��A���A�$�A�(�A�-A�+A�+A�&�A�"�A�9XA�-A�-A��A�/A�(�A�$�A�&�A� �A̓Aҗ�A�
=A��TA�$�A�&�A�$�A�$�A��A�ƨA�-A�(�A���A�XA�-A�E�A�(�A�ƨA�\)A�$�A��A�"�A��Aҟ�A�A�"�A�$�A�$�A�-AѸRA�/A�-A�-A�/A���A��TAΩ�A�-A�=qA�+A�(�Aɧ�A��#A�O�A�(�A�&�A�~�A�M�A�-A�(�A�-AҬA�{A�(�A�1'A�1'A�1'A�-A�K�A�JAҍPA�/A���A�/A�33A�33A�5?A�33A�5?A�33A�-A�/A�9XA�ƨA�ƨA� �A��A�(�A�-A��A�
=A�1'A�1'A�7LA�7LA�7LA�7LA�5?A�7LA�5?A�7LA�7LA�5?A�5?A�5?A�1'A�33A�7LA�/A�-A�-A�5?A�5?A�-A�-A�33A�/A�-A�9XA�7LA�33A�5?A�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�9XA�;dA�=qA�;dA�;dA�;dA�9XA�7LA�;dA�=qA�;dA�9XA�9XA�;dA�9XA�9XA�9XA�;dA�=qA�=qA�;dA�=qA�;dA�;dA�=qA�;dA�9XA�9XA�9XA�;dA�9XA�;dA�;dA�9XA�9XA�7LA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�9XA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�;dA�=qA�;dA�=qA�=qA�?}A�=qA�A�A�=qA�?}A�=qA�=qA�=qA�?}A�?}A�?}A�?}A�=qA�A�A�?}A�C�A�?}A�E�A�E�A�?}A�?}A�?}A�C�A�A�A�E�A�?}A�C�A�A�A�E�A�G�A�E�A�A�A�;dA�A�A�E�A�E�A�E�A�?}A�A�A�?}A�A�A�?}A�?}A�=qA�=qA�?}A�A�A�A�A�?}A�?}A�?}A�?}A�=qA�?}A�?}A�A�A�?}A�A�A�?}A�?}A�;dA�=qA�?}A�=qA�=qA�A�A�G�A�E�A�A�A�=qA�?}A�?}A�=qA�A�A�A�A�=qA�;dA�9XA�5?A�7LA�5?A�5?A�1'A�33A�7LA�=qA�?}A�?}A�A�A�=qA�=qA�;dA�;dA�;dA�G�A�G�A�G�A�C�A�A�A�=qA�A�A�?}A�?}A�A�A�E�A�G�A�I�A�I�@�z�@�I�@��@�  @���@���@��@��m@��@��m@��m@��;@��;@��
@���@��
@��
@���@��
@���@���@�ƨ@��w@��F@���@���@��P@�|�@�l�@�dZ@�\)@�K�@�;d@�;d@�+@�"�@�"�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�@��y@��y@��H@��H@��@��@���@��@��@���@���@���@���@���@��@���@�@�@�@�@�@�@�@���@���@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��@��@��@��y@��H@��y@��y@��y@��H@��@��@���@���@�ȴ@��R@��R@��R@��R@��!@��!@��!@��!@��!@���@���@��!@���A�9XA�9XA�9XA�;dA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�9XA�9XA�;dA�9XA�;dA�;dA�=qA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�9XA�;dA�?}A�=qA�?}A�?}A�=qA�=qA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�?}A�?}A�=qA�?}A�?}A�A�A�A�A�A�A�?}A�A�A�C�A�C�A�E�A�=qA�;dA�?}A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�E�A�E�A�E�A�=qA�A�A�A�A�E�A�E�A�C�A�E�A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�A�A�A�A�=qA�?}A�=qA�A�A�A�A�A�A�A�A�A�A�=qA�=qA�=qA�=qA�?}A�=qA�C�A�G�A�E�A�=qA�=qA�=qA�=qA�?}A�?}A�A�A�?}A�;dA�=qA�7LA�7LA�7LA�33A�7LA�5?A�9XA�?}A�?}A�?}A�?}A�?}A�=qA�=qA�;dA�C�A�I�A�G�A�G�A�C�A�A�A�E�A�=qA�A�A�E�A�E�A�G�A�I�A�I�A�I�@�bN@�(�@�1@�  @���@��@��@��m@��m@��m@��m@��;@��;@��
@��
@��
@���@���@��
@���@���@�ƨ@��w@��F@���@��P@��@�t�@�dZ@�dZ@�S�@�C�@�;d@�33@�+@�"�@�"�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�o@���@��y@��y@��y@��y@��@��@���@���@���@���@���@���@���@���@���@�@�@�@�@�@�@�@���@���@�@���@���@���@���@���@���@���@��@���@���@���@��@��@��@��@��@��y@��y@��y@��y@��y@��H@��@��@��@���@�ȴ@���@��R@��R@��R@��R@��!@��!@��!@��!@��!@��!@��!@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�9XA�;dA�;dA�;dA�;dA�=qA�?}A�A�A�A�A�?}A�?}A�;dA�A�A�I�A�K�A�I�A�?}A�9XA�(�A�&�A�(�A��A��A�ĜAҺ^A�z�A�A��HAэPA�E�A���A�r�A˧�A�jAȇ+AǕ�A�dZA�1A�x�A�VA�ĜA�1'Aġ�A�I�A�t�A¾wA�C�A�7LA�M�A�1A�ȴA�l�A�1'A��A��jA��A���A�1'A���A��^A�n�A�~�A���A�(�A���A�O�A�A�1A�bA�x�A��A�$�A��`A�$�A�"�A�jA��hA�-A���A��A�?}A���A��;A�|�A�{A�A�A�ƨA�ĜA�
=A�A�A�=qA��RA�XA��A��TA�ĜA��-A�G�A���A�`BA���A�JA�  A��FA�E�A���A�ĜA�E�A��A��-A�Q�A�ffA��A�VA���A�/A��^A�-A{/AyC�Aw��At�uAr~�Ap�`An�DAk�Ah��AfbNAdI�Aa�-A\�A[
=AY33AV��AT��AS�hAQdZAP��AN�9AL�+AIK�AG��AF��AFVAD5?AB�\A@E�A>(�A;��A:�!A9A8�DA8jA8�A7S�A6��A6  A533A4E�A37LA2  A/�A.�A-;dA*  A'�PA&��A&��A& �A#�FA!�FA7LA�A�PA�9Av�A��AK�A��AG�A|�A�#A�#A�A�A�Al�AĜA�PA�#A �A��A`BA��A��A$�A5?A�!A�hAS�Al�A(�A`BA	�A��Az�AE�A5?AƨA7LA^5A��A~�AAp�AȴA��A~�A�Ax�A �A �!@��F@��h@���@��@��^@���@�?}@�Q�@�{@�V@��u@�j@�A�@�?}@�Q�@��@���@��@�~�@�"�@�C�@噚@��@��@���@�z�@�"�@�E�@�`B@�r�@�
=@��T@׾w@�V@�&�@��
@Гu@υ@ΰ!@�@͑h@�x�@Ͳ-@��@Η�@���@ˮ@�
=@��@ʰ!@�5?@�/@�;d@Ų-@�&�@�z�@��m@���@�X@� �@��H@�j@�b@�ƨ@�ƨ@��@�{@��@�J@���@�7L@��@��D@��m@��@�K�@��@���@���@�~�@�E�@��@�@��7@�bN@�+@��T@��-@�x�@�/@��@��\@���@�@��h@�p�@�V@�1@���@��@�ƨ@��w@��w@�;d@�ff@�$�@��@��-@��T@�K�@��@��j@�Z@��m@���@�\)@�"�@���@�^5@���@���@�7L@��!@�ƨ@��T@�l�@��@��7@�p�@���@���@�^5@��j@��
@��w@��@�I�@���@��9@��D@��@�@��9@��+@�V@��/@�?}@�%@��@���@��@�bN@��D@��^@�x�@�7L@��j@��@��@���@��!@���@��@�I�@��`@��9@�I�@�  @��@�(�@�1@��w@���@���@���@�(�@���@�9X@���@�-@��@�E�@���@�bN@�9X@�bN@���@�`B@�t�@�Q�@���@�{@�Ĝ@��@��@�Q�@�b@��m@��
@�ƨ@��F@�|�@�;d@��@��y@���@�E�@�x�@�?}@�&�@���@���@�1'@���@���@�33@�S�@�K�@�+@�33@�K�@�\)@�S�@���@���@��@���@��@�33@��H@�1@�b@���@�Z@�bN@�Q�@�b@��w@��m@��F@�l�@�dZ@�\)@�o@�v�@�$�@�@���@��T@���@�x�@�hs@��@��D@�I�@�  @��@�E�@��@��@�  @���@�"�@���@���@��G�O�@��@|@sX�@nd�@dѷ@`��@V��@Oo�@G i@A�o@>�B@7�$@1f�@,oi@&��@!��@}�@�9@ �@��@/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��AŶFA���A�~�A��A���A�bA��A�r�A��mA�ĜA�p�A��A��A��AҼjAİ!A�$�A���A�
=Aò-A��TA�%A�ƨA�l�A�$�AƅA�(�A�{AёhA�(�A��yA��#A���A�x�A�ZAũ�AȮA�+A��A�O�A���A�I�AōPA�t�A��A�1'A��`A�ƨA��A�XA��/A�(�Aϗ�A�AǅA�v�A�jA���A°!A�-A�ffA��A��A�^5A���A�`BA�K�A���A�n�A���AÛ�A�Q�A��A��TAÙ�A�-A�I�AʼjA��A̕�A�O�Aɗ�A��
A�ȴA�|�A��HA��A�{A�9XA�JA�+A���A·+A��A�ZAƉ7A�;dAˏ\AѬA��A�oAď\Aə�A�/A���Aå�A�hsA�&�AǕ�A��
A�`BAŰ!A�ƨA�XA��yA��A��A��PA���A�`BA+A�9XAĺ^A+A�JA�9XA�-A�9XA�S�A�v�A��HA���A�&�A�$�A�x�A�bA��A�VA�1A�oAѮA�ȴA�A�hsA�n�A�+A��TA��7A��A��A��/A�%A�{A�VA�A�jA�=qA���AҋDA��TAʑhA�JA�ȴAğ�A�M�A�"�ÃA��A�JAғuAÅA���AĶFA� �A�r�AĮA�S�A�K�AȸRA�ZAҲ-Aͩ�A�
=A�$�A��`A� �Aƙ�A�(�A�33AōPA���A�~�Aͧ�A�ƨA��
AƋDAőhAƗ�AŇ+A�VA�
=A�
=Aѧ�A�jA�S�AǕ�A�;dA�
=A�%A��yA��AɁA�Q�A���A�{A�ĜA�A���A�%A�n�A�oA��TA�(�A�1A�
=A��mA�^5A�G�A�{Aβ-A��A�{A�VA�n�A���A�jA���A�-A�%A���A�bA�VA�C�A�jA�A�JA�VA�1A�JA���A�1A�JA�{A�bA�oA�{A�JA�bA��A�{A��TA�oA�bA�oA�{A�oA�bA�&�AѬA���A� �A�bA�oA�VA�JA�oA�oA�VA��A��A��A�`BA��A�$�A� �A� �A�$�A�oA��#A΃A�"�A�7LAϾwA��A��A�"�A��A�K�A�bA�&�A�&�A�(�A�&�A�$�AҸRA�&�A��A�A͏\A�&�A�&�A�(�A�&�Aҡ�A�&�A�1A�VA�$�A� �A�$�A�&�A�&�A�(�A���A҇+A� �A���A���A�"�A�;dA� �A��A�&�A� �A��A�r�A�$�A�+A�$�A�&�A� �A��HA� �A�(�A�oA��A�&�A�(�A�E�A�(�A�"�A��/A��A�&�A�+A�-A��A�5?A�&�A̼jA�bNA��A��A�jA�+A�-A�(�A�(�A�(�A�(�A��A�t�A�ZA�&�A���AҋDA�$�A�$�A�{Aҕ�A�"�AҬA��A�dZA�$�A��A��#A�+A� �A�&�A�(�A�&�A�|�A�VA�$�A�"�A�JA�O�AЧ�A�(�A�+A�$�A�+A�oA��A���A�$�A�(�A�-A�+A�+A�&�A�"�A�9XA�-A�-A��A�/A�(�A�$�A�&�A� �A̓Aҗ�A�
=A��TA�$�A�&�A�$�A�$�A��A�ƨA�-A�(�A���A�XA�-A�E�A�(�A�ƨA�\)A�$�A��A�"�A��Aҟ�A�A�"�A�$�A�$�A�-AѸRA�/A�-A�-A�/A���A��TAΩ�A�-A�=qA�+A�(�Aɧ�A��#A�O�A�(�A�&�A�~�A�M�A�-A�(�A�-AҬA�{A�(�A�1'A�1'A�1'A�-A�K�A�JAҍPA�/A���A�/A�33A�33A�5?A�33A�5?A�33A�-A�/A�9XA�ƨA�ƨA� �A��A�(�A�-A��A�
=A�1'A�1'A�7LA�7LA�7LA�7LA�5?A�7LA�5?A�7LA�7LA�5?A�5?A�5?A�1'A�33A�7LA�/A�-A�-A�5?A�5?A�-A�-A�33A�/A�-A�9XA�7LA�33A�5?A�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�9XA�9XA�;dA�9XA�;dA�;dA�=qA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�9XA�;dA�?}A�=qA�?}A�?}A�=qA�=qA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�?}A�?}A�=qA�?}A�?}A�A�A�A�A�A�A�?}A�A�A�C�A�C�A�E�A�=qA�;dA�?}A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�E�A�E�A�E�A�=qA�A�A�A�A�E�A�E�A�C�A�E�A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�A�A�A�A�=qA�?}A�=qA�A�A�A�A�A�A�A�A�A�A�=qA�=qA�=qA�=qA�?}A�=qA�C�A�G�A�E�A�=qA�=qA�=qA�=qA�?}A�?}A�A�A�?}A�;dA�=qA�7LA�7LA�7LA�33A�7LA�5?A�9XA�?}A�?}A�?}A�?}A�?}A�=qA�=qA�;dA�C�A�I�A�G�A�G�A�C�A�A�A�E�A�=qA�A�A�E�A�E�A�G�A�I�A�I�A�I�@�bN@�(�@�1@�  @���@��@��@��m@��m@��m@��m@��;@��;@��
@��
@��
@���@���@��
@���@���@�ƨ@��w@��F@���@��P@��@�t�@�dZ@�dZ@�S�@�C�@�;d@�33@�+@�"�@�"�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�o@���@��y@��y@��y@��y@��@��@���@���@���@���@���@���@���@���@���@�@�@�@�@�@�@�@���@���@�@���@���@���@���@���@���@���@��@���@���@���@��@��@��@��@��@��y@��y@��y@��y@��y@��H@��@��@��@���@�ȴ@���@��R@��R@��R@��R@��!@��!@��!@��!@��!@��!@��!@���@���A�9XA�9XA�9XA�;dA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�9XA�9XA�;dA�9XA�;dA�;dA�=qA�=qA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�;dA�9XA�;dA�?}A�=qA�?}A�?}A�=qA�=qA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�?}A�?}A�=qA�?}A�?}A�A�A�A�A�A�A�?}A�A�A�C�A�C�A�E�A�=qA�;dA�?}A�C�A�C�A�A�A�A�A�C�A�C�A�C�A�E�A�E�A�E�A�=qA�A�A�A�A�E�A�E�A�C�A�E�A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�A�A�A�A�=qA�?}A�=qA�A�A�A�A�A�A�A�A�A�A�=qA�=qA�=qA�=qA�?}A�=qA�C�A�G�A�E�A�=qA�=qA�=qA�=qA�?}A�?}A�A�A�?}A�;dA�=qA�7LA�7LA�7LA�33A�7LA�5?A�9XA�?}A�?}A�?}A�?}A�?}A�=qA�=qA�;dA�C�A�I�A�G�A�G�A�C�A�A�A�E�A�=qA�A�A�E�A�E�A�G�A�I�A�I�A�I�@�bN@�(�@�1@�  @���@��@��@��m@��m@��m@��m@��;@��;@��
@��
@��
@���@���@��
@���@���@�ƨ@��w@��F@���@��P@��@�t�@�dZ@�dZ@�S�@�C�@�;d@�33@�+@�"�@�"�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�o@���@��y@��y@��y@��y@��@��@���@���@���@���@���@���@���@���@���@�@�@�@�@�@�@�@���@���@�@���@���@���@���@���@���@���@��@���@���@���@��@��@��@��@��@��y@��y@��y@��y@��y@��H@��@��@��@���@�ȴ@���@��R@��R@��R@��R@��!@��!@��!@��!@��!@��!@��!@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=B�=U{�=MJ�=o^�=�*=��D=���=�'(>"8�@�h@i=�=�y>'��?��5@��@�I�=Դ�>b�?�Cl>���=K}A=W�=�Pr=eU�=^��=ix�=��=�U�=�}?1�>�(�=���>^�?�-b=u��=���=�=ڿ4>��e@�H�@�W�=��>��2=�W�=�R�>���=��d=��>?��@�K4?(��=�3�>�L�?�u�?"Ta?��=��=qu�=��=�K�=���>��>H�>��=� �=��[=~� =���=�b=עI>J�`@=�3=U�'=e�=w�k=���=�-#=�1�=�F�?���@'��=���@ �@�L=�e=��>*��?��&=��p>YR�@�C�=�*�=ݢs>kF5@�I@�h4=�m�>0�?���?�9�=���=�)�=�K>|�p> f<?sʗ=�a=�t?=�~�=���>go*@�Ln=��=��O=�'>���@-��=5�X=I�e=]��=�5�=�k'=�#�>�u? �9=f�X=���=���=���>�?c¹@��=�
�=��>:�@rj�=�s=�&�>k`k?@�P�@�:�=���>��"@.��=��F=Dŗ=h},=��=���=�4�=�iY>6|�@�R?�`>Ե�=�m=��9?��@�T"=��>]�@�T7=Z��=���=�wG=�y�>'�{@�Ov@�T�@�YK=�o=�6�=��>B�@?�=}At=��m=�.�=�WT>��@e,> �I@���>-�?��"=�;�=��(?��W=�^_=���=�1=���>H�H@�W�=nN'=��=�&B>��5=���=��)?�^t@�W?@�U�=��=�|�=��>.`�@�QD@�W >b@�X�=��2?0+A=�h4=��>���@�X%@�Z�@�Z�?-�6=��@Q�I?-��@�X�@�W@�OL>{�?av@�\�>G:�@�`�@�Y�@�[-@�U�>n=@X6e@�U�>��	@�Y@�W�@�\@�_?�ؙ?f�@�W?@�[�@�[�@�V.@�U�@�X�@�W~@�W�@�X�@�X�@�Y6@�[@�Y6@�Z�@�[�@�Zq@�Y`@�Y`@�Z�@�Z@�Z@�Z@�Y�@�Z2@(I?VDR@\��@�Z@�Zq@�Y`@�X�@�Z@�Y�@�Y`@�\�@�_�@�`�?6'|@�[@�a|@�_[@�ZG@�a(@�_�?@$@��@�`�@�G�>���@�^�@�^J@�`-@�^�@�&-@�_�@�b9@�b9@�a�@�b9@�a�@�F@�a(@�a�>:cs>��k@�a�@�b�@�Y�@�a�@v��@�a�@�`�>Y@�`�@�_p@�a�@�b9@�a�@�a(@�a�@x)J@�_[?ɰ@��@�`?{=�@�`�@�_�@�`-@�`-@�^>q�j@�`�@�a(@�`-@�b�@�a�@�]�@�b9@�b9@�`�@�`k>X*�@�b�@~C�@�`�@�`-@���@�^J@�d@�c�@�dZ@�b�@V$@�cI@u�?��6@k<@�cI?o?�@�c^@�d@�b�@�b�@�b�@�b�@�a�=御>V_�@�a�@@�$@gՑ@�a�@�a(@�^_@^��@�`k@�a�@R�?���@�b�@+�N@�i�@�dZ@�b�@�c�@�c�@�b�@��@���@�b�@�b�?!�@���?xn�@�c�@�c�@�a�@�a�@�Z?�g#?/�D?�V@�d@�c^@�b�@�c^@�b9@�`�>Gw@�d@�e�@�b�@�`�@�b9@�a|@�a�@�b�>��[@q]�@D��?��@�c�@�a�@�a�@�_�@�b�?�A�@�d@�d@�b�@
��@�b�@�a=@�`-?��D>�6@�V@�E�@�K�@�N'@�3�@j�@��@�	B@�P�@�dZ@���?�r�@�f'@�f{@�e@�d@�a(>7��@�dZ@���@�dZ@�dZ@#� >-�@�d@�f'@�d�@h��@=��@�e�@�e�@�f�@�f<@CB@�e�@�gM@�f�@�e�@�f<@�
R@�@�f�@�f�@�f�@�g�@�hI@�i@�i@�h�@�i@�g�@�hI@�g�?͓>@�@.E@�e,@�d>�N�@�e�@�b9@�e�@�g�@�i@�i�@�i�@�j@�i�@�i@�i�@�j+@�j+@�j+@�in@�i@�in@�j@�k<@�j+@�f�@�g�@�h�@�in@�g�@�g�@�i@�h�@�i@�g�@�j�@�i�@�i@�j�@�k�@�k<@�lL@�lL@�k�@�k�@�lL@�lL@�lL@�l�@�l�@�m	@�l�@�l�@�m]@�m	@�m	@�m�@�m]@�n@�n@�m�@�m�@�m]@�m	@�m�@�n@�m�@�mr@�m]@�mr@�m�@�m�@�n@�n�@�n�@�n�@�n�@�o*@�o?@�n�@�n�@�o*@�n�@�n@�n/@�n�@�n�@�n�@�n�@�n�@�o*@�o?@�o?@�o?@�o?@�o?@�o?@�o�@�o�@�o�@�o�@�o�@�o�@�pP@�p�@�p�@�q"@�qa@�p�@�q@�q@�q@�qv@�r@�q�@�r@�r2@�rq@�rq@�rq@�rq@�r�@�r�@�r�@�s.@�s�@�s�@�s�@�t?@�t@�t?@�t�@�uO@�t�@�r�@�s�@�t�@�ud@�t�@�ud@�u%@�t�@�uy@�v�@�v�@�t�@�t�@�tT@�v@�w2@�v�@�u�@�ud@�t�@�u@�u@�u@�t�@�u@�u%@�ud@�ud@�ud@�uy@�ud@�u%@�u@�u%@�u@�ud@�v!@�v!@�v6@�u�@�t�@�t�@�u�@�vu@�u�@�v!@�x@�x�@�w�@�u�@�v!@�v�@�u�@�v!@�v�@�v�@�v!@�u�@�tT@�tT@�sX@�sm@�s�@�sm@�sX@�t~@�v�@�x@�x�@�w\@�x@�wG@�wG@�wG@�y�@�{5@�{�@�{5@�z%@�yh@�y�@�yh@�y�@�zx@�{�@�}V@�}�@�~g@�~�@P�m@P��@P�E@P�@P�#@P�(@P�@P��@P��@P��@P��@P��@P��@P�>@P�>@P��@P�@P�l@P�@P�@P��@P��@P��@P�@P�;@P�@P�L@P�@P܇@P�8@P��@P��@P֡@P��@PԪ@P�V@PӮ@P�@P�V@P��@P�R@P�R@P��@PԪ@P��@P��@P��@P��@PԪ@PԪ@P�V@P�V@Pҳ@P�>@P�B@P�B@P��@P��@P�9@P�5@Pҳ@Pҳ@P��@PӮ@PӮ@P�,@PӮ@PԀ@PԪ@P��@P�M@P��@Pם@P��@P�E@P��@P�E@P�E@P��@P��@P��@P�@P�E@P�E@P�E@Pם@Pם@P��@Pם@Pם@P�@Pם@P�s@P�@P�@P��@P֡@P�$@P��@P��@P�(@PԀ@P��@Pӄ@P�5@P�5@P��@P��@Pϖ@P�@P�B@P�B@PΚ@PΚ@PΚ@P��@P��@P��@P͟@P�!@�jj@�j@�j@�j�@�j�@�j�@�k'@�k<@�k'@�k<@�kf@�kf@�k�@�k�@�k�@�lL@�k�@�l7@�l7@�k�@�k�@�l"@�lv@�l7@�kf@�k�@�l7@�k�@�l"@�l7@�lv@�l�@�l�@�l�@�l�@�l�@�m@�m]@�l7@�l"@�la@�lv@�lv@�l�@�l�@�l�@�l�@�mr@�mr@�mr@�mH@�m@�m@�m3@�m�@�m�@�m�@�mr@�m�@�m�@�m�@�n�@�n�@�o @�o*@�n�@�o@�n�@�n�@�n�@�o*@�o�@�o�@�p@�p@�p&@�p;@�pP@�pP@�p�@�p�@�p�@�q@�p�@�q�@�rG@�r@�s�@�s�@�q�@�o�@�q�@�s�@�sX@�r@�qa@�r�@�sX@�s�@�t�@�t?@�t�@�q7@�qv@�s�@�t @�u@�sC@�t�@�r�@�r@�r@�r@�r2@�q�@�rG@�s@�s@�s@�r�@�s@�s@�q�@�r\@�q�@�s@�r�@�s@�s�@�s�@�qL@�qv@�q�@�r@�rG@�r�@�u@�v�@�u�@�s.@�r�@�sC@�sX@�s@�t @�t @�sX@�q�@�r�@�p�@�pz@�p&@�oT@�qL@�p;@�p;@�t @�t�@�ti@�t�@�t�@�t @�s�@�r�@�v6@�x�@�x@�x�@�w2@�u�@�wp@�ud@�u:@�v6@�x-@�x�@�z%@�zc@�z:@P��@P�@P�>@P�@P�K@P�O@P��@P�@P��@P�@P�@P�\@P��@P�@P�;@P�@P�;@P�@P�;@P�;@P�@P��@P�@Pߤ@P�Y@Pٔ@P�o@P��@P�@P��@P�9@P��@P�!@P�O@Pʬ@P�@P�\@P�@P�\@P�@P��@P��@Pʬ@Pʬ@Pʂ@Pʬ@P��@P��@P��@P��@P��@P��@P�.@Pŗ@P��@P�L@Pà@P��@P��@Pƽ@Pǹ@PǏ@Pǹ@P�6@P��@P��@P�6@PȊ@P�2@Pʬ@P�S@P��@P̣@P�!@P�u@P��@P͟@P�u@P��@P͟@P�u@P��@P͟@P͟@P��@P͟@P͟@P͟@P��@P�u@P�K@P�K@P��@P��@P��@P̣@P�O@P�%@P�%@P��@P�S@P�X@P�@Pɰ@PȊ@P�`@P�e@P�m@P�@P��@P��@P��@P�H@P�H@P�@P��@P��@P��@P��@PàG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4444444443344433444444444444444444444443344444444344444444444444444444444444444444444444443444334444444444444443444444444444444444444443444433444444444443444443443444443334444444444343444444444434444444334444334344444333443433344343333433433334433333333333333333333333344333333333334333333433343333333333333344333333343333333334334333334333333333343333333333333443433333334434333333334343333333333434333334443333334333333334344333334333433344333333333334333334333344333343333433333343333333333334443343333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�h@i=�G�O�G�O�G�O�@��@�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�H�@�W�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�K5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�C�G�O�G�O�G�O�@�I@�h5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�LqG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@rj�G�O�G�O�G�O�G�O�@�P�@�:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�RG�O�G�O�G�O�G�O�G�O�@�T#G�O�G�O�@�T6G�O�G�O�G�O�G�O�G�O�@�Ox@�T�@�YNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@e0G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�W�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�WC@�U�G�O�G�O�G�O�G�O�@�QD@�V�G�O�@�X�G�O�G�O�G�O�G�O�G�O�@�X(@�Z�@�Z�G�O�G�O�@Q�KG�O�@�X�@�W@�OLG�O�G�O�@�\�G�O�@�`�@�Y�@�[.@�U�G�O�@X6j@�U�G�O�@�Y	@�W�@�\@�_G�O�G�O�@�W@@�[�@�[�@�V+@�U�@�X�@�Wy@�W�@�X�@�X�@�Y7@�[@�Y:@�Z�@�[�@�Zr@�Y^@�Yf@�Z�@�Z@�Z @�Z@�Y�@�Z2G�O�G�O�@\��@�Z@�Zr@�Yc@�X�@�Z@�Y�@�Ya@�\�@�_�@�`�G�O�@�[@�a}@�_^@�ZI@�a*@�_�G�O�@��@�`�@�G�G�O�@�^�@�^J@�`,@�^�@�&.@�_�@�b;@�b;@�a�@�b;@�a�@�F
@�a'@�a�G�O�G�O�@�a�@�b�@�Y�@�a�@v��@�a�@�`�G�O�@�`�@�_r@�a�@�b;@�a�@�a*@�a�@x)K@�_ZG�O�@��@�` G�O�@�`�@�_�@�`*@�`*@�^G�O�@�`�@�a&@�`*@�b�@�a�@�]�@�b6@�b;@�`�@�`hG�O�@�b�@~D@�`�@�`,@���@�^L@�d@�c�@�d]@�b�@V$@�cJ@u�G�O�G�O�@�cHG�O�@�cb@�d
@�b�@�b�@�b�@�b�@�a�G�O�G�O�@�a�G�O�@gՕ@�a�@�a*@�^_@^��@�`k@�a�@R�"G�O�@�b�G�O�@�i�@�dX@�b�@�c�@�c�@�b�@��@���@�b�@�b�G�O�@���G�O�@�c�@�c�@�a�@�a�@�ZG�O�G�O�G�O�@�d@�c_@�b�@�cb@�b;@�`�G�O�@�d@�e~@�b�@�`�@�b;@�a�@�a�@�b�G�O�@q]�G�O�G�O�@�c�@�a�@�a�@�_�@�b�G�O�@�d@�d@�b�G�O�@�b�@�a=@�`+G�O�G�O�@�V@�E�@�K�@�N'@�3�@j�@��@�	?@�P�@�d\@���G�O�@�f'@�f}@�e@�d@�a$G�O�@�dZ@���@�dX@�dZG�O�G�O�@�d@�f(@�d�@h��G�O�@�e{@�e�@�f�@�f=G�O�@�e�@�gO@�f�@�e�@�f7@�
PG�O�@�f�@�f�@�f�@�g�@�hJ@�i@�i@�h�@�i@�g�@�hM@�g�G�O�G�O�G�O�@�e.@�dG�O�@�e�@�b8@�e�@�g�@�i
@�i�@�i�@�j@�i�@�i@�i�@�j+@�j*@�j*@�ij@�i@�im@�j~@�k;@�j*@�f�@�g�@�h�@�ip@�g�@�g�@�i@�h�@�i@�g�@�j�@�i�@�i@�j�@�k�@�k;@�lL@�lO@�k�@�jj@�j}@�j{@�j�@�j�@�j�@�k(@�k>@�k(@�k;@�kd@�kb@�k�@�k�@�k�@�lN@�k�@�l5@�l5@�k�@�k�@�l#@�lv@�l8@�kl@�k�@�l8@�k�@�l"@�l5@�lv@�l�@�l�@�l�@�l�@�l�@�m@�m`@�l:@�l)@�lb@�lw@�lx@�l�@�l�@�l�@�l�@�mt@�mt@�mq@�mI@�m@�m@�m2@�m�@�m�@�m�@�mq@�m�@�m�@�m�@�n�@�n�@�o@�o,@�n�@�o@�n�@�n�@�n�@�o+@�o�@�o�@�p@�p@�p&@�p:@�pP@�pQ@�p�@�p�@�p�@�q
@�p�@�q�@�rG@�r@�s�@�s�@�q�@�o�@�q�@�s�@�sZ@�r@�qb@�r�@�sZ@�s�@�t�@�tB@�t�@�q7@�qx@�s�@�t@�u@�sD@�t�@�r�@�r@�r@�r@�r2@�q�@�rG@�s@�s@�s@�r�@�s@�s@�q�@�rY@�q�@�s@�r�@�s@�s�@�s�@�qN@�qw@�q�@�r@�rK@�r�@�u@�v�@�u�@�s2@�r�@�sD@�sT@�s@�t@�t @�sV@�q�@�r�@�p�@�p}@�p$@�oR@�qJ@�p8@�p:@�s�@�t�@�tf@�t�@�t�@�t@�s�@�r�@�v7@�x�@�x@�x�@�w2@�u�@�wn@�uf@�u:@�v4@�x.@�x�@�z'@�zf@�z?@P��@P�@P�>@P�@P�J@P�P@P��@P�@P��@P�@P�@P�[@P��@P�@P�;@P�@P�:@P�@P�5@P�:@P�@P��@P�@Pߥ@P�[@Pٓ@P�n@P��@P�@P��@P�:@P��@P�@P�R@Pʭ@P�@P�`@P�@P�Z@P�@P��@P��@Pʭ@Pʨ@P�~@Pʭ@P��@P��@P��@P��@P��@P��@P�*@PŚ@P��@P�P@Pà@P��@PŽ@PƸ@PǺ@Pǎ@Pǵ@P�6@P��@P��@P�6@Pȍ@P�5@Pʭ@P�Z@P��@P̣@P�"@P�u@P��@Pͣ@P�s@P��@P͠@P�s@P��@P͛@P͝@P��@P͠@Pͣ@P͝@P��@P�s@P�K@P�K@P��@P��@P��@P̠@P�N@P�&@P�*@P��@P�V@P�Z@P�@Pɰ@PȆ@P�`@P�c@P�p@P�@P��@P��@P��@P�H@P�J@P�@P��@P��@P��@P��@Pâ@�jj@�j}@�j{@�j�@�j�@�j�@�k(@�k>@�k(@�k;@�kd@�kb@�k�@�k�@�k�@�lN@�k�@�l5@�l5@�k�@�k�@�l#@�lv@�l8@�kl@�k�@�l8@�k�@�l"@�l5@�lv@�l�@�l�@�l�@�l�@�l�@�m@�m`@�l:@�l)@�lb@�lw@�lx@�l�@�l�@�l�@�l�@�mt@�mt@�mq@�mI@�m@�m@�m2@�m�@�m�@�m�@�mq@�m�@�m�@�m�@�n�@�n�@�o@�o,@�n�@�o@�n�@�n�@�n�@�o+@�o�@�o�@�p@�p@�p&@�p:@�pP@�pQ@�p�@�p�@�p�@�q
@�p�@�q�@�rG@�r@�s�@�s�@�q�@�o�@�q�@�s�@�sZ@�r@�qb@�r�@�sZ@�s�@�t�@�tB@�t�@�q7@�qx@�s�@�t@�u@�sD@�t�@�r�@�r@�r@�r@�r2@�q�@�rG@�s@�s@�s@�r�@�s@�s@�q�@�rY@�q�@�s@�r�@�s@�s�@�s�@�qN@�qw@�q�@�r@�rK@�r�@�u@�v�@�u�@�s2@�r�@�sD@�sT@�s@�t@�t @�sV@�q�@�r�@�p�@�p}@�p$@�oR@�qJ@�p8@�p:@�s�@�t�@�tf@�t�@�t�@�t@�s�@�r�@�v7@�x�@�x@�x�@�w2@�u�@�wn@�uf@�u:@�v4@�x.@�x�@�z'@�zf@�z?@P��@P�@P�>@P�@P�J@P�P@P��@P�@P��@P�@P�@P�[@P��@P�@P�;@P�@P�:@P�@P�5@P�:@P�@P��@P�@Pߥ@P�[@Pٓ@P�n@P��@P�@P��@P�:@P��@P�@P�R@Pʭ@P�@P�`@P�@P�Z@P�@P��@P��@Pʭ@Pʨ@P�~@Pʭ@P��@P��@P��@P��@P��@P��@P�*@PŚ@P��@P�P@Pà@P��@PŽ@PƸ@PǺ@Pǎ@Pǵ@P�6@P��@P��@P�6@Pȍ@P�5@Pʭ@P�Z@P��@P̣@P�"@P�u@P��@Pͣ@P�s@P��@P͠@P�s@P��@P͛@P͝@P��@P͠@Pͣ@P͝@P��@P�s@P�K@P�K@P��@P��@P��@P̠@P�N@P�&@P�*@P��@P�V@P�Z@P�@Pɰ@PȆ@P�`@P�c@P�p@P�@P��@P��@P��@P�H@P�J@P�@P��@P��@P��@P��@PâG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4444444443344433444444444444444444444443344444444344444444444444444444444444444444444444443444334444444444444443444444444444444444444443444433444444444443444443443444443334444444444343444444444434444444334444334344444333443433344343333433433334433333333333333333333333344333333333334333333433343333333333333344333333343333333334334333334333333333343333333333333443433333334434333333334343333333333434333334443333334333333334344333334333433344333333333334333334333344333343333433333343333333333334443343333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�rO9�r^9�r\9�r�9�r~9�r�9�r�9�r�9�r�9�r�9�s9�s9�sO9�sO9�s�9�s�9�s9�s�9�s�9�sO9�sm9�s�9�s�9�s�9�s9�s9�s�9�s�9�s�9�s�9�s�9�t@9�t9�t=9�t@9�t!9�t\9�t�9�s�9�s�9�s�9�s�9�s�9�t9�s�9�t9�t@9�t�9�t�9�t�9�t9�t\9�t_9�tn9�t�9�t�9�t�9�t�9�t�9�t�9�t�9�u}9�u�9�u�9�u�9�u�9�u�9�u�9�u�9�u�9�u�9�vM9�vP9�v�9�v�9�v�9�v�9�v�9�v�9�v�9�w9�w=9�w\9�wA9�w�9�xN9�x!9�yM9�yo9�x9�vP9�w�9�yo9�y9�x-9�w�9�x�9�y9�yO9�z/9�y�9�z>9�w~9�w�9�yM9�y�9�zn9�y9�z9�x�9�x9�x,9�x+9�x>9�x9�xN9�x�9�x�9�x�9�x�9�x�9�x�9�x9�x\9�x9�x�9�x�9�x�9�y�9�yl9�w�9�w�9�w�9�x9�xQ9�x|9�zo9�{�9�z�9�y9�x�9�y9�y9�x�9�y�9�y�9�y9�w�9�x�9�v�9�v�9�v�9�v9�w�9�v�9�v�9�y�9�z9�y�9�zO9�z-9�y�9�y_9�x�9�{N9�}9�|�9�}-9�|9�z�9�|;9�z�9�z�9�{L9�|�9�}A9�~O9�~9�~a9S�9M9H(9F�9E�9E)9D�9C�9D(9C�9C-9B�9B�9A�9AJ9A�9AI9A�9AE9AI9A*9@L9>�9=�9<99*98J96l94�94
92�90�9/�9/9-�9-P9,�9,�9,�9-L9.9-�9-�9-�9-�9-�9-�9-�9.9-�9-�9-�9-j9)�9(�9(19(n9)N9*
9*�9+�9+m9+�9+�9,p9,m9+�9,/9,�9-�9.R9.�9/M9/�9/�90,909/�90,909/�90.90
9090L90909090,9/�9/�9/�9/�9/�9/�9/J9/9.�9.�9.�9.O9-�9-L9-9,*9,9+L9)�9)�9)m9)p9)N9(�9(�9(�9(�9(�9(�9(�9(oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
{B
{B
uB
{B
�B
�B
�B
 �B
 �B
"�B
)�B
-B
>wB
E�B
J�B
VB
hsB
jB
r�B
r�B
y�B
�oB
�!B
�9B
�?B
�ZB
��BBA�BXBl�Bp�Bq�B{�Bz�B}�B�=B�=B�\B�BÖB�B�B��BƨB�dBŢBȴB��B��B�#B�ZB�B�5B��B�#B�5BɺB��B��B�B�B�mB�5B�#B�/B��B�RB�'B��B�PB�Bx�BaHBK�B5?B{BB
��B
��B
�B
�sB
�B
�)BBB
��B
�;B
�LB
��B
��B
�1B
k�B
[#B
N�B
7LB
DB	�TB	��B	�B	��B	�\B	u�B	hsB	`BB	P�B	>wB	)�B	�B	
=B��B�HB�
B��BŢB�qB�LB�!B�B��B��B��B�oB�bB�VB�%B{�Bp�BffB^5B[#BXBcTBy�B�B� B~�B|�B�B�B�+B�7B�JB�\B�VB�%Bv�Bq�Bo�Bp�Bm�Bs�Bx�B{�B�B�B�7B�\B��B�!B��B��B��B�3B�?B�FB�RBŢB�#B�mB�B��B��B��B��B	oB	�B	�B	�B	JB��B�B�BB��B�wB�XB�FB�?B�9B�?B�9B�3B�-B�3B�?B�^B�jB�dB�dB�jB�}B�qB�dB�FB�B��B��B�'B�9B�?B�?B�RB�RB�XB�RB�LB�RB�RB�LB�FB�?B�-B�B��B��B��B��B��B�uB�\B�JB�=B�=B�DB�=B�7B�+B�B�B}�B|�B{�B{�B�B�\B�{B��B��B��B��B�B�B�B�B�3B�LB�9B�9B�?B�FBȴBǮBÖB�}B��BǮB��B��B�)B�B��B��B��B��B	B	
=B	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	#�B	'�B	'�B	'�B	'�B	+B	+B	1'B	5?B	;dB	<jB	?}B	B�B	A�B	B�B	D�B	K�B	Q�B	`BB	m�B	o�B	r�B	w�B	w�B	x�B	x�B	y�B	y�B	x�B	t�B	x�B	n�B	bNB	]/B	S�B	N�B	P�B	iyB	z�B	�B	�VB	�7B	�+B	�7B	�PB	�hB	�{B	�{B	�uB	�bB	�DB	�%B	~�B	x�B	y�B	|�B	}�B	}�B	}�B	~�B	� B	�B	�VB	�\B	�VB	�\B	�{B	��B	��B	��B	�B	�-B	�?B	�dB	�jB	�qB	�qB	�wB	�}B	�}B	�wB	�qB	�wB	�XB	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�!B	�B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�'B	�!B	�!B	�-B	�3B	�-B	�-B	�'B	�!B	�'B	�3B	�9B	�?B	�FB	�RB	�jB	�qB	�wB	�}B	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ÖB	��B	��B	ÖB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�5B	�BB	�BB	�BB	�BB	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�HB	�/B	�B	�B	�)B	�/B	�;B	�BB	�TB	�ZB	�ZB	�B	�2B
mB
�B
$&B
.�B
72B
;JB
<�B
@�B
M6B
R�B
Y1B
^�B
^jB
a�B
f2B
kB
o5B
t�B
wLG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>l��>���>y�*>��>��>�or>ԛ�?.�?H�4A�-7A���?jU?R@�@�B	�B
(Q?wW?$і@��G@,T�>x��>��>�
|>� �>�#�>���>�"�>ô�?�@lM�@&�>�h�?9!�@�Tx>�@_>���>ȸ?�?�9&B
B
]�>�V.?��e>���?�[?���>�A_?��?n��B
�@hKU?	�?�T7@��@Y�@���>ƃ�>�:d>��>�y?Nw?�}t?J6~?-��>���?Y>��\>�$�>١4?s�?}��A�q>���>���>���>���>�ѻ>���?N�AɆA|�6?^tA=�SAU�>�o>�(?Ug4A3�H?"%?���B
e>�o?	g4?��.B
�B*>���?\��@�%$@��=>�>�*?^�?��?![�@�L�>���>��`>�(�? �?���B]?yW>�#?�?�r�A�8�>_�W>x�k>�If>�/�?t>��w?&�a@]��>���>���>�8>��:?#�@�6�AdU}>�Ϸ?|?h.A�e}>���>�&�?�Sa@D�B
\B
�!>�A@+֌A��E>�L�>r��>���>���>�� >ҷ�?�#?efZB
�A+��@^>ڸ�?.A�SB
D?ݭ?��B
�>���>�j�>�R�>Ȧ�?P�$B

�B
B
E�>�53>�I�>�?%�AS!>�"�>��>�+�>��?%�A��b?F�	A��?W'@�Bn>�7~>늠A	��>��?�>��?��?z�NB
0�>�Te>��>�,�?�:>�0D?�&Ae�B
9B
�)>�'>���>蛷?YsdB
�B
x�?<PB	�?[@kњ>���?
y5?�K�B
#B
�B
@o.�?�A�� @g��B
4B
B
V[?��>@���B
xi?xl�B
�B
�B
3B	iR?��A��B
N?���B
iB
:B
OB
�A@�%�B
�B
�B
�B
B
3B
B
>B
�B
�B
eB
�B
�B
:B
3B
uB
DB
'�B
B
+B
�B
�B
�B
5B
oIAzlA@�@�A��=B
�B
B
�B
�B
�B
yB
�B
|B
�B
;@uSB
B
�B
B
�B
.B
�@���A��B
�B
Xd?�bB
KB
�B
B
�Bz�B
�B
_B
_B
B
WB
�A℠B
rB
�?i��@ j�B
B
�B
B
A�a�B
B
 �?C_pB
�B
*B
�B
_B
�B
�B
%=A��MB
@N:�B �B
�@���B
sB
�B
�B
�B
>?�5�B
�B
�B
SB
�B
B
,�B
�B
�B
B
�?���B
AŎ�B
B
B�<B
�B
B
2B
�B
�A�e�B
UA�ŻA��Am��B
3@��]B
�B
�B
�B
�B
EB
�B
�?��?���B
�A���A���B
�B
.B
�A�گB
PB
C�A��cA�FB
oA���B
9�B
�B
 B
�B
�B
�B@�A�ƸB
�B
�@Z�A��@�P�B
�B
+B
�B
�B
�A��@n>�Ak�B
JB
"B
�B
�B
_B
�?xb�B
�B
B
�B
B
�B
zB
	B
 @(��A�НA���@��=B
�B
	B
�B
�B	F.@��B
�B
3B
)2AL:B
fB
jB
�A&�@!��B
+B
:B
�B
&B @�A���B	ԯB	�B
�B
�BPA�B
�B
�B
�B
8@B
.�?d4B
A�� B
�B
�Ax?Y%B
h�B
6B
�A�� A��B
�B
�B
B
HAa�`B
�B
B
�B
�B
�Bk�Ah�B
S�B
?B
:-B
�B
;B
�B
!B
�B
!B
�B
�B
AI�?qj8A���B
]B
?�	B
YB
�B
$fB
pB
�B
�B
�B
:B
�B
	B
�B
B
=B
=B
OB
�B
?B
�B
�B
B
3B
�B
�B
 B
�B
�B
�B
B
�B
rB
�B
gB
>B
B
BB
�B
�B
�B
nB
fB
�B
�B
�B
�B
�B
/B
�B
B
RB
�B
3B
�B
+B
B
4B
�B
�B
.B
�B
�B
�B
�B
	B
�B
�B
5B
�B
-B
�B
�B
�B
:B
'B
eB
�B
�B
:B
XB
�B
�B
�B
(B
�B
dB
TB
\B
_B
B
�B
zB
jB
bB
�B
�B
�B
�B
�B
vB
B
LB
<B
�B

B
cB
�B
cB
�B
�B
tB
 B
�B
gB
B
�B
�B
pB
�B
�B
�B
.B
jB
aB
<B
/B
�B
SB
=B
lB
�B
kB
�B
FB
B
B
7B
<B
�B
�B
IB
DB
�B
�B
�B
�B
hB
'B
�B
�B
wB
�B
�B
zB
9B
.B
9B
�B
�B
�B
�B
~B
5B
B
�B
B
FB
$B
�B
B
uB
dB
�B
,B
�B
�B
EB
gB
�B
zB
IB
B
�B
B
�B
B
�B
�B
>B
�B
 B
pB
8B
hB
�B
�B
AB
�B
WB
�B
�B
�B
/B
�B
�B
0B
�B
CB
�B
'B
2B
$B
B
�B
�B
B
�B
�B
AB
�B	ۢB	�B	��B	�B	�dB	�B	�B	�B	�FB	�IB	�B	��B	��B	��B	��B	�iB	�B	��B	��B	�AB	�B	��B	�iB	�wB	��B	��B	�B	�hB	�gB	�tB	�tB	�B	�4B	�B	�B	�B	��B	�:B	�kB	��B	��B	��B	�B	�YB	�|B	�oB	�bB	�UB	�
B	��B	�B	�B	�WB	�B	�B	��B	�}B	�B	�+B	��B	�B	�B	�B	�B	�B	��B	�dB	��B	�B	��B	�B	�lB	��B	�
B	�;B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�
B	�;B	��B	��B	�}B	��B	�B	�HB	�;B	��B	��B	�nB	�B	�B	�}B	��B	�B	�6B	�CB	�)B	�6B	�B	�FB	��B	��B	��B	�\B	�BB	�5B	�B	�B	�B	�:B	��B
B
B
B
yB
#B
KB
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
B
qB
&B
�B
�B
�B
rB
�B
�B
�B
�B
�B
�B
�B
�B
@B
;B
dB
B
�B
jB
_B
GB
,B
]B
_B
B
2B
eB
-B
�B
�B
.B
&B
�B
B
wB
�B
B
�B
�B
�B
�B
]B
�B
�B
�B
!B
7B
�B
�B
7B
'B
EB
�B
aB
YB
�B
�B
�B
�B
�B
mB
�B
�B
HB
fB
1B
�B
ZB
UB
�B
B
oB
B
mB
�B
;B
�B
"B
iB
B
<B
pB
�B
kB
6B
�B
�B
�B
tB
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
)B
�B
�B
B
�B
�B
^B
CB
FB
B
�B
"B
@B
hB
�B
B
B
�B
�B
�B
uB
4B
pB
{B
vB
6B
iB
�B
hB
�B
�B
�B
B
CB
xB
7B
�B
�B
>B
�B
^B
1B
;B
�B
�B
�B
�B
�B
RB
B
�B
�B
B
NB
�B
VB
B
sB
�B
nB	��B	݅B	�
B	�}B	��B	�$B	ܺB	��B	�B	ܭB	��B	ܾB	�UB	�bB	�B	�HB	� B	�0B	��B	��B	ܟB	��B	� B	�jB	��B	�"B	�NB	ݔB	�7B	�RB	�%B	�kB	�:B	ޤB	�fB	��B	�fB	�,B	�\B	߽B	�hB	�<B	�B	�B	��B	��B	��B	��B	��B	߸B	߫B	ߞB	�&B	��B	�B	�7B	�gB	�%B	�B	�kB	�B	��B	��B	�.B	�B	�B	��B	�)B	�B	�B	��B	�]B	��B	�B	�KB	�nB	�SB	�&B	�FB	�B	��B	�"B	��B	��B	�B	��B	��B	�B	�B	�aB	�EB	�8B	��B	��B	��B	�B	�AB	�B	��B	��B	�WB	�B	�IB	��B	�*B	�B	�LB	��B	�B	�ZB	�MB	�2B	��B	�B	�B	�VB	�IB	�;B	�2B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444443344433444444444444444444444443344444444344444444444444444444444444444444444444443444334444444444444443444444444444444444444443444433444444444443444443443444443334444444444343444444444434444444334444334344444333443433344343333433433334433333333333333333333333344333333333334333333433343333333333333344333333343333333334334333334333333333343333333333333443433333334434333333334343333333333434333334443333334333333334344333334333433344333333333334333334333344333343333433333343333333333334443343333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   B
�B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
B
zB
zB
sB
yB
�B
�B
�B
 �B
 �B
"�B
)�B
-B
>vB
E�B
J�B
VB
hrB
j}B
r�B
r�B
y�B
�qB
�"B
�:B
�>B
�[B
��BBA�BXBl�Bp�Bq�B{�Bz�B}�B�=B�>B�[B� BÕB�B�B��BƨB�dBŠBȳB��B��B�#B�YB�B�3B��B� B�7BɹB��B��B�B�B�mB�5B�#B�.BʿB�RB�(B��B�QB�Bx�BaHBK�B5?B{BB
��B
��B
�B
�pB
�B
�'BBB
��B
�;B
�LB
��B
��B
�0B
k�B
[#B
N�B
7MB
AB	�TB	��B	�B	��B	�\B	u�B	hsB	`AB	P�B	>vB	)�B	�B	
<B��B�GB�B��BšB�qB�JB�!B�B��B��B��B�oB�_B�VB�#B{�Bp�BfeB^4B[#BXBcRBy�B�B�B~�B|�B�B�B�*B�6B�JB�XB�UB�%Bv�Bq�Bo�Bp�Bm�Bs�Bx�B{�B�B�B�7B�[B��B�"B��B��B��B�2B�>B�GB�QBŠB�!B�nB�B��B��B��B��B	mB	�B	�B	B	IB��B�B�AB��B�vB�VB�DB�>B�9B�?B�7B�1B�+B�1B�=B�_B�jB�dB�bB�hB�zB�rB�cB�EB�B��B��B�(B�9B�<B�?B�PB�PB�WB�PB�KB�QB�NB�JB�FB�>B�-B�B��B��B��B��B��B�sB�ZB�HB�;B�;B�AB�;B�5B�)B�B�B}�B|�B{�B{�B�B�[B�yB��B��B��B��B�B�B�B�B�/B�KB�7B�8B�?B�DBȲBǯBÓB�{B��BǬB��B��B�)B�B��B��B��B��B	B	
>B	gB	zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	}B	�B	�B	"�B	%�B	#�B	'�B	'�B	'�B	'�B	+ B	+B	1&B	5?B	;bB	<iB	?zB	B�B	A�B	B�B	D�B	K�B	Q�B	`@B	m�B	o�B	r�B	w�B	w�B	x�B	x�B	y�B	y�B	x�B	t�B	x�B	n�B	bKB	].B	S�B	N�B	P�B	iwB	z�B	�B	�SB	�6B	�*B	�5B	�NB	�hB	�yB	�zB	�tB	�dB	�DB	�%B	~�B	x�B	y�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�SB	�^B	�VB	�\B	�{B	��B	��B	��B	�B	�+B	�=B	�dB	�hB	�pB	�oB	�sB	�zB	�{B	�uB	�pB	�wB	�VB	�*B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�%B	�'B	�&B	�'B	�B	� B	�,B	�2B	�,B	�+B	�)B	� B	�&B	�/B	�8B	�?B	�HB	�RB	�jB	�oB	�vB	�}B	�{B	��B	B	ÖB	ÔB	ĚB	ŠB	šB	ÕB	��B	��B	ÕB	ʾB	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	� B	�(B	�2B	�@B	�AB	�@B	�?B	�8B	�9B	�AB	�GB	�LB	�SB	�SB	�HB	�-B	�B	�B	�(B	�.B	�;B	�AB	�UB	�ZG�O�B	�B	�/B
lB
�B
$#B
.�B
72B
;KB
<�B
@�B
M4B
R�B
Y1B
^�B
^hB
a�B
f1B
kB
o4B
t�B
wKG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-5A���G�O�G�O�G�O�B	�B
(QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
B
]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
eG�O�G�O�G�O�B
�B*G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�e~G�O�G�O�G�O�G�O�B
[B
�"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�B
DG�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�B

�B
B
E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��dG�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
0�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
9B
�)G�O�G�O�G�O�G�O�B
�B
x�G�O�B	�G�O�G�O�G�O�G�O�G�O�B
#B
�B
G�O�G�O�A��!G�O�B
3B
B
VYG�O�G�O�B
xjG�O�B
�B
�B
3B	iRG�O�A��B
LG�O�B
gB
7B
PB
�G�O�G�O�B
�B
�B
�B
B
3B
B
:B
�B
�B
cB
�B
�B
:B
2B
wB
CB
'�B
B
*B
�B
�B
�B
2B
oHG�O�G�O�A��9B
�B
B
�B
�B
�B
xB
�B
}B
�B
<G�O�B
B
�B
B
�B
.B
�G�O�A��B
�B
XdG�O�B
JB
�B
B
�Bz�B
�B
`B
`B
B
WB
�AℜB
oB
�G�O�G�O�B
B
�B
B
A�a�B
B
 �G�O�B
�B
+B
�B
`B
�B
�B
%<A��KB
G�O�B �
B
�G�O�B
qB
�B
�B
�B
<G�O�B
�B
�B
PB
�B

B
,�B
�B
�B
~B
�G�O�B
 AŎ�B
B
B�9B
�B
B
0B
�B
�A�e�B
UA�żG�O�G�O�B
2G�O�B
�B
�B
�B
�B
DB
�B
�G�O�G�O�B
�G�O�A���B
�B
.B
�A�ڬB
NB
C�A��aG�O�B
pG�O�B
9�B
�B
 B
�B
�B
�B@�A�ƷB
�B
�G�O�A��G�O�B
�B
'B
�B
�B
�G�O�G�O�G�O�B
GB
!B
B
�B
`B
�G�O�B
�B
B
�B
B
�B
zB
B
 G�O�A�ЛG�O�G�O�B
B
B
�B
�B	F.G�O�B
�B
0B
)2G�O�B
fB
jB
�G�O�G�O�B
*B
7B
�B
&B @�A���B	ԭB	��B
�B
�BPG�O�B
�B
�B
�B
8>B
.�G�O�B
A�� B
�B
�G�O�G�O�B
h�B
5B
�A�� G�O�B
�B
�B
B
HG�O�B
�B
B
�B
�B
�Bk�G�O�B
S�B
=B
:)B
�B
;B
�B
!B
�B
!B
�B
�B
G�O�G�O�G�O�B
[B
G�O�B
WB
�B
$dB
rB
�B
�B
�B
8B
�B
	B
�B
 B
<B
<B
KB
�B
=B
�B
�B
B
3B
�B
�B
!B
�B
�B
�B
B
�B
pB
�B
gB
=B
B
CB
�B
�B
�B
kB
B
B
	B
wB
!B
KB
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
B
oB
#B
�B
�B
�B
qB
�B
�B
�B
�B
�B
�B
�B
�B
@B
;B
bB
B
�B
fB
_B
HB
.B
\B
]B
B
2B
dB
,B
�B
�B
.B
$B
�B
|B
xB
�B
B
�B
�B
�B
�B
ZB
�B
�B
�B
!B
6B
�B
�B
7B
'B
GB
�B
_B
YB
�B
�B
�B
�B
�B
mB
�B
�B
FB
cB
2B
�B
XB
VB
�B
B
rB
B
mB
�B
=B
�B
"B
iB
	B
<B
oB
�B
kB
5B
�B
�B
�B
sB
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
'B
�B
�B
B
�B
�B
\B
DB
DB
B
�B
"B
?B
iB
�B
B
B
�B
�B
�B
uB
2B
oB
xB
uB
5B
fB
�B
fB
�B
�B
�B
|B
BB
wB
5B
�B
�B
<B
�B
]B
/B
<B
�B
�B
�B
B
�B
RB
|B
�B
�B
B
MB
�B
TB
B
rB
�B
oB	��B	݅B	�	B	�{B	��B	�"B	ܺB	��B	�B	ܫB	��B	ܽB	�SB	�^B	�B	�FB	��B	�-B	��B	��B	ܟB	��B	� B	�iB	��B	�"B	�KB	ݓB	�6B	�PB	�$B	�jB	�8B	ޥB	�fB	��B	�fB	�,B	�ZB	ߺB	�fB	�<B	�B	� B	��B	��B	��B	��B	��B	߷B	߫B	ߞB	�"B	��B	��B	�8B	�eB	�#B	�B	�hB	�B	��B	��B	�,B	�B	�B	��B	�*B	�B	�B	��B	�\B	��B	�B	�IB	�kB	�VB	�$B	�CB	�B	��B	�"B	��B	��B	�	B	��B	��B	�B	�B	�_B	�DB	�6B	��B	��B	��B	�B	�>B	�B	��B	��B	�VB	�B	�FB	��B	�&B	� B	�KB	��B	�B	�YB	�NB	�0B	��B	�B	�B	�UB	�IB	�:B	�1B	��B
B
B
	B
wB
!B
KB
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
B
oB
#B
�B
�B
�B
qB
�B
�B
�B
�B
�B
�B
�B
�B
@B
;B
bB
B
�B
fB
_B
HB
.B
\B
]B
B
2B
dB
,B
�B
�B
.B
$B
�B
|B
xB
�B
B
�B
�B
�B
�B
ZB
�B
�B
�B
!B
6B
�B
�B
7B
'B
GB
�B
_B
YB
�B
�B
�B
�B
�B
mB
�B
�B
FB
cB
2B
�B
XB
VB
�B
B
rB
B
mB
�B
=B
�B
"B
iB
	B
<B
oB
�B
kB
5B
�B
�B
�B
sB
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
'B
�B
�B
B
�B
�B
\B
DB
DB
B
�B
"B
?B
iB
�B
B
B
�B
�B
�B
uB
2B
oB
xB
uB
5B
fB
�B
fB
�B
�B
�B
|B
BB
wB
5B
�B
�B
<B
�B
]B
/B
<B
�B
�B
�B
B
�B
RB
|B
�B
�B
B
MB
�B
TB
B
rB
�B
oB	��B	݅B	�	B	�{B	��B	�"B	ܺB	��B	�B	ܫB	��B	ܽB	�SB	�^B	�B	�FB	��B	�-B	��B	��B	ܟB	��B	� B	�iB	��B	�"B	�KB	ݓB	�6B	�PB	�$B	�jB	�8B	ޥB	�fB	��B	�fB	�,B	�ZB	ߺB	�fB	�<B	�B	� B	��B	��B	��B	��B	��B	߷B	߫B	ߞB	�"B	��B	��B	�8B	�eB	�#B	�B	�hB	�B	��B	��B	�,B	�B	�B	��B	�*B	�B	�B	��B	�\B	��B	�B	�IB	�kB	�VB	�$B	�CB	�B	��B	�"B	��B	��B	�	B	��B	��B	�B	�B	�_B	�DB	�6B	��B	��B	��B	�B	�>B	�B	��B	��B	�VB	�B	�FB	��B	�&B	� B	�KB	��B	�B	�YB	�NB	�0B	��B	�B	�B	�UB	�IB	�:B	�1B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444443344433444444444444444444444443344444444344444444444444444444444444444444444444443444334444444444444443444444444444444444444443444433444444444443444443443444443334444444444343444444444434444444334444334344444333443433344343333433433334433333333333333333333333344333333333334333333433343333333333333344333333343333333334334333334333333333343333333333333443433333334434333333334343333333333434333334443333334333333334344333334333433344333333333334333334333344333343333433333343333333333334443343333333333333333333333333333333333111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455152020082814551520200828145515202008281455152020082814551520200828145515202008281455152020082814551520200828145515202008281455152020082814551520200828145515AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730412019021417304120190214173041    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730412019021417304120190214173041  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730412019021417304120190214173041  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455152020082814551520200828145515  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                