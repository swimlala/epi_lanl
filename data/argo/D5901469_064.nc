CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  I   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:21:08Z creation      
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
resolution        =���   axis      Z        'l  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'l  vX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'l  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'l  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'l  T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� '�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'l 1�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'l Y   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �t   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'l �P   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'l ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'l �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 
p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'l L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ;�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'l E�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � m    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   m�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   y�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20181120212108  20200901153524  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               @   @   @AAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @�ŏA@�ŏA@�ŏA111 @�ŏ��@�ŏ��@�ŏ��@4Õ�$�@4Õ�$�@4Õ�$��cv��+�cv��+�cv��+111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dts3Dy�{D��D�R�D�
D��HD��D�C3D�x D���D��D�@�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    =���>���                =���    =���                    >L��        =���        =���=���    =���>���        >L��>���                =���>L��    =���>���>L��                =���>L��                    =���                    =���=���                >L��    =���            >L��    =���            =���        =���                    =���>L��                    =���    =���        =���>L��>L��        =���            =���        =���    =���        =���>L��    =���                =���                    =���            =���=���    =���>L��                    =���=���    =���=���                >L��            >L��>L��=���    >L��>���>L��>���>���        =���    =���        >L��                        =���    =���    >L��            =���=���    =���    =���>L��>L��>L��        >L��=���=���>L��    >L��=���    =���=���>L��=���        >L��=���=���=���=���>L��>���>L��=���=���>L��>L��=���>L��>L��>���>L��>L��>L��>L��>���>L��=���>���>���>L��>L��>L��>���>L��>L��>���>���>���>L��>L��>���>���=���>���=���>L��>���>���>L��>���>���>L��>���=���>���>���>L��>���>���>���>���>���>L��>L��>L��>���>���>L��>���>���>���>���>L��>���>���>���>���>���>���>L��>L��>���>���>���>���>���>���>���=���>���>���>���>L��>���>���>L��>L��>L��>���>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>���>L��>L��>���>���>���>���>���>L��=���>���>L��>���>���>L��>���>���>L��>L��>L��=���>L��>L��>���>L��>L��>L��>���>���>���>L��>���>���>���>L��>���>���>L��>L��>���>���>���>���>L��=���>���>L��>L��>L��>���>L��>���>L��>���>L��>���>���>���=���>���>L��>���>���>L��>���>L��>���>L��>L��>���>���>L��>L��>���>���>���>���>���>���>���>���>L��>L��>L��>L��>L��>���>L��>���>���>���>���>���>���>���>���>L��>L��>L��>���>���>���>L��>L��>L��>L��>���>���>���>L��>���>���>L��>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>L��>L��>L��>L��>L��>���>L��>���>���>���>���>���>���>���>���>L��>���>���>���>L��>���>L��>���>L��>L��>���>L��?   >���>L��>L��>���=���>L��>���>L��>���?   ?   ?   >���?333?��?��?333?L��?fff?fff?�  ?�  ?���?���?���?�ff?�ff?�ff?�  ?�  ?���?ٙ�?ٙ�?�33?�33?�33?�33@   @��@��@33@33@33@��@   @&ff@&ff@,��@,��@9��@9��@@  @L��@S33@Y��@`  @fff@l��@y��@y��@�33@�ff@���@���@�  @�ff@���@���@�  @�33@�ff@���@�  @�33@�ff@���@���@�33@�33@ə�@���@�  @�33@�ff@ٙ�@�  @�  @�33@陚@���@�  @�33@�ff@���A   A��A33A��AffA  A33A33AffA  A��A33A��AffA  A33A��AffA   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A4��A6ffA9��A;33A<��A@  AA��AC33AFffAH  AI��AL��ANffAP  AS33AT��AX  AY��A[33A^ffA`  Aa��Ad��AfffAi��Ak33Al��Ap  Aq��At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�33A�  A���A�ffA�33A�  A���Aݙ�Dp�fDp��Dp�3Dp� Dp�fDp��Dp�3Dq  DqfDq�Dq3Dq  Dq&fDq,�Dq33Dq@ DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt� @9��@9��@@  @L��@S33@Y��@`  @fff@l��@y��@y��@�33@�ff@���@���@�  @�ff@���@���@�  @�33@�ff@���@�  @�33@�ff@���@���@�33@�33@ə�@���@�  @�33@�ff@ٙ�@�  @�  @�33@陚@���@�  @�33@�ff@���A   A��A33A��AffA  A33A33AffA  A��A33A��AffA  A33A��AffA   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A4��A6ffA9��A;33A<��A@  AA��AC33AFffAH  AI��AL��ANffAP  AS33AT��AX  AY��A[33A^ffA`  Aa��Ad��AfffAi��Ak33Al��Ap  Aq��At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�33A�  A���A�ffA�33A�  A���Aݙ�Dp�fDp��Dp�3Dp� Dp�fDp��Dp�3Dq  DqfDq�Dq3Dq  Dq&fDq,�Dq33Dq@ DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @6fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�A�fgAA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C2�C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds�3Ds��Dtp Dy�HD��D�P�D�}pD���D� �D�A�D�vfD��D� RD�?
D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��=L��>�  �L�ͽL�ͽL�ͽL��=L�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL��>���L�ͽL��=L�ͽL�ͽL��=L��=L�ͽL��=L��>�  �L�ͽL��>��>�33�L�ͽL�ͽL�ͽL��=L��>���L��=L��>�33>���L�ͽL�ͽL�ͽL��=L��>���L�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL��=L��=L�ͽL�ͽL�ͽL�ͽL��>���L��=L�ͽL�ͽL�ͽL��>���L��=L�ͽL�ͽL�ͽL��=L�ͽL�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL��=L��>���L�ͽL�ͽL�ͽL�ͽL��=L�ͽL��=L�ͽL�ͽL��=L��>��>���L�ͽL��=L�ͽL�ͽL�ͽL��=L�ͽL�ͽL��=L�ͽL��=L�ͽL�ͽL��=L��>���L��=L�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL�ͽL�ͽL��=L�ͽL�ͽL�ͽL��=L��=L�ͽL��=L��>���L�ͽL�ͽL�ͽL�ͽL��=L��=L�ͽL��=L��=L�ͽL�ͽL�ͽL�ͽL��>���L�ͽL�ͽL��>��>��=L�ͽL��>��>�  >��>�  >�  �L�ͽL��=L�ͽL��=L�ͽL�ͽL��>���L�ͽL�ͽL�ͽL�ͽL�ͽL��=L�ͽL��=L�ͽL��>���L�ͽL�ͽL��=L��=L�ͽL��=L�ͽL��=L��>��>��>���L�ͽL��>��=L��=L��>���L��>��=L�ͽL��=L��=L��>��=L�ͽL�ͽL��>��=L��=L��=L��=L��>��>�33>��=L��=L��>��>��=L��>��>��>�  >��>��>��>��>�  >��=L��>�  >�33>��>��>��>�  >��>��>�  >�33>�  >��>��>�  >�  =L��>�  =L��>��>�  >�  >��>�33>�  >��>�  =L��>�  >�  >��>�33>�33>�  >�  >�  >��>��>��>�33>�  >��>�  >�  >�  >�  >��>�  >�  >�  >�33>�  >�  >��>��>�  >�  >�  >�  >�33>�33>�  =L��>�  >�33>�33>��>�33>�  >��>��>��>�  >�33>�  >�33>�  >��>�33>�  >�  >�33>��>�  >�  >�  >�33>�  >��>��>�  >�  >�  >�  >�  >��=L��>�  >��>�  >�  >��>�  >�  >��>��>��=L��>��>��>�  >��>��>��>�  >�  >�  >��>�  >�  >�  >��>�  >�  >��>��>�  >�  >�  >�  >��=L��>�  >��>��>��>�  >��>�  >��>�  >��>�  >�33>�  =L��>�  >��>�  >�  >��>�  >��>�  >��>��>�  >�  >��>��>�  >�  >�33>�  >�  >�  >�  >�  >��>��>��>��>��>�  >��>�  >�  >�  >�  >�  >�  >�33>�33>��>��>��>�  >�  >�  >��>��>��>��>�  >�  >�  >��>�  >�  >��>�33>�33>�  >�  >��>�  >�  >�  >�  >��>�33>�  >�33>�  >��>��>��>��>��>�  >��>�  >�  >�  >�  >�  >�33>�33>�  >��>�33>�  >�  >��>�  >��>�  >��>��>�  >��>�ff>�  >��>��>�  =L��>��>�  >��>�  >�ff>�ff>�ff>�33?&ff?��?��?&ff?@  ?Y��?Y��?s33?s33?�fg?�34?�34?�  ?�  ?�  ?���?���?�fg?�34?�34?���?���?���?���?���@	��@	��@  @  @  @fg@��@#33@#33@)��@)��@6fg@6fg@<��@I��@P  @Vfg@\��@c33@i��@vfg@vfg@���@���@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�ff@���@���@�  @�33@���@���@�  @�33@�ff@љ�@���@�  @�ff@�ff@ᙙ@�  @�33@�ff@�@���@�  @�ffA ��AffA  A��A33A
ffA
ffA��A33A��AffA  A��A33AffA  A��A33A ��A"ffA$  A'33A(��A*ffA,  A/33A0��A2ffA4  A5��A8��A:ffA<  A?33A@��ABffAE��AG33AH��AL  AM��AO33ARffAT  AW33AX��AZffA]��A_33A`��Ad  Ae��Ah��AjffAl  Ao33Ap��At  Au��Ax��AzffA|  A33A�fgA�34A���A���A�34A�  A���A�fgA�  A���A�fgA�34A���A���A�fgA�  A���A�fgA�34A���A���A�fgA�  A���A�fgA�  A���A���A�34A�  A���A�fgA�34A���A���A�fgA�  A���A���A�fgA�  A���A���A�fgA�  A���A���A�34A�  A���A���A�fgA�  A���A���A�fgA�  A���A���A�fgA�  A���AÙ�A�fgA�  A���AǙ�A�fgA�  A���A˙�A�fgA�34A���Aϙ�A�fgA�34A���Aә�A�fgA�34A���Aי�A�fgA�  A���Aۙ�A�fgA�34Dp�3DpɚDp� Dp��Dp�3Dp�Dp� Dp��Dq3Dq	�Dq Dq�Dq#3Dq)�Dq0 Dq<�DqC3DqI�DqP Dq\�Dqc3Dqi�DqvgDq|�Dq�3Dq��Dq�gDq��Dq�3Dq��Dq�gDq��Dq�3DqɚDq�gDq��Dq�3Dq�Dq�gDq��Dr3Dr DrgDr�Dr#3Dr0 Dr6gDr<�DrC3DrP DrVgDr\�Drc3Drp DrvgDr|�Dr�3Dr� Dr�gDr��Dr��Dr� Dr�gDr��DrɚDr� Dr�gDr�3Dr�Dr� Dr��Ds3Ds	�Ds Ds�Ds#3Ds)�Ds6gDs<�DsC3DsP DsVgDs\�Dsi�Dsp DsvgDs�3Ds��Ds� Ds��Ds�3Ds��Ds�gDs��Ds�3Ds� Ds�gDs��Ds�3Ds� Ds�gDs��Dt	�Dt DtgDt#3Dt)�Dt0 Dt<�DtC3DtI�DtVgDt\�Dti�Dtp DtvgDt|�Dt��Dt� Dt��@6fg@6fg@<��@I��@P  @Vfg@\��@c33@i��@vfg@vfg@���@���@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�ff@���@���@�  @�33@���@���@�  @�33@�ff@љ�@���@�  @�ff@�ff@ᙙ@�  @�33@�ff@�@���@�  @�ffA ��AffA  A��A33A
ffA
ffA��A33A��AffA  A��A33AffA  A��A33A ��A"ffA$  A'33A(��A*ffA,  A/33A0��A2ffA4  A5��A8��A:ffA<  A?33A@��ABffAE��AG33AH��AL  AM��AO33ARffAT  AW33AX��AZffA]��A_33A`��Ad  Ae��Ah��AjffAl  Ao33Ap��At  Au��Ax��AzffA|  A33A�fgA�34A���A���A�34A�  A���A�fgA�  A���A�fgA�34A���A���A�fgA�  A���A�fgA�34A���A���A�fgA�  A���A�fgA�  A���A���A�34A�  A���A�fgA�34A���A���A�fgA�  A���A���A�fgA�  A���A���A�fgA�  A���A���A�34A�  A���A���A�fgA�  A���A���A�fgA�  A���A���A�fgA�  A���AÙ�A�fgA�  A���AǙ�A�fgA�  A���A˙�A�fgA�34A���Aϙ�A�fgA�34A���Aә�A�fgA�34A���Aי�A�fgA�  A���Aۙ�A�fgA�34Dp�3DpɚDp� Dp��Dp�3Dp�Dp� Dp��Dq3Dq	�Dq Dq�Dq#3Dq)�Dq0 Dq<�DqC3DqI�DqP Dq\�Dqc3Dqi�DqvgDq|�Dq�3Dq��Dq�gDq��Dq�3Dq��Dq�gDq��Dq�3DqɚDq�gDq��Dq�3Dq�Dq�gDq��Dr3Dr DrgDr�Dr#3Dr0 Dr6gDr<�DrC3DrP DrVgDr\�Drc3Drp DrvgDr|�Dr�3Dr� Dr�gDr��Dr��Dr� Dr�gDr��DrɚDr� Dr�gDr�3Dr�Dr� Dr��Ds3Ds	�Ds Ds�Ds#3Ds)�Ds6gDs<�DsC3DsP DsVgDs\�Dsi�Dsp DsvgDs�3Ds��Ds� Ds��Ds�3Ds��Ds�gDs��Ds�3Ds� Ds�gDs��Ds�3Ds� Ds�gDs��Dt	�Dt DtgDt#3Dt)�Dt0 Dt<�DtC3DtI�DtVgDt\�Dti�Dtp DtvgDt|�Dt��Dt� Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�K�A�M�A�S�A�E�A�;dA�=qA�5?A��A���A���A�t�A�;dA��A��+A�;dA�-A� �A���A�^5A��A��HA���A��7A�VA�9XA�1'A�+A��A�A�A�  A�  A���A��#A��A�dZA�v�A��A��A��A�C�A�G�A�S�A�^5A�"�A���A�x�A�C�A�
=A��HA�/A�;dA�l�A��-A��A��uA���A���A��/A���A�$�A���A�oA�A�A��7A��A���A�oA�hsA�-A�ZA��A�M�A��RA�`BA�M�A��-A�(�A��A�I�A���A��;A��#A��^A���A��+A��7A��A�C�A��A�M�A��A�+A���A�7LA��\A���A�t�A�\)A�E�A�=qA�A��\A��A}ƨAz�\AwdZArZAm/Ai�Af�Ae��Ab�A`�\A`z�A^I�AZ�HAW�AU�mAU?}AT��AT�!AT-AS�7ARZAO��AK�AI�7AGƨAE�AC�TAAVA@$�A>5?A=%A;�wA;K�A:�A9��A9�-A9?}A8�`A7�A6�A6��A7A7�A6��A6�!A6��A5C�A2�A1��A1%A0��A.��A+dZA*��A)S�A(�A&5?A$��A#�mA"�`A"��A"=qA"�A"1A!�A!�wA!x�A!33A �+A�A�Az�A�A/A�A��An�A�AffA%A��AbA�AXA�-A�uA(�A%A�RAr�A$�A��A��AA�AhsA�A�A��A%A	�#A�DA�/A�Ap�A�`A~�A-A�7AXA�A^5A�;A bN@��\@�x�@���@���@�ȴ@�M�@�@��@�`B@���@�M�@�j@�~�@��#@�X@�Ĝ@�j@���@��@�%@���@�Q�@旍@��@�K�@��@�J@�j@߾w@�=q@�bN@�"�@��@֏\@�Ĝ@ӝ�@Ѻ^@�ƨ@ϕ�@�t�@θR@͡�@�;d@�E�@��m@ǅ@���@š�@�%@��/@��/@�1'@���@þw@���@Õ�@�C�@+@�~�@�=q@�X@�j@��
@��@�@�=q@���@��^@��7@��@���@��#@��@���@��#@���@�@��-@���@��7@�p�@�`B@�G�@��@���@��`@���@��j@�Z@�S�@��!@�5?@���@�  @�ƨ@���@�C�@�ȴ@��@��/@��@��u@�Q�@��F@�K�@��R@�ff@��T@�`B@�&�@�Ĝ@��u@��D@�z�@���@���@��-@�Z@���@�|�@�"�@���@�^5@�5?@�-@�@��T@���@���@�A�@���@��@�v�@���@��@�7L@�(�@�+@��+@�v�@�n�@�^5@�$�@��^@�x�@�`B@�?}@�%@��@��@��u@��@�Q�@�A�@�1'@�(�@� �@�1'@�1'@��@��;@��@��@���@���@�t�@��!@�E�@�@��h@��@��@�bN@�A�@��@���@�\)@�;d@�33@��@�o@�@��H@�E�@��@��@��@�$�@�-@�E�@�E�@�Z@�Q�@�1@�33@�^5@��T@�p�@��@��/@��@��D@���@��@�V@�7L@�&�@�?}@�7L@�O�@�G�@��@�%@���@�z�@��F@�ff@��T@��7@�`B@�/@���@���@���@�bN@�Q�@� �@��;@��@�dZ@�;d@�33@�ȴ@���@��@��u@�z�@�z�@�Q�@�A�@���@��F@��@���@�l�@�K�@���@�n�@�5?@���@��@��@�@��-@���@��h@�p�@�`B@���@��@�(�@�  @��m@���@�C�@�"�@�~�@��#@�?}@�&�@���@��b@{!-@s�@hی@ea�@_�	@U�@J�@F��@=}�@74�@0�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�oA�E�A�=qA�1'A�bNA���A��hA��!A���A�VA�A�A��PA��A��A���A�jA��HA�;dA�hsA���A��TA�1'A���A��A���A�/A�oA�oA�1A��7A�^5A�=qA�-A�JA��jA��wA��A�VA���A��^A�=qA�=qA�n�A��A���A��+A���A��hA���A�?}A�M�A��FA�|�A�^5A��7A��A���A��A�ȴA�~�A�1'A��;A��
A���A�9XA���A�-A��HA�C�A�M�A�A��-A��A��A���A�ĜA�`BA�jA�9XA�l�A���A�;dA���A�-A���A��#A���A�|�A�1A�E�A�bNA� �A���A�O�A�?}A�VA� �A�O�A�;dA��#A�M�A��9A���A���A���A�1'A�bA�"�A���A��A��A�
=A�1'A��\A��RA��A�
=A��A�9XA��hA���A��uA�S�A�-A�t�A��jA��mA� �A�VA��yA�G�A�x�A�$�A��A���A��A�\)A�ĜA��RA�ȴA�bA�-A��;A��jA�x�A���A��A��`A�XA���A��
A��A���A��A�
=A� �A� �A�-A�+A��A�$�A��A�JA���A���A�?}A�oA�A��A���A�
=A�?}A���A���A�JA��/A�7LA��`A��jA��#A��;A�1A��
A�7LA���A���A�p�A�\)A��hA�+A�-A�"�A��A�33A�(�A�$�A�A�(�A� �A��#A�oA���A��A���A�$�A�"�A�ȴA�t�A�1A�~�A��A�(�A�$�A� �A�(�A�(�A��mA���A��A�$�A��A��A��A�$�A��A��A��A� �A�9XA��A��A��A�$�A� �A��A��A� �A��A�"�A�$�A�"�A�"�A�&�A�+A��A�$�A��A��A��A�1'A��A��A��A��A�"�A�oA��A��A��A�"�A�"�A�"�A�+A�&�A��A�(�A�(�A�(�A�+A�(�A�-A�33A�+A�&�A�/A�1'A�&�A�(�A�+A�+A�-A�+A�+A�(�A�&�A�&�A�+A�+A�+A�+A�+A�(�A�$�A�{A�+A�+A�+A�&�A�-A�/A�+A�"�A�-A�5?A�/A�9XA�33A�5?A�-A�5?A�5?A�-A�/A�5?A�7LA�33A�7LA�7LA�-A�+A�1'A�-A�-A�(�A�&�A�&�A�+A�/A�33A�/A�9XA�(�A�-A�-A�/A�+A�+A� �A�/A�(�A�&�A�$�A�&�A�$�A�&�A��A�$�A�-A�&�A��A��A�&�A�"�A��A��A��A��A��A�"�A��A��A�{A�
=A��A��A��A�"�A�"�A�"�A� �A��A��A� �A� �A�"�A� �A��A��A��A��A�oA��A��A��A�"�A�$�A��A�-A�+A��TA�-A�-A�-A�-A�1'A�/A�+A�+A�$�A�(�A�&�A�&�A�1'A�"�A�&�A�(�A�1'A�&�A�$�A�+A�33A�(�A�-A�(�A�&�A�"�A�"�A�(�A�-A�1'A�-A�-A�"�A� �A�+A�"�A��A��A��A��A��A�&�A�"�A�(�A�$�A�"�A�-A�/A�5?A�(�A�33A�7LA�7LA�7LA�1'A�7LA�5?A�1'A�1'A�/A�9XA�1'A�;dA�;dA�9XA�5?A�=qA�=qA�?}A�;dA�?}A�?}A�5?A�5?A�A�A�=qA�;dA�;dA�;dA�;dA�?}A�=qA�?}A�A�A�33A�33A�7LA�;dA�?}A�A�A�E�A�C�A�=qA�=qA�9XA�E�A�G�A�E�A�G�A�E�A�E�A�C�A�C�A�E�A�G�A�G�A�A�A�C�A�G�A�E�A�C�A�E�A�C�A�E�A�E�A�E�A�I�A�K�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�G�A�G�A�G�A�E�A�G�A�E�A�E�A�E�A�G�A�E�A�E�A�E�A�G�A�I�A�E�A�G�A�I�A�I�A�M�A�O�A�O�A�O�A�O�A�K�A�K�A�E�A�I�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�I�A�K�A�M�A�M�A�Q�A�M�A�M�A�Q�A�S�A�Q�A�Q�A�O�A�M�A�O�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�S�A�VA�Q�A�O�A�O�A�K�A�K�A�A�A�G�A�C�A�A�A�A�A�?}A�A�A�A�A�?}A�?}A�A�A�?}A�=qA�=qA�=qA�=qA�5?A�33A�9XA�5?A�5?A�;dA�33A�33A�9XA�7LA�5?A�5?A�7LA�5?A�7LA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�=qA�?}A�=qA�=qA�;dA�/A�9XA�7LA�7LA�9XA�7LA�+A�&�A� �A� �A�$�A�"�A��A�{A�A���A��TA��;A��mA���A���A���A���A���A���A���A�ȴA�ȴA�A��jA��jA��^A��^A��FA��-A��-A��-A��-A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��hA��\A��DA�~�A�t�A�p�A�p�A�r�A�l�A�p�A�jA�jA�dZA�dZA�ffA�\)A�ZA�VA�I�A�K�A�C�A�A�A�A�A�?}A�A�A�7LA�5?A�33A�1'A�&�A� �A��A��A��A��A��A��A�JA���A���@�ƨ@�ƨ@��F@��F@��F@��@���@���@���@��@�|�@�t�@�l�@�dZ@�dZ@�\)@�\)@�K�@�K�@�K�@�C�@�;d@�33@�33@�;d@�33@�33@�33@�33@�33@�+@�33@�33@�+@�+@�+@�+@�"�@�"�@�o@�o@�o@��@�
=@�
=@�
=@�
=@���@��@��H@�ȴ@���@�v�@�v�@�v�@�V@�M�@�V@�E�@�=q@�=q@�=q@�5?@�5?@�$�@�$�@��@�J@�@��@��T@��#@��#@���@���@��@���@�p�@�hs@�hs@�hs@�X@�O�@�/@�&�@�&�@�&�@�&�@�&�@�/@�&�@�&�@�&�@�&�@�&�@�&�@�&�@�&�@�&�@�&�@�&�@�&�@�&�@��@�V@��@�V@�V@��@��@�V@�V@��@�V@�V@��@�V@�V@�%@�%A�G�A�I�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�I�A�I�A�K�A�K�A�M�A�I�A�VA�S�A�Q�A�Q�A�Q�A�O�A�K�A�G�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�I�A�Q�A�Q�A�Q�A�Q�A�M�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�Q�A�S�A�S�A�VA�S�A�VA�VA�XA�XA�S�A�Q�A�O�A�K�A�C�A�G�A�E�A�E�A�E�A�E�A�C�A�A�A�C�A�C�A�A�A�A�A�A�A�?}A�?}A�=qA�=qA�=qA�7LA�9XA�;dA�;dA�9XA�;dA�;dA�9XA�;dA�;dA�9XA�9XA�9XA�=qA�;dA�=qA�;dA�=qA�=qA�?}A�A�A�A�A�C�A�A�A�A�A�A�A�=qA�?}A�7LA�9XA�;dA�;dA�;dA�=qA�5?A�1'A�$�A�"�A�$�A�"�A��A��A�oA�  A��mA��`A��`A��TA���A���A���A���A���A���A���A���A�ƨA�A���A��jA��wA��jA��RA��FA��FA��FA��FA��-A��A���A���A���A���A���A���A���A���A���A���A���A���A��uA��\A��\A��A�x�A�x�A�v�A�t�A�p�A�t�A�t�A�n�A�hsA�l�A�jA�ffA�ZA�\)A�O�A�I�A�G�A�E�A�C�A�A�A�C�A�=qA�=qA�9XA�7LA�-A�(�A�"�A� �A��A��A��A��A�bA�A�  @���@�ƨ@��w@��w@��F@��F@��@���@���@��P@��@�t�@�l�@�l�@�l�@�dZ@�\)@�S�@�S�@�K�@�K�@�C�@�;d@�;d@�;d@�;d@�;d@�33@�;d@�;d@�;d@�33@�33@�33@�33@�33@�+@�+@�"�@�"�@��@��@�o@�o@�
=@�
=@�
=@�@��@��y@���@���@�~�@�~�@�~�@�n�@�V@�V@�M�@�=q@�=q@�=q@�=q@�=q@�-@��@��@�{@�J@��@��@��T@��T@��^@���@��h@��h@��h@�hs@�p�@�hs@�X@�O�@�7L@�/@�&�@�7L@�/@�/@�/@�/@�/@�7L@�/@�/@�/@�&�@�&�@�&�@�/@�&�@�/@�&�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�V@�ƨG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111114111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�I�A�K�A�M�A�S�A�E�A�;dA�=qA�5?A��A���A���A�t�A�;dA��A��+A�;dA�-A� �A���A�^5A��A��HA���A��7A�VA�9XA�1'A�+A��A�A�A�  A�  A���A��#A��A�dZA�v�A��A��A��A�C�A�G�A�S�A�^5A�"�A���A�x�A�C�A�
=A��HA�/A�;dA�l�A��-A��A��uA���A���A��/A���A�$�A���A�oA�A�A��7A��A���A�oA�hsA�-A�ZA��A�M�A��RA�`BA�M�A��-A�(�A��A�I�A���A��;A��#A��^A���A��+A��7A��A�C�A��A�M�A��A�+A���A�7LA��\A���A�t�A�\)A�E�A�=qA�A��\A��A}ƨAz�\AwdZArZAm/Ai�Af�Ae��Ab�A`�\A`z�A^I�AZ�HAW�AU�mAU?}AT��AT�!AT-AS�7ARZAO��AK�AI�7AGƨAE�AC�TAAVA@$�A>5?A=%A;�wA;K�A:�A9��A9�-A9?}A8�`A7�A6�A6��A7A7�A6��A6�!A6��A5C�A2�A1��A1%A0��A.��A+dZA*��A)S�A(�A&5?A$��A#�mA"�`A"��A"=qA"�A"1A!�A!�wA!x�A!33A �+A�A�Az�A�A/A�A��An�A�AffA%A��AbA�AXA�-A�uA(�A%A�RAr�A$�A��A��AA�AhsA�A�A��A%A	�#A�DA�/A�Ap�A�`A~�A-A�7AXA�A^5A�;A bN@��\@�x�@���@���@�ȴ@�M�@�@��@�`B@���@�M�@�j@�~�@��#@�X@�Ĝ@�j@���@��@�%@���@�Q�@旍@��@�K�@��@�J@�j@߾w@�=q@�bN@�"�@��@֏\@�Ĝ@ӝ�@Ѻ^@�ƨ@ϕ�@�t�@θR@͡�@�;d@�E�@��m@ǅ@���@š�@�%@��/@��/@�1'@���@þw@���@Õ�@�C�@+@�~�@�=q@�X@�j@��
@��@�@�=q@���@��^@��7@��@���@��#@��@���@��#@���@�@��-@���@��7@�p�@�`B@�G�@��@���@��`@���@��j@�Z@�S�@��!@�5?@���@�  @�ƨ@���@�C�@�ȴ@��@��/@��@��u@�Q�@��F@�K�@��R@�ff@��T@�`B@�&�@�Ĝ@��u@��D@�z�@���@���@��-@�Z@���@�|�@�"�@���@�^5@�5?@�-@�@��T@���@���@�A�@���@��@�v�@���@��@�7L@�(�@�+@��+@�v�@�n�@�^5@�$�@��^@�x�@�`B@�?}@�%@��@��@��u@��@�Q�@�A�@�1'@�(�@� �@�1'@�1'@��@��;@��@��@���@���@�t�@��!@�E�@�@��h@��@��@�bN@�A�@��@���@�\)@�;d@�33@��@�o@�@��H@�E�@��@��@��@�$�@�-@�E�@�E�@�Z@�Q�@�1@�33@�^5@��T@�p�@��@��/@��@��D@���G�O�@�V@�7L@�&�@�?}@�7L@�O�@�G�@��@�%@���@�z�@��F@�ff@��T@��7@�`B@�/@���@���@���@�bN@�Q�@� �@��;@��@�dZ@�;d@�33@�ȴ@���@��@��u@�z�@�z�@�Q�@�A�@���@��F@��@���@�l�@�K�@���@�n�@�5?@���@��@��@�@��-@���@��h@�p�@�`B@���@��@�(�@�  @��m@���@�C�@�"�@�~�@��#@�?}@�&�G�O�@��b@{!-@s�@hی@ea�@_�	@U�@J�@F��@=}�@74�@0�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�oA�E�A�=qA�1'A�bNA���A��hA��!A���A�VA�A�A��PA��A��A���A�jA��HA�;dA�hsA���A��TA�1'A���A��A���A�/A�oA�oA�1A��7A�^5A�=qA�-A�JA��jA��wA��A�VA���A��^A�=qA�=qA�n�A��A���A��+A���A��hA���A�?}A�M�A��FA�|�A�^5A��7A��A���A��A�ȴA�~�A�1'A��;A��
A���A�9XA���A�-A��HA�C�A�M�A�A��-A��A��A���A�ĜA�`BA�jA�9XA�l�A���A�;dA���A�-A���A��#A���A�|�A�1A�E�A�bNA� �A���A�O�A�?}A�VA� �A�O�A�;dA��#A�M�A��9A���A���A���A�1'A�bA�"�A���A��A��A�
=A�1'A��\A��RA��A�
=A��A�9XA��hA���A��uA�S�A�-A�t�A��jA��mA� �A�VA��yA�G�A�x�A�$�A��A���A��A�\)A�ĜA��RA�ȴA�bA�-A��;A��jA�x�A���A��A��`A�XA���A��
A��A���A��A�
=A� �A� �A�-A�+A��A�$�A��A�JA���A���A�?}A�oA�A��A���A�
=A�?}A���A���A�JA��/A�7LA��`A��jA��#A��;A�1A��
A�7LA���A���A�p�A�\)A��hA�+A�-A�"�A��A�33A�(�A�$�A�A�(�A� �A��#A�oA���A��A���A�$�A�"�A�ȴA�t�A�1A�~�A��A�(�A�$�A� �A�(�A�(�A��mA���A��A�$�A��A��A��A�$�A��A��A��A� �A�9XA��A��A��A�$�A� �A��A��A� �A��A�"�A�$�A�"�A�"�A�&�A�+A��A�$�A��A��A��A�1'A��A��A��A��A�"�A�oA��A��A��A�"�A�"�A�"�A�+A�&�A��A�(�A�(�A�(�A�+A�(�A�-A�33A�+A�&�A�/A�1'A�&�A�(�A�+A�+A�-A�+A�+A�(�A�&�A�&�A�+A�+A�+A�+A�+A�(�A�$�A�{A�+A�+A�+A�&�A�-A�/A�+A�"�A�-A�5?A�/A�9XA�33A�5?A�-A�5?A�5?A�-A�/A�5?A�7LA�33A�7LA�7LA�-A�+A�1'A�-A�-A�(�A�&�A�&�A�+A�/A�33A�/A�9XA�(�A�-A�-A�/A�+A�+A� �A�/A�(�A�&�A�$�A�&�A�$�A�&�A��A�$�A�-A�&�A��A��A�&�A�"�A��A��A��A��A��A�"�A��A��A�{A�
=A��A��A��A�"�A�"�A�"�A� �A��A��A� �A� �A�"�A� �A��A��A��A��A�oA��A��A��A�"�A�$�A��A�-A�+A��TA�-A�-A�-A�-A�1'A�/A�+A�+A�$�A�(�A�&�A�&�A�1'A�"�A�&�A�(�A�1'A�&�A�$�A�+A�33A�(�A�-A�(�A�&�A�"�A�"�A�(�A�-A�1'A�-A�-A�"�A� �A�+A�"�A��A��A��A��A��A�&�A�"�A�(�A�$�A�"�A�-A�/A�5?A�(�A�33A�7LA�7LA�7LA�1'A�7LA�5?A�1'A�1'A�/A�9XA�1'A�;dA�;dA�9XA�5?A�=qA�=qA�?}A�;dA�?}A�?}A�5?A�5?A�A�A�=qA�;dA�;dA�;dA�;dA�?}A�=qA�?}A�A�A�33A�33A�7LA�;dA�?}A�A�A�E�A�C�A�=qA�=qA�9XA�E�A�G�A�E�A�G�A�E�A�E�A�C�A�C�A�E�A�G�A�G�A�A�A�C�A�G�A�E�A�C�A�E�A�C�A�E�A�E�A�E�A�I�A�K�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�I�A�I�A�K�A�K�A�M�A�I�A�VA�S�A�Q�A�Q�A�Q�A�O�A�K�A�G�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�I�A�Q�A�Q�A�Q�A�Q�A�M�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�Q�A�S�A�S�A�VA�S�A�VA�VA�XA�XA�S�A�Q�A�O�A�K�A�C�A�G�A�E�A�E�A�E�A�E�A�C�A�A�A�C�A�C�A�A�A�A�A�A�A�?}A�?}A�=qA�=qA�=qA�7LA�9XA�;dA�;dA�9XA�;dA�;dA�9XA�;dA�;dA�9XA�9XA�9XA�=qA�;dA�=qA�;dA�=qA�=qA�?}A�A�A�A�A�C�A�A�A�A�A�A�A�=qA�?}A�7LA�9XA�;dA�;dA�;dA�=qA�5?A�1'A�$�A�"�A�$�A�"�A��A��A�oA�  A��mA��`A��`A��TA���A���A���A���A���A���A���A���A�ƨA�A���A��jA��wA��jA��RA��FA��FA��FA��FA��-A��A���A���A���A���A���A���A���A���A���A���A���A���A��uA��\A��\A��A�x�A�x�A�v�A�t�A�p�A�t�A�t�A�n�A�hsA�l�A�jA�ffA�ZA�\)A�O�A�I�A�G�A�E�A�C�A�A�A�C�A�=qA�=qA�9XA�7LA�-A�(�A�"�A� �A��A��A��A��A�bA�A�  @���@�ƨ@��w@��w@��F@��F@��@���@���@��P@��@�t�@�l�@�l�@�l�@�dZ@�\)@�S�@�S�@�K�@�K�@�C�@�;d@�;d@�;d@�;d@�;d@�33@�;d@�;d@�;d@�33@�33@�33@�33@�33@�+@�+@�"�@�"�@��@��@�o@�o@�
=@�
=@�
=@�@��@��y@���@���@�~�@�~�@�~�@�n�@�V@�V@�M�@�=q@�=q@�=q@�=q@�=q@�-@��@��@�{@�J@��@��@��T@��T@��^@���@��h@��h@��h@�hs@�p�@�hs@�X@�O�@�7L@�/@�&�@�7L@�/@�/@�/@�/@�/@�7L@�/@�/@�/@�&�@�&�@�&�@�/@�&�@�/@�&�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�V@�ƨA�G�A�I�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�I�A�I�A�K�A�K�A�M�A�I�A�VA�S�A�Q�A�Q�A�Q�A�O�A�K�A�G�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�I�A�Q�A�Q�A�Q�A�Q�A�M�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�Q�A�S�A�S�A�VA�S�A�VA�VA�XA�XA�S�A�Q�A�O�A�K�A�C�A�G�A�E�A�E�A�E�A�E�A�C�A�A�A�C�A�C�A�A�A�A�A�A�A�?}A�?}A�=qA�=qA�=qA�7LA�9XA�;dA�;dA�9XA�;dA�;dA�9XA�;dA�;dA�9XA�9XA�9XA�=qA�;dA�=qA�;dA�=qA�=qA�?}A�A�A�A�A�C�A�A�A�A�A�A�A�=qA�?}A�7LA�9XA�;dA�;dA�;dA�=qA�5?A�1'A�$�A�"�A�$�A�"�A��A��A�oA�  A��mA��`A��`A��TA���A���A���A���A���A���A���A���A�ƨA�A���A��jA��wA��jA��RA��FA��FA��FA��FA��-A��A���A���A���A���A���A���A���A���A���A���A���A���A��uA��\A��\A��A�x�A�x�A�v�A�t�A�p�A�t�A�t�A�n�A�hsA�l�A�jA�ffA�ZA�\)A�O�A�I�A�G�A�E�A�C�A�A�A�C�A�=qA�=qA�9XA�7LA�-A�(�A�"�A� �A��A��A��A��A�bA�A�  @���@�ƨ@��w@��w@��F@��F@��@���@���@��P@��@�t�@�l�@�l�@�l�@�dZ@�\)@�S�@�S�@�K�@�K�@�C�@�;d@�;d@�;d@�;d@�;d@�33@�;d@�;d@�;d@�33@�33@�33@�33@�33@�+@�+@�"�@�"�@��@��@�o@�o@�
=@�
=@�
=@�@��@��y@���@���@�~�@�~�@�~�@�n�@�V@�V@�M�@�=q@�=q@�=q@�=q@�=q@�-@��@��@�{@�J@��@��@��T@��T@��^@���@��h@��h@��h@�hs@�p�@�hs@�X@�O�@�7L@�/@�&�@�7L@�/@�/@�/@�/@�/@�7L@�/@�/@�/@�&�@�&�@�&�@�/@�&�@�/@�&�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�V@�ƨG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111114111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>at?@�/�@�!�=��*?���>p�?�4�@�4Y>-3?��7>!�@#��?�^_=�	�?ᓴ@��&=�U�>k@�6�=�f�?�~@�6�>n;�>[7�@�7L@�5�=�?�>�T�@�2�@�9m=�b�=�+>٩>��<@�,�@�7�>]@�6�@�8�>խ�=�_�?�̸=�
?9��@�:�@�-�=}6�=�V�=ʛ�=�#d>M��=��=�ƨ=�ҳ@	��=���>j@@�7�>wֶ=���=�f'>��@O�0@O��>(C�?A�a=��]=ֱ>l~(@�;�>3�*@�7�=��6=��>Gd�@X�_=��>0�@w�d=ï�> ��@"*�> ѷ>k�[@�<�@�~g>��>:�=���=�I�?5>�r2>�@��e=�M�>
�@��b@�+,?�S�=���=���=�G=�h>�	@���?�{�=�'R>~��?��>S��?�|=��>{�@�;d@u��>ZK>�N�=���=��@=��>U'�@78=��>��?� �=�8�@��7>f[�=�,=��>��@�6&@Un�>#��@�;%@�B>"�=��&@�6z=ښ�>J&>�� @�33>D(N>k�q@�D�=��>.��=�>m@�>�=��>}h�@�
(@�=@�=�>�Mj?=�9@�B�@�Bp@�A_@�?�?��9>
w@�=@�8�@���?	��=��@{�@�A�>J�@3�k>�l?��Z=���><�@H��>o� >�z?Z5�@�F�=���>�:?��@�D�=�*Z>i��?j��>��@�F�@�E�@�D�?�r�>z�@���@�I�>YJ�@�G�@�F�>�9m@�E9>�f'>��@�F�@�E�@�E�?0'�?�@�D�@�Hk?oJw@�J�@�H�@�I�@�I{@�I�@�I{@�D�@�G�@�H�@�HV@�HV@�H@�H@�H@�I(@�I�@�Jw@�HV@�I(@�G�@�H�@�I@�J�@�GZ@�GZ@�H�@�I{@�I�@�Jw@�J8@�J�@�H�@�H�@�I{@�I{@�GZ@�D�@�F�@G4�@�I(@�HV@�H�@�G�@�KI@�HV@�G�@�I(@�J#@�L�@�KI@�LY@�K�@�K4@�J8@�K�@�L�@�L�@�L�@�M�@�N�@�O"@�N@�Oa@�P�@�RT@�M@�L�@�L�@�Mj@�MU@�MU@�Mj@�KI@�K�@�KI@�M@�M�@�NQ@�M@�N@�If@�K�@�Mj@�Mj@�M�@�J�@�Mj@�N�@�O�@�O�@�Ov@�P�@�Q@�Q�@�P�@�Qn@�QD@�Ov@�P�@�Pr@�O�@�QD@�Q@�Qn@�P�@�QD@�Qn@�O"@�Ov@�O�@�Oa@�N@�M@�MU@�M�@�N@�N@�P@�Nf@�Q�@�P�@�N�@�Nf@�Mj@�L�@�K4@�K�@�L�@�K4@�J�@�J�@�K�@�If@�G�@�FJ@�I�@�I@�H�@�G�@�GZ@�Hk@�F�@�FJ@�E9@�E9@�G@�H�@�Hk@�I(@�H�@�FJ@�C�@�G@�If@�J#@�J8@�J�@�If@�I{@�J8@�Jw@�If@�I(@�H�@�I�@�*Z@�Hk@�Hk@�HV@�F�@�J8@�If@�I�@�I�@�I{@�L�@�MU@�Mj@�M�@�N�@�P3@�N�@�NQ@�Oa@�P3@�Nf@�L�@�L0@�M@�LY@�M�@�Mj@�M�@�MU@�N�@�Q�@�Nf@�NQ@�M�@�N�@�K�@�L�@�L�@�KI@�J#@�J#@�L�@�L�@�K�@�NQ@�K�@�J#@�J�@�J8@�H�@�H�@�F�@�K�@�K�@�J�@�Nf@�N�@�M�@�K�@�O�@�Q�@�Q�@�S;@�R�@�S�@�T7@�S�@�T"@�S�@�S�@�Se@�Q�@�S�@�S�@�T�@�Tv@�V@�VX@�U�@�V.@�VX@�V�@�V�@�VX@�W @�W�@�U�@�T�@�U�@�U�@�U@�VX@�V�@�V�@�VX@�VX@�V�@�V.@�T�@�S�@�U�@�U�@�Wi@�X@�XO@�V�@�W @�X�@�Y`@�Xd@�Z�@�Zq@�Z�@�Y!@�Y!@�Y`@�Y`@�Z2@�Y!@�W�@�V�@�V�@�V�@�V�@�W @�VX@�U�@�U�@�T�@�UG@�U�@�VC@�V@�V@�Tv@�T"@�T�@�Tv@�VC@�VX@�V�@�V�@�V�@�V�@�U�@�U�@�U�@�U�@�U�@�VX@�VC@�VX@�V�@�V�@�W�@�WT@�W�@�V�@�Y@�X%@�WT@�_�@�`�@�`�@�_�@�_p@�\�@�Z�@�[�@�_@�_�@�dZ@�b�@�b�@�f'@�f'@�g8@�k@�l7@�l�@�m3@�oT@�o�@�o?@�r�@�s@�o�@�o�@�o�@�p@�o�@�sm@�v@�},@�},@�{�@�|@�|�@�~�@�}A@��@��
@��o@���@��{@���@���@���@���@���@���@��i@���@��z@��"@���@���@���@��.@���@��@��V@���@���@���@��$@���@��5@���@���@��@��c@��
@��x@���@���@���@��A@���@��V@���@���@���@���@���@��@���@��c@���@��@���@��V@���@���@���@��b@���@���@��c@���@���@��@��k@��J@��x@���@��"@���@��i@���@���@��+@��+@���@���@���@��@���@��@���@���@��0@���@��4@�$@�$@�~�@�~g@�~(@�|F@�{�@�y}@�xl@�x-@�xl@�xl@�w�@�u�@�t*@�t @�s�@�sX@�s.@�q�@�o�@�n�@�i�@�gw@�g#@�f�@�e�@�e@�c�@�c�@�b�@�a(@�`@�^�@�]:@�[B@�Z2@�V�@�T"@�S@�Q�@�Q�@�P�@�O�@�N�@�LD@�K4@�J�@�F�@�E9@�C�@�Bp@�B@�?�@�>�@�=�@�<u@�6�@�4@�2#@Sk�@Sk@Sjj@Sj@@Si�@ShI@Sg#@Sf{@Se�@Sc�@Sb9@Sa�@S`�@S`�@S`�@S_p@S^t@S]�@S]%@S\}@S\)@S[�@S[�@S[�@S[-@SZ�@SZ�@SZ�@SZ�@SZ�@SZ�@SY�@SY�@SY�@SY6@SXd@SW�@SW?@SV@SUq@STv@STv@SS�@SR~@SR @SQ�@SP3@SN<@SK�@SI(@SD�@S>�@S>W@S=\@S<@S9@S8q@S7�@S7"@S5T@S5@S4�@S4Y@S28@S0@@S/�@S.�@S-#@S+V@S(c@S'@S&@S#O@Sd@S?@Sm@S�@S/@S/@SY@Sb@Sj@Ss@SM@S�@S#@S#@S�@S�@Sw@Sw@S�@S�@S�@S�@S�@SM@S#@Sw@S�@S�@S�@S�@S�@S(@S(@S�@S�@S|@S�@S�@S(@S(@S(@S|@S(@S�@S�@S(@S�@���@�ݘ@�܇@��D@��@��H@���@��Y@���@��+@��@�݃@��3@��@��*@�߹@��@���@��@��@��@���@�� @���@���@��P@��-@��@���@��@��6@��@��Z@��@��q@��#@���@��@���@���@��@���@��/@��R@��@��s@��t@���@�@�K@��@��@��@��m@���@�l@��@�@��@��@��@�@�V@��@��@��@��@�4@�4@��@�@�j@�@�{@�<@�b@�/@�:@�:@��@�\@��@��@�@��@�!W@�!@�"�@� �@��@� �@��@� @� @� @� @� �@�!�@�!B@�#:@�~@�K@�d@�!l@�u@�$�@�%�@�%p@�'�@�($@�)_@�(�@�%�@�$5@�#�@�#�@�#�@�!�@�!B@��@�@�@��@�Z@�j@�@��@�0@��@�@�@��@��@�
�@�	�@�[@��@��@�!@��@�h@�h@�>@��@��@��@�P@�  @���@���@���@���@��v@��P@��P@���@���@���@���@��M@��#@��@��x@��@��x@��R@���@��@���@��@��)@��%@��@��@��@��@��U@���@�ں@��@���@���@�٩@�զ@���@��@��V@���@��@��:@���@���@��@��K@�ƽ@���@���@���@Q��@Q��@Q��@Q�A@Q�A@Q�k@Q��@Q��@Q��@Q��@Q��@Q�d@Q�@Q�@Q�C@Q�@Q��@Q��@Q��@Q�~@Q� @Q�]@Q�]@Q�3@Q��@Q�3@Q��@Q��@Q��@Q�3@Q��@Q�	@Q��@Q�a@Q�a@Q�a@Q��@Q�@Q��@Q�D@Q�D@Q��@Q�I@Q��@Q��@Q��@Q��@Q�@Q�^@Q~=@Q{�@Qu�@Qn�@Qn�@Qn�@Ql�@Qh�@Qh�@Qg�@Qe�@Qe�@Qe�@Qe,@Qe@Qc5@Qa�@Q`�@Q_@Q^�@Q[W@QY�@QX�@QW�@QU@QM�@QL@QN�@QM�@QF@QG�@QGZ@QD�@QBF@Q?�@Q?S@Q>W@Q@y@Q>�@Q>�@Q>�@Q>�@Q?)@Q?}@Q?S@Q?�@Q?�@Q?)@Q>�@Q>�@Q?�@Q?�@Q@O@Q?�@Q>W@Q>-@Q=�@Q=�@Q=�@Q=�@Q>@Q>@Q>-@Q=�@Q>@Q=�@Q>@Q=�@Q=�@Q=�@Q=�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         43344443444444434434434433443344443343344444334444444444434444334444434344434434444433444444434433444444344444444334444444444434444334334434443443444434433344333344333444344444434443444344443334433433434433344334333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�/�@�!�G�O�G�O�G�O�G�O�@�4WG�O�G�O�G�O�G�O�G�O�G�O�G�O�@��&G�O�G�O�@�6�G�O�G�O�@�6�G�O�G�O�@�7L@�5�G�O�G�O�@�2�@�9nG�O�G�O�G�O�G�O�@�,�@�7�G�O�@�6�@�8�G�O�G�O�G�O�G�O�G�O�@�:�@�-�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�7�G�O�G�O�G�O�G�O�@O�2@O��G�O�G�O�G�O�G�O�G�O�@�;�G�O�@�7�G�O�G�O�G�O�@X�]G�O�G�O�@w�^G�O�G�O�G�O�G�O�G�O�@�<�@�~jG�O�G�O�G�O�G�O�G�O�G�O�G�O�@��dG�O�G�O�@��b@�+-G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�;h@u��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��9G�O�G�O�G�O�G�O�@�6'@Un�G�O�@�;%@�BG�O�G�O�@�6zG�O�G�O�G�O�@�37G�O�G�O�@�D�G�O�G�O�G�O�G�O�@�>�G�O�G�O�@�
(@�=@�=�G�O�G�O�@�B�@�Br@�Ab@�?�G�O�G�O�@�=@�8�@���G�O�G�O�G�O�@�A�G�O�G�O�G�O�G�O�G�O�G�O�@H��G�O�G�O�G�O�@�F�G�O�G�O�G�O�@�D�G�O�G�O�G�O�G�O�@�F�@�E�@�D�G�O�G�O�@���@�I�G�O�@�G�@�F�G�O�@�E;G�O�G�O�@�F�@�E�@�E�G�O�G�O�@�D�@�HjG�O�@�J�@�H�@�I�@�I~@�I�@�I~@�D�@�G�@�H�@�H\@�HT@�H@�H@�H@�I,@�I�@�Jz@�HV@�I)@�G�@�H�@�I@�J�@�G[@�GZ@�H�@�I{@�I�@�J|@�J8@�J�@�H�@�H�@�I{@�Ix@�G]@�D�@�F�G�O�@�I*@�HT@�H�@�G�@�KI@�HT@�G�@�I)@�J$@�L�@�KI@�L[@�K�@�K6@�J:@�K�@�L�@�L�@�L�@�M�@�N�@�O$@�N@�Oc@�P�@�RV@�L�@�L�@�L�@�Mj@�MV@�MQ@�Mj@�KI@�K�@�KJ@�M@�M�@�NN@�M@�N@�Il@�K�@�Mj@�Mj@�M�@�J�@�Mf@�N�@�O�@�O�@�Ot@�P�@�Q@�Q�@�P�@�Qp@�QD@�Os@�P�@�Pn@�O�@�QE@�Q@�Qp@�P�@�QE@�Qn@�O$@�Ow@�O�@�O^@�N@�M@�MV@�N@�N@�N@�P$@�Ni@�Q�@�P�@�N�@�Ng@�Mm@�L�@�K1@�K�@�L�@�K1@�J�@�J�@�K�@�Ig@�G�@�FH@�I�@�I@�H�@�G�@�G[@�Hj@�F�@�FL@�E6@�E:@�G@�H�@�Hn@�I(@�H�@�FJ@�C�@�G
@�Ii@�J"@�J5@�J�@�Ih@�I�@�J:@�Jv@�Id@�I)@�H�@�I�@�*^@�Hm@�Hh@�HT@�F�@�J>@�Ie@�I�@�I�@�Iz@�L�@�MV@�Mj@�M�@�N�@�P6@�N�@�NN@�Oc@�P6@�Ng@�L�@�L,@�M@�LZ@�M�@�Ml@�M�@�MV@�N�@�Q�@�Ng@�NS@�N@�N�@�K�@�L�@�L�@�KJ@�J"@�J"@�L�@�L�@�K�@�NO@�K�@�J"@�J�@�J=@�H�@�H�@�F�@�K�@�K�@�J�@�Nb@�N�@�M�@�K�@�O�@�Q�@�Q�@�S=@�R�@�S�@�T5@�S�@�T$@�S�@�S�@�Se@�Q�@�S�@�S�@�T�@�Tv@�V@�V[@�U�@�V0@�VU@�V�@�V�@�V[@�W@�W�@�U�@�T�@�U�@�U�@�U @�V[@�V�@�V�@�VY@�VX@�V�@�V.@�T�@�S�@�U�@�U�@�Wj@�X@�XR@�V�@�V�@�X�@�Ye@�Xf@�Z�@�Zr@�Z�@�Y@�Y@�Y^@�Y^@�Z3@�Y#@�W�@�V�@�V�@�V�@�V�@�W@�VW@�U�@�U�@�T�@�UF@�U�@�VB@�V@�V@�Tx@�T"@�T�@�Tv@�V?@�VX@�V�@�V�@�V�@�V�@�U�@�U�@���@�ݕ@�܇@��D@��@��J@���@��Y@���@��-@��@�݂@��3@��@��*@�߹@��@���@��@��@��@���@��@���@���@��O@��+@��@��@��@��8@��@��b@��@��r@��(@���@���@���@���@�� @���@��2@��V@��@��o@��w@���@�	@�N@��@��@��@��m@���@�n@��@�@��@��@��@�@�W@��@��@��@��@�5@�2@��@��@�h@�@�z@�:@�a@�1@�:@�:@��@�a@��@��@�@��@�!W@�!	@�"�@� �@��@� �@��@� 	@� @� @� @� �@�!�@�!B@�#;@�{@�H@�d@�!m@�v@�$�@�%�@�%q@�'�@�($@�)]@�(�@�%�@�$6@�#�@�#�@�#�@�!�@�!B@��@�@�@��@�[@�m@�@��@�2@��@�
@�@��@��@�
�@�	�@�\@��@��@�!@��@�i@�i@�<@� @��@��@�S@�  @���@���@���@���@��x@��R@��N@���@���@���@���@��M@��&@��@��z@��@��v@��P@���@��@���@��@��-@��"@��@��@��@��@��X@���@�ں@��@���@���@�٪@�զ@���@��@��V@���@��@��8@���@���@��@��M@�ƾ@���@���@���@Q��@Q��@Q��@Q�B@Q�>@Q�j@Q��@Q��@Q��@Q��@Q��@Q�e@Q�@Q�@Q�E@Q�@Q��@Q��@Q��@Q�z@Q��@Q�]@Q�[@Q�5@Q��@Q�5@Q��@Q��@Q��@Q�3@Q��@Q�@Q��@Q�c@Q�c@Q�b@Q��@Q�@Q��@Q�E@Q�E@Q��@Q�F@Q��@Q��@Q��@Q��@Q�@Q�]@Q~=@Q{�@Qu�@Qn�@Qn�@Qn�@Ql�@Qh�@Qh�@Qg�@Qe�@Qe�@Qe}@Qe*@Qe@Qc:@Qa�@Q`�@Q_@Q^�@Q[X@QY�@QX�@QW�@QU@QM�@QL@QN�@QM�@QF
@QG�@QG[@QD�@QBJ@Q?�@Q?R@Q>V@Q@s@Q? @Q>�@Q>�@Q>�@Q?&@Q?~@Q?R@Q?�@Q?�@Q?+@Q>~@Q>�@Q?�@Q?�@Q@M@Q?�@Q>V@Q>3@Q=�@Q=�@Q=�@Q=�@Q>@Q>@Q>+@Q=�@Q> @Q=�@Q>@Q=�@Q=�@Q=�@Q=�@���@�ݕ@�܇@��D@��@��J@���@��Y@���@��-@��@�݂@��3@��@��*@�߹@��@���@��@��@��@���@��@���@���@��O@��+@��@��@��@��8@��@��b@��@��r@��(@���@���@���@���@�� @���@��2@��V@��@��o@��w@���@�	@�N@��@��@��@��m@���@�n@��@�@��@��@��@�@�W@��@��@��@��@�5@�2@��@��@�h@�@�z@�:@�a@�1@�:@�:@��@�a@��@��@�@��@�!W@�!	@�"�@� �@��@� �@��@� 	@� @� @� @� �@�!�@�!B@�#;@�{@�H@�d@�!m@�v@�$�@�%�@�%q@�'�@�($@�)]@�(�@�%�@�$6@�#�@�#�@�#�@�!�@�!B@��@�@�@��@�[@�m@�@��@�2@��@�
@�@��@��@�
�@�	�@�\@��@��@�!@��@�i@�i@�<@� @��@��@�S@�  @���@���@���@���@��x@��R@��N@���@���@���@���@��M@��&@��@��z@��@��v@��P@���@��@���@��@��-@��"@��@��@��@��@��X@���@�ں@��@���@���@�٪@�զ@���@��@��V@���@��@��8@���@���@��@��M@�ƾ@���@���@���@Q��@Q��@Q��@Q�B@Q�>@Q�j@Q��@Q��@Q��@Q��@Q��@Q�e@Q�@Q�@Q�E@Q�@Q��@Q��@Q��@Q�z@Q��@Q�]@Q�[@Q�5@Q��@Q�5@Q��@Q��@Q��@Q�3@Q��@Q�@Q��@Q�c@Q�c@Q�b@Q��@Q�@Q��@Q�E@Q�E@Q��@Q�F@Q��@Q��@Q��@Q��@Q�@Q�]@Q~=@Q{�@Qu�@Qn�@Qn�@Qn�@Ql�@Qh�@Qh�@Qg�@Qe�@Qe�@Qe}@Qe*@Qe@Qc:@Qa�@Q`�@Q_@Q^�@Q[X@QY�@QX�@QW�@QU@QM�@QL@QN�@QM�@QF
@QG�@QG[@QD�@QBJ@Q?�@Q?R@Q>V@Q@s@Q? @Q>�@Q>�@Q>�@Q?&@Q?~@Q?R@Q?�@Q?�@Q?+@Q>~@Q>�@Q?�@Q?�@Q@M@Q?�@Q>V@Q>3@Q=�@Q=�@Q=�@Q=�@Q>@Q>@Q>+@Q=�@Q> @Q=�@Q>@Q=�@Q=�@Q=�@Q=�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         43344443444444434434434433443344443343344444334444444444434444334444434344434434444433444444434433444444344444444334444444444434444334334434443443444434433344333344333444344444434443444344443334433433434433344334333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9"�9"��9"��9"�9"�>9"��9"�9"��9"�79"�b9"�I9"��9"�T9"��9"�?9"��9"��9"��9"�9"��9"�|9"��9"�9"� 9"��9"�]9"�9"� 9"��9"�o9"� 9"��9"�"9"�D9"�r9"��9"�B9"�9"�W9"��9"ʝ9"�/9"δ9"�
9"ɷ9"�9"�=9"̋9"�R9"֜9"�9"�(9"�9"��9"�9"خ9"��9"�H9"��9"��9"��9"�9"�X9"��9"�9"�9"��9"�$9"�!9"�9"�`9"�I9"��9"�T9"�9"�59"��9"��9"��9"�T9"�9"�9"�9"��9"�S9"��9"��9"�v9"�9"�`9"�(9"�X9"�9"�9"�9"�9"�.9"�n9"��9"��9"�9"��9"��9"��9"�	9"��9"��9"��9"�9"��9"��9"�99"�9"��9"�19"�19"�^9"� 9"��9"�G9"�9"��9"�9"�B9"�M9"�	9"��9"�:9"��9"�9"�#9"��9"��9"ޫ9"��9"܃9"۲9"��9"�P9"�9"٣9"٣9"�w9"�=9"�
9"��9"է9"�]9"�U9"��9"�*9"�9"��9"��9"��9"�S9"�*9"�r9"�!9"��9"̵9"�a9"�G9"��9"�C9"�$9"��9"��9"¶9"�p9"�"9"�9"��9"��9"��9"��9"��9"�49"�9"�a9"�%9"�9"��9"�9"�Y9"��9"��9"�f9"��9"��9"��9"��9"��9"�9"��9"��9"�~9"��8�a�8�_�8�^�8�^B8�^>8�^i8�\�8�[�8�Z�8�Y�8�X8�V�8�US8�T\8�T�8�T[8�S>8�Q�8�QS8�P�8�PZ8�N�8�N�8�N�8�N�8�N�8�N�8�NG8�ND8�N�8�N8�Np8�M�8�M�8�M�8�M�8�M8�L�8�L78�J�8�J�8�Ju8�I�8�I|8�H`8�H48�H48�F�8�E8�B�8�@d8�:�8�3�8�3�8�3�8�1�8�./8�.8�-8�+8�+#8�*�8�*~8�*[8�(�8�&�8�&8�$�8�$j8� �8�M8�28�e8��8��8�
8�r8��8�%8��8�m8�
�8�}8�8��8��8��8�H8�8�F8�8�m8��8��8��8�8�r8��8�E8��8�88��8�8��8��8�8��8�)8��8�P8�Q8�x8�$8�N8�8�S8�)8�8�8�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�
B�B�HB�BBuB�B�B+B0!B2-B33B33B33B6FB8RB8RB:^BE�Be`Bn�Bo�Bp�Bu�By�Bz�B|�B� B�B�B�B�B�B�%B�7B�7B�1B� B}�B|�B{�Bt�Bt�By�B{�Bz�B{�B|�B�B�7B��BĜB�jB�9B�B��B��B�=Bv�Bk�BbNBS�BO�BF�B49B+B$�B#�B�B��B�HB��BȴB�qB�9B�B�B��B��B��B�uB��B��B��B��B��B��B�B��B��B�\Bz�B\)B5?B�BB
��B
�B
ŢB
�!B
�B
l�B
O�B
+B
�B
B	�B	�
B	�3B	�B	r�B	aHB	S�B	1'B	#�B	"�B	�B	B��B�B�mB�ZB�BB�)B�
B��BĜB��B��B�^B�!B��B��B��B��B�{B�hB�hB�oB�oB�uB��B�uB�bB��B��B��B��B��B��B��B��B�PB�VB�bB�VB�JB�JB�=B�7B�1B�7B�1B�+B�+B�+B�%B�%B�%B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~�B}�B|�B{�By�Bv�Bw�Bw�Bx�Bw�Bu�Bs�Bp�Bm�Bk�BiyBhsBgmBe`BcTBaHB^5B^5B]/B]/B\)B\)B[#B[#BZBYBYBXB[#B[#B[#B[#B[#B\)B[#B[#B[#B[#BYBXBXBXBXBW
BW
BVBVBYBZB^5Be`Bm�BjBl�Bn�Bl�BhsBgmBbNB^5B_;B^5B`BBk�BjBn�B|�B}�B~�B�B�B�=B�DB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�FB�dB�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�;B�TB�`B�B�B�B�B�B�B��B��B��B��B��B	B	B	+B	1B		7B	
=B	DB	JB	DB	DB	
=B	JB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	"�B	$�B	&�B	%�B	&�B	,B	2-B	7LB	8RB	8RB	9XB	9XB	<jB	A�B	C�B	D�B	E�B	F�B	G�B	L�B	Q�B	T�B	VB	W
B	XB	ZB	\)B	\)B	_;B	bNB	dZB	e`B	e`B	e`B	ffB	l�B	o�B	q�B	t�B	w�B	y�B	y�B	y�B	{�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�7B	�=B	�DB	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B�uB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�XB	�dB	�jB	�jB	�jB	�qB	�qB	�qB	�qB	�qB	�jB	�jB	�dB	�dB	�^B	�^B	�^B	�^B	�XB	�dB	�qB	�wB	��B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�BB	�TB	�`B	�`B	�mB	�B	�B	�B	�B	�B
�B
�B
�B
B
"B
.B
:�B
>]B
G_B
M�B
RTG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��B-JB~�?��@��P?Pm�@�GBf�?a��A[t?S?!A���@�4�?-�A)�EB��?�m?;�B#�?($@R��BF�?��?���B2BKF?$�o?�MB0EB�>�pH?O?+�!?�ؾB,�B"�?@S	B/�B7*@�?�	A0pG?/<@��B6BW>��.>��?�>�C?���?)v>��?�cAS�/?�?�IB`�?�ܔ>���?(`x?G�zA�6A�c�?\	�@��j>�a�?y�?�z)B��?lĬB��>�?K�?��6A���?�8?N��A��? ib?(��A{�?*}�?�B/!B/?LlU?{�:>�&-?3�@]e�@j?E��B��?)��?6ԀB�1B#TA&q�>�N�>��>�>?in?/	SA��A�A?N/?�G�A6j/?��=A9��>�A�?H�GB,2A��}?�b@I�>��U>��? ��?��A���?L�?8PAU�?&�B6�?�+V>׫�>�hs?,g�B \�A�Ί?V�B5�BW�?XZ�? n�BR�??���?�$pB��? X?��B}�>��p?jF�>��b?�$�Bk�?i?�QB	M�B7�B=�@Gs@�t?B4�B56B:�B4�@�?]?4�BA�Bg�A�L�@Dz�?�3Av8Bmn?<��A��?/��@�ײ?��?v��A�bZ?��.?G�5@��BWG>��S?E��A�Bt�?�?���@�I�?�V�B99B7sB:�A$�?AtB	�B>�?���B;
B=R@W�BA�?��l?��Bc}B;	B;�@w�^A5��BEjB��@���B>B=�B@KB<�B<�BV�B[JBA�B=�BB�B@�B?rB=B?jBACBAB@�B5$BB
B?�B@B=�B@�B?�B@_B?AB@�B?_B?BB?�B@cB<�B;3BBbB>LB@WB=FB@�A��B@tB?�B@�B?�B@�BD�B?�BB
BAjBBB@�BA�B=�B?BB<B>�B?�B?�B>�B@�B?�B=�B@;BCBA BA�B@�B?�B>�B?�B>�B?wB?�B>`B?tB?8B?1B?�B@nB?1B@)B<�B@�BH�B?�B?�B<�BA;B?�B@BA�BD�BA�B?BA�B<�B@B?)B@�B>�B>cBABA�B?
B>�B?FB>YB>xB@dBA�B?XB@�B?bB@ BABA�B@;B>�B>�B>�B=�BC�B?�B?�B=�B?%B=�BA�B=TB>UB>�B?MB?tB>AB;�B@$B>�B:�B<�B?B?�B<{B<�B?UB?�B?�B?@B@�B>B@kB@�BA�BC�B@�BABAaB?�B@B?B?�BBEBA�B?�B?�B>`B@&B$5B@�B?�B?�BB�BAuBCBD<B?jB>TBD�B>�B?�B\PB@EBAeB@B?�B? B@�B@�B>�B@�B@	B@:BAcB=.BCBABA�BABB BB�B@B=�B>�B=�B?�B?8B?�B?�B?�B>B;^B?�B=B?�BA0B<�B>iB@�B@�BFKBD�BB�BBBDB@�B@�BE-BB�BBBABE�BBSBAB@�BABB�B@�BA4BAoBB�BDB@�BC�BA7BA�BA�BC�B@�B@�B@BA�B@�BABC~BBUB>nB@,B@eBA�BA�BBB?�B@�B@LB>�BC|BBzBB�BA B@�B@�B?\B>�BACBB�BE!B?_B@�BA<B@�B?�B?�B@�B@�B@�B?B=�B?B>JB<�B=�B>�B=B=jB<�B;�B;�B:�B:mB;�B;�B:PB9�B:fB:6B;�B;�B='B=FB<�B<GB;=B;B<"B;+B<B<B<cB;�B<�B<�B=�B<bB<1B<�B=�B<6B;gBA�BA�BA�B@�B@mB?�B=�BABBbBC�BHBE�BFbBH�BH�BI�BM_BM�BMNBM�BNBPBO~BQQBP�BNBNRBN�BPBOBQ{BT�BZ�BZ�BX�BY�BYTBZ8BZuB]zB]�B`�BdBk`BiKBkxBl�Bm8Bn;Bn(Bn�Bo�Bp�BpPBq�Br�Bs1Bs�Bt�B}aB�AB|nB�B}�Bz�B�@B~�B{�B~|B��B��B��B��B�CB~�B~�B}0B}�B}*B}�B}UB}�B|�B{�Bz�B|�B}B}DB}�B�B�0B��B�.B�lB��B�[B��B�YB�-B�%B��B��B��B��B��B�ZB�B�1B�IB��B��B��B��B��B�iB�-B�kB��B�!B�/B��B��B��B�"B��B�YB��B��B�qB�~B�%B��B�wB�zB�9B�VB��B�YB��B��B�@B�B��B�iB�B�4B��B�kB��B��B�B�{B��B�8B��B��B��B�TB�xB�>B�mB��B�B�B��B��B��B��B��B�LB��B��B�3B�B��B�_B��B��B�:B�B��B�rB��B5JB4�B6@B6B5�B5�B5�B6:B6�B7lB7>B7�B8FB9KB9B9"B8\B9�B9qB8�B9�B:.B;dB;B9�B:�B:+B:B:NB:B:�B9aB9B:B9�B9B8vB9>B8LB9�B9$B9	B7nB8�B8'B7�B6�B7UB6�B6�B6{B8�B<�B;�B:�B<�B=TB;�B=IB<�B<�B<YB="B;�B<B;�B<B<�B<�B<|B=�B=�B;�B?�B?�BBUB=�B@zBA�BA�B@B@�B@ABC�BDdBDuBD[BD/BD�BCPBDVBDgBDkBD^BDpBD6BC�BC�BC�BC�BC�BC�BB�BD�BE�BD|BEcBEVBD�BDBD�BE%BDBD�BE-BC�BD�BD^BE�BEhBץBםB�aB�B׾B�6B�rB��B֛B��B׿B�#B��B�5B��B�BܠB��BܟB�ZB�B�?B�B��B�B�6B�B�B�DB�B��B�^B�B�BޱB�jB�B�B��B��B��B�[B��B�XB�"B�@B�B�B�B�B��B� B��B��B�B�$B� B�9BB>B�B�B�BDB�B�B
�B%BB3BB
�BAB�B%B2B�B�BB�BgB�BB	BGB�BxBB�B�BBBxBIB_BjBBhB�B�BB�B�B@B�BB�B�B�BBrB"!B �B$B$^B#B$lB$�B$CB&�B*zB+�B,-B+�B-lB.�B/0B/vB.\B/5B/@B0�B/}B/�B0�B0"B0�B0$B0sB0�B1$B1B0�B0�B1
B1�B1�B2-B1�B1�B2~B1�B2�B2uB2lB2�B2cB2vB2�B3CB3B2�B2�B2HB3qB3B3HB2iB3�B2�B2�B2#B2�B2�B2"B2%B2�B2B2�B2�B3hB3,B3'B1�B2�B2�B2�B2�B2yB3B2�B2XB2kB2bB2}B4B3�B3�B	� B	�uB	��B	�hB	�jB	�|B	�kB	�B	�B	��B	��B	�B	��B	��B	�B	��B	�B	�B	�B	�/B	��B	�B	�B	�`B	�B	�FB	�iB	��B	��B	�B	�B	��B	�dB	�9B	�B	�B	�{B	�B	��B	�B	�B	�TB	��B	�B	�B	�B	�wB	�GB	�'B	�B	��B	��B	��B	�B	�B	�OB	�B	�wB	��B	�dB	�JB	��B	�B	�B	�=B	�B	�XB	�9B	�B	�B	�`B	�B	��B	�>B	��B	��B	�B	��B	�yB	�B	�HB	�pB	�B	��B	�B	��B	�6B	�$B	��B	��B	��B	�B	�B	��B	�)B	�-B	�B	�-B	�|B	��B	�B	�-B	��B	�B	�B	�#B	��B	�B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�QB	�TB
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111114111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943344443444444434434434433443344443343344444334444444444434444334444434344434434444433444444434433444444344444444334444444444434444334334434443443444434433344333344333444344444434443444344443334433433434433344334333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B�B�B�KB�BBvB�B�B+B0(B22B38B38B38B6JB8XB8UB:bBE�BebBn�Bo�Bp�Bu�By�Bz�B|�B�B�B�B�B�B�B�)B�9B�;B�4B�B}�B|�B{�Bt�Bt�By�B{�Bz�B{�B|�B�
B�;B��BĞB�sB�AB�B��B��B�ABv�Bk�BbSBS�BO�BF�B4>B+B$�B#�B�B��B�LB�BȺB�wB�=B�B�	B��B��B��B�wB��B��B��B��B��B��B�B�B��B�aBz�B\-B5DB�BB
��B
�B
ťB
�&B
�&B
l�B
O�B
+B
�B
B	�B	�B	�7B	�B	r�B	aLB	S�B	1+B	#�B	"�B	�B	$B��B�B�rB�]B�HB�.B�B��BĤB��B��B�`B�'B��B��B��B��B��B�mB�nB�tB�uB�{B��B�|B�gB��B��B��B��B��B��B��B��B�UB�\B�hB�ZB�NB�PB�AB�<B�6B�=B�7B�0B�2B�0B�)B�*B�*B�.B�'B�$B� B�B�B�B�B�B�B�B�B�B�B�BB}�B|�B{�By�Bv�Bw�Bw�Bx�Bw�Bu�Bs�Bp�Bm�Bk�Bi�BhzBgqBeeBc[BaPB^9B^<B]3B]7B\/B\0B[)B[+BZ$BYBYBXB[*B[*B[+B[(B[(B\.B['B[)B[+B[*BYBXBXBXBXBWBWBV
BV
BYBZ$B^;BefBm�Bj�Bl�Bn�Bl�BhyBgsBbUB^<B_CB^;B`KBk�Bj�Bn�B|�B}�B B�B�'B�AB�GB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�*B�LB�lB�~B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�>B�[B�hB�B�B�B�B�B�B��B��B��B��B��B	B	%B	3B	8B		=B	
DB	LB	RB	IB	KB	
BB	SB	]B	nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	"�B	$�B	&�B	%�B	&�B	,B	24B	7RB	8XB	8YB	9_B	9_B	<qB	A�B	C�B	D�B	E�B	F�B	G�B	L�B	Q�B	UB	VB	WB	XB	Z"B	\2B	\0B	_@B	bTB	d`B	ehB	ehB	egB	fmB	l�B	o�B	q�B	t�B	w�B	y�B	y�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�&B	�)B	�*B	�?B	�FB	�KB	�QB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�B	��B	�B	�B	�	B	�B	�B	�B	�B	�)B	�:B	�]B	�lB	�pB	�oB	�nB	�wB	�xB	�wB	�wB	�xB	�pB	�qB	�lB	�mB	�gB	�eB	�dB	�eB	�_B	�lB	�xB	�}B	��B	��B	ÚB	ĢB	ŨB	ŧB	ǳB	ǶB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�$B	�,B	�6B	�IB	�ZB	�gB	�gB	�wB	�B	�B	�G�O�B	��B
�B
�B
�B
"B
"B
.B
:�B
>dB
GdB
M�B
R]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B-LB~�G�O�G�O�G�O�G�O�Bf�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B#�G�O�G�O�BF�G�O�G�O�B2BKHG�O�G�O�B0KB�G�O�G�O�G�O�G�O�B,�B"�G�O�B/�B7.G�O�G�O�G�O�G�O�G�O�B6!BW$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B`�G�O�G�O�G�O�G�O�A�8A�c�G�O�G�O�G�O�G�O�G�O�B��G�O�B��G�O�G�O�G�O�A���G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�B/%B/G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B�5B#WG�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B,8A�ŃG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6�G�O�G�O�G�O�G�O�B \�A�ΓG�O�B5�BW�G�O�G�O�BR�G�O�G�O�G�O�B��G�O�G�O�B}�G�O�G�O�G�O�G�O�Bk�G�O�G�O�B	M�B7�B=�G�O�G�O�B4�B5;B:�B4�G�O�G�O�A�Bg�A�L�G�O�G�O�G�O�BmsG�O�G�O�G�O�G�O�G�O�G�O�A�b]G�O�G�O�G�O�BWIG�O�G�O�G�O�Bt�G�O�G�O�G�O�G�O�B9=B7{B:�G�O�G�O�B	�B>�G�O�B;B=UG�O�BA�G�O�G�O�Bc�B;B;�G�O�G�O�BEnB��G�O�B>B=�B@PB<�B<�BV�B[NBA�B=�BB�B@�B?wB=B?oBAJBAB@�B5)BBB?�B@B=�B@�B?�B@aB?GB@�B?dB?IB?�B@lB<�B;6BBeB>MB@\B=JB@�G�O�B@yB?�B@�B?�B@�BD�B?�BBBAoBBB@�BA�B=�B?BBAB?B?�B?�B>�B@�B?�B=�B@BBCBA$BA�B@�B?�B>�B?�B>�B?yB?�B>cB?yB?=B?6B?�B@pB?6B@+B<�B@�BH�B?�B?�B<�BA>B?�B@%BA�BD�BA�B?BA�B<�B@B?-B@�B>�B>dBABA�B?B>�B?HB>\B>}B@iBA�B?^B@�B?fB@BA$BA�B@BB>�B>�B>�B=�BC�B?�B?�B=�B?,B=�BA�B=YB>WB>�B?PB?yB>FB;�B@(B>�B:�B<�B?B?�B<}B<�B?ZB?�B@ B?GB@�B>!B@pB@�BA�BC�B@�BA�BAeB?�B@"B?B?�BBJBA�B?�B?�B>cB@)B$<B@�B?�B?�BB�BA}BCBDAB?oB>WBD�B>�B?�B\SB@KBAiB@B?�B?B@�B@�B>�B@�B@B@?BAgB=4BCBA$BA�BABB%BB�B@%B=�B>�B=�B?�B?=B?�B?�B?�B>B;`B?�B=B?�BA1B<�B>kB@�B@�BFPBD�BB�BBBD!B@�B@�BE2BB�BBBA	BE�BBVBAB@�BA	BB�B@�BA8BAtBC BDB@�BC�BA;BA�BA�BC�B@�B@�B@BA�B@�BABC�BBXB>sB@0B@lBA�BA�BB	B?�B@�B@SB>�BC~BBBB�BA"B@�B@�B?`B>�BAEBB�BE(B?dB@�BAAB@�B?�B?�B@�B@�B@�B?B=�B?
B>NB<�B=�B>�B=B=oB<�B;�B<B:�B:nB;�B;�B:VB9�B:jB::B;�B<B=+B=JB<�B<KB;AB;BרBנB�dB�B��B�9B�uB��B֟B��B��B�'B��B�<B��B�BܥB��BܢB�`B�B�BB�!B��BمB�;B�B�B�JB�B��B�cB�B�B޴B�pB�B�B��B�B��B�_B�B�^B�"B�AB�B�B�B�B��B�B��B��B�B�'B�B�>BBBB�B�B�BHB�B�B
�B(BB6BB
�BCB�B'B5B�B�BB�BoB�BBBMB�B�BB�BBBB}BNBfBmB
BmB�B�BB�B�BEB�B�B�B�B�B"BuB"$B �B$B$bB#�B$tB$�B$GB&�B*}B+�B,/B+�B-rB.�B/6B/|B.`B/:B/EB0�B/�B/�B0�B0(B0�B0*B0vB0�B1(B1!B0�B0�B1B1�B1�B22B1�B1�B2�B1�B2�B2{B2nB2�B2eB2xB2�B3GB3B2�B2�B2OB3uB3B3LB2jB3�B2�B2�B2%B2�B3B2%B2)B3B2 B2�B2�B3lB31B3,B1�B2�B2�B2�B2�B2~B3B2�B2ZB2qB2eB2�B4B3�B3�B	�B	�zB	�B	�oB	�nB	�B	�qB	�B	�B	��B	��B	�B	��B	�B	�B	��B	�B	�$B	�B	�3B	��B	�B	�B	�iB	�B	�NB	�oB	��B	��B	�B	�B	��B	�kB	�AB	�&B	�B	�B	�B	��B	�B	�B	�\B	��B	�B	��B	�B	�}B	�OB	�-B	�B	��B	��B	��B	�B	�B	�UB	�B	�}B	��B	�gB	�PB	�B	�B	�B	�FB	�#B	�^B	�AB	�!B	�B	�dB	�B	��B	�AB	�B	��B	�B	��B	�B	�B	�OB	�vB	�B	��B	�B	��B	�9B	�+B	��B	�B	��B	�B	�B	��B	�0B	�5B	�B	�2B	�B	��B	�B	�2B	��B	�B	�B	�,B	��B	�B	��B	�%B	�B	�B	��B	��B	�B	�B	�B	�XB	�\B
�BרBנB�dB�B��B�9B�uB��B֟B��B��B�'B��B�<B��B�BܥB��BܢB�`B�B�BB�!B��BمB�;B�B�B�JB�B��B�cB�B�B޴B�pB�B�B��B�B��B�_B�B�^B�"B�AB�B�B�B�B��B�B��B��B�B�'B�B�>BBBB�B�B�BHB�B�B
�B(BB6BB
�BCB�B'B5B�B�BB�BoB�BBBMB�B�BB�BBBB}BNBfBmB
BmB�B�BB�B�BEB�B�B�B�B�B"BuB"$B �B$B$bB#�B$tB$�B$GB&�B*}B+�B,/B+�B-rB.�B/6B/|B.`B/:B/EB0�B/�B/�B0�B0(B0�B0*B0vB0�B1(B1!B0�B0�B1B1�B1�B22B1�B1�B2�B1�B2�B2{B2nB2�B2eB2xB2�B3GB3B2�B2�B2OB3uB3B3LB2jB3�B2�B2�B2%B2�B3B2%B2)B3B2 B2�B2�B3lB31B3,B1�B2�B2�B2�B2�B2~B3B2�B2ZB2qB2eB2�B4B3�B3�B	�B	�zB	�B	�oB	�nB	�B	�qB	�B	�B	��B	��B	�B	��B	�B	�B	��B	�B	�$B	�B	�3B	��B	�B	�B	�iB	�B	�NB	�oB	��B	��B	�B	�B	��B	�kB	�AB	�&B	�B	�B	�B	��B	�B	�B	�\B	��B	�B	��B	�B	�}B	�OB	�-B	�B	��B	��B	��B	�B	�B	�UB	�B	�}B	��B	�gB	�PB	�B	�B	�B	�FB	�#B	�^B	�AB	�!B	�B	�dB	�B	��B	�AB	�B	��B	�B	��B	�B	�B	�OB	�vB	�B	��B	�B	��B	�9B	�+B	��B	�B	��B	�B	�B	��B	�0B	�5B	�B	�2B	�B	��B	�B	�2B	��B	�B	�B	�,B	��B	�B	��B	�%B	�B	�B	��B	��B	�B	�B	�B	�XB	�\B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111114111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943344443444444434434434433443344443343344444334444444444434444334444434344434434444433444444434433444444344444444334444444444434444334334434443443444434433344333344333444344444434443444344443334433433434433344334333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011535242020090115352420200901153524202009011535242020090115352420200901153524202009011535242020090115352420200901153524202009011535242020090115352420200901153524AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202121082018112021210820181120212108    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121082018112021210820181120212108  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121082018112021210820181120212108  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011535242020090115352420200901153524  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                