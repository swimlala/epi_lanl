CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  H   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:37Z creation      
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
resolution        =���   axis      Z        '`  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  lp   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '`  vH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '`  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '`  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '`     TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 'x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '` 1P   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '` X�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '` ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �H   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '` �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '` �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 	�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '` �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ;   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '` D�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � lP   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   m   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   y   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
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
_FillValue                  0 �4Argo profile    3.1 1.2 19500101000000  20190214173037  20200828145505  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               -   -   -AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @ת�E7�@ת�E7�@ת�E7�111 @ת���O@ת���O@ת���O@5�t�j~�@5�t�j~�@5�t�j~��cxě��T�cxě��T�cxě��T111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    -   -   -ADA BDA  DA BDA @9��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  AᙚA�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�3D��D�G
D�v�D��
D��D�D{D�y�D���D�
D�B=D���D���D�HD�9�Dڇ�D࢏D��HD�(RD�m�D��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    =���=���    =���>���=���            >L��>L��    =���=���>���            =���        =���                        =���        >���?��>���                            >���>���=���    =���=���        =���        >L��                >L��                    =���                    =���    =���                >L��                =���>L��        =���                    >L��>���                >���>���        =���=���        =���>L��        =���                            =���                            =���>L��        >L��        =���        >L��>���        =���>���>���        >L��>L��=���        =���            >L��                >L��>���>L��        =���>���>L��                =���>L��>���>L��    =���=���    >L��        =���=���=���=���            >���=���    =���=���=���    =���=���    =���=���            >���=���=���>L��    >L��    =���>L��    =���>���>L��    >L��=���    =���>L��>L��=���>���=���>L��>L��=���>L��>L��>���>���>L��>L��>L��>���>L��=���>L��>L��>���>���>L��>���>L��>���>L��?   >���>���>L��>L��=���=���>���>���>���>���>���>L��=���>L��>���>L��>L��>���>���>���>���>���=���=���>���>L��>L��>L��>L��>���>���>L��>���?   >���>L��>���>���>L��>L��>L��>L��>���>L��=���>���>L��>L��>���>���>���>���?   >���>���>���>���>L��=���>L��>���>L��>L��>L��>���>L��>���>L��>L��>���>���>L��>���>���>L��>���>L��>���>���>L��>���>L��>���>L��>���>L��>���>���>���>L��>L��>���>���>L��>L��>L��>���>L��>L��>���>L��>���>L��>L��>���>���>L��>L��>���>L��=���>���>���>L��>���>���>L��>L��>���>���>���>L��=���>L��>���=���>L��>L��>���>���>���>L��>L��>���>���>���>���>���>���>L��=���>L��>L��>���>���>L��>���>���>L��>���>L��>���=���>L��>���>���>���>���>���>���>L��>���>L��>���>���>���>���>���>���>���=���>L��>���>���>L��=���>L��>L��>L��>���>L��>L��>���>L��>���>L��>L��>���>���>���>���>���>L��>L��>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���=���>L��=���>L��>L��>L��>L��>���>L��>���>���>L��>���>L��=���>���>L��>L��>L��>���>���>���>���>L��>L��>L��>���>���>���>���>L��>���>���>���>L��>L��>L��=���>L��=���>���>���>���>���?   ?   ?��?��?L��?fff?fff?fff?���?���?���?���?�ff?�33?�33?�  ?���?���?ٙ�?�ff?�33@   @   @ff@��@33@33@��@   @&ff@&ff@333@9��@@  @L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@���@�  @�33@�ff@���A   A33AffA  A	��A33AffA  A��A��AffA��A33AffA   A!��A$��A&ffA(  A+33A,��A.ffA1��A333A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AL��ANffAP  AQ��AS33AVffAX  AY��A[33A\��A`  Aa��Ac33Ad��AfffAh  Ak33AnffAp  Aq��As33At��AvffAy��A{33A|��A~ffA���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  Aə�A�ffA�  A���A�ffA�33A�  Aљ�A�ffA�  A���Aՙ�A�33A�  Aٙ�A�ffA�33A���Aݙ�A�ffDp��Dq  DqfDq�Dq�Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3@9��@@  @L��@S33@Y��@fff@l��@s33@�  @�33@�ff@���@�  @�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@���@�  @�33@�ff@���A   A33AffA  A	��A33AffA  A��A��AffA��A33AffA   A!��A$��A&ffA(  A+33A,��A.ffA1��A333A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AL��ANffAP  AQ��AS33AVffAX  AY��A[33A\��A`  Aa��Ac33Ad��AfffAh  Ak33AnffAp  Aq��As33At��AvffAy��A{33A|��A~ffA���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  Aə�A�ffA�  A���A�ffA�33A�  Aљ�A�ffA�  A���Aՙ�A�33A�  Aٙ�A�ffA�33A���Aݙ�A�ffDp��Dq  DqfDq�Dq�Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�Dq` DqffDqs3Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@8Q�@~�R@�\)@�\)A�AAG�A_�A�A��
A��
A��
A��
A��
A�p�A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Ch{Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp�RDq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dy��D�
D�FfD�vD��fD�
D�C�D�yHD��RD�fD�A�D���D��RD� �D�9HDڇ
D��D���D�'�D�mD�ǮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����
=��
=��
���
=��
>�\)=��
���
���
���
>8Q�>8Q켣�
=��
=��
>�\)���
���
���
=��
���
���
=��
���
���
���
���
���
���
=��
���
���
>�\)?z�>\���
���
���
���
���
���
���
>�\)>\=��
���
=��
=��
���
���
=��
���
���
>8Q켣�
���
���
���
>8Q켣�
���
���
���
���
=��
���
���
���
���
���
=��
���
=��
���
���
���
���
>8Q켣�
���
���
���
=��
>8Q켣�
���
=��
���
���
���
���
���
>8Q�>�\)���
���
���
���
>�\)>�\)���
���
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
���
���
���
���
=��
���
���
���
���
���
���
���
=��
>8Q켣�
���
>8Q켣�
���
=��
���
���
>8Q�>�\)���
���
=��
>�\)>�\)���
���
>8Q�>8Q�=��
���
���
=��
���
���
���
>8Q켣�
���
���
���
>8Q�>�\)>8Q켣�
���
=��
>�\)>8Q켣�
���
���
���
=��
>8Q�>�\)>8Q켣�
=��
=��
���
>8Q켣�
���
=��
=��
=��
=��
���
���
���
>�\)=��
���
=��
=��
=��
���
=��
=��
���
=��
=��
���
���
���
>�\)=��
=��
>8Q켣�
>8Q켣�
=��
>8Q켣�
=��
>�\)>8Q켣�
>8Q�=��
���
=��
>8Q�>8Q�=��
>�\)=��
>8Q�>8Q�=��
>8Q�>8Q�>\>\>8Q�>8Q�>8Q�>�\)>8Q�=��
>8Q�>8Q�>\>�\)>8Q�>�\)>8Q�>�\)>8Q�>�>�\)>�\)>8Q�>8Q�=��
=��
>�\)>�\)>�\)>�\)>�\)>8Q�=��
>8Q�>�\)>8Q�>8Q�>�\)>�\)>�\)>�\)>�\)=��
=��
>�\)>8Q�>8Q�>8Q�>8Q�>�\)>�\)>8Q�>�\)>�>�\)>8Q�>�\)>�\)>8Q�>8Q�>8Q�>8Q�>�\)>8Q�=��
>�\)>8Q�>8Q�>�\)>�\)>�\)>�\)>�>\>�\)>�\)>�\)>8Q�=��
>8Q�>�\)>8Q�>8Q�>8Q�>\>8Q�>�\)>8Q�>8Q�>�\)>�\)>8Q�>�\)>�\)>8Q�>�\)>8Q�>�\)>�\)>8Q�>�\)>8Q�>�\)>8Q�>�\)>8Q�>�\)>�\)>�\)>8Q�>8Q�>�\)>�\)>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>8Q�>\>8Q�>8Q�>�\)>�\)>8Q�>8Q�>�\)>8Q�=��
>�\)>�\)>8Q�>�\)>�\)>8Q�>8Q�>�\)>�\)>\>8Q�=��
>8Q�>\=��
>8Q�>8Q�>�\)>�\)>�\)>8Q�>8Q�>\>�\)>�\)>�\)>�\)>�\)>8Q�=��
>8Q�>8Q�>�\)>�\)>8Q�>�\)>�\)>8Q�>�\)>8Q�>�\)=��
>8Q�>�\)>�\)>�\)>�\)>�\)>�\)>8Q�>�\)>8Q�>\>\>�\)>�\)>\>�\)>�\)=��
>8Q�>�\)>�\)>8Q�=��
>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>8Q�>�\)>8Q�>8Q�>�\)>�\)>�\)>�\)>�\)>8Q�>8Q�>�\)>�\)>8Q�>�\)>�\)>�\)>\>�\)>�\)>�\)>\>\>\>\>�\)>�\)=��
>8Q�=��
>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>�\)>�\)>8Q�>�\)>8Q�=��
>�\)>8Q�>8Q�>8Q�>�\)>\>�\)>�\)>8Q�>8Q�>8Q�>�\)>�\)>�\)>�\)>8Q�>\>�\)>�\)>8Q�>8Q�>8Q�=��
>8Q�=��
>�\)>�\)>\>\>�>�?z�?z�?G�?aG�?aG�?aG�?�=q?�=q?�
>?�
>?��
?���?���?�p�?�=q?�=q?�
>?��
?��?�p�?�p�@�@�@�@�@Q�@�R@%�@%�@1�@8Q�@>�R@K�@Q�@XQ�@e�@k�@q�@~�R@��\@�@�(�@�\)@�@���@�\)@��\@�@�(�@�\)@�@���@�(�@\@�@�(�@�\)@ҏ\@���@�(�@�\@�@�(�@�\)@�\@�@�(�@�\)A�GAzA�A	G�A
�GAzA�AG�Az�AzAG�A�GAzA�A!G�A$z�A&zA'�A*�GA,z�A.zA1G�A2�GA6zA7�A9G�A<z�A>zA?�AAG�AB�GAFzAG�AIG�ALz�ANzAO�AQG�AR�GAVzAW�AYG�AZ�GA\z�A_�AaG�Ab�GAdz�AfzAg�Aj�GAnzAo�AqG�Ar�GAtz�AvzAyG�Az�GA|z�A~zA���A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
Aģ�A�=pA�
=A��
A�p�A�=pA��
Ạ�A�=pA�
=A��
A�p�A�=pA��
Aԣ�A�p�A�
=A��
A�p�A�=pA�
=Aܣ�A�p�A�=pDp�RDp��DqDq�DqRDq�Dq%Dq1�Dq8RDq>�DqK�DqQ�DqXRDq^�DqeDqq�DqxRDq~�Dq��Dq��Dq�RDq�Dq��Dq��Dq�RDq�Dq˅Dq��Dq�RDq�Dq�Dq��Dq��DrDr�DrRDr�Dr%Dr+�Dr8RDr>�DrEDrQ�DrXRDr^�DreDrq�DrxRDr~�Dr��Dr��Dr�RDr��Dr��Dr��Dr�RDr�Dr˅Dr��Dr�RDr�Dr�Dr��Dr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsK�DsQ�DsXRDs^�Dsk�Dsq�DsxRDs�Ds��Ds��Ds��Ds�Ds��Ds�RDs��Ds�Ds˅Ds�RDs޸Ds�Ds��Ds�RDs��Dt�Dt�DtRDt%Dt+�Dt1�Dt>�DtEDtK�DtXRDt^�DteDtq�DtxRDt~�Dt��Dt��Dt�RDt�Dt��Dt��Dt��Dt�Dt˅Dt��@8Q�@>�R@K�@Q�@XQ�@e�@k�@q�@~�R@��\@�@�(�@�\)@�@���@�\)@��\@�@�(�@�\)@�@���@�(�@\@�@�(�@�\)@ҏ\@���@�(�@�\@�@�(�@�\)@�\@�@�(�@�\)A�GAzA�A	G�A
�GAzA�AG�Az�AzAG�A�GAzA�A!G�A$z�A&zA'�A*�GA,z�A.zA1G�A2�GA6zA7�A9G�A<z�A>zA?�AAG�AB�GAFzAG�AIG�ALz�ANzAO�AQG�AR�GAVzAW�AYG�AZ�GA\z�A_�AaG�Ab�GAdz�AfzAg�Aj�GAnzAo�AqG�Ar�GAtz�AvzAyG�Az�GA|z�A~zA���A�p�A�=pA��
A���A�p�A�
=A��
A���A�=pA�
=A���A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA�
=A���A�p�A�
=A��
A�p�A�=pA�
=A���A�p�A�
=A��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
A���A�=pA�
=A��
A�p�A�=pA��
Aģ�A�=pA�
=A��
A�p�A�=pA��
Ạ�A�=pA�
=A��
A�p�A�=pA��
Aԣ�A�p�A�
=A��
A�p�A�=pA�
=Aܣ�A�p�A�=pDp�RDp��DqDq�DqRDq�Dq%Dq1�Dq8RDq>�DqK�DqQ�DqXRDq^�DqeDqq�DqxRDq~�Dq��Dq��Dq�RDq�Dq��Dq��Dq�RDq�Dq˅Dq��Dq�RDq�Dq�Dq��Dq��DrDr�DrRDr�Dr%Dr+�Dr8RDr>�DrEDrQ�DrXRDr^�DreDrq�DrxRDr~�Dr��Dr��Dr�RDr��Dr��Dr��Dr�RDr�Dr˅Dr��Dr�RDr�Dr�Dr��Dr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsK�DsQ�DsXRDs^�Dsk�Dsq�DsxRDs�Ds��Ds��Ds��Ds�Ds��Ds�RDs��Ds�Ds˅Ds�RDs޸Ds�Ds��Ds�RDs��Dt�Dt�DtRDt%Dt+�Dt1�Dt>�DtEDtK�DtXRDt^�DteDtq�DtxRDt~�Dt��Dt��Dt�RDt�Dt��Dt��Dt��Dt�Dt˅Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aǡ�Aǣ�Aǟ�AǗ�AǛ�AǑhA�l�A�ffA�K�A�/A��A�VA�1A�A���A���A���A��A��A�ȴAƬA�I�A��AŲ-A�p�A��AĴ9AĮA�l�A�O�A�$�A�A��
A��
A�ȴA���A���AîAÕ�A�v�A�E�A�;dA�+A��A���A���A¼jA¥�A�t�A�r�A�t�A�O�A��jA�z�A�(�A��A�{A���A��\A�?}A���A��-A�M�A�ffA���A��\A�Q�A�A��HA���A�x�A��TA���A��;A���A�A�&�A�G�A��7A��A��-A��A��RA�33A�%A���A�z�A�v�A�K�A�bA�bA��A��A���A�Q�A��FA�~�A���A���A���A�O�A���A�K�A��PA�ĜA�K�A��^A�ffA��A�$�A�v�A�G�A�VA�\)A�~�A��jA��!A�ƨA�Q�A�"�A�x�A�jA�"�A�|�A��+A�ĜA�%A}XA|��A{x�Aw+As�Ao�#Am�Ak�;Ak�Ai��Ah�Ae�Ab(�A_C�A]�TA\bA[hsA[�AX�AT1AQ�mAPM�AOK�ALbNAJ��AI��AG&�ADE�AC33AB�RAA�PA@�!A?p�A>A;C�A:�9A:Q�A:$�A9x�A8r�A7�TA6�A4�DA4VA4JA2�A/��A/G�A.A�A-��A-ƨA-K�A*��A)\)A(��A(A&ĜA%��A$��A$=qA#"�A!ƨA ��A��A|�A�/A9XAO�A��A?}A��A�;AG�A��A�;AƨA|�Al�AG�A
=A��A�AZA�7A
-A	��A	\)AȴAZA�AƨA�A5?AdZA&�A�A��A�;A�A�A��A�mA�A(�A�AA~�A��A��A��A~�A��A%A�A��Al�@��
@�hs@�bN@�M�@��@�1@�ȴ@��^@�7L@�/@�/@��9@�@���@��m@�ȴ@�A�@�/@�J@�+@�7L@ߝ�@��@�Ĝ@�j@�+@�~�@�hs@���@؋D@�j@�1@���@��@�S�@���@ա�@�/@ԣ�@�1'@�  @ӶF@��y@�ff@��@�`B@�bN@�;d@Ͳ-@�&�@�1'@�K�@ə�@ȣ�@�A�@��@��@�hs@��@��@��@��@��y@�~�@���@��@�l�@��@��@�%@�I�@��@�M�@���@�X@��/@�z�@�  @��;@�"�@�`B@��`@�Z@��;@�S�@���@�ȴ@�ff@���@��@�&�@�Ĝ@���@��T@��@�7L@��/@��j@�1'@�o@���@���@���@�ff@���@���@���@�I�@���@��@�\)@�;d@��@�@�X@���@��@�`B@�?}@�r�@�j@�I�@��;@��P@�+@�o@�
=@�
=@���@�ff@��@���@�5?@�E�@�E�@�n�@���@���@���@�~�@�M�@�-@���@���@�hs@�?}@�&�@�%@���@��/@���@�I�@��m@��;@��
@�ƨ@�ƨ@�ƨ@��;@�1@� �@�  @��
@���@��@�l�@�K�@�o@��\@�~�@���@��+@�V@�=q@�5?@�@��@�@���@���@��h@�x�@�G�@�&�@���@��@�Q�@���@��F@��@�S�@�K�@�ȴ@�v�@�^5@�$�@���@��-@�V@��`@���@�bN@�I�@��@���@���@�dZ@�S�@�K�@�;d@���@�V@�-@��@��@���@���@�p�@�%@�%@���@��@�j@��@��
@�|�@��y@���@�J@��#@�hs@�p�@�hs@�?}@��@���@��u@�  @��P@�dZ@�;d@�@��y@�ȴ@���@�^5@���@z��@sdZ@l|�@d��@]T�@S1�@K�+@D��@?�r@:�@5@-�"@)�@&�@��@I�@�9@�$@+@�.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�A��A�\)A�A�ffA���A���A��wA�x�A���A�O�A���A���A�(�A�t�A�A���A�VA�Q�A��mA�
=A�|�A��A��mA�G�A�VA�7LA�&�A��A���A�x�Aǝ�AǁA�XA�A���A�Q�A�bA���A�x�A���AǛ�A�ZA���A���A���A��A�1'A�bA���A���A���A�VA�$�A�7LA�z�A�ĜA�A���A��A���A�O�A�z�A�;dA�dZA��A�oA�33A�&�A�+A�`BA�bA�I�A�A�A�;dA�|�A�1A�K�A��A���A�K�AÑhA�x�A�{A�{A�\)A�O�A��A�ĜA�VA���A�-A���A�VA���A�1'A���AǅA�^5A��yA���A�+A�JA�{A�1AǅA��DA�  A���A��A���A���A�E�A���A�$�A�l�A�{A�
=A�\)A�ȴA�;dA��A��RA��9A��-AŲ-A�|�A�z�Aç�A�+A�n�A��RA��#A�O�A��Aǝ�A�ZA�+A��A���AǍPA��A���A�C�A�oA��RA��!A��/A��A�E�A��hA��+A��A��-A��\A��+A�ȴA��Aǉ7A�l�A��!A��jA�bAǏ\A���A��RA���A��mA�A��#A��A���AǏ\A�VA�ĜA��FA�jA�I�A�-A�x�A��
AŶFA�O�A�dZA�|�A���A�?}Aǉ7A�\)A���A��A�ƨA�ȴA��A��FA�/A��`A��A���A�p�A��A��mA�VA�ffA��A��A��`A�&�A���A�\)AǋDA�bNA���A���A�^5A���A�VA���A�
=A�&�AǑhAǡ�A�ffA�G�A�?}A��A�\)A�  A�S�A�n�A��Aǧ�A�ȴA��AǅAǧ�Aǡ�A�z�A���Aǝ�Aǡ�AǬAǡ�A�x�AčPAƼjA�x�Aǟ�AǬAǩ�A�A�AǛ�A��;A���A��Aǩ�Aǰ!Aǥ�Aǥ�Aǟ�A�VA���Aǟ�Aǝ�AǍPAǙ�Aǣ�Aǥ�A���Aǧ�A�dZA�-A�dZA�^5AǬA��Aǰ!AǬAǧ�A��Aǡ�Aǧ�Aǩ�A�Aǣ�Aǩ�Aǣ�A��A��
A�&�Aǣ�A��A�K�A��PAǡ�A��Aǧ�AǬAǬAǡ�Aǥ�AǬAǩ�Aǩ�A�Aǟ�A���A�bA�Q�Aǡ�A�A�Aǩ�Aǩ�Aǟ�Aǣ�AƗ�Aǣ�A��Aǣ�Aǣ�A��Aǣ�A�;dA��Aǡ�Aǥ�Aǝ�Aǧ�A�;dAǡ�A�r�A�bA�Aǝ�A�E�Aǡ�Aǥ�A�  AǓuAǡ�A�(�Aǣ�A�M�AǙ�Aǝ�A��+A��Aǣ�A�ZAǧ�Aǡ�A�A�Aǟ�Aǩ�A�z�A�K�A�t�Aǣ�AāA�bNAǡ�A�5?Aǥ�Aǩ�Aǥ�Aǡ�Aǩ�Aǡ�Aǥ�Aǣ�Aº^A���Aǧ�A�S�A�r�A�?}Aǥ�AǬAǩ�Aǩ�AþwAǣ�Aǧ�AǮAǧ�Aǩ�Aǥ�AŅAǇ+A�~�Aǣ�Aǣ�AǬAǏ\AǬAǩ�AǋDAǧ�Aǣ�Aǣ�A�t�A��AǕ�Aǥ�AǬAǡ�Aǝ�AǮA�"�AǇ+Aǝ�A�bNAǬAǧ�A�(�Aǣ�Aǩ�Aǧ�A��A�Aŗ�AǬAǧ�A�AǕ�Aǩ�AǅAǧ�A��yAǛ�A�O�Aǣ�AǮAǣ�A���AǕ�Aǧ�Aǥ�A���Aǥ�A��A�v�Aǣ�Aǧ�AǑhA�Aǩ�AǛ�Aǩ�AǬAǰ!AǬAǰ!Aǩ�AǬAǩ�Aǝ�Aǣ�AǍPA�-A���A�?}Aǣ�A���Aǩ�AǬA�C�Aǧ�Aǧ�A���Aǩ�A��;A�5?Aś�Aǩ�AǛ�A�^5Aǧ�Aǧ�Aǩ�Aǲ-AǮAǣ�AāAǥ�AǮAǬAǩ�A���AǓuAǥ�Aǧ�Aǟ�A�{Aǡ�A�=qA�+A�A�Aǣ�Aǥ�Aǥ�AǮAǬAǬAǬAǥ�AǬAǰ!Aǥ�Aǩ�AǬAǮAǬAǰ!Aǩ�Aǧ�AǮAǮAǩ�AǬAǬAǩ�AǬAǥ�Aǩ�AǮAǮAǩ�Aǥ�Aǥ�Aǧ�Aǩ�Aǣ�Aǩ�Aǧ�Aǩ�AǬAǣ�AǮAǰ!AǴ9AǶFAǰ!AǬAǮAǮAǮAǧ�AǮAǬAǩ�AǬAǰ!AǮAǬAǬAǰ!Aǰ!Aǣ�AǛ�AǬAǣ�Aǟ�Aǝ�Aǩ�AǬAǩ�AǬAǮAǮAǬAǙ�Aǝ�AǓuAǕ�AǓuAǕ�AǕ�AǗ�AǗ�AǓuAǝ�Aǧ�Aǧ�Aǥ�Aǩ�Aǣ�Aǟ�Aǩ�Aǩ�Aǥ�Aǧ�Aǩ�Aǧ�Aǥ�Aǥ�AǬAǮAǬAǬAǩ�Aǰ!AǬAǅA�z�A�v�A�t�A�x�Aǩ�AǬAǥ�AǁA�p�A�r�A�p�A�p�A�r�A�n�A�jA�r�A�r�A�v�A�t�A�p�A�p�A�r�A�r�A�t�A�r�A�n�A�r�A�p�A�r�A�l�A�l�A�p�A�l�A�l�A�jA�l�A�jA�n�A�r�A�p�A�l�A�n�A�S�A�n�A�ffA�;dA�=qA�;dA�=qA�;dA�;dA�;dA�;dA�;dA�9XA�9XA�9XA�7LA�7LA�5?A�5?A�33A�33A�5?A�-A�-A�-A�(�A�+A�/A�+A�+A�(�A� �A� �A�"�A� �A��A� �A� �A��A��A��A��A��A��A��A�{A�oA�VA�VA�bA�VA�VA�JA�VA�bA�oA�{A�{A�JA�VA�VA�VA�JA�JA�JA�VA�VA�JA�VA�JA�VA�VA�V@��@�|�@�t�@�t�@�t�@�t�@�l�@�t�@�t�@�t�@�l�@�l�@�l�@�l�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�\)@�S�@�\)@�S�@�S�@�K�@�K�@�K�@�K�@�C�@�C�@�;d@�;d@�;d@�;d@�+@�+@��@��@�o@�o@�
=@�@�@�@�@�@�@�@�@�@���@���@���@���@���@���@���@��@��y@��y@��y@��H@��H@��H@��H@��H@��@��@���@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@���@��R@���@���@��R@��R@��R@��R@��!@���@��!@���@���@���@���@���@�v�@�~�@�v�@�~�@�n�@�~�@�~�@�ff@�ff@�ff@�V@�^5@�V@�^5@�^5@�V@�M�@�M�@�M�@�M�@�M�@�E�@�E�Aǡ�Aǡ�Aǡ�Aǥ�Aǡ�Aǧ�Aǩ�Aǧ�Aǣ�Aǣ�Aǥ�Aǡ�Aǣ�Aǣ�Aǣ�Aǣ�Aǣ�Aǥ�Aǣ�Aǣ�Aǥ�Aǣ�Aǝ�AǙ�AǛ�AǙ�AǛ�AǛ�AǛ�Aǟ�Aǡ�Aǡ�Aǣ�Aǣ�Aǣ�Aǟ�AǑhAǛ�AǍPAǍPAǋDAǉ7AǋDAǍPAǉ7AǋDAǓuAǕ�Aǝ�Aǝ�AǛ�AǛ�Aǝ�Aǟ�Aǡ�Aǡ�Aǡ�Aǣ�Aǟ�Aǝ�Aǝ�Aǝ�Aǥ�Aǡ�Aǡ�Aǥ�Aǥ�AǓuA�l�A�ffA�l�A�n�AǇ+Aǟ�Aǝ�Aǉ7AǁA�ffA�hsA�jA�ffA�jA�ffA�ffA�jA�n�A�n�A�l�A�ffA�ffA�jA�jA�hsA�ffA�jA�jA�jA�ffA�ffA�ffA�dZA�bNA�dZA�dZA�ffA�ffA�ffA�hsA�dZA�dZA�bNA�XA�ffA�O�A�7LA�5?A�5?A�5?A�33A�5?A�5?A�33A�33A�1'A�1'A�1'A�1'A�/A�/A�/A�+A�+A�-A�-A�&�A�&�A�$�A�"�A�$�A�$�A�"�A�"�A��A��A��A��A��A��A��A��A�{A�{A�{A�{A�oA�oA�JA�%A�1A�
=A�1A�
=A�1A�1A�1A�JA�JA�VA�JA�%A�1A�1A�1A�%A�%A�%A�1A�
=A�
=A�
=A�1A�1A�
=A�1@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�l�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�\)@�\)@�\)@�\)@�S�@�S�@�K�@�K�@�K�@�K�@�C�@�C�@�;d@�;d@�;d@�+@�+@�"�@��@��@�o@�o@�
=@�@�@�@�@�@�@���@���@�@���@���@���@���@���@���@��@��@��y@��y@��y@��H@��H@��@��H@��@��@��@���@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@���@���@���@���@��R@���@��R@��R@��!@���@��!@���@���@���@���@���@�v�@�v�@�v�@�v�@�~�@�~�@�~�@�ff@�ff@�^5@�^5@�^5@�^5@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�E�@�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999Aǡ�Aǣ�Aǟ�AǗ�AǛ�AǑhA�l�A�ffA�K�A�/A��A�VA�1A�A���A���A���A��A��A�ȴAƬA�I�A��AŲ-A�p�A��AĴ9AĮA�l�A�O�A�$�A�A��
A��
A�ȴA���A���AîAÕ�A�v�A�E�A�;dA�+A��A���A���A¼jA¥�A�t�A�r�A�t�A�O�A��jA�z�A�(�A��A�{A���A��\A�?}A���A��-A�M�A�ffA���A��\A�Q�A�A��HA���A�x�A��TA���A��;A���A�A�&�A�G�A��7A��A��-A��A��RA�33A�%A���A�z�A�v�A�K�A�bA�bA��A��A���A�Q�A��FA�~�A���A���A���A�O�A���A�K�A��PA�ĜA�K�A��^A�ffA��A�$�A�v�A�G�A�VA�\)A�~�A��jA��!A�ƨA�Q�A�"�A�x�A�jA�"�A�|�A��+A�ĜA�%A}XA|��A{x�Aw+As�Ao�#Am�Ak�;Ak�Ai��Ah�Ae�Ab(�A_C�A]�TA\bA[hsA[�AX�AT1AQ�mAPM�AOK�ALbNAJ��AI��AG&�ADE�AC33AB�RAA�PA@�!A?p�A>A;C�A:�9A:Q�A:$�A9x�A8r�A7�TA6�A4�DA4VA4JA2�A/��A/G�A.A�A-��A-ƨA-K�A*��A)\)A(��A(A&ĜA%��A$��A$=qA#"�A!ƨA ��A��A|�A�/A9XAO�A��A?}A��A�;AG�A��A�;AƨA|�Al�AG�A
=A��A�AZA�7A
-A	��A	\)AȴAZA�AƨA�A5?AdZA&�A�A��A�;A�A�A��A�mA�A(�A�AA~�A��A��A��A~�A��A%A�A��Al�@��
@�hs@�bN@�M�@��@�1@�ȴ@��^@�7L@�/@�/@��9@�@���@��m@�ȴ@�A�@�/@�J@�+@�7L@ߝ�@��@�Ĝ@�j@�+@�~�@�hs@���@؋D@�j@�1@���@��@�S�@���@ա�@�/@ԣ�@�1'@�  @ӶF@��y@�ff@��@�`B@�bN@�;d@Ͳ-@�&�@�1'@�K�@ə�@ȣ�@�A�@��@��@�hs@��@��@��@��@��y@�~�@���@��@�l�@��@��@�%@�I�@��@�M�@���@�X@��/@�z�@�  @��;@�"�@�`B@��`@�Z@��;@�S�@���@�ȴ@�ff@���@��@�&�@�Ĝ@���@��T@��@�7L@��/@��j@�1'@�o@���@���@���@�ff@���@���@���@�I�@���@��@�\)@�;d@��@�@�X@���@��@�`B@�?}@�r�@�j@�I�@��;@��P@�+@�o@�
=@�
=@���@�ff@��@���@�5?@�E�@�E�@�n�@���@���@���@�~�@�M�@�-@���@���@�hs@�?}@�&�@�%@���@��/@���@�I�@��m@��;@��
@�ƨ@�ƨ@�ƨ@��;@�1@� �@�  @��
@���@��@�l�@�K�@�o@��\@�~�@���@��+@�V@�=q@�5?@�@��@�@���@���@��h@�x�@�G�@�&�@���@��@�Q�@���@��F@��@�S�@�K�@�ȴ@�v�@�^5@�$�@���@��-@�V@��`@���@�bN@�I�@��@���@���@�dZ@�S�@�K�@�;d@���@�V@�-@��@��@���@���@�p�@�%@�%@���@��@�j@��@��
@�|�@��y@���@�J@��#@�hs@�p�@�hs@�?}@��@���@��u@�  @��P@�dZ@�;d@�@��y@�ȴ@���G�O�@���@z��@sdZ@l|�@d��@]T�@S1�@K�+@D��@?�r@:�@5@-�"@)�@&�@��@I�@�9@�$@+@�.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�A��A�\)A�A�ffA���A���A��wA�x�A���A�O�A���A���A�(�A�t�A�A���A�VA�Q�A��mA�
=A�|�A��A��mA�G�A�VA�7LA�&�A��A���A�x�Aǝ�AǁA�XA�A���A�Q�A�bA���A�x�A���AǛ�A�ZA���A���A���A��A�1'A�bA���A���A���A�VA�$�A�7LA�z�A�ĜA�A���A��A���A�O�A�z�A�;dA�dZA��A�oA�33A�&�A�+A�`BA�bA�I�A�A�A�;dA�|�A�1A�K�A��A���A�K�AÑhA�x�A�{A�{A�\)A�O�A��A�ĜA�VA���A�-A���A�VA���A�1'A���AǅA�^5A��yA���A�+A�JA�{A�1AǅA��DA�  A���A��A���A���A�E�A���A�$�A�l�A�{A�
=A�\)A�ȴA�;dA��A��RA��9A��-AŲ-A�|�A�z�Aç�A�+A�n�A��RA��#A�O�A��Aǝ�A�ZA�+A��A���AǍPA��A���A�C�A�oA��RA��!A��/A��A�E�A��hA��+A��A��-A��\A��+A�ȴA��Aǉ7A�l�A��!A��jA�bAǏ\A���A��RA���A��mA�A��#A��A���AǏ\A�VA�ĜA��FA�jA�I�A�-A�x�A��
AŶFA�O�A�dZA�|�A���A�?}Aǉ7A�\)A���A��A�ƨA�ȴA��A��FA�/A��`A��A���A�p�A��A��mA�VA�ffA��A��A��`A�&�A���A�\)AǋDA�bNA���A���A�^5A���A�VA���A�
=A�&�AǑhAǡ�A�ffA�G�A�?}A��A�\)A�  A�S�A�n�A��Aǧ�A�ȴA��AǅAǧ�Aǡ�A�z�A���Aǝ�Aǡ�AǬAǡ�A�x�AčPAƼjA�x�Aǟ�AǬAǩ�A�A�AǛ�A��;A���A��Aǩ�Aǰ!Aǥ�Aǥ�Aǟ�A�VA���Aǟ�Aǝ�AǍPAǙ�Aǣ�Aǥ�A���Aǧ�A�dZA�-A�dZA�^5AǬA��Aǰ!AǬAǧ�A��Aǡ�Aǧ�Aǩ�A�Aǣ�Aǩ�Aǣ�A��A��
A�&�Aǣ�A��A�K�A��PAǡ�A��Aǧ�AǬAǬAǡ�Aǥ�AǬAǩ�Aǩ�A�Aǟ�A���A�bA�Q�Aǡ�A�A�Aǩ�Aǩ�Aǟ�Aǣ�AƗ�Aǣ�A��Aǣ�Aǣ�A��Aǣ�A�;dA��Aǡ�Aǥ�Aǝ�Aǧ�A�;dAǡ�A�r�A�bA�Aǝ�A�E�Aǡ�Aǥ�A�  AǓuAǡ�A�(�Aǣ�A�M�AǙ�Aǝ�A��+A��Aǣ�A�ZAǧ�Aǡ�A�A�Aǟ�Aǩ�A�z�A�K�A�t�Aǣ�AāA�bNAǡ�A�5?Aǥ�Aǩ�Aǥ�Aǡ�Aǩ�Aǡ�Aǥ�Aǣ�Aº^A���Aǧ�A�S�A�r�A�?}Aǥ�AǬAǩ�Aǩ�AþwAǣ�Aǧ�AǮAǧ�Aǩ�Aǥ�AŅAǇ+A�~�Aǣ�Aǣ�AǬAǏ\AǬAǩ�AǋDAǧ�Aǣ�Aǣ�A�t�A��AǕ�Aǥ�AǬAǡ�Aǝ�AǮA�"�AǇ+Aǝ�A�bNAǬAǧ�A�(�Aǣ�Aǩ�Aǧ�A��A�Aŗ�AǬAǧ�A�AǕ�Aǩ�AǅAǧ�A��yAǛ�A�O�Aǣ�AǮAǣ�A���AǕ�Aǧ�Aǥ�A���Aǥ�A��A�v�Aǣ�Aǧ�AǑhA�Aǩ�AǛ�Aǩ�AǬAǰ!AǬAǰ!Aǩ�AǬAǩ�Aǝ�Aǣ�AǍPA�-A���A�?}Aǣ�A���Aǩ�AǬA�C�Aǧ�Aǧ�A���Aǩ�A��;A�5?Aś�Aǩ�AǛ�A�^5Aǧ�Aǧ�Aǩ�Aǲ-AǮAǣ�AāAǥ�AǮAǬAǩ�A���AǓuAǥ�Aǧ�Aǟ�A�{Aǡ�A�=qA�+A�A�Aǣ�Aǥ�Aǥ�AǮAǬAǬAǬAǥ�AǬAǰ!Aǥ�Aǩ�AǬAǮAǬAǰ!Aǩ�Aǧ�AǮAǮAǩ�AǬAǬAǩ�AǬAǥ�Aǩ�AǮAǮAǩ�Aǥ�Aǥ�Aǧ�Aǩ�Aǣ�Aǩ�Aǧ�Aǡ�Aǡ�Aǡ�Aǥ�Aǡ�Aǧ�Aǩ�Aǧ�Aǣ�Aǣ�Aǥ�Aǡ�Aǣ�Aǣ�Aǣ�Aǣ�Aǣ�Aǥ�Aǣ�Aǣ�Aǥ�Aǣ�Aǝ�AǙ�AǛ�AǙ�AǛ�AǛ�AǛ�Aǟ�Aǡ�Aǡ�Aǣ�Aǣ�Aǣ�Aǟ�AǑhAǛ�AǍPAǍPAǋDAǉ7AǋDAǍPAǉ7AǋDAǓuAǕ�Aǝ�Aǝ�AǛ�AǛ�Aǝ�Aǟ�Aǡ�Aǡ�Aǡ�Aǣ�Aǟ�Aǝ�Aǝ�Aǝ�Aǥ�Aǡ�Aǡ�Aǥ�Aǥ�AǓuA�l�A�ffA�l�A�n�AǇ+Aǟ�Aǝ�Aǉ7AǁA�ffA�hsA�jA�ffA�jA�ffA�ffA�jA�n�A�n�A�l�A�ffA�ffA�jA�jA�hsA�ffA�jA�jA�jA�ffA�ffA�ffA�dZA�bNA�dZA�dZA�ffA�ffA�ffA�hsA�dZA�dZA�bNA�XA�ffA�O�A�7LA�5?A�5?A�5?A�33A�5?A�5?A�33A�33A�1'A�1'A�1'A�1'A�/A�/A�/A�+A�+A�-A�-A�&�A�&�A�$�A�"�A�$�A�$�A�"�A�"�A��A��A��A��A��A��A��A��A�{A�{A�{A�{A�oA�oA�JA�%A�1A�
=A�1A�
=A�1A�1A�1A�JA�JA�VA�JA�%A�1A�1A�1A�%A�%A�%A�1A�
=A�
=A�
=A�1A�1A�
=A�1@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�l�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�\)@�\)@�\)@�\)@�S�@�S�@�K�@�K�@�K�@�K�@�C�@�C�@�;d@�;d@�;d@�+@�+@�"�@��@��@�o@�o@�
=@�@�@�@�@�@�@���@���@�@���@���@���@���@���@���@��@��@��y@��y@��y@��H@��H@��@��H@��@��@��@���@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@���@���@���@���@��R@���@��R@��R@��!@���@��!@���@���@���@���@���@�v�@�v�@�v�@�v�@�~�@�~�@�~�@�ff@�ff@�^5@�^5@�^5@�^5@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�E�@�E�Aǡ�Aǡ�Aǡ�Aǥ�Aǡ�Aǧ�Aǩ�Aǧ�Aǣ�Aǣ�Aǥ�Aǡ�Aǣ�Aǣ�Aǣ�Aǣ�Aǣ�Aǥ�Aǣ�Aǣ�Aǥ�Aǣ�Aǝ�AǙ�AǛ�AǙ�AǛ�AǛ�AǛ�Aǟ�Aǡ�Aǡ�Aǣ�Aǣ�Aǣ�Aǟ�AǑhAǛ�AǍPAǍPAǋDAǉ7AǋDAǍPAǉ7AǋDAǓuAǕ�Aǝ�Aǝ�AǛ�AǛ�Aǝ�Aǟ�Aǡ�Aǡ�Aǡ�Aǣ�Aǟ�Aǝ�Aǝ�Aǝ�Aǥ�Aǡ�Aǡ�Aǥ�Aǥ�AǓuA�l�A�ffA�l�A�n�AǇ+Aǟ�Aǝ�Aǉ7AǁA�ffA�hsA�jA�ffA�jA�ffA�ffA�jA�n�A�n�A�l�A�ffA�ffA�jA�jA�hsA�ffA�jA�jA�jA�ffA�ffA�ffA�dZA�bNA�dZA�dZA�ffA�ffA�ffA�hsA�dZA�dZA�bNA�XA�ffA�O�A�7LA�5?A�5?A�5?A�33A�5?A�5?A�33A�33A�1'A�1'A�1'A�1'A�/A�/A�/A�+A�+A�-A�-A�&�A�&�A�$�A�"�A�$�A�$�A�"�A�"�A��A��A��A��A��A��A��A��A�{A�{A�{A�{A�oA�oA�JA�%A�1A�
=A�1A�
=A�1A�1A�1A�JA�JA�VA�JA�%A�1A�1A�1A�%A�%A�%A�1A�
=A�
=A�
=A�1A�1A�
=A�1@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�l�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�\)@�\)@�\)@�\)@�S�@�S�@�K�@�K�@�K�@�K�@�C�@�C�@�;d@�;d@�;d@�+@�+@�"�@��@��@�o@�o@�
=@�@�@�@�@�@�@���@���@�@���@���@���@���@���@���@��@��@��y@��y@��y@��H@��H@��@��H@��@��@��@���@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@���@���@���@���@��R@���@��R@��R@��!@���@��!@���@���@���@���@���@�v�@�v�@�v�@�v�@�~�@�~�@�~�@�ff@�ff@�^5@�^5@�^5@�^5@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�E�@�E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�܇=��=��-=��9>X��@�A =o�=�٩=��%=�x-@.��>��9>�?#��?T��@�D�=��N=�D�@��=t��=�-�>�=���=�d�=v��=���=��P=�Sz>3P	=��*=��j>���@�<@�5�?�m�=�=d3=��=��-=��>#^t?�S�@�1�@�#:=�o>9�=�\�=���=��?�5T>n�>a?�j=�i=�If=���=��p>�@�qL=?�[=P�o=csm=�t�=��4=!��=\)=�=2��=5��=D(N=K�=Q$J=_eV=��/=�A =W=�z:?gI�=n.�=���=��=���?�?��n=A*�=<j=Mt�=M�g=q6=�c�=��=���@Z4@��=�o=��=��>pȊ@�NQ@_�=i��=tI=~�f=�Ta=İ�>
�@�x>��=@�j=?��=^�=y=�=��E>�|=�">=�p�=�_1?h�>��=?�j=Tk'=|Z�=��k=��K=���>h?N5�?�T�=�Z�?K�q=I�=R�=t��=�#�=�Ri>-(�@�G@��=�s>�?t��@�P�?��>&�?��u@�Vm?G=xl"=f��=�'�=��%=�g=���?
�?�X:=���=��b>�U@p@�C-@�?�=�1f=���>*o?@�D�@�7L=pEx=���=��0=���=��=���@H�@�P�>Gl�>�?>��U=��>#�A?/��=�e>
gb?���=�8=��
=���=��>!�.@���@�r=�v6=��>vK=Ͽ�=���=�Z=��s=�J�>Bw�@�Tv>*	=��m>>�@_�.@�V�>��@Z�=��9>��o?��>}�@���=��>hI?xP@�S�?�t >q1f>dE$?bJ>�B�@�T�@�S�?��?E<@�Se>�6@���@I �@��@@�T�?�Ȋ@�Y6@�X:@Xd�@�W�@�W*@�V>r?��[@�W*@�X%@�Xd@�Vm?���@�U�@��@�U�@���@�Y@�X�@-*Z@�W @���>�4�?�_@�X�@�Y�@�X�@�V�@�W*>��>E��@�W~@�UG@�Q�@�Ta@�V�@�WT@G]�@�WT@�O7>�@~�@��@�Y6@�X�@�X�@�X�@�Xd@��@�X%@�X�@�WT@� G@��@�X@�W~@t�&@�VC@-Ŭ@�W*@�W*@) �>Yz%@�X?
J�@�Y�@�X�@�X�@�X�@�X�@�X�@�W�@�X?!��@�X?'�?�d?���@�W~?2/@�W~@�W~@�V@�W @��f@�V�@av�@�X%@�V�@pe@�V@��v@UR�@�W*@�V�@�V�@�T�@�Ta@�Uq@�T7@�Tv@d�z@�Vm@�V�@�W*@�V@�U�@�Ks@�U�@�t@�Ta@�|1@�Ta@�Ta?�Y6?�
@�UG@�R�@�W~@�V�@�C�@�W*@�W�?
�+@��@��@�Vm@�Ta@��@�V�?�\�@�W~@�V�@�Vm@�U�@�VC@�W @�W~@�V�@�Bp?�	@�W~@�W ?U>�?��@�Xd@�W�@�W�@�W�@1+�@�W @�W*@�X�@�WT@�X%@�W�@?�$@�Vm>I8@�W~@�X@�W~@�V@�X�@�X@�W*@�W�@�W�@�W*@�VC?_�N@�>W@�V�@�Uq@�R�@�Uq@�X%@�W�@�W�@�WT@�/0@�Y�@�X�@+�@�Vm@�X%@�W*@��>���?�l@�X�@�X�@�`�@�X�@�X%@���@�X@���@�W*@��@�Xd@�X�@�W�@�?h@�Xd@�X�@�X%?Lr\@�X%?���@U�@�Xd@�Xd@�W*>��@�Xd@�Uq@�Y6@�Y`@�Xd@�Y6@�Y6@�X@�U@�X�@�X�@�W�@�X�>@4n@e��?��@�X%>��@�X�@�Y`@�X�@�X�@�X%@�X@�X%@�V.@�V�?��?@�Xd@�W~@`(�@�X�@�X�@�Y@�X�@�X%@�X@�W�@�W�@�X�@�X�@�X�@�W?@��1@�X�@�X�@�X�>i{t@�W�@�S�>��j>we�@�X�@�X@�W~@�X�@�Y6@�Y�@�Y�@�Y�@�Y�@�Y`@�Y�@�ZG@�Zq@�Y�@�Y�@�ZG@�Zq@�Y�@�X�@�WT@�Y�@�X:@�Y�@�Y`@�Y�@�Y6@�Z�@�[@�Y6@�Y�@�Y�@�Y`@�ZG@�Y6@�Y@�X�@�ZG@�Y`@�Y�@�Y@�[�@�^@�]y@�^t@�^@�]@�\h@�\�@�\h@�\)@�]@�\@�\}@�]@�]�@�\�@�\�@�]y@�]@�\}@�Z\@�[-@�[l@�[l@�Y�@�[l@�]:@�]%@�]%@�]�@�]%@�\�@�[l@�Wi@�Z@�V�@�Vm@�VC@�V�@�V�@�V�@�Wi@�X�@�[�@�\@�[�@�]y@�\�@�Z�@�[�@�]%@�]�@�]%@�]O@�]%@�\�@�\)@�\}@�]�@�^�@�]�@�]%@�\�@�\}@�W�@�M�@�L�@�MU@�N�@�Q�@�Y�@�W�@�Oa@�P�@�J8@�K4@�K�@�I�@�J�@�J�@�J8@�LD@�M@�M@�M@�KI@�J�@�K�@�K�@�J�@�J�@�J�@�KI@�J�@�JM@�I�@�I�@�I�@�I=@�H�@�H�@�I=@�I=@�I�@�I�@�I=@�H�@�G�@�E�@�F5@�@�@�<�@�<�@�=@�<�@�<�@�<�@�<�@�<�@�=@�<�@�=@�<�@�<�@�<�@�<�@�<�@�<6@�<�@�<@�:*@�:�@�:�@�9�@�:*@�:*@�:*@�9C@�8@�6�@�6�@�7v@�6�@�5�@�6@�5�@�5�@�5@�4D@�4@�3�@�3�@�2�@�1�@�0�@�/�@�/�@�0@@�0@@�0+@�/�@�0@�1�@�1�@�1�@�1�@�0U@�0+@�/�@�0U@�0+@�0U@�0@�0�@�0�@�0�@�0�@�1Q@�1<@�0�@�1Q@P��@P�S@P�S@P�S@P�@P��@P��@P�@P��@P��@P�S@P�S@P�S@P��@P��@P��@P��@P��@P�@P�@P�X@P�.@P�@P�@P�2@P�2@P�2@P��@P�6@P��@P�@P��@P��@P�@P�?@P�C@P��@P�@P�@P��@P��@P�@P��@P܇@P�3@P�
@P�3@Pی@P��@Pی@Pی@Pی@P�8@Pی@P��@P�8@P��@P��@Pڐ@Pڐ@P��@Pٔ@Pؙ@P�E@P��@P��@P�I@P��@P��@P��@P֡@Pզ@Pզ@Pզ@P��@PԀ@PԀ@P��@P��@P�,@P��@P�@P��@P��@P��@P��@P��@P҉@Pэ@PБ@PБ@P�>@P�>@P�>@Pϖ@P�@P͟@P�.@P��@P��@P��@P�.@P��@P�2@P�;@P�;@P�?@P��@P�?@P��@P��@Pŗ@P�H@PĜ@P��@P�H@P�H@P��@Pà@P��@�W�@�W�@�W?@�Y@�X�@�Y`@�Z2@�Z@�X@�W�@�Y�@�X@�Xd@�X�@�X�@�X�@�Xd@�X�@�X�@�Y@�Y`@�Y@�W�@�U�@�V@�Vm@�V�@�U�@�U�@�X�@�X�@�X�@�Y�@�Y�@�YK@�X�@�T�@�XO@�Q�@�Q�@�Q�@�O�@�Q�@�Q�@�Q@�Q@�U\@�T7@�V�@�X�@�W?@�W?@�Xy@�Xy@�Y�@�Y�@�Y6@�Z�@�X�@�X�@�X�@�X�@�[-@�Z\@�Z�@�Z�@�[-@�VX@�F�@�F�@�I@�H@�KI@�Y�@�Y`@�QD@�S�@�F_@�E@�GZ@�F�@�FJ@�E�@�D�@�G�@�H,@�I(@�I{@�E�@�Ex@�Go@�G�@�GZ@�FJ@�G�@�GZ@�G�@�F5@�E�@�F�@�E�@�Ex@�E@�Ex@�E�@�E�@�E�@�G�@�F@�E�@�F@�@@�F�@�A�@�8�@�8\@�8q@�8q@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8q@�8q@�82@�82@�8q@�82@�6�@�5i@�5�@�5�@�6&@�6&@�5T@�4�@�2#@�2�@�3	@�2#@�1�@�28@�1�@�1Q@�0U@�/Z@�/E@�/E@�/E@�.�@�-�@�*�@�*�@�+,@�+k@�+�@�+@�+@�+�@�,=@�-#@�-w@�,�@�+@�+V@�+,@�+V@�+V@�+@�*�@�+,@�+�@�+�@�+�@�+�@�+�@�+k@�+�@P�z@P��@P݃@Pݭ@P�+@P�@P�+@P�+@P�@Pީ@P�@P�U@P�@P��@Pݭ@P݃@P݃@P݃@P�Y@P��@P�@P�@Pܱ@P܇@P�]@P�]@P�3@Pی@P��@P��@P��@P��@P�f@Pپ@P�@@P��@Pؙ@P��@P֡@P�|@PԀ@P�@Pӄ@P�@P�9@P�c@P�9@P�@P�@P�c@P�c@Pл@Pл@P��@Pл@Pл@PБ@P�h@P�h@P�>@P��@P�B@P�p@P�@P��@P�u@P��@P̣@P̣@P̣@P�%@P�%@P��@P��@Pʬ@P�.@P�.@P�@P��@Pɰ@P�6@P�6@P�`@P�`@P�`@Pȴ@PȊ@P�6@P��@P�@P�@P��@P�?@P�@P�@P��@P��@P��@P�Y@P��@P��@P��@P�Y@P�Y@P��@P�j@P�@P��@P�E@P��@P�E@P�E@P�@P�x@P��@P�x@P�N@P��@P�|@P�(G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        444443444444444344444444444444443344444444334444444444444434444444444444444444444444444444443344443444444434444444444444444444444444444433444344434444444444444334443344444443444444444444443344444444434443343444434443444443344343333433333344333343333334334433333443333334334333333333333333334334434333333334344434333333333333333333333333333334333344333333344333334333333333433443333433333343433333333333433333333333343333443333333333333333343433334333333333333343434333333333433333333333333333334334433333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�A!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�D�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�<@�5�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�1�@�#=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�qJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@Z2@��G�O�G�O�G�O�G�O�@�NOG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�G@��G�O�G�O�G�O�@�P�G�O�G�O�G�O�@�VlG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�C.@�?�G�O�G�O�G�O�@�D�@�7JG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@�rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�TwG�O�G�O�G�O�@_�5@�V�G�O�@Z�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�@�S�G�O�G�O�G�O�G�O�G�O�@�T�@�S�G�O�G�O�@�SfG�O�@���@I �@��>@�T�G�O�@�Y9@�X:@Xd�@�W�@�W.@�VG�O�G�O�@�W)@�X&@�Xf@�VqG�O�@�U�@��@�U�@���@�Y@�X�G�O�@�W@���G�O�G�O�@�X�@�Y�@�X�@�V�@�W,G�O�G�O�@�W�@�UF@�Q�@�Ta@�V�@�WSG�O�@�WT@�O7G�O�@~�@��@�Y5@�X�@�X�@�X�@�Xc@��@�X&@�X�@�WU@� J@��@�X@�W|@t�#@�V>G�O�@�W(@�W*G�O�G�O�@�XG�O�@�Y�@�X�@�X�@�X�@�X�@�X�@�W�@�XG�O�@�XG�O�G�O�G�O�@�W}G�O�@�W@�W@�V@�W@��f@�V�@av�@�X*@�V�@pe@�V@��x@US@�W+@�V�@�V�@�T�@�T`@�Uv@�T9@�Tx@d�z@�Vn@�V�@�W+@�V@�U�@�Kp@�U�G�O�@�Tb@�|4@�Ta@�TbG�O�G�O�@�UL@�R�@�W~@�V�@�C�@�W.@�W�G�O�G�O�@��@�Vk@�T^@��@�V�G�O�@�W~@�V�@�Vn@�U�@�VB@�V�@�W�@�V�@�BqG�O�@�W~@�W G�O�G�O�@�Xj@�W�@�W�@�W�G�O�@�W@�W,@�X�@�WT@�X&@�W�G�O�@�VnG�O�@�W~@�X@�W}@�V@�X�@�X@�W*@�W�@�W�@�W(@�VFG�O�@�>Y@�V�@�Uv@�R�@�Ur@�X'@�W�@�W�@�WU@�//@�Y�@�X�G�O�@�Vk@�X&@�W+@��G�O�G�O�@�X�@�X�@�`�@�X�@�X'@���@�X@���@�W+@��@�Xe@�X�@�W�@�?f@�X`@�X�@�X&G�O�@�X&G�O�@U�@�X`@�Xc@�W+G�O�@�Xf@�Uv@�Y4@�Y`@�Xf@�Y8@�Y4@�X@�U#@�X�@�X�@�W�@�X�G�O�@e�G�O�@�X'G�O�@�X�@�Y^@�X�@�X�@�X&@�X@�X'@�V.@�V�G�O�@�Xf@�W~@`(�@�X�@�X�@�Y@�X�@�X'@�X@�W�@�W�@�X�@�X�@�X�@�W@@��2@�X�@�X�@�X�G�O�@�W�@�S�G�O�G�O�@�X�@�X@�W�@�X�@�Y9@�Y�@�Y�@�Y�@�Y�@�Ya@�Y�@�ZG@�Zr@�Y�@�Y�@�ZJ@�Zv@�Y�@�X�@�WV@�Y�@�X>@�Y�@�Ya@�Y�@�Y5@�Z�@�[@�Y7@�Y�@�Y�@�Y^@�ZI@�Y4@�Y@�X�@�ZF@�W�@�W�@�W@@�Y@�X�@�Y`@�Z6@�Z@�X@�W�@�Y�@�X@�Xf@�X�@�X�@�X�@�Xe@�X�@�X�@�Y@�Yb@�Y@�W�@�U�@�V@�Vn@�V�@�U�@�U�@�X�@�X�@�X�@�Y�@�Y�@�YO@�X�@�T�@�XO@�Q�@�Q�@�Q�@�O�@�Q�@�Q�@�Q@�Q	@�UZ@�T9@�V�@�X�@�W@@�W>@�Xz@�X{@�Y�@�Y�@�Y8@�Z�@�X�@�X�@�X�@�X�@�[.@�Z]@�Z�@�Z�@�[.@�VU@�F�@�F�@�I@�H@�KH@�Y�@�Yc@�QB@�S�@�F`@�E@�G[@�F�@�FF@�E�@�D�@�G�@�H*@�I,@�I}@�E�@�Ev@�Gp@�G�@�Ga@�FF@�G�@�G^@�G�@�F8@�E�@�F�@�E�@�Ew@�E@�Ev@�E�@�E�@�E�@�G�@�F@�E�@�F@�@@�F�@�A�@�8�@�8\@�8s@�8p@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8r@�8s@�82@�85@�8t@�87@�6�@�5j@�5�@�5�@�6&@�6!@�5V@�4�@�2"@�2�@�3@�2"@�1�@�29@�1�@�1M@�0Z@�/Z@�/G@�/F@�/E@�.�@�-�@�*�@�*�@�+.@�+i@�+�@�+@�+@�+�@�,=@�-$@�-y@�,�@�+@�+V@�+1@�+V@�+V@�+@�*�@�+-@�+�@�+�@�+�@�+�@�+�@�+m@�+�@P�}@P��@P݂@Pݭ@P�*@P� @P�*@P�-@Pނ@Pު@P�~@P�V@P�@P��@Pݫ@P݃@P݃@P݆@P�V@P��@P�@P�@Pܰ@P܆@P�^@P�^@P�2@Pۋ@P��@P��@P��@P��@P�c@P��@P�E@P��@Pؘ@P��@P֠@P�z@Pԃ@P�@PӃ@P�@P�:@P�f@P�=@P�@P�@P�e@P�e@Pл@Pи@P��@Pн@Pи@PВ@P�f@P�f@P�=@P��@P�>@P�r@P�@P��@P�s@P��@P̦@P̢@P̣@P�%@P�&@P� @P��@Pʪ@P�0@P�0@P�@P��@Pɮ@P�6@P�8@P�b@P�`@P�b@Pȵ@PȆ@P�6@P��@P�@P�@P��@P�@@P�@P�@P��@P��@P��@P�Z@P��@P��@P��@P�Z@P�Z@P��@P�n@P�@P��@P�J@P��@P�@@P�F@P�@P�z@P��@P�v@P�M@P��@P�}@P�%@�W�@�W�@�W@@�Y@�X�@�Y`@�Z6@�Z@�X@�W�@�Y�@�X@�Xf@�X�@�X�@�X�@�Xe@�X�@�X�@�Y@�Yb@�Y@�W�@�U�@�V@�Vn@�V�@�U�@�U�@�X�@�X�@�X�@�Y�@�Y�@�YO@�X�@�T�@�XO@�Q�@�Q�@�Q�@�O�@�Q�@�Q�@�Q@�Q	@�UZ@�T9@�V�@�X�@�W@@�W>@�Xz@�X{@�Y�@�Y�@�Y8@�Z�@�X�@�X�@�X�@�X�@�[.@�Z]@�Z�@�Z�@�[.@�VU@�F�@�F�@�I@�H@�KH@�Y�@�Yc@�QB@�S�@�F`@�E@�G[@�F�@�FF@�E�@�D�@�G�@�H*@�I,@�I}@�E�@�Ev@�Gp@�G�@�Ga@�FF@�G�@�G^@�G�@�F8@�E�@�F�@�E�@�Ew@�E@�Ev@�E�@�E�@�E�@�G�@�F@�E�@�F@�@@�F�@�A�@�8�@�8\@�8s@�8p@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8r@�8s@�82@�85@�8t@�87@�6�@�5j@�5�@�5�@�6&@�6!@�5V@�4�@�2"@�2�@�3@�2"@�1�@�29@�1�@�1M@�0Z@�/Z@�/G@�/F@�/E@�.�@�-�@�*�@�*�@�+.@�+i@�+�@�+@�+@�+�@�,=@�-$@�-y@�,�@�+@�+V@�+1@�+V@�+V@�+@�*�@�+-@�+�@�+�@�+�@�+�@�+�@�+m@�+�@P�}@P��@P݂@Pݭ@P�*@P� @P�*@P�-@Pނ@Pު@P�~@P�V@P�@P��@Pݫ@P݃@P݃@P݆@P�V@P��@P�@P�@Pܰ@P܆@P�^@P�^@P�2@Pۋ@P��@P��@P��@P��@P�c@P��@P�E@P��@Pؘ@P��@P֠@P�z@Pԃ@P�@PӃ@P�@P�:@P�f@P�=@P�@P�@P�e@P�e@Pл@Pи@P��@Pн@Pи@PВ@P�f@P�f@P�=@P��@P�>@P�r@P�@P��@P�s@P��@P̦@P̢@P̣@P�%@P�&@P� @P��@Pʪ@P�0@P�0@P�@P��@Pɮ@P�6@P�8@P�b@P�`@P�b@Pȵ@PȆ@P�6@P��@P�@P�@P��@P�@@P�@P�@P��@P��@P��@P�Z@P��@P��@P��@P�Z@P�Z@P��@P�n@P�@P��@P�J@P��@P�@@P�F@P�@P�z@P��@P�v@P�M@P��@P�}@P�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        444443444444444344444444444444443344444444334444444444444434444444444444444444444444444444443344443444444434444444444444444444444444444433444344434444444444444334443344444443444444444444443344444444434443343444434443444443344343333433333344333343333334334433333443333334334333333333333333334334434333333334344434333333333333333333333333333334333344333333344333334333333333433443333433333343433333333333433333333333343333443333333333333333343433334333333333333343434333333333433333333333333333334334433333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�b 9�b 9�a�9�c�9�c!9�c�9�d�9�d�9�b�9�bw9�df9�b�9�b�9�ce9�c"9�ca9�b�9�c!9�cd9�c�9�c�9�c�9�bt9�`09�`�9�`�9�a9�`D9�_�9�c9�cd9�c9�d9�d9�c�9�cg9�_U9�b�9�[�9�\ 9�[�9�Z49�[�9�[�9�[K9�[N9�_�9�^�9�a79�c
9�a�9�a�9�b�9�b�9�d(9�d9�c�9�eQ9�c$9�c9�c9�cN9�e�9�d�9�e9�eT9�e�9�`�9�P�9�P�9�S9�R9�Ua9�dQ9�c�9�[�9�^?9�PS9�N�9�QV9�P�9�P89�O�9�N�9�Q�9�R+9�S59�S�9�O�9�Ob9�Qk9�Q�9�Q\9�P89�Q�9�QY9�Q�9�P*9�O�9�P�9�O�9�Oc9�N�9�Ob9�O�9�O�9�Ox9�Q�9�O�9�O�9�P 9�I�9�P|9�KU9�B9�A�9�A�9�A�9�B9�B9�B?9�B;9�BS9�B9�BQ9�B<9�BT9�B<9�A�9�A�9�A�9�A�9�A�9�A�9�@I9�>�9�?F9�?F9�?�9�?�9�>�9�>29�;z9�;�9�<g9�;z9�;)9�;�9�:�9�:�9�9�9�8�9�8�9�8�9�8�9�7�9�6�9�3�9�3�9�4Q9�4�9�4�9�4?9�4%9�4�9�5h9�6V9�6�9�5�9�4<9�4z9�4T9�4z9�4z9�4#9�49�4P9�59�4�9�4�9�4�9�59�4�9�4�9W �9W�9W�9W�9WJ9W9WJ9WM9W�9W�9W�9Ww9W"9W�9W�9W�9W�9W�9Wo9W�9W9W9W�9W�9Wp9Wp9WC9W�9W�9W�9W�9W�9Wf9W�9W?9W�9W�9W�9W�9WW9WY9W�9WQ9W�9W�9W$9W�9W�9W�9W#9W#9Wt9Wq9W�9Wv9Wq9WJ9W9W9W�9W�9W�9W9W�9Wj9W9W�9W@9W;9W<9W�9W�9W�9W�9W49W
�9W
�9W
�9W
b9W
19W�9W�9W�9W�9W�9W	09W	 9W�9W[9W}9W|9WQ9W�9Wy9W|9WV9W'9V��9W �9V��9V��9W 9W �9W �9V�9V��9V�19V��9V�^9V�9V�T9V�Z9V�19V��9V��9V�|9V�R9V�9V�|9V�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
B
B
B
ÖB
B
ÖB
ŢB
ŢB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B#�B;dBD�BM�B^5B`BBaHBdZBgmBffBdZBdZBm�Bw�B~�B�%B�{B��B��B�RBBƨB��B�B�sB�B�B��B%B+BhB�B�B0!B?}B_;Bm�B�+B��B��B��B��B��B�3B��BÖBƨBȴB��B�B�yBBbB�B�B �B#�B&�B&�B'�B,B!�B%B��B��B��B�sB��B�wB�?B��B��B�=B� Bk�BW
BH�B;dB8RB49B1'B!�BPB��B��B�B�sB�HB��B�B��B�BaHB?}B1'B�BJB
�ZB
ĜB
�XB
�LB
�B
�{B
iyB
O�B
C�B
1'B
)�B
�B
  B	�NB	��B	�RB	�B	��B	��B	��B	�%B	o�B	dZB	_;B	S�B	L�B	F�B	1'B	JB��B�B�BBƨB�^B�'B��B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B�uB�oB�hB�VB�bB�JB�=B�=B�JB�JB�JB�JB�DB�7B�PB�\B�\B�VB�\B�uB�uB�oB�{B�VB�1B�B� Bx�Bq�BgmBn�Br�Bx�Bu�BcTBM�BL�BK�BJ�BI�BH�BF�BF�BE�BE�BD�BD�BD�BC�BC�BC�BD�BC�BF�BJ�BL�BN�BO�BQ�BR�B`BBjBl�Bo�By�B� B�B� B� B�B�B�B�B�B�B�B� B~�B}�B|�B{�B{�B|�B}�B}�B|�B� B}�B|�Bz�Br�Be`BW
BL�BF�BA�B;dB9XB9XB8RB:^B;dB=qBA�BD�BF�BL�BT�BZB]/BaHBcTBffBk�Bl�Bk�Bk�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bp�Bp�Bo�Bl�Be`BaHB\)BZBW
BXB]/BaHBbNBbNBgmBk�BjBe`BffBx�Bz�By�Bw�Bx�By�B{�B}�B�B�B�+B�DB�PB�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�?B�?B�FB�FB�LB�XB�}BĜBŢBȴB��B��B��B��B�
B�#B�BB�B�B�B�B�B��B��B�B�B�B��B��B	B	%B	DB	\B	�B	�B	�B	�B	"�B	'�B	/B	49B	8RB	;dB	?}B	F�B	J�B	M�B	O�B	Q�B	VB	YB	^5B	aHB	dZB	dZB	ffB	gmB	iyB	k�B	m�B	r�B	|�B	�B	�B	�7B	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�?B	�LB	�XB	�dB	�dB	�dB	�jB	�jB	�wB	�wB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�#B	�/B	�5B	�5B	�;B	�HB	�NB	�TB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	��B
	RB
�B
]B
$�B
.�B
6�B
<�B
AoB
G+B
L�B
S�B
W�B
Z�B
a�B
e,B
i�B
mCB
q[B
v+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��?>�Nw>�U??�΅B
ŵ>��i>�W?>�}�?G(A�N�?�ӄ?>�@cl@�� B�T?
<?"�PAKB(>�<�>�]h?A�]>�~�>�[�>�]S>��>��5?�?lb�>�G?&�.?���B
��B
��A�?4�>�>y>�@>>�0�?��?U�bA>��B
��B
��?�?t>ג�>�p�?&n3AVf?�^�?*�AJ�>�-{>�[!>�E>��?I��B�>w5�>���>���>�)E>̏u>N�e>6�v>J�>eG�>i��>z�>�E�>��>��>�;�>��>���>�"h@��=>�Q�>�&�>���>�ri@W�#@���>u��>o &>�w}>��f>���>���>��?!ËA�R�B|�>�E>�x�?��?�jfB	��AW�W>�*�>�V�>��G>��? o�?5V�B2@'�'>w�+>t�*>�:~>�t�>Ñ�?@I>�| ? �@?��@[o�@,�>t�*>��V>���>�}�>�">�?,H�@��I@��?!A�@��
>�iY>���>�6�>ӓ%?�G?_�B
��B->�?,��@�۸B
�@լ�?-PAB�B
��@9�T>�G(>�2D>�6�>���>�(�?��@<t�A	��>�<�?��?AәAg`=B
�BB
�>�Ĥ?�?^�wB
��A��>�i>��l>��q>ȩ�>ӧ?��A��B
Č?��?E3	?�
?
p?Ry@x�?�3?1��A?�>ُ�>��>��?&�?U��A�`�Be�>��>>��j?9L4?�>���?Qr>�>��?|��B?eg>���?>'JA�fB
��?<�A��?!�E?�_�@Y��?FJ�B�d>��+?2�T@���B
�A1�?���?�J�@��7?���B
ǟB
�@<w�@��dB
�?+�VB��A�I�A��B
ԬA,�B
��B�A�׍B
��B
��B
�@?B��A v�B
��B
�AB
�|B
¡@૕B�(A�S�B
��B
Z$B
�B
��A��B
ŎB	;}@��@�isB
��B
�(B
�UB
�oB
�?ڨ�?~bVB
�oB
�$B
�[B
��B
�9B
��A�6YB
�B
Ӑ?5b9A�)�B��B
�OB�B
�[B
��B
�B�bB
�AB
B
�@A�kB
|�B
� B
��AŞ�Bs�A��B
BA�E�?� �B
�&@;�-B
�0B
��B
��B
��B
�UB
��B
��B
� @]�:B
��@nY�A��@НoB
Þ@x�/B
��B
��B
�B
�hB	�B
�AA��B
�pB
�9A�fB
��B�A�?+B
�PB
�xB
�nB
��B
�%B
��B
ҸB
��A�g�B
�4B
�UB
�PB
��B HB
��B
�Am�B
��B
�B
��B
�VAMA��B
��B
ڞB
�JB
��B
�2B
�"B
��@=1]A]�3B �%B
��B��A��DB
��A?��B
�B
��B
�B
�B
�]B
�)B
�B
�0B��@G��B
�JB
�@�deA+B
��B
��B
��B
��A���B
�hB
��B
��B
�B
�B
�YA�9B
��?��JB
��B
�dB
��B
�iB
��B
� B
��B
�qB
�"B
B
��@���B
�B
�oB
��B
�]B
�KB
��BW�B
�/B
�
A�  B
��B
�6A��9B
��B
�B
��B�k?�;2A!$:B
��B
�>B�vB
�dB
�Bs�B
��B
�B
ŵA�v�B
êB
��B
�"B/�B
�5B
�>B
¦@���B
¦@�<\A�Y�B
òB
�B
ɜ?�ˊB
�NB
�B
�B
�eB
��B
�GB
��B
��B
�rB
�lB
�%B
��B
�y?x�A��o@ĕDB
�x?���B
��B
�vB
��B
B
��B'B
�BKB
�A��B
�NB
�A��B
�FB
�eB
��B
�OB
��B
�dB��B
�aB
��B
��B
�uB!A�[�B
��B
�eB
�d?��B
��BI�?���?�}B
�/B
B
�B
�'B
�?B
��B
��B
��B
��B
��B
��B
��B
�9B
��B
�fB
�fB
��B
�6B
��B
�cB
�!B
��B
�<B
��B
�yB
�B
��B
��B
��B
�"B
ÆB
�$B
�(B
�ZB
ÇB
�B
�B
�`B
��B
�UB
�B
�.B
�B
�B
�B
æB
�9B
B
� B
�9B
¢B
�zB
ÔB
�KB
�$B
�B
�B
�zB
�~B
��B
ÜB
�|B
�ZB
�xB
�~B
ƻB
æB
¹B
�zB
�B
��B
�>B
��B
�:B
�B
�	B
ĻB
�UB
��B
��B
�<B
ĩB
�WB
�B
B
�2B
ċB
�LB
��B
�.B
�yB
�3B
��B
�HB
�PB
B
��B
�&B
��B
��B
��B
�3B
��B
��B
�B
��B
ġB
��B
�B
�B
��B
�B
�bB
��B
��B
�B
�0B
ŏB
�YB
��B
�B
��B
�wB
��B
ƝB
ƇB
��B
��B
��B
�}B
�>B
��B
�sB
��B
�yB
�B
�B
��B
ŰB
�QB
�B
ŏB
�GB
�HB
��B
��B
ĹB
�CB
ˑB
��B
��B
�`B
˭B
̕B
˜B
�UB
�&B
�DB
��B
�SB
��B
�B
��B
�SB
�^B
�B
��B
�SB
άB
�RB
ΩB
�OB
�B
��B
�IB
ͥB
�0B
�IB
��B
��B
ϩB
ϚB
ϑB
ϣB
��B
��B
�[B
ίB
�AB
�,B
ίB
϶B
�TB
�B
��B
��B
ϳB
�BB
��B
��B
�fB
�B
�SB
��B
��B
ΖB
�}B
τB
�9B
ϒB
�$B
�CB
�bB
ϮB
ϦB
�LB
��B
��B
��B
ϺB
��B	�B	�@B	�CB	�6B	�YB	��B	��B	�$B	�UB	�GB	��B	��B	��B	�]B	�PB	�(B	�9B	�B	�YB	�LB	�B	�SB	��B	��B	�B	�YB	�\B	�B	�B	�BB	��B	�'B	�eB	�B	��B	�B	��B	�B	��B	�B	�B	��B	�B	�B	�lB	�QB	�UB	��B	��B	�B	�B	�B	�@B	�dB	��B	�B	��B	�B	�lB	�_B	��B	�B	��B	�B	�7B	�;B	�B	�ZB	�MB	�3B	��B	�3B	�)B	�B	�B	�:B	� B	�PB	�B	�B	�B	��B	�B	�B	�yB	�oB	�bB	�B	�QB	�B	�B	�*B	�-B	� B	�B	�!B	��B	��B	�B	�iB	�KB	�B	��B	�B	�BB	�4B	�nB	�8B	�WB	�B	��B	�B	�B	��B	�`B	�B	�vB	�+B	��B	�iB
��B
��B
�sB
B
íB
��B
��B
B
�*B
�B
��B
��B
�NB
²B
�pB
B
�B
��B
�xB
B
�
B
�~B
��B
�KB
��B
��B
�#B
�kB
�B
�AB
µB
�_B
�nB
�fB
�#B
�MB
�B
�?B
�cB
àB
�B
�cB
�B
�<B
�?B
�mB
�=B
�[B
�B
�#B
¥B
B
��B
�B
�SB
�7B
��B
�xB
�	B
¯B
¦B
��B
��B
B
µB
�TB
��B
�>B
ĕB
��B
��B
��B
�|B
�tB
��B
�%B
ȱB
�TB
�KB
řB
ƉB
ČB
ŭB
��B
ŶB
ďB
�pB
�B
ŇB
�B
�9B
ŒB
��B
ţB
�?B
��B
�`B
�fB
��B
��B
ŚB
�!B
��B
�>B
ħB
��B
�gB
ŏB
ŌB
�6B
�=B
ęB
�B
�B
�B
ʥB
ʰB
ʨB
�tB
ʢB
��B
�zB
˅B
�B
�6B
�B
�B
��B
̀B
�xB
��B
͸B
� B
��B
��B
̌B
ͧB
�hB
��B
��B
��B
�9B
�B
̓B
ΦB
ΉB
�3B
΄B
��B
͝B
�6B
�DB
� B
�B
��B
�B
΅B
�B
�}B
��B
��B
�LB
͆B
�jB
��B
��B
ͧB
�#B
�-B
��B
�eB
�-B
�LB
�B
ͯB
͓B
��B
��B
̬B
�}B
�fB
�iB
�"B
�B	��B	�B	�SB	�eB	�B	�{B	�B	�rB	�B	�B	�{B	�`B	�B	��B	��B	�B	�B	�zB	�AB	��B	��B	��B	�B	�hB	�<B	�"B	�B	�~B	�B	��B	��B	��B	�kB	��B	�B	�B	��B	��B	�}B	�B	��B	�|B	�B	��B	�]B	�B	�FB	�B	�B	�0B	�#B	�B	�B	�B	�wB	�jB	�0B	�B	��B	��B	�B	��B	�eB	�B	��B	�B	�B	��B	�B	�B	�FB	�8B	�WB	�JB	��B	�B	�B	�`B	�4B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	��B	��B	�B	��B	�B	�B	�dB	�8B	�bB	�|B	�;B	��B	��B	�)B	�B	�B	�CB	�	B	��B	�HB	��B	� B	�#B	��B	�B	��B	�B	�fB	�B	��B	�xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444443444444444344444444444444443344444444334444444444444434444444444444444444444444444444443344443444444434444444444444444444444444444433444344434444444444444334443344444443444444444444443344444444434443343444434443444443344343333433333344333343333334334433333443333334334333333333333333334334434333333334344434333333333333333333333333333334333344333333344333334333333333433443333433333343433333333333433333333333343333443333333333333333343433334333333333333343434333333333433333333333333333334334433333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999B
B
B
B
ÖB
B
ØB
ťB
ţB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B#�B;cBD�BM�B^8B`DBaKBd\BgmBfjBd[Bd]Bm�Bw�B~�B�&B�}B��B��B�SBBƩB��B�B�tB�B�B��B(B.BlB�B�B0 B?�B_;Bm�B�.B��B��B��B��B��B�3B��BÙBƫBȶB��B�B�}BBbB�B�B �B#�B&�B&�B'�B,	B!�B'B��B��B��B�uB��B�wB�BB��B��B�>B� Bk�BWBH�B;fB8SB4:B1&B!�BNB��B��B�B�tB�JB��B�B��B�BaKB?B1*B�BNB
�[B
ĝB
�ZB
�PB
�B
�{B
i{B
O�B
C�B
1*B
)�B
�B
 B	�QB	��B	�WB	�B	��B	��B	��B	�(B	o�B	d\B	_@B	S�B	L�B	F�B	1(B	LB��B�B�GBƪB�bB�*B��B��B��B��B��B��B��B�xB��B��B��B��B��B��B��B��B�xB�rB�kB�UB�dB�KB�?B�@B�OB�LB�KB�KB�GB�9B�QB�^B�`B�WB�]B�xB�wB�rB�|B�YB�2B�B�Bx�Bq�BgnBn�Br�Bx�Bu�BcWBM�BL�BK�BJ�BI�BH�BF�BF�BE�BE�BD�BD�BD�BC�BC�BC�BD�BC�BF�BJ�BL�BN�BO�BQ�BR�B`FBj�Bl�Bo�By�B�B�B�B�B�B�B�B�B�B�B�B�B~�B}�B|�B{�B{�B|�B}�B}�B|�B�B}�B|�Bz�Br�BebBWBL�BF�BA�B;gB9ZB9ZB8UB:`B;fB=rBA�BD�BF�BL�BT�BZ!B]0BaKBcXBfhBk�Bl�Bk�Bk�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bp�Bp�Bo�Bl�BebBaIB\,BZBWBXB]3BaKBbQBbPBgoBk�BjBebBfiBx�Bz�By�Bw�Bx�By�B{�B}�B�
B�B�.B�EB�PB�jB�qB�~B��B��B��B��B��B��B��B��B��B�B�B�B�B�#B�DB�AB�HB�GB�PB�[B�BĠBŤBȷB��B��B��B��B�B�$B�DB�B�B�B�B�B��B��B�B�B�B��B��B	B	)B	FB	_B	�B	�B	�B	�B	"�B	'�B	/B	4=B	8VB	;eB	?�B	F�B	J�B	M�B	O�B	Q�B	V	B	YB	^7B	aJB	d\B	d\B	fhB	gqB	i}B	k�B	m�B	r�B	|�B	�B	�"B	�:B	�YB	�^B	�bB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�$B	�0B	�AB	�MB	�YB	�fB	�hB	�hB	�lB	�nB	�zB	�{B	�B	��B	��B	B	ØB	ÕB	ÙB	ţB	ȵB	ɽB	ɾB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�&B	�+B	�-B	�&B	�2B	�9B	�8B	�>B	�IB	�PB	�UB	�dB	�hB	�oB	�tB	�vB	�wB	�wG�O�B	��B	��B
	WB
�B
^B
$�B
.�B
6�B
<�B
AqB
G-B
L�B
S�B
W�B
Z�B
a�B
e/B
jB
mEB
q_B
v-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ŷG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�UG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�R�B|�G�O�G�O�G�O�G�O�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B0G�O�G�O�G�O�B
�G�O�G�O�G�O�B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�CB
�G�O�G�O�G�O�B
��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ĎG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`�Be�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�A�fB
��G�O�A��G�O�G�O�G�O�G�O�B�dG�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�B
ǢB
�G�O�G�O�B
�G�O�B��A�I�A��B
ԭG�O�B
��B�A�׌B
��B
��B
�BG�O�G�O�B
��B
�BB
�~B
¤G�O�B�(A�S�B
��B
Z'B
�B
��G�O�B
ŐB	;�G�O�G�O�B
��B
�-B
�XB
�nB
�G�O�G�O�B
�sB
�$B
�^B
��B
�<B
��G�O�B
�B
ӐG�O�A�)�B��B
�QB�B
�\B
��B
�B�dB
�BB
B
�AA�kB
|�B
�B
��AŞ�Bs�G�O�B
BG�O�G�O�B
�)G�O�B
�0B
��B
��B
��B
�XB
��B
��B
�G�O�B
��G�O�G�O�G�O�B
ßG�O�B
��B
��B
�B
�kB	�B
�BA��B
�tB
�<A�fB
��B�A�?0B
�SB
�|B
�pB
��B
�&B
��B
ҹB
��A�g�B
�8B
�VB
�SB
��B JB
��B
�G�O�B
��B
�B
��B
�XG�O�G�O�B
��B
ڠB
�KB
��B
�5B
�%B
��G�O�G�O�B �)B
��B��A��EB
��G�O�B
�B
��B
�B
�B
�^B
�)B
�B
�2B��G�O�B
�KB
�G�O�G�O�B
��B
��B
��B
��G�O�B
�kB
��B
��B
�B
�B
�[G�O�B
��G�O�B
��B
�eB
��B
�jB
��B
�B
��B
�qB
�$B
B
��G�O�B
�B
�nB
��B
�bB
�MB
��BW�B
�/B
�A� #B
��B
�9G�O�B
��B
�B
��B�mG�O�G�O�B
��B
�@B�vB
�iB
�Bs�B
��B
�B
ŶA�v�B
íB
��B
�$B/�B
�3B
�@B
¨G�O�B
¨G�O�A�Y�B
ðB
�B
ɞG�O�B
�OB
�B
�B
�gB
��B
�HB
��B
��B
�vB
�jB
�(B
��B
�{G�O�A��rG�O�B
�{G�O�B
��B
�uB
��B
B
��B*B
�BMB
�G�O�B
�OB
�A��B
�GB
�gB
��B
�QB
��B
�eB��B
�dB
��B
��B
�wB#A�[�B
� B
�gB
�fG�O�B
��BI�G�O�G�O�B
�2B
B
�B
�&B
�BB
��B
��B
��B
��B
��B
��B
��B
�:B
��B
�jB
�iB
��B
�7B
��B
�fB
�B
��B
�>B
��B
�yB
�B
��B
��B
��B
�"B
ÈB
�#B
�)B
�ZB
ÊB
�B
�	B
��B
��B
�tB
B
îB
��B
��B
B
�*B
�B
��B
��B
�PB
µB
�qB
B
�B
��B
�{B
B
�B
B
��B
�LB
��B
��B
�$B
�nB
�B
�CB
¶B
�`B
�rB
�hB
�(B
�OB
�B
�?B
�bB
àB
�B
�eB
�B
�<B
�@B
�pB
�=B
�^B
��B
�$B
§B
B
��B
�B
�TB
�9B
��B
�yB
�B
²B
¨B
��B
��B
 B
¹B
�VB
��B
�?B
ĖB
��B
��B
��B
�|B
�vB
��B
�&B
ȲB
�VB
�NB
ŚB
ƉB
ČB
ŮB
��B
źB
ďB
�sB
ƁB
ŉB
� B
�;B
őB
��B
ţB
�CB
��B
�aB
�iB
��B
��B
ŜB
�!B
��B
�>B
ĩB
��B
�fB
ŒB
ōB
�9B
�BB
ĜB
�B
�B
�B
ʦB
ʴB
ʨB
�xB
ʤB
��B
�zB
ˉB
�B
�6B
�B
�B
��B
́B
�{B
��B
ͻB
�$B
��B
��B
̎B
ͪB
�jB
��B
��B
��B
�=B
� B
̈́B
ΨB
ΉB
�8B
ΆB
��B
͜B
�:B
�FB
�"B
�B
��B
�B
ΆB
�B
�B
��B
��B
�MB
͊B
�nB
��B
��B
ͨB
�$B
�/B
��B
�fB
�1B
�MB
�B
ͰB
͔B
��B
��B
̬B
�}B
�fB
�kB
�#B
� B	��B	�B	�VB	�hB	�B	�~B	�B	�vB	�B	�B	�~B	�cB	�B	��B	��B	�B	�B	�~B	�BB	��B	��B	��B	�B	�jB	�>B	�%B	�B	�B	�	B	��B	��B	��B	�lB	��B	�B	�B	��B	��B	�~B	�B	��B	�B	�B	��B	�_B	�B	�JB	�B	�B	�2B	�&B	�B	�B	�B	�zB	�lB	�1B	�B	��B	��B	�B	��B	�hB	�B	��B	�B	�B	��B	�B	�B	�GB	�<B	�[B	�MB	� B	�B	�B	�aB	�7B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�<B	��B	��B	�B	��B	�B	�B	�hB	�<B	�aB	�B	�@B	��B	��B	�,B	�B	�B	�GB	�B	��B	�LB	��B	�B	�&B	��B	�B	��B	�B	�hB	�B	��B	�xB
��B
��B
�tB
B
îB
��B
��B
B
�*B
�B
��B
��B
�PB
µB
�qB
B
�B
��B
�{B
B
�B
B
��B
�LB
��B
��B
�$B
�nB
�B
�CB
¶B
�`B
�rB
�hB
�(B
�OB
�B
�?B
�bB
àB
�B
�eB
�B
�<B
�@B
�pB
�=B
�^B
��B
�$B
§B
B
��B
�B
�TB
�9B
��B
�yB
�B
²B
¨B
��B
��B
 B
¹B
�VB
��B
�?B
ĖB
��B
��B
��B
�|B
�vB
��B
�&B
ȲB
�VB
�NB
ŚB
ƉB
ČB
ŮB
��B
źB
ďB
�sB
ƁB
ŉB
� B
�;B
őB
��B
ţB
�CB
��B
�aB
�iB
��B
��B
ŜB
�!B
��B
�>B
ĩB
��B
�fB
ŒB
ōB
�9B
�BB
ĜB
�B
�B
�B
ʦB
ʴB
ʨB
�xB
ʤB
��B
�zB
ˉB
�B
�6B
�B
�B
��B
́B
�{B
��B
ͻB
�$B
��B
��B
̎B
ͪB
�jB
��B
��B
��B
�=B
� B
̈́B
ΨB
ΉB
�8B
ΆB
��B
͜B
�:B
�FB
�"B
�B
��B
�B
ΆB
�B
�B
��B
��B
�MB
͊B
�nB
��B
��B
ͨB
�$B
�/B
��B
�fB
�1B
�MB
�B
ͰB
͔B
��B
��B
̬B
�}B
�fB
�kB
�#B
� B	��B	�B	�VB	�hB	�B	�~B	�B	�vB	�B	�B	�~B	�cB	�B	��B	��B	�B	�B	�~B	�BB	��B	��B	��B	�B	�jB	�>B	�%B	�B	�B	�	B	��B	��B	��B	�lB	��B	�B	�B	��B	��B	�~B	�B	��B	�B	�B	��B	�_B	�B	�JB	�B	�B	�2B	�&B	�B	�B	�B	�zB	�lB	�1B	�B	��B	��B	�B	��B	�hB	�B	��B	�B	�B	��B	�B	�B	�GB	�<B	�[B	�MB	� B	�B	�B	�aB	�7B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�<B	��B	��B	�B	��B	�B	�B	�hB	�<B	�aB	�B	�@B	��B	��B	�,B	�B	�B	�GB	�B	��B	�LB	��B	�B	�&B	��B	�B	��B	�B	�hB	�B	��B	�xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444443444444444344444444444444443344444444334444444444444434444444444444444444444444444444443344443444444434444444444444444444444444444433444344434444444444444334443344444443444444444444443344444444434443343444434443444443344343333433333344333343333334334433333443333334334333333333333333334334434333333334344434333333333333333333333333333334333344333333344333334333333333433443333433333343433333333333433333333333343333443333333333333333343433334333333333333343434333333333433333333333333333334334433333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455052020082814550520200828145505202008281455052020082814550520200828145505202008281455052020082814550520200828145505202008281455052020082814550520200828145505AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730372019021417303720190214173037    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730372019021417303720190214173037  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730372019021417303720190214173037  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455052020082814550520200828145505  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                