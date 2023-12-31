CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  l   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:47Z creation      
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
resolution        =���   axis      Z        )  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D  n    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )  xd   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D  �t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D 1,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) ;p   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ) d�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ) ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ) �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) �8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) '�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D P�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) Z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173047  20200828145528  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               K   K   KAAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @���f@���f@���f111 @��[�P@��[�P@��[�P@8C��%@8C��%@8C��%�c2���m�c2���m�c2���m111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    K   K   KADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�  B�33B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dy}qD�qD�9�D��qD���D�3D�7
D��D���D�{D�FD�~�DǷ
D� �D�S�D�{�D��3D���D�L�D�h�D��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�            >���>���=���        =���    =���>L��=���            =���=���    =���=���=���    =���    =���>L��        >���=���        =���    =���>L��>���        =���=���=���    =���    =���>���>L��    =���>L��=���=���        =���            =���=���=���=���    =���=���        =���>���=���    =���    =���>L��    =���                >L��        >L��=���            =���        =���>L��    >L��>���>L��    =���>L��>���=���    >L��>L��=���        >L��=���    =���>���>L��        >L��=���                =���>L��        =���=���        >L��    =���    =���=���    =���>���>���        =���=���            =���=���            >���>L��    >L��=���    =���=���    =���>L��>L��    >L��>L��>���        =���>L��    >L��=���            =���>L��=���    >L��>L��=���            >���>���=���=���    =���        =���>L��>���>L��=���>L��>L��>���>L��=���=���=���>L��=���=���=���=���>L��=���>L��>L��>L��>���>L��=���>L��=���>L��>���>L��>L��>���    >L��>L��>L��>L��>���>���>L��=���>L��>���>���>���>���>���>���>���>���>���>L��>���>���>���>L��>���>���>���>���>L��>���>���>���>L��>L��>���>���>���>L��>L��>���>���>L��>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>L��>L��>L��>���>L��>���>���>���>���>���>���>���>L��>L��>���>L��>���>���>���>L��>���>L��>L��>���>���>���>���>���>���>���>���>L��>L��>���>L��>���>L��>���>���>���>L��>���>���>���>���>���>���>���>���>���>L��>���>L��>���>���>���>���>���>L��>���>���>���>���>���>L��>L��>���>���>���>���>���>L��>���>L��>���>���=���>���>���>L��>���>���>L��>���>���>L��>���>���>���>���>���>���>���>���>L��>���>L��>���>���>���>���>���?   >L��>���>���>���>���>���>���>���>���>���>L��=���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>L��>���>���>���>���>L��>���>���>���>���>L��>���>L��>���>���>���>���>L��>���>���>L��>���>���>L��>���>L��>L��>���>L��>���>���>���>���>���>L��>L��>���>���>���>���>���>���>���>L��>���>���>���>L��>L��>���>���>���>���>���>���>���>���>���>���>L��>���>L��>L��>���>���>���>���>���>���>L��>���>L��>���>���>���>L��>���>���>���>���>���>L��>���>���>L��>L��>L��>���>���>���>���?   ?   ?��?��?333?333?fff?fff?fff?�  ?���?���?���?���?�ff?�ff?�33?�  ?�  ?���?ٙ�?�ff?�33?�33@   @ff@ff@��@33@��@   @&ff@&ff@,��@333@333@9��@Fff@L��@S33@Y��@fff@l��@s33@y��@�33@�ff@���@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@ə�@���@�33@�ff@ٙ�@���@�  @�33@陚@���@�  @�33@�ff@���@���A��A33A��AffA  A33A��AffA  A33A��AffA  A��A��AffA   A!��A#33A$��A(  A)��A+33A,��A.ffA1��A333A4��A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAQ��AS33AT��AVffAX  AY��A\��A^ffA`  Ac33Ad��AfffAh  Ai��Ak33Al��Ap  Aq��As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  Aə�A�ffA�  A���A�ffA�33A�  Aљ�A�ffA�33A���Aՙ�A�ffA�  A���A�ffA�33A�  Aݙ�A�ffA�33Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrS3DrY�Dr` DrffDrs3Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr�fDr��Dr��Ds  DsfDs�Ds3Ds�Ds&fDs,�Ds33Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt��@9��@Fff@L��@S33@Y��@fff@l��@s33@y��@�33@�ff@���@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@ə�@���@�33@�ff@ٙ�@���@�  @�33@陚@���@�  @�33@�ff@���@���A��A33A��AffA  A33A��AffA  A33A��AffA  A��A��AffA   A!��A#33A$��A(  A)��A+33A,��A.ffA1��A333A4��A6ffA8  A9��A<��A>ffA@  AA��AC33AFffAH  AI��AK33AL��ANffAQ��AS33AT��AVffAX  AY��A\��A^ffA`  Ac33Ad��AfffAh  Ai��Ak33Al��Ap  Aq��As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  Aə�A�ffA�  A���A�ffA�33A�  Aљ�A�ffA�33A���Aՙ�A�ffA�  A���A�ffA�33A�  Aݙ�A�ffA�33Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrS3DrY�Dr` DrffDrs3Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr�fDr��Dr��Ds  DsfDs�Ds3Ds�Ds&fDs,�Ds33Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@5@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA�Q�A�A��BBBB (�B'B/B7B?BGBOBWB_Bh(�BoBw\)BB��HB��HB�{B��B��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C
>C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CH
>CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dqu�Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dyy�D��D�7�D���D���D�	GD�5D��(D���D��D�D(D�|�DǵD���D�Q�D�y�D��GD�� D�J�D�f�D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��u�u�u>u>u=#��u�u=#��u=#�>\)=#��u�u�u=#�=#��u=#�=#�=#��u=#��u=#�>\)�u�u>u=#��u�u=#��u=#�>\)>u�u�u=#�=#�=#��u=#��u=#�>u>\)�u=#�>\)=#�=#��u�u=#��u�u�u=#�=#�=#�=#��u=#�=#��u�u=#�>u=#��u=#��u=#�>\)�u=#��u�u�u�u>\)�u�u>\)=#��u�u�u=#��u�u=#�>\)�u>\)>u>\)�u=#�>\)>u=#��u>\)>\)=#��u�u>\)=#��u=#�>u>\)�u�u>\)=#��u�u�u�u=#�>\)�u�u=#�=#��u�u>\)�u=#��u=#�=#��u=#�>�{>u�u�u=#�=#��u�u�u=#�=#��u�u�u>u>\)�u>\)=#��u=#�=#��u=#�>\)>\)�u>\)>\)>u�u�u=#�>\)�u>\)=#��u�u�u=#�>\)=#��u>\)>\)=#��u�u�u>u>u=#�=#��u=#��u�u=#�>\)>u>\)=#�>\)>\)>u>\)=#�=#�=#�>\)=#�=#�=#�=#�>\)=#�>\)>\)>\)>u>\)=#�>\)=#�>\)>u>\)>\)>u�u>\)>\)>\)>\)>u>u>\)=#�>\)>u>u>u>u>u>u>�{>�{>u>\)>u>u>u>\)>u>�{>u>u>\)>u>u>�{>\)>\)>u>u>u>\)>\)>u>�{>\)>u>�{>u>u>u>u>u>u>u>\)>u>u>u>�{>u>�{>u>�{>u>�{>�{>�{>\)>\)>\)>u>\)>u>u>�{>�{>u>u>u>\)>\)>u>\)>u>u>u>\)>�{>\)>\)>u>u>u>�{>�{>�{>�{>u>\)>\)>u>\)>u>\)>u>u>u>\)>u>�{>u>u>u>�{>�{>u>u>\)>u>\)>u>u>u>u>u>\)>u>u>u>�{>u>\)>\)>u>u>u>u>u>\)>u>\)>u>u=#�>u>u>\)>�{>u>\)>u>u>\)>u>u>u>u>u>u>u>u>\)>u>\)>u>�{>u>�{>u>�G�>\)>u>u>�{>u>�{>�{>�{>�{>u>\)=#�>\)>u>u>u>u>u>u>u>u>u>�{>u>�{>u>u>u>\)>u>u>\)>u>u>�{>u>\)>u>�{>u>u>\)>�{>\)>u>�{>�{>�{>\)>u>u>\)>�{>u>\)>u>\)>\)>u>\)>u>u>u>�{>u>\)>\)>u>u>u>u>u>�{>u>\)>u>u>u>\)>\)>�{>�{>u>u>u>�{>u>�{>u>�{>\)>u>\)>\)>u>u>u>u>u>u>\)>u>\)>u>u>�{>\)>u>u>u>�{>u>\)>u>u>\)>\)>\)>u>u>�{>�{>�G�>�G�?
=q?
=q?#�
?#�
?W
=?W
=?W
=?p��?��?��?��?��?��R?��R?��?�Q�?�Q�?��?��?޸R?�?�?�Q�@�\@�\@��@\)@@(�@"�\@"�\@(��@/\)@/\)@5@B�\@H��@O\)@U@b�\@h��@o\)@u@�G�@�z�@��@��H@�{@�G�@�z�@��H@�{@�G�@�z�@��@�{@�G�@�z�@��@�{@�G�@�z�@Ǯ@��H@�G�@�z�@׮@��H@�{@�G�@�@��H@�{@�G�@�z�@��@��HA ��A=pA�
Ap�A
=A
=pA�
Ap�A
=A=pA�
Ap�A
=A��A�
Ap�A
=A ��A"=pA#�
A'
=A(��A*=pA+�
A-p�A0��A2=pA3�
A5p�A7
=A8��A;�
A=p�A?
=A@��AB=pAEp�AG
=AH��AJ=pAK�
AMp�AP��AR=pAS�
AUp�AW
=AX��A[�
A]p�A_
=Ab=pAc�
Aep�Ag
=Ah��Aj=pAk�
Ao
=Ap��Ar=pAs�
Aup�Ax��Az=pA{�
A}p�A
=A��A��A��RA��A��A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA��A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��A��A�Q�A��A¸RAÅA�Q�A��AƸRAǅA��A��A˅A�Q�A��AθRAυA��A��AҸRA�Q�A��A��AׅA�Q�A��AڸRAۅA��A��A޸RDqu�Dq|)Dq��Dq��Dq��Dq�)Dq��Dq��Dq�\Dq��Dq�)DqDq�\Dq��Dq�)Dq�Dq��Dq�\Dq��Dq�)Dr�Dr\Dr�Dr)Dr"�Dr(�Dr/\Dr<)DrB�DrH�DrO\DrU�Dr\)Drb�Dro\Dru�Dr|)Dr��Dr��Dr�\Dr�)Dr��Dr��Dr�\Dr��Dr�)DrDr�\Dr��Dr�)Dr�Dr��Dr��Dr�)Ds�Ds�Ds\Ds�Ds"�Ds(�Ds/\Ds5�Ds<)DsB�DsO\DsU�Ds\)Dsb�Dsh�Dso\Ds|)Ds��Ds��Ds�\Ds��Ds��Ds��Ds�\Ds��Ds�)DsDs�\Ds��Ds�)Ds�Ds�\Ds��Ds�)Dt�Dt�Dt\Dt)Dt"�Dt(�Dt/\Dt<)DtB�DtH�DtO\DtU�Dt\)Dth�Dto\Dtu�Dt|)Dt��Dt�\Dt��Dt�)Dt��Dt�\Dt��Dt�)DtDt��Dt��Dt�)Dt�Dt��Dt��@5@B�\@H��@O\)@U@b�\@h��@o\)@u@�G�@�z�@��@��H@�{@�G�@�z�@��H@�{@�G�@�z�@��@�{@�G�@�z�@��@�{@�G�@�z�@Ǯ@��H@�G�@�z�@׮@��H@�{@�G�@�@��H@�{@�G�@�z�@��@��HA ��A=pA�
Ap�A
=A
=pA�
Ap�A
=A=pA�
Ap�A
=A��A�
Ap�A
=A ��A"=pA#�
A'
=A(��A*=pA+�
A-p�A0��A2=pA3�
A5p�A7
=A8��A;�
A=p�A?
=A@��AB=pAEp�AG
=AH��AJ=pAK�
AMp�AP��AR=pAS�
AUp�AW
=AX��A[�
A]p�A_
=Ab=pAc�
Aep�Ag
=Ah��Aj=pAk�
Ao
=Ap��Ar=pAs�
Aup�Ax��Az=pA{�
A}p�A
=A��A��A��RA��A��A��A��RA��A��A��A��RA�Q�A��A��A��RA�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA��A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A��A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��A��A�Q�A��A¸RAÅA�Q�A��AƸRAǅA��A��A˅A�Q�A��AθRAυA��A��AҸRA�Q�A��A��AׅA�Q�A��AڸRAۅA��A��A޸RDqu�Dq|)Dq��Dq��Dq��Dq�)Dq��Dq��Dq�\Dq��Dq�)DqDq�\Dq��Dq�)Dq�Dq��Dq�\Dq��Dq�)Dr�Dr\Dr�Dr)Dr"�Dr(�Dr/\Dr<)DrB�DrH�DrO\DrU�Dr\)Drb�Dro\Dru�Dr|)Dr��Dr��Dr�\Dr�)Dr��Dr��Dr�\Dr��Dr�)DrDr�\Dr��Dr�)Dr�Dr��Dr��Dr�)Ds�Ds�Ds\Ds�Ds"�Ds(�Ds/\Ds5�Ds<)DsB�DsO\DsU�Ds\)Dsb�Dsh�Dso\Ds|)Ds��Ds��Ds�\Ds��Ds��Ds��Ds�\Ds��Ds�)DsDs�\Ds��Ds�)Ds�Ds�\Ds��Ds�)Dt�Dt�Dt\Dt)Dt"�Dt(�Dt/\Dt<)DtB�DtH�DtO\DtU�Dt\)Dth�Dto\Dtu�Dt|)Dt��Dt�\Dt��Dt�)Dt��Dt�\Dt��Dt�)DtDt��Dt��Dt�)Dt�Dt��Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AѸRAѸRAѼjAѺ^AѸRAѼjAѺ^AѺ^AѺ^AѶFA�^5A�&�A��A��A��A��A��A��A��A��A�JA�A���A���A��A��HAЧ�A��AϼjAϟ�A�\)AζFA�p�ÃA�bA�v�A�|�A��A��TA�(�Aũ�A�
=A�C�A�bNA�l�A�ƨA�ȴA�VA�p�A���A��A�~�A�A�-A�r�A��jA���A�G�A��A���A���A�I�A���A�33A�dZA��RA���A��A�t�A�$�A�~�A���A��#A��7A���A� �A�v�A���A�1'A�VA���A�VA�A�7LA�hsA��^A��9A��A�I�A��A��wA�z�A�jA��A�/A�XA�7LA���A�VA�E�A��RA�JA�x�A���A��wA�%A��^A� �A�"�A��A�#A{7LAoƨAk��Ag�FAdA�Ab��A`��A^9XAZ��AYt�AX�uAV~�AU�^AT�RAR{AO�AN�+ANz�AM�AMG�AK�AI��AG��AF�AD�AB�9AAoA?%A=S�A<ZA:��A9�7A8�9A7�A6��A6��A6��A6�A6 �A5�A3|�A2��A1��A1K�A/�-A.n�A-p�A,�!A,v�A+��A*��A*JA)��A)�A(A�A&�9A%t�A$$�A#x�A"��A"jA!��A ^5A  �A�hA��AXA��A�A&�AE�A5?AZA~�A�wA�A;dA�A�A�wA�`Az�A{A�A|�A1A\)A+A��AAC�A�DAJAA�PA$�A
�HA	��AdZA�`A��A��AȴAM�AE�A|�AI�AI�@�J@�@�\)@���@��7@�r�@�P@��@�`B@��/@�t�@���@�ȴ@��H@�"�@�+@���@�33@�C�@��@�  @��@�;d@�+@�P@�hs@�$�@�I�@�9@��@���@��@އ+@ް!@�9X@��@�E�@���@�z�@ڇ+@ԃ@�n�@ѩ�@�9X@�V@̛�@��@̋D@�^5@��@�^5@�^5@�E�@�@�I�@��/@˕�@Ȭ@�
=@�~�@�M�@Ƈ+@��H@ƸR@�M�@Ɨ�@��@�C�@�O�@ģ�@�Q�@�A�@���@�"�@��@���@��`@��@���@��9@���@�Z@���@�K�@��\@�r�@�ȴ@�v�@��#@���@��@�bN@��
@�|�@�+@���@��@��H@���@�M�@�o@�@�
=@�;d@�\)@�S�@���@�^5@��#@�V@��D@���@�l�@��@�M�@��@���@��^@�G�@��
@��y@�E�@��-@�V@�V@�V@�I�@�ȴ@��^@��9@��
@�|�@�
=@���@��#@��`@�Q�@��w@�ȴ@��@�;d@�+@��R@���@�^5@��@���@�p�@�/@�z�@��
@�S�@�;d@���@�ff@�M�@��T@���@���@���@��#@��@��@���@�V@��9@�A�@�ƨ@���@�\)@�C�@�K�@�K�@�"�@�^5@��@��@��@���@�ff@���@��@��+@�$�@�x�@�I�@�dZ@�dZ@�dZ@�"�@���@�^5@��@��@��h@��@�A�@�Q�@��
@�C�@��@��y@��!@�5?@���@�O�@��@���@��/@��@���@���@�?}@�@��^@��h@��@��7@�7L@�A�@�o@�C�@�dZ@�"�@�ȴ@��+@�E�@�$�@��@��h@�hs@�hs@�7L@���@�G�@��@�V@��@���@�b@�1'@�(�@��
@���@�t�@�t�@�|�@�t�@�
=@��@���@�p�@�`B@�?}@�/@��@�/@�`B@�G�@�7L@�/@�&�@��`@��D@�j@�A�@��@��
@�|�@�;d@���@�p�@y�@q��@gƨ@`��@Y#�@T�D@M�9@GiD@@G@:� @3~�@,��@& �@ C-@qv@_�@.�@@�@.IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�x�A���A��yAуAЋDA�9XA�|�A�O�A�bA�I�A�ffA��A��
A�=qAA�z�A���A�-A��jA�ffA�VA���A��hA��A��A�t�A�5?A�v�A�?}A�Q�A�l�A��DA��uA�{A�t�A���A��A��A���A�
=A��A�A�A�ĜA���A�?}A�dZA���AЉ7A�A�A���A�JA�%A�p�A�dZA�(�A��A���A���A�VA�
=A���A��FA��uA�bNA�VA�&�A�JAÉ7A�bNA��
A�dZA�v�A�ƨA���A��A��A���A��A���A�$�A��`A²-A�VA��A�A�z�A�  A�p�A���A��mA���A���A���A��A�"�A��`A�n�AыDA�M�AÑhA�VA�ƨAЁA�jA���A�bNA�l�A�{A�x�A���APA��A�p�A�$�AˮA�v�A�+AA���A���A���A���A���A��+A�=qA��A��;A���A�E�A�(�A�9XA���A�33A���A��A���A��A�{A�oA�I�A�p�A�n�A���A��A�XA�dZA�dZA�p�A�7LA�n�A�=qA�%A�JA�;dA�hsA�`BA�K�A�AȼjA�
=A���A¾wA�l�AÅA���A�\)A�{A�$�A�I�A�n�A�jA�jA�p�A�=qA�"�A�v�A�A�A��!A��A���Aɗ�A�t�A²-A�/AЍPA���A�33A��TAǴ9A�&�A�x�A�{A��TA�n�A���A���A�l�Aá�A�v�A�|�A��-A�M�AɃA�A�l�A�jA��;A��jA��`AøRA�n�A��TA�O�A��AμjA�Q�A�oA�-A�5?A�Q�AсA��A�O�A� �A�
=A�z�A���Aȣ�A�z�A�bA�hsA�l�A��AЅA�t�A�v�A�x�A�VA�-A�`BA�K�A�x�A�33A�|�A�ffAсAуAуA��HA��A��Aч+A�\)A���AхAч+AыDA�t�A�jAуAыDAч+A΃A���AуAуA�5?Aч+A�\)Aч+AхA��AэPAщ7A�&�AэPA�E�AыDA�+AыDAϕ�AсAыDAщ7Aч+Aч+A�7LAэPAыDAыDAсAщ7Aч+A�E�A�n�A�x�A�+A�~�AЍPA�~�AэPAыDAуA�hsA��A�z�A�^5A�p�A�hsAмjA�~�A�z�A�bA�~�A�|�A�x�A�JAч+A�x�A�x�A�z�A�z�A�z�AыDA�ZA�n�A�E�A�&�A�\)A��A�l�A�bNA�r�AЙ�A���A�|�AсA�`BA�ffA�~�A�~�AсA��`A�\)A�~�A�~�A�n�A�~�AсA�I�AсAсA�p�AхAуAч+AхA�ZA�JAхAщ7AхA�\)AыDA�\)A�|�A�\)AхAуA�1A�Q�A�p�AС�AсA�|�AǑhAʋDAхA�|�A�hsA�t�A�t�A�n�A�n�A�n�A���A�p�A�Q�A��A�`BA���A�z�A�r�A�v�A�n�AуA�ZA��`A�G�AуAыDAч+AуA�|�AсAуA�\)A�A���A��AхA�;dA�|�A�x�Aя\A�l�Aщ7A�~�A�|�AѓuAѓuAѕ�AэPA�ffAѕ�A�z�AэPAˡ�AЕ�Aѕ�AхAуA�A�A�oAя\AуA�XA�/AыDA�^5A�~�Aч+AхAхAсAхAсA�t�AсAуA��A�-A�r�A�+AсA�jA��A�~�A�x�AуAсA��yA��yA���AуA�|�A�n�A͉7A�~�A�|�A�l�A� �A�jA�r�A�v�Aβ-A�t�A�v�A�x�Aϧ�A�z�A�|�A�t�A�x�A�t�A�jA�z�A�ffA�VA�\)A�A�l�A�ffA�p�AЧ�A�r�A�t�Aа!A�7LA�n�A�S�A�t�A�jA�p�A�z�A�z�A�z�AсȀ\A�r�A�bNA��HA̍PA�jAϟ�AсAсAсAуA�x�AхAыDAч+Aя\Aѕ�Aџ�Aѥ�Aћ�Aћ�Aћ�Aѥ�Aѡ�Aѡ�Aћ�Aї�Aџ�Aћ�Aћ�Aџ�Aћ�Aљ�Aѝ�AѴ9AѰ!AѰ!AѲ-AѮAѬAѮAѬAѬAѬAѬAѬAѴ9AѶFAѸRAѶFAѺ^AѸRAѸRAѸRAѶFAѶFAѴ9AѶFAѶFAѶFAѸRAѸRAѸRAѺ^AѺ^AѺ^AѺ^AѺ^AѺ^AѼjAѸRAѸRAѴ9AѸRAѺ^AѺ^AѼjAѾwAѸRAѺ^AѼjAѼjAѼjAѼjAѼjAѼjAѾwAѼjAѼjAѼjAѾwAѾwAѼjAѶFAѺ^AѶFAѼjAѾwAѾwA���Aѩ�AѶFAѺ^AѲ-AѺ^AѸRAѸRAѶFAѸRAѶFAѸRAѸRAѸRAѺ^AѸRAѸRAѸRAѺ^AѸRAѼjAѾwAѺ^AѼjAѾwAѼjAѼjAѼjAѾwAѼjAѺ^AѼjAѼjAѸRAѺ^AѼjAѾwAѺ^AѶFAѼjAѶFAѺ^AѸRAѶFAѺ^AѶFAѸRAѴ9AѶFAѴ9AѶFAѸRAѸRAѸRAѸRAѸRAѺ^AѸRAѸRAѺ^AѺ^AѺ^AѼjAѺ^AѺ^AѺ^AѺ^AѺ^AѸRAѼjAѼjAѺ^AѼjAѼjAѸRAѼjAѺ^AѺ^AѺ^AѸRAѸRAѸRAѸRAѶFAѶFAѸRAѸRAѶFAѼjA���A�A���A���AѶFAѶFAѬAѝ�AэPAч+A�|�A�z�A�x�A�r�A�n�A�`BA�dZA�Q�A�G�A�K�A�5?A�33A�1'A�-A�-A�-A�+A�+A�+A�+A�&�A�$�A� �A� �A� �A� �A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��@��D@��D@��@��@��@�z�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�bN@�Q�@�Z@�Q�@�Q�@�Q�@�Q�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�1'@�1'@�(�@�(�@� �@�1@�  @�  @���@��@��m@��;@��;@��;@��m@��;@��;@��;@��;@��;@��;@��;@��
@��
@���@�ƨ@�ƨ@�ƨ@��w@��F@��F@��@���@���@���@���@��P@��@�l�@�dZ@�dZ@�\)@�\)@�\)@�S�@�S�@�\)@�S�@�\)@�S�@�C�@�C�@�;d@�;d@�;d@�;d@�33@�;d@�33@�33@�+@�"�@��@��@��@�o@�o@�
=@�@�AѸRAѸRAѸRAѺ^AѸRAѸRAѸRAѶFAѸRAѸRAѶFAѸRAѶFAѺ^AѺ^AѺ^AѺ^AѺ^AѺ^AѸRAѺ^AѼjAѼjAѺ^AѶFAѸRAѼjAѼjAѼjAѼjAѾwAѼjAѼjAѼjAѼjAѼjAѾwAѼjAѾwAѺ^AѼjAѼjAѾwAѾwAѼjAѺ^AѴ9AѲ-AѴ9AѼjAѼjAѼjAѴ9AѲ-AѶFAѴ9AѸRAѺ^AѺ^AѶFAѸRAѸRAѺ^AѸRAѸRAѺ^AѸRAѸRAѺ^AѺ^AѺ^AѾwAѼjAѼjAѼjAѼjAѾwAѾwAѼjAѼjAѾwAѾwAѼjAѼjAѼjAѺ^AѺ^AѾwAѾwAѼjAѼjAѼjAѸRAѶFAѸRAѺ^AѸRAѸRAѺ^AѸRAѶFAѸRAѶFAѶFAѴ9AѸRAѸRAѺ^AѺ^AѺ^AѼjAѾwAѼjAѼjAѼjAѼjAѺ^AѺ^AѶFAѸRAѸRAѼjAѼjAѼjAѼjAѼjAѼjAѺ^AѺ^AѺ^AѸRAѸRAѸRAѶFAѶFAѶFAѺ^AѶFAѶFAѺ^AѾwA�A�A�AѼjAѶFAѲ-Aѧ�AѓuAщ7AуA�|�A�z�A�v�A�n�A�l�A�`BA�\)A�M�A�E�A�9XA�5?A�5?A�1'A�-A�-A�-A�-A�+A�+A�+A�&�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��@��D@��D@��D@��@�z�@�z�@�r�@�r�@�j@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�r�@�j@�j@�j@�j@�Z@�Z@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�1'@�9X@�9X@�1'@�1'@�(�@� �@�b@�1@�  @�  @���@��m@��;@��;@��;@��;@��m@��;@��;@��;@��;@��;@��;@��
@���@���@���@�ƨ@�ƨ@�ƨ@��w@��F@��@��@���@���@���@��P@��P@�|�@�l�@�dZ@�dZ@�\)@�\)@�\)@�S�@�S�@�\)@�S�@�S�@�K�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�;d@�33@�33@�+@�"�@��@��@�o@�o@�
=@�
=@�
=@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999AѸRAѸRAѼjAѺ^AѸRAѼjAѺ^AѺ^AѺ^AѶFA�^5A�&�A��A��A��A��A��A��A��A��A�JA�A���A���A��A��HAЧ�A��AϼjAϟ�A�\)AζFA�p�ÃA�bA�v�A�|�A��A��TA�(�Aũ�A�
=A�C�A�bNA�l�A�ƨA�ȴA�VA�p�A���A��A�~�A�A�-A�r�A��jA���A�G�A��A���A���A�I�A���A�33A�dZA��RA���A��A�t�A�$�A�~�A���A��#A��7A���A� �A�v�A���A�1'A�VA���A�VA�A�7LA�hsA��^A��9A��A�I�A��A��wA�z�A�jA��A�/A�XA�7LA���A�VA�E�A��RA�JA�x�A���A��wA�%A��^A� �A�"�A��A�#A{7LAoƨAk��Ag�FAdA�Ab��A`��A^9XAZ��AYt�AX�uAV~�AU�^AT�RAR{AO�AN�+ANz�AM�AMG�AK�AI��AG��AF�AD�AB�9AAoA?%A=S�A<ZA:��A9�7A8�9A7�A6��A6��A6��A6�A6 �A5�A3|�A2��A1��A1K�A/�-A.n�A-p�A,�!A,v�A+��A*��A*JA)��A)�A(A�A&�9A%t�A$$�A#x�A"��A"jA!��A ^5A  �A�hA��AXA��A�A&�AE�A5?AZA~�A�wA�A;dA�A�A�wA�`Az�A{A�A|�A1A\)A+A��AAC�A�DAJAA�PA$�A
�HA	��AdZA�`A��A��AȴAM�AE�A|�AI�AI�@�J@�@�\)@���@��7@�r�@�P@��@�`B@��/@�t�@���@�ȴ@��H@�"�@�+@���@�33@�C�@��@�  @��@�;d@�+@�P@�hs@�$�@�I�@�9@��@���@��@އ+@ް!@�9X@��@�E�@���@�z�@ڇ+@ԃ@�n�@ѩ�@�9X@�V@̛�@��@̋D@�^5@��@�^5@�^5@�E�@�@�I�@��/@˕�@Ȭ@�
=@�~�@�M�@Ƈ+@��H@ƸR@�M�@Ɨ�@��@�C�@�O�@ģ�@�Q�@�A�@���@�"�@��@���@��`@��@���@��9@���@�Z@���@�K�@��\@�r�@�ȴ@�v�@��#@���@��@�bN@��
@�|�@�+@���@��@��H@���@�M�@�o@�@�
=@�;d@�\)@�S�@���@�^5@��#@�V@��D@���@�l�@��@�M�@��@���@��^@�G�@��
@��y@�E�@��-@�V@�V@�V@�I�@�ȴ@��^@��9@��
@�|�@�
=@���@��#@��`@�Q�@��w@�ȴ@��@�;d@�+@��R@���@�^5@��@���@�p�@�/@�z�@��
@�S�@�;d@���@�ff@�M�@��T@���@���@���@��#@��@��@���@�V@��9@�A�@�ƨ@���@�\)@�C�@�K�@�K�@�"�@�^5@��@��@��@���@�ff@���@��@��+@�$�@�x�@�I�@�dZ@�dZ@�dZ@�"�@���@�^5@��@��@��h@��@�A�@�Q�@��
@�C�@��@��y@��!@�5?@���@�O�@��@���@��/@��@���@���@�?}@�@��^@��h@��@��7@�7L@�A�@�o@�C�@�dZ@�"�@�ȴ@��+@�E�@�$�@��@��h@�hs@�hs@�7L@���@�G�@��@�V@��@���@�b@�1'@�(�@��
@���@�t�@�t�@�|�@�t�@�
=@��@���@�p�@�`B@�?}@�/@��@�/@�`B@�G�@�7L@�/@�&�@��`@��D@�j@�A�@��@��
@�|�G�O�@���@�p�@y�@q��@gƨ@`��@Y#�@T�D@M�9@GiD@@G@:� @3~�@,��@& �@ C-@qv@_�@.�@@�@.IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�x�A���A��yAуAЋDA�9XA�|�A�O�A�bA�I�A�ffA��A��
A�=qAA�z�A���A�-A��jA�ffA�VA���A��hA��A��A�t�A�5?A�v�A�?}A�Q�A�l�A��DA��uA�{A�t�A���A��A��A���A�
=A��A�A�A�ĜA���A�?}A�dZA���AЉ7A�A�A���A�JA�%A�p�A�dZA�(�A��A���A���A�VA�
=A���A��FA��uA�bNA�VA�&�A�JAÉ7A�bNA��
A�dZA�v�A�ƨA���A��A��A���A��A���A�$�A��`A²-A�VA��A�A�z�A�  A�p�A���A��mA���A���A���A��A�"�A��`A�n�AыDA�M�AÑhA�VA�ƨAЁA�jA���A�bNA�l�A�{A�x�A���APA��A�p�A�$�AˮA�v�A�+AA���A���A���A���A���A��+A�=qA��A��;A���A�E�A�(�A�9XA���A�33A���A��A���A��A�{A�oA�I�A�p�A�n�A���A��A�XA�dZA�dZA�p�A�7LA�n�A�=qA�%A�JA�;dA�hsA�`BA�K�A�AȼjA�
=A���A¾wA�l�AÅA���A�\)A�{A�$�A�I�A�n�A�jA�jA�p�A�=qA�"�A�v�A�A�A��!A��A���Aɗ�A�t�A²-A�/AЍPA���A�33A��TAǴ9A�&�A�x�A�{A��TA�n�A���A���A�l�Aá�A�v�A�|�A��-A�M�AɃA�A�l�A�jA��;A��jA��`AøRA�n�A��TA�O�A��AμjA�Q�A�oA�-A�5?A�Q�AсA��A�O�A� �A�
=A�z�A���Aȣ�A�z�A�bA�hsA�l�A��AЅA�t�A�v�A�x�A�VA�-A�`BA�K�A�x�A�33A�|�A�ffAсAуAуA��HA��A��Aч+A�\)A���AхAч+AыDA�t�A�jAуAыDAч+A΃A���AуAуA�5?Aч+A�\)Aч+AхA��AэPAщ7A�&�AэPA�E�AыDA�+AыDAϕ�AсAыDAщ7Aч+Aч+A�7LAэPAыDAыDAсAщ7Aч+A�E�A�n�A�x�A�+A�~�AЍPA�~�AэPAыDAуA�hsA��A�z�A�^5A�p�A�hsAмjA�~�A�z�A�bA�~�A�|�A�x�A�JAч+A�x�A�x�A�z�A�z�A�z�AыDA�ZA�n�A�E�A�&�A�\)A��A�l�A�bNA�r�AЙ�A���A�|�AсA�`BA�ffA�~�A�~�AсA��`A�\)A�~�A�~�A�n�A�~�AсA�I�AсAсA�p�AхAуAч+AхA�ZA�JAхAщ7AхA�\)AыDA�\)A�|�A�\)AхAуA�1A�Q�A�p�AС�AсA�|�AǑhAʋDAхA�|�A�hsA�t�A�t�A�n�A�n�A�n�A���A�p�A�Q�A��A�`BA���A�z�A�r�A�v�A�n�AуA�ZA��`A�G�AуAыDAч+AуA�|�AсAуA�\)A�A���A��AхA�;dA�|�A�x�Aя\A�l�Aщ7A�~�A�|�AѓuAѓuAѕ�AэPA�ffAѕ�A�z�AэPAˡ�AЕ�Aѕ�AхAуA�A�A�oAя\AуA�XA�/AыDA�^5A�~�Aч+AхAхAсAхAсA�t�AсAуA��A�-A�r�A�+AсA�jA��A�~�A�x�AуAсA��yA��yA���AуA�|�A�n�A͉7A�~�A�|�A�l�A� �A�jA�r�A�v�Aβ-A�t�A�v�A�x�Aϧ�A�z�A�|�A�t�A�x�A�t�A�jA�z�A�ffA�VA�\)A�A�l�A�ffA�p�AЧ�A�r�A�t�Aа!A�7LA�n�A�S�A�t�A�jA�p�A�z�A�z�A�z�AсȀ\A�r�A�bNA��HA̍PA�jAϟ�AсAсAсAуA�x�AхAыDAч+Aя\Aѕ�Aџ�Aѥ�Aћ�Aћ�Aћ�Aѥ�Aѡ�Aѡ�Aћ�Aї�Aџ�Aћ�Aћ�Aџ�Aћ�Aљ�Aѝ�AѴ9AѰ!AѰ!AѲ-AѮAѬAѮAѬAѬAѬAѬAѬAѴ9AѸRAѸRAѸRAѺ^AѸRAѸRAѸRAѶFAѸRAѸRAѶFAѸRAѶFAѺ^AѺ^AѺ^AѺ^AѺ^AѺ^AѸRAѺ^AѼjAѼjAѺ^AѶFAѸRAѼjAѼjAѼjAѼjAѾwAѼjAѼjAѼjAѼjAѼjAѾwAѼjAѾwAѺ^AѼjAѼjAѾwAѾwAѼjAѺ^AѴ9AѲ-AѴ9AѼjAѼjAѼjAѴ9AѲ-AѶFAѴ9AѸRAѺ^AѺ^AѶFAѸRAѸRAѺ^AѸRAѸRAѺ^AѸRAѸRAѺ^AѺ^AѺ^AѾwAѼjAѼjAѼjAѼjAѾwAѾwAѼjAѼjAѾwAѾwAѼjAѼjAѼjAѺ^AѺ^AѾwAѾwAѼjAѼjAѼjAѸRAѶFAѸRAѺ^AѸRAѸRAѺ^AѸRAѶFAѸRAѶFAѶFAѴ9AѸRAѸRAѺ^AѺ^AѺ^AѼjAѾwAѼjAѼjAѼjAѼjAѺ^AѺ^AѶFAѸRAѸRAѼjAѼjAѼjAѼjAѼjAѼjAѺ^AѺ^AѺ^AѸRAѸRAѸRAѶFAѶFAѶFAѺ^AѶFAѶFAѺ^AѾwA�A�A�AѼjAѶFAѲ-Aѧ�AѓuAщ7AуA�|�A�z�A�v�A�n�A�l�A�`BA�\)A�M�A�E�A�9XA�5?A�5?A�1'A�-A�-A�-A�-A�+A�+A�+A�&�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��@��D@��D@��D@��@�z�@�z�@�r�@�r�@�j@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�r�@�j@�j@�j@�j@�Z@�Z@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�1'@�9X@�9X@�1'@�1'@�(�@� �@�b@�1@�  @�  @���@��m@��;@��;@��;@��;@��m@��;@��;@��;@��;@��;@��;@��
@���@���@���@�ƨ@�ƨ@�ƨ@��w@��F@��@��@���@���@���@��P@��P@�|�@�l�@�dZ@�dZ@�\)@�\)@�\)@�S�@�S�@�\)@�S�@�S�@�K�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�;d@�33@�33@�+@�"�@��@��@�o@�o@�
=@�
=@�
=@�AѸRAѸRAѸRAѺ^AѸRAѸRAѸRAѶFAѸRAѸRAѶFAѸRAѶFAѺ^AѺ^AѺ^AѺ^AѺ^AѺ^AѸRAѺ^AѼjAѼjAѺ^AѶFAѸRAѼjAѼjAѼjAѼjAѾwAѼjAѼjAѼjAѼjAѼjAѾwAѼjAѾwAѺ^AѼjAѼjAѾwAѾwAѼjAѺ^AѴ9AѲ-AѴ9AѼjAѼjAѼjAѴ9AѲ-AѶFAѴ9AѸRAѺ^AѺ^AѶFAѸRAѸRAѺ^AѸRAѸRAѺ^AѸRAѸRAѺ^AѺ^AѺ^AѾwAѼjAѼjAѼjAѼjAѾwAѾwAѼjAѼjAѾwAѾwAѼjAѼjAѼjAѺ^AѺ^AѾwAѾwAѼjAѼjAѼjAѸRAѶFAѸRAѺ^AѸRAѸRAѺ^AѸRAѶFAѸRAѶFAѶFAѴ9AѸRAѸRAѺ^AѺ^AѺ^AѼjAѾwAѼjAѼjAѼjAѼjAѺ^AѺ^AѶFAѸRAѸRAѼjAѼjAѼjAѼjAѼjAѼjAѺ^AѺ^AѺ^AѸRAѸRAѸRAѶFAѶFAѶFAѺ^AѶFAѶFAѺ^AѾwA�A�A�AѼjAѶFAѲ-Aѧ�AѓuAщ7AуA�|�A�z�A�v�A�n�A�l�A�`BA�\)A�M�A�E�A�9XA�5?A�5?A�1'A�-A�-A�-A�-A�+A�+A�+A�&�A�"�A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��@��D@��D@��D@��@�z�@�z�@�r�@�r�@�j@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�r�@�j@�j@�j@�j@�Z@�Z@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�9X@�1'@�9X@�9X@�1'@�1'@�(�@� �@�b@�1@�  @�  @���@��m@��;@��;@��;@��;@��m@��;@��;@��;@��;@��;@��;@��
@���@���@���@�ƨ@�ƨ@�ƨ@��w@��F@��@��@���@���@���@��P@��P@�|�@�l�@�dZ@�dZ@�\)@�\)@�\)@�S�@�S�@�\)@�S�@�S�@�K�@�C�@�C�@�;d@�;d@�;d@�;d@�;d@�;d@�33@�33@�+@�"�@��@��@�o@�o@�
=@�
=@�
=@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=��>5i@�<�@��v=q�b=���=�8q=�,�>��@�{�@�-M=x-8=�^�=�Bp=�f�?��{=G�=�/Z=c4�=HӮ=J��==�=ECl=��=���=�o?=�#�>fYK@�O=��={�=��>&F�=ξ�> �>ɜ9@�8	=�y>ç�=KH�=���=w�=���=���=���>T�@�1�=`u�=��`?��s=/��=#S=3]%=?	�=M� =N�,=kF5=� �=�O�=�҉=�o?=֌i=� q>8�f@�*Z=�o~=>�@��@�'=�Y�@�4=�<�=�S=��=��K=��b=laR=�jj=� G=���>}`�=�+�=©�>9Ц@�1{=\	�=Kۡ=e!W=��=�A=��>$U@_rq>��A>*��@�<6@�0=�Yu=�h�>�{?���@�:*>�?��@�+k>h��=p
=~��=�F�@.��=�x> 1�>W��@�3�@}�=�]�>@/0@�@:=TK�=b˧=�ek=I�m=M5�=�Y!>*��=��H=�u=�J=���=�%�>K�=h]�=_;d=�Xd=���>"}=��>/��@�(�@�/Z@�=�xW>�K@MF=��:=��q=��]>�P?c�#=��5=��F=���>j+@�2�=��r>GG�@�O�=q/=��=��w=��e=�o>F�;@�<6?h^>rk�?�y�@�<�?�.�=͙F>'Jw@��@�<`=��>��/=m�h=���=��0=�n�>!�n@�=�+�>�|@�=\>���=��=�b�=�g�>Gy�@�9�>��=�O?ĥ'=�f�?EO7=�?=�$J>jZG@�F�@#�w>��?�E@���?ޮ}@�E9>��Q=�b�=�'g=�i�?IЦ>c��?Mr�=�EN?G�Q@��>SP	@���>�L@L��@�I�=�>l=���=�@�>G�n@�Ec@*z�>�P3@�F�?���>2��@�E9>�ҳ@O@���@�C@�CB>�w@�o@�AJ@�D(@�E9?9�@�F_@�GE@�G�@�H�@�H�@dU2>dO�@01@�J�@�L?��?@�If@�M@�R*?���@�L�@�N<@�M@�N<?�?{+V@�I(@�K^?��@�K�@"��@�Ln@�L�?���@�Ln@��}?4�u@�M@���@�K�@�L@�L@��@���@�Ln@�L�@�K�@�L@�JM@�MU@�M@�K�@�L@�K�@�JM@�J�>��%?/Jw@���@�J�@|m�@�Go@�M@�J�@�F�@�AJ@��	@�Go>>��@�G�@KO�@}\�@�I(@�H@��y@�H@�Go?��@L�p@�H@�E�@�I�@�M@�H�@�JM@�QY@�T@vk�@P��@O,�@�;:>Py�@�@:@�B�@�I�@���@uf'@�If@�I(@�>�@�C@�H@�H�@�I(@�Ԁ@�H@�9�@�Z�@�o @�H�@�Ec@�HV@�K4@�K�?儡@�H�@�Jw@�Ln@�K^@�H�?cl�@�K^@�K�@�Jw>��@�L@�L�@�H�@�K�@�I�@�I(@�E$@oT@h��@�@:@�JM@�H�?���>���@�JM@�I�@�G�@�D�@�I@�C�@�E�@�A�>��j@�D|@�@�?��U@�B>zS�@�F�@�H�@�H�@�JM@�J�@�K�>H�@:~|@�I(@�JM@�J�@�I�@�G@�I@�I�@�H�@)��@	O�@��?@�Ln@f	�@T<!@�J�@�M@�~@�J�@�Jw@�K�@�P]@�P	@�S�@�OL@�Oa@�Qn@��@�J�@�?�j@�Ln@�G�@�E�@i�?���@�Ln@�JM@�E�?@h�@�K�@�K�@�K�@�K�@�J�@�J�@�J�@�Jw@�K^@�H@�N�@�K^@�Z�?��@�E9>�}k@�Go@�E�@f|@�H�@�I(@�H�@�Jw?�o??gf�@�5�@�I(@�I(@�B>���@�I@�I@�G@5+@�E9@�F_@�H�?SdZ@�F @�D|@�B�@O�@�E�@�E�@�G@�E�@�F�@�Go@�I�@|��@L��@�<�?�y@�C�@�C�@�E�?��@�E�@�E�@�'�@�B[@�5�@Z��@�E9@�D�@���@�H@�H�@�G�@�K^@,'�@�D|@�F�@�Go@wH,@�B?\�@�J�@�J�@�K^@�J�@�H�@�Q@�QY@�Qn@�N�@�U�@�V�@�T�@�T�@�U@�TL@�XO@�V�@�V@�T�@�TL@�V@�U�@�T�@�-�@�Y`@�[-@�\�@�^@�^@�]�@�\>@�[�@�[-@�\�@�[�@�\>@�\�@�]d@�_1@�`-@�`�@�`�@�`�@�`�@�`W@�`�@�_�@�`W@�_�@�`�@�`�@�`�@�aR@�aR@�aR@�b$@�a�@�a�@�b$@�a�@�b�@�bx@�b�@�a�@�aR@�a�@�c @�c @�b�@�b�@�c @�b�@�c @�c�@�c�@�dZ@�c�@�d0@�d@�d0@�c�@�d@�dZ@�d0@�c�@�d@�c�@�c�@�a�@�d�@�d�@�d�@�dZ@�`�@�c @�cI@�c @�c @�c�@�cI@�c @�c�@�d@�c�@�c�@�d@�d0@�d0@�c�@�d�@�d�@�e�@�e�@�e�@�f@�e�@�f<@�f@�f�@�ff@�f@�e�@�f@�f<@�e�@�eV@�f<@�f@�f@�e�@�ek@�e�@�ek@�ek@�e@�e@�d�@�e,@�e@�e@�do@�e@�ek@�e@�ek@�ek@�e�@�e�@�f{@�f'@�fQ@�g8@�f�@�f�@�g8@�f{@�f{@�f'@�f'@�f�@�fQ@�g8@�g�@�g�@�g�@�g�@�h�@�hI@�h@�h�@�i@�i�@�i@�in@�i�@�i�@�i�@�j+@�j�@�m	@�p�@�q�@�r�@�rq@�q�@�p�@�q�@�qL@�o�@�m�@�m�@�l�@�k�@�j�@�i@�gM@�e�@�d�@�a�@�_@�\�@�Z\@�Z�@�Y�@�YK@�YK@�X�@�X�@�X�@�X�@�X:@�W�@�W*@�W*@�W*@�V�@�V�@�V�@�V�@�W*@�V�@�W~@�W�@�W*@�W�@�XO@�XO@�X�@�XO@�Y@�Y@�Y`@�Y`@�Y`@�Y`@�Y�@�Z@�Z�@�Z�@�[-@Qڥ@Q�Q@Q٩@Q�U@Q�@Q�Z@Q�Z@Q�@Q��@Q�0@Qײ@Q�Z@Q�@Q�@Qײ@Q׈@Q׈@Q׈@Q׈@Q׈@Q�
@Q֌@Q��@Q��@Q�@Q�A@Q�A@Qә@Q�F@Q�F@Q��@QҞ@Q�J@Q��@Q�J@Q�J@Q�J@QѢ@Q�N@QѢ@QѢ@Q�N@Q�N@Q�N@Q�$@Q��@Q�N@Q��@Q��@Q�S@Q��@Q�@Q�`@Q̎@Q�m@Q�q@Q�@Q�u@Q�&@Q�*@QŬ@Q��@Q�.@Q�.@Q�.@Q�.@Q��@Q��@Q��@Qı@Q�]@Qõ@Q�@Q�e@Q�@Q�j@Q��@Q�n@Q�r@Q�#@Q�{@Q��@Q��@Q��@Q�^@Q��@Q�k@Q��@Q��@Q��@Q��@Q�W@Q�W@Q��@Q��@Q��@Q��@Q��@Q�@Q��@Q�:@Q��@Q��@Q��@Q��@Q��@Q�C@Q��@Q�G@Q��@Q��@Q��@Q�T@Q� @Q�X@Q��@Q�]@Q��@Q��@Q�@�_@�_[@�_�@�_�@�_�@�_�@�_�@�_�@�_�@�_F@�^�@�_�@�`@�`B@�`B@�`�@�`�@�a@�a=@�]�@�a(@�a�@�b@�a|@�_1@�_p@�b�@�a�@�b9@�bN@�bx@�bN@�b�@�c@�bx@�b�@�b�@�c5@�c@�b�@�b�@�b9@�c5@�cI@�c@�b9@�`B@�_F@�`@�c�@�c5@�cs@�a�@�^_@�a@�`�@�b9@�b9@�a�@�a�@�a�@�a�@�bN@�b�@�bx@�bx@�b�@�b�@�b�@�cs@�cI@�d�@�d@�d0@�d�@�d�@�d�@�d�@�eA@�d�@�eA@�d�@�d�@�do@�c�@�c�@�dE@�d�@�ek@�e�@�d�@�d�@�cI@�cI@�cs@�cs@�c�@�b�@�c�@�cI@�cs@�cI@�b�@�c @�b�@�c�@�d0@�dZ@�d�@�dZ@�d0@�e�@�d�@�ek@�e�@�f'@�d�@�b�@�c�@�d�@�dZ@�e�@�e�@�f@�e�@�f<@�f�@�f�@�f�@�f�@�gb@�g�@�h�@�h�@�h�@�h4@�h@�hs@�h^@�i�@�o @�r@�r�@�r\@�q�@�q7@�qv@�q"@�o�@�m�@�mr@�lv@�l"@�k<@�iY@�g�@�d�@�c^@�`�@�]d@�Zq@�X�@�Y�@�W�@�W?@�Wi@�W@�V�@�V�@�VX@�VX@�U�@�U@�T�@�Ta@�Ta@�TL@�S�@�S�@�S�@�TL@�T�@�T�@�T�@�T�@�T�@�T�@�UG@�U@�U�@�V@�V@�VC@�V@�VC@�VX@�V�@�V�@�V�@�V�@Q��@Q� @Q��@Q��@Q�$@Q�)@Q��@Qϫ@Q�@Qρ@Q��@Q��@Qρ@Qρ@Qρ@Q�W@Q�W@Q�@Q�W@Q�-@Q�@Qί@Qί@Q̸@Q��@Q�d@Q�@Q�@Q��@Q��@Q�C@Q�C@Qɛ@Q�G@Q�q@Q��@Q��@Q�@Q��@Q��@Q�@Q��@Q��@Q�@Q�@Q��@Q�@Q��@Q�@Q��@Q��@Qȟ@Q�~@Q�~@Q�@Q�j@Q�@@Q��@Q��@Q�w@Q�'@Q�V@Q�@Q��@Q�'@Q�'@Q��@Q��@Q��@Q��@Q��@Q�V@Q�Z@Q�@Q�4@Q��@Q��@Q�@Q�=@Q�@Q�F@Q�t@Q�x@Q�}@Q�)@Q�@Q�6@Q��@Q�K@Q��@Q�&@Q��@Q�~@Q�~@Q�T@Q�T@Q�T@Q�*@Q�*@Q��@Q��@Q�@Q�@Q��@Q��@Q��@Q��@Q��@Q�j@Q��@Q��@Q�w@Q�{@Q�Q@Q��@Q�@Q��@Q�Z@Q�@Q�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            444334444433444444444444444443444444434444444443444444444444444443444434444444444444443444444434433444434434444444434443444444444444444444443344434444444443443444444344434443344444444443444443444444443444343444444444343443444434434434433343333433333344334333433334433434334334333333333333333333344333333333343433333344333333343443433333333333333333333334333334333433333334333344333333334334343333334433333333443333333333333333333433334333433333333333334343333333443333433343334333433333333434333433333333333334333334333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�<�@��vG�O�G�O�G�O�G�O�G�O�@�{�@�-OG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�LG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�8
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�*ZG�O�G�O�G�O�G�O�@�'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�1vG�O�G�O�G�O�G�O�G�O�G�O�G�O�@_rzG�O�G�O�@�<:@�0G�O�G�O�G�O�G�O�@�:*G�O�G�O�@�+iG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�3�G�O�G�O�G�O�@�@BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�(�@�/]G�O�G�O�G�O�@MFG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�2�G�O�G�O�@�O�G�O�G�O�G�O�G�O�G�O�G�O�@�<4G�O�G�O�G�O�@�<�G�O�G�O�G�O�@��@�<cG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�=\G�O�G�O�G�O�G�O�G�O�@�9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�F�G�O�G�O�G�O�@���G�O�@�E;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�@���G�O�G�O�@�I�G�O�G�O�G�O�G�O�@�EfG�O�G�O�@�F�G�O�G�O�@�E:G�O�G�O�@���@�C@�CFG�O�@�q@�AJ@�D*@�E<G�O�@�F]@�GC@�G�@�H~@�H~@dU0G�O�G�O�@�J�@�LG�O�@�Ig@�M@�R-G�O�@�L�@�N=@�M~@�N>G�O�G�O�@�I(@�K`G�O�@�K�G�O�@�Lo@�L�G�O�@�Lo@�ЀG�O�@�M@���@�K�@�L@�L@��@���@�Ls@�L�@�K�@�L@�JM@�MW@�M@�K�@�L@�K�@�JM@�J�G�O�G�O�@���@�J�@|m�@�Gr@�M@�J�@�F�@�AJ@��
@�GoG�O�@�G�G�O�@}\�@�I*@�H@��~@�H@�GrG�O�G�O�@�H@�E�@�I�@�M@�H�@�JN@�QZG�O�@vk�G�O�G�O�@�;:G�O�@�@:@�B�@�I�@���@uf*@�Id@�I(@�>�@�C@�H@�H�@�I(@��~@�H@�9�@�Z�@�o@�H~@�Ed@�HV@�K2@�K�G�O�@�H�@�Jv@�Lo@�K^@�H�G�O�@�K^@�K�@�JyG�O�@�L@�L�@�H�@�K�@�I�@�I(@�E"G�O�@h��@�@8@�JL@�H�G�O�G�O�@�JN@�I�@�G�@�D�@�I@�C�@�E�@�A�G�O�@�D~@�@�G�O�@�BG�O�@�F�@�H�@�H�@�JR@�J�@�K�G�O�G�O�@�I(@�JR@�J�@�I�@�G@�I@�I�@�H�G�O�G�O�@��@@�Lp@f	�@T<%@�J�@�M~@��@�J�@�Jy@�K�@�P^@�P@�S�@�OM@�O^@�Ql@��@�J�@�G�O�@�Lo@�G�@�E�@i�G�O�@�Lr@�JO@�E�G�O�@�K�@�K�@�K�@�K�@�J�@�J�@�J�@�Jw@�K^@�H@�N�@�K^@�Z�G�O�@�E:G�O�@�Gq@�E�@f|@�H�@�I(@�H�@�JxG�O�G�O�@�5�@�I(@�I'@�BG�O�@�I@�I@�G
G�O�@�E;@�F`@�H�G�O�@�F"@�D@�B�G�O�@�E�@�E�@�G@�E�@�F�@�Go@�I�@|��G�O�@�<�G�O�@�C�@�C�@�E�G�O�@�E�@�E�@�'�@�B\@�5�@Z��@�E6@�D�@���@�H@�H~@�G�@�K`G�O�@�D{@�F�@�Gm@wH+@�BG�O�@�J�@�J�@�K`@�J�@�H�@�Q@�QV@�Qn@�N�@�U�@�V�@�T�@�T�@�U@�TI@�XS@�V�@�V@�T�@�TO@�V@�U�@�T�@�-�@�Y_@�[1@�\�@�^@�^@�]�@�\=@�[�@�[-@�\�@�[�@�\?@�\�@�]f@�_5@�`,@�_
@�_Y@�_�@�_�@�_�@�_�@�_�@�_�@�_�@�_I@�^�@�_�@�`@�`A@�`C@�`�@�`�@�a@�a>@�]�@�a)@�a�@�b@�a|@�_2@�_r@�b�@�a�@�b<@�bQ@�bx@�bN@�b�@�c@�bt@�b�@�b�@�c3@�c@�b�@�b�@�b<@�c:@�cL@�c
@�b6@�`F@�_F@�`@�c�@�c2@�cv@�a�@�^`@�a@�`�@�b:@�b;@�a�@�a�@�a�@�a�@�bL@�b�@�bx@�bz@�b�@�b�@�b�@�cu@�cJ@�d�@�d	@�d2@�d�@�d�@�d�@�d�@�e?@�d�@�eA@�d�@�d�@�dl@�c�@�c�@�d@@�d�@�em@�e�@�d�@�d�@�cG@�cL@�cr@�cu@�c�@�b�@�c�@�cJ@�cq@�cN@�b�@�c!@�b�@�c�@�d2@�d[@�d�@�d[@�d0@�e�@�d�@�eh@�e�@�f(@�d�@�b�@�c�@�d�@�dZ@�e�@�e�@�f@�e�@�f=@�f�@�f�@�f�@�f�@�gb@�g�@�h�@�h�@�h�@�h5@�h@�ht@�h]@�i�@�o@�r@�r�@�r[@�q�@�q8@�qu@�q'@�o�@�m�@�mr@�lw@�l"@�k<@�iZ@�g�@�d�@�c^@�`�@�]g@�Zp@�X�@�Y�@�W�@�WB@�Wi@�W@�V�@�V�@�V\@�VW@�U�@�U@�T�@�Tc@�T`@�TL@�S�@�S�@�S�@�TM@�T�@�T�@�T�@�T�@�T�@�T�@�UH@�U@�U�@�V@�V@�VA@�U�@�VG@�VW@�V�@�V�@�V�@�V�@Q��@Q�@Q��@Q��@Q�#@Q�-@Q��@Qϫ@Q�@Qπ@Q� @Q��@Qς@Qπ@Qς@Q�X@Q�S@Q�@Q�X@Q�-@Q��@Qΰ@Qΰ@Q̶@Q��@Q�e@Q�@Q�@Q��@Q��@Q�@@Q�E@Qɘ@Q�F@Q�k@Q��@Q��@Q� @Q��@Q��@Q� @Q��@Q��@Q� @Q�@Q��@Q�@Q��@Q� @Q��@Q��@QȠ@Q�{@Q�~@Q�@Q�h@Q�=@Q��@Q��@Q�x@Q�(@Q�U@Q�~@Q��@Q�&@Q�(@Q��@Q��@Q��@Q��@Q��@Q�R@Q�[@Q�@Q�2@Q��@Q��@Q�@Q�;@Q�@Q�E@Q�s@Q�{@Q�{@Q�*@Q�@Q�3@Q��@Q�J@Q��@Q�%@Q��@Q�}@Q�~@Q�S@Q�V@Q�S@Q�*@Q�.@Q��@Q��@Q�@Q�@Q��@Q��@Q��@Q��@Q��@Q�k@Q��@Q��@Q�v@Q�}@Q�R@Q��@Q�@Q��@Q�[@Q�@Q�b@�_
@�_Y@�_�@�_�@�_�@�_�@�_�@�_�@�_�@�_I@�^�@�_�@�`@�`A@�`C@�`�@�`�@�a@�a>@�]�@�a)@�a�@�b@�a|@�_2@�_r@�b�@�a�@�b<@�bQ@�bx@�bN@�b�@�c@�bt@�b�@�b�@�c3@�c@�b�@�b�@�b<@�c:@�cL@�c
@�b6@�`F@�_F@�`@�c�@�c2@�cv@�a�@�^`@�a@�`�@�b:@�b;@�a�@�a�@�a�@�a�@�bL@�b�@�bx@�bz@�b�@�b�@�b�@�cu@�cJ@�d�@�d	@�d2@�d�@�d�@�d�@�d�@�e?@�d�@�eA@�d�@�d�@�dl@�c�@�c�@�d@@�d�@�em@�e�@�d�@�d�@�cG@�cL@�cr@�cu@�c�@�b�@�c�@�cJ@�cq@�cN@�b�@�c!@�b�@�c�@�d2@�d[@�d�@�d[@�d0@�e�@�d�@�eh@�e�@�f(@�d�@�b�@�c�@�d�@�dZ@�e�@�e�@�f@�e�@�f=@�f�@�f�@�f�@�f�@�gb@�g�@�h�@�h�@�h�@�h5@�h@�ht@�h]@�i�@�o@�r@�r�@�r[@�q�@�q8@�qu@�q'@�o�@�m�@�mr@�lw@�l"@�k<@�iZ@�g�@�d�@�c^@�`�@�]g@�Zp@�X�@�Y�@�W�@�WB@�Wi@�W@�V�@�V�@�V\@�VW@�U�@�U@�T�@�Tc@�T`@�TL@�S�@�S�@�S�@�TM@�T�@�T�@�T�@�T�@�T�@�T�@�UH@�U@�U�@�V@�V@�VA@�U�@�VG@�VW@�V�@�V�@�V�@�V�@Q��@Q�@Q��@Q��@Q�#@Q�-@Q��@Qϫ@Q�@Qπ@Q� @Q��@Qς@Qπ@Qς@Q�X@Q�S@Q�@Q�X@Q�-@Q��@Qΰ@Qΰ@Q̶@Q��@Q�e@Q�@Q�@Q��@Q��@Q�@@Q�E@Qɘ@Q�F@Q�k@Q��@Q��@Q� @Q��@Q��@Q� @Q��@Q��@Q� @Q�@Q��@Q�@Q��@Q� @Q��@Q��@QȠ@Q�{@Q�~@Q�@Q�h@Q�=@Q��@Q��@Q�x@Q�(@Q�U@Q�~@Q��@Q�&@Q�(@Q��@Q��@Q��@Q��@Q��@Q�R@Q�[@Q�@Q�2@Q��@Q��@Q�@Q�;@Q�@Q�E@Q�s@Q�{@Q�{@Q�*@Q�@Q�3@Q��@Q�J@Q��@Q�%@Q��@Q�}@Q�~@Q�S@Q�V@Q�S@Q�*@Q�.@Q��@Q��@Q�@Q�@Q��@Q��@Q��@Q��@Q��@Q�k@Q��@Q��@Q�v@Q�}@Q�R@Q��@Q�@Q��@Q�[@Q�@Q�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            444334444433444444444444444443444444434444444443444444444444444443444434444444444444443444444434433444434434444444434443444444444444444444443344434444444443443444444344434443344444444443444443444444443444343444444444343443444434434434433343333433333344334333433334433434334334333333333333333333344333333333343433333344333333343443433333333333333333333334333334333433333334333344333333334334343333334433333333443333333333333333333433334333433333333333334343333333443333433343334333433333333434333433333333333334333334333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9���9��,9���9���9���9��s9��^9��c9���9�� 9���9��s9���9���9���9��\9��\9���9���9��9���9���9��Y9���9��9��A9���9��*9���9���9���9���9��9��(9���9���9���9��G9��'9���9���9���9��M9��[9��&9��{9���9��9���9���9��F9��}9��9��d9���9��M9��~9��9���9��)9��9��J9���9���9���9���9���9���9���9��|9��Z9��v9���9��9��X9���9���9���9���9���9���9���9��T9��C9���9���9�� 9���9��9��$9��y9��X9��W9��[9��z9��|9���9���9���9��Z9��y9��]9���9��99���9���9��9��69��g9��69��9��F9���9��9��S9���9��W9���9���9���9��59��I9��f9���9��v9���9��9��49��B9��P9���9���9�9�9�9��P9��>9�9��q9��a9���9��M9�ʳ9��~9���9�ɔ9���9�Ɇ9�ȃ9�Ƭ9�Ɖ9�ſ9��z9���9��=9���9���9��j9��`9���9��79���9���9��9���9���9���9��R9��9���9���9��Q9���9���9��W9��U9��D9���9���9���9��E9��x9��v9���9���9���9���9��9���9��r9���9���9���9���9���9���9��9��!9��@9��O9)�9)e9) 9)"9)�9)�9)�9)l9)�9)I9)�9)�9)K9)I9)K9))9)%9)�9))9)9)�9)�9)�9)
9)*9)�9)�9)�9)�9)�9)9)9)�9)D9)b9)�9)�9)&9)9)9)&9)9)�9)&9)#9)9)#9)�9)&9)�9)�9)�9)9)9)C9)�9)�9)j9)y9)�9)
�9)	�9)	�9)
9)
9)
�9)
=9)
99)
9)
9)
9)	�9)	9)�9)9)�9)�9)49)�9)�9)�9)J9)�9)�9)s9)�9) 9(�9(�9(��9(�.9(��9(��9(��9(��9(��9(��9(�c9(�g9(�49(��9(��9(�9(��9(��9(��9(��9(��9(��9(��9(�a9(�.9(�e9(�C9(��9(�49(��9(�9(�j9(��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�JB��B�B�'B�'B�3B�?B�?B�RB�dBBÖBÖBĜBĜBĜBŢB��B�)B�TB�TB�mB��BoB �B$�B,B>wBM�BdZB}�B�\B��B�FB�}BĜBǮB��B�B�;B��BÖB�wB�}B�wB�^B�9B�FB��BBƨBɺBɺBǮB��B�FB��B�DBhsBF�BJB��BoBI�B}�B�uB�PB�1B�Bw�BbNBT�BP�BJ�B@�B6FB,B �BoB
=B�B�B�^B�PBr�Be`B`BBZBS�BM�B=qB33B#�B+B
��B
�B
�5B
�'B
�DB
n�B
<jB
hB	��B	N�B	(�B	bB	B��B�sB�BƨBĜBɺB�B�5B�`B�HB��B��B�/B�BB�BB�B��B��B�9B�B��B�{B�bB�{B�\B�7B�B� B|�B~�B�B�B�B�7B�DB�7B�1B�%B�B�%B�%B�B�B�B�B� B|�B{�By�Bw�Bt�Bp�Bl�Bk�BiyBgmBdZBcTBbNBffBs�Br�Bw�Bu�BdZB]/B_;BiyBv�B}�B~�Bt�BhsBjB� Bz�Bv�Br�Bm�BiyB^5B\)BcTBp�Bm�BjBn�Bn�Bm�Bm�BbNBS�BG�B;dB>wB@�BH�BR�B\)BcTB\)BQ�BF�B8RB.B'�B%�B#�B"�B!�B"�B"�B!�B"�B#�B%�B&�B+B/B/B;dBD�BC�B=qB5?B49B6FB;dB?}BI�BYBffBbNB]/BZBR�BT�BdZBo�BjBiyBdZB\)BF�B@�B?}B>wB>wBB�BJ�BXBjBq�B� B�B�%B�B�+B�PB�7B�B~�B�B�%B�DB�bB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�LBƨBɺB��B��B��B��B��BȴBȴB��B��B��B��B��B��B��B�B�)B�BB�fB�B�B��B��B��B	B	%B	1B	PB	PB	VB	oB	uB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	�B	!�B	!�B	#�B	%�B	&�B	'�B	(�B	)�B	+B	+B	+B	,B	.B	2-B	49B	6FB	7LB	<jB	B�B	D�B	G�B	K�B	P�B	Q�B	T�B	XB	[#B	\)B	[#B	]/B	`BB	e`B	ffB	gmB	hsB	jB	jB	jB	jB	k�B	m�B	p�B	q�B	s�B	s�B	s�B	u�B	u�B	y�B	z�B	{�B	}�B	� B	�B	�B	�1B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�RB	�jB	�qB	�wB	�}B	��B	��B	�wB	�jB	�}B	��B	��B	��B	ĜB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�B	�)B	�5B	�/B	�5B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
MB
	B
VB
(>B
0UB
5�B
9�B
A�B
H�B
M6B
O\B
S�B
Z7B
_�B
eB
j�B
n�B
r�B
u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>ݣ?#�?aA�BzB	�>�,�>�Qa>�>�>�.�?D��B	Bh�>�n>��l>��?t`@�v`>v�[>���>�c>x��>|a�>k��>r��>�U�>��f>ղ-?�7?�iBҳ>�?)>�R�>׭K?T�?��? �@� BA?��@�>z�j>��>>�/�>��>Ǻ�?r�?��B�|>���>���A�q>W�+>G��>\/�>j�
>}M�>�>��>���>���>�i
>��k?��?��?l�B
��>��>�Y?:�xAm;�Br:?��AQ��>��D>�	?q>�p~>�ղ>�}$>�:�>��?X2?�\p>�a>���?h�LBe>���>{��>�=>�/>��p>�X?N�A��g?���?W��Bv�B
��>��)>ގ�?&�!@�םB�2?6~1@�ZBs?�{�>���>�p�>�w�A���?|?!�?��Bv�A]`�>�U�?rULBv>��+>�%$>���>z]�>+g>���?\7�>��\>��>�Q>�[�?�<?� >��G>�W�>���>���?5��?��?^jBoCBu�As�?T�?4u�A���>���>�K�>�-?%<G@���>�tm>��8?b�?2vSB~�? ۭ?y�VA�?�>��3>�U�>�A>�e?"�?y<6B��@[��?�ŋ@�qB�3@��?W�?T�AۿB�z>���@�>��L>���>ៀ?:!?JC�AY4Y?�0@^�B�M?��>�-�>Ƿ�?Z�?{"B{b?ˎT>۞�AZ�?�@���?��?��?���B��A}��?1�?A9B�WA �B�9?훑>�U�>��Y?\@�6A?���@���?�@�]�B�#?��B�??<f,A��gB��>���>��W?�y?|4B�.AFA?��B�^A�"?`�B�z?�]�A_�zB_kB��B��?G�+A��B��B��B��@z��B�CB��B�B��B��A���?���A�1gB�<B��@ۣ�B��B�nB��A1�B�B�B�FB�x@P .@���B�tB�uA" �B�;ArDB��B��@�j�B��B�@t�B� B	 �B��B��B�B�B+hB�OB�bB�2B��B�JB�QB��B��B��B�cB��B�?���@lZ�B7�B�XA�[xB�sB� B��B�1B�pA��yB�?nL�B�9A��FA��B�B��Bc+B�B�2A�%A�`�B��B�&B�B�B�<B��B��ABCA�T�A��A���B��?��3B��B�B�B{A��B�B�2B�?B��B�B�bB�2B	_#B�EB	��B�dB	��B�jB��B�B�B��A%u<B�:B��B��B��B�k@���B��B�kB��?ƙ�B�B��B��B��B�B�tB,AF8�A���B�B�MB�uAhe?��0B��B�(B�UB��B��B�gB�
B�y?�؝B�8B��@�}YB�7?�N�B�0B�YB��B�=B��B�A?{G�A�ӒB�tB�YB��B�B��B� B��B��A�AH�@A��oB��A�ջA��B��B��B%B��B�2B�OB��B�wB��B�!BYsB��B
e�B�2A�m�A/�-B�lB�iB��A��@���B��B�vB��@��`B��B��B�WB�2B��B�4B��B��B�<B��B�FB�mBKIA=�B�@	e�B��B��A�G�B��B�WB�(B�cA%�@��_B		B�tB��B��?�zWB��B��B�A<�B�9B�&B��@��B�.B��B��A�&B��B��B��B�rB��B�:B�3A��A���B��@��[B�/B�8B�hA#&�B�{B� B	��B�B�A��rB�UB��B
-�B��B��B�MB�4A�'�B�yB��B�lA��B�n@Gy�B��B��B�4B��B��B��B��B�LB��B��B��B��B�eB��B��B��B��B�;B�EB�tB��B�,B�"Ba%B�`B��B�nB�6B��B�UB�KB�rB��B�B� B�wB��B�yB�B��B�kB��B��B�B�BB�~B��B��B�oB��B�B�YB��B��B��B�{B�~B�vB��B�eB�	B��B�)B��B�dB�.B��B�B��B��B�pB��B��B�~B��B�,B��B��B��B�B��B��B��B��B��B�B�ZB�B��B�'B�B�B��B�#B��B�:B�(B��B�B��B�rB�B�2B�5B�-B�KB��B�aB�B��B��B��B��B�B�B��B��B�'B��B�ZB�?B��B��B�B��B��B��B��B��B�WB�kB�QB�RB��B�B��B��B��B��B�zB�&B�bB��B��B�B�B�NB� B�"B��B��B��B�EB�=B��B��B��B�}B�uB�B�JB��B��B�B��B�B�0B�;B��B�IB��B��B�;B��B��B��B��B�B�vB�\B�CB��B��B�6B�{B��B�NB��B��B�B�zB�DB�EB� B��B��B��B�:B�NB��B�PB��B��B�WB��B��B�-B��B��B��B�B�SB�gB��B��B��B�zB� B�NB��B�6B��B��B�ZB��B�TB�B��B�4B��B��B�B��B�+B��B�&B�}B�>B�6B��B	��B	�fB	��B	��B	�LB	��B	��B	��B	�aB	��B	�(B	��B	�>B	�1B	��B	��B	��B	��B	��B	��B	� B	��B	�/B	�zB	��B	��B	��B	�cB	�B	�B	��B	��B	�KB	�B	�$B	�B	�
B	��B	�GB	�xB	�]B	�B	�B	��B	��B	��B	��B	�zB	�mB	��B	��B	��B	��B	�B	�vB	��B	��B	�B	�B	�jB	�B	�zB	��B	��B	�sB	�vB	�,B	�B	�B	��B	��B	�B	��B	�B	��B	�GB	��B	�uB	��B	��B	�EB	��B	�B	�FB	��B	��B	��B	�oB	�"B	��B	�`B	�B	�B	��B	��B	�lB	�OB	�RB	��B	��B	�~B	��B	�qB	�'B	�B	��B	��B	�jB	��B	�kB	��B	��B	��B	�nB	��B	�bB	�B	��B	��B	�B�;B�vB��B�B��B��B�B�>B��B�B�sB�aB�B�B�B��B�tB��B��B��B��B�'B��B��B�=B��B��B�(B�B��B��B�qB��B�B�B��B��B�
B�B�OB��B�B�B�B��B��B�)B�B��B�B��B��B�>B��B��B�BB�B�+B��B�JB�hB��B�B�B��B�B��B�B�GB��B��B�UB�|B��B��B�B�8B�0B�aB�B��B�B��B�rB��B��B��B��B�_B�1B�kB�5B��B�\B��B��B��B��B�B�]B�BB�EB�B��B�HB��B��B�<B�mB�,B�7B��B��B�3B�wB��B�	B�LB�B��B��B�B�1B�cB�4B�xB��B��B��B��B��B�B��B��B��B�$B�{B�MB�*B��B� B�<B��B�\B�	B��B��B�3B��B��B��B�B��B�FB��B��B��B�B�1B�(B�B�5B��B��B��B��B�wB�6B��B�sB�cB�>B�SB��B�WB�OB��B�B��B�B�!B�B�	B�EB�vB��B�B�|B�NB��B�?B�JB�`B�B�B�SB�qB�;B��B��B	��B	�iB	�B	�"B	�B	��B	�B	�qB	��B	�8B	�B	�zB	�B	��B	��B	�B	��B	�vB	�B	�zB	�AB	��B	��B	�B	�B	�5B	��B	�(B	�B	��B	�wB	�jB	��B	�B	�B	��B	��B	�XB	�,B	�B	�#B	��B	��B	��B	��B	��B	��B	�B	�B	�bB	�eB	�9B	�B	�B	�2B	�B	��B	�}B	�B	��B	��B	�@B	�QB	�cB	�B	�B	�MB	�@B	�B	�B	��B	�B	��B	�B	�B	�B	�zB	�B	�zB	�B	�B	�\B	�B	��B	�B	��B	�xB	�B	�B	�B	�$B	�B	�B	�B	�iB	�OB	�2B	�B	�	B	��B	�aB	�B	��B	��B	�B	�hB	�yB	�NB	�2B	�B	�B	��B	�@B	�B	�B	�B	��B	�B	�8B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444334444433444444444444444443444444434444444443444444444444444443444434444444444444443444444434433444434434444444434443444444444444444444443344434444444443443444444344434443344444444443444443444444443444343444444444343443444434434434433343333433333344334333433334433434334334333333333333333333344333333333343433333344333333343443433333333333333333333334333334333433333334333344333333334334343333334433333333443333333333333333333433334333433333333333334343333333443333433343334333433333333434333433333333333334333334333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999B�B�B� B�&B�&B�B� B�B�&B�NB��B�B�,B�*B�7B�BB�BB�WB�fBBÞBÝBğBĠBġBŧB��B�-B�ZB�XB�rB��BuB �B$�B,	B>|BM�Bd`B}�B�_B��B�MB��BĠBǶB��B�"B�?B�BØB�~B�B�~B�eB�BB�KB��BBƭBɿBɼBǰB��B�MB��B�IBhwBF�BQB��BuBI�B}�B�zB�WB�6B�Bw�BbUBU BP�BJ�B@�B6KB,B �BvB
BB�B�B�fB�TBr�BeeB`FBZ#BT BM�B=wB38B#�B0B
��B
�B
�<B
�-B
�LB
n�B
<pB
rB	��B	N�B	(�B	jB	B��B�zB�BưBģBɿB�B�<B�hB�MB��B��B�5B�JB�KB�%B��B��B�BB�B��B��B�jB��B�cB�<B� B�B|�B B�B�!B�%B�?B�JB�=B�8B�,B�%B�-B�+B�B�B�B�!B�B|�B{�By�Bw�Bt�Bp�Bl�Bk�Bi~BgrBd`Bc[BbTBfjBs�Br�Bw�Bu�BdbB]5B_BBiBv�B}�B~�Bt�Bh}Bj�B�Bz�Bv�Br�Bm�Bi�B^>B\/BcYBp�Bm�Bj�Bn�Bn�Bm�Bm�BbUBT BG�B;jB>~B@�BH�BR�B\1Bc]B\/BQ�BF�B8ZB.B'�B%�B#�B"�B!�B"�B"�B!�B"�B#�B%�B&�B+B/#B/"B;jBD�BC�B=xB5EB4AB6LB;kB?�BI�BYBfnBbUB]7BZ%BR�BUBdaBo�Bj�Bi�BdbB\0BF�B@�B?�B>}B>~BB�BJ�BXBj�Bq�B�B�!B�-B�!B�1B�WB�<B�BB�B�-B�NB�kB��B��B��B��B��B��B��B��B��B��B��B�B� B�)B�0B�TBƲB��B��B��B��B��B��BȽBȽB��B��B��B��B��B��B� B�&B�/B�JB�oB�B�B��B��B��B	B	+B	9B	VB	YB	]B	uB	|B	{B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	!�B	!�B	#�B	%�B	&�B	'�B	) B	*B	+B	+B	+
B	,B	.B	25B	4BB	6PB	7RB	<tB	B�B	D�B	G�B	K�B	P�B	Q�B	UB	XB	[)B	\1B	[*B	]7B	`KB	egB	fnB	gvB	h|B	j�B	j�B	j�B	j�B	k�B	m�B	p�B	q�B	s�B	s�B	s�B	u�B	u�B	y�B	z�B	{�B	}�B	�B	�B	�)B	�6B	�ZB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	��B	�B	�	B	�B	�B	�(B	�/B	�7B	�BB	�XB	�pB	�zB	�B	��B	��B	��B	�~B	�rB	��B	��B	��B	��B	ĤB	ūB	ȼB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�*B	�#B	�1B	�<B	�6B	�<B	�JB	�MB	�QB	�NB	�UB	�VB	�WB	�\B	�[B	�oB	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	��B
�B
UB
B
_B
(HB
0^B
5�B
9�B
A�B
H�B
M>B
OdB
TB
Z>B
_�B
eB
j�B
n�B
sB
u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BzB	�G�O�G�O�G�O�G�O�G�O�B	!Bh�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BҶG�O�G�O�G�O�G�O�G�O�G�O�G�O�BIG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BσG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��G�O�G�O�G�O�G�O�Br>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Be�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��rG�O�G�O�Bv�B
��G�O�G�O�G�O�G�O�B�6G�O�G�O�BsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bv�G�O�G�O�G�O�Bv%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BoEBu�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B~�G�O�G�O�A�?�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B�:G�O�G�O�G�O�Aۿ!B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�SG�O�G�O�G�O�G�O�G�O�B{gG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B�ZG�O�B�?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�,G�O�B�EG�O�G�O�B��G�O�G�O�G�O�G�O�B�3G�O�G�O�B�bG�O�G�O�B��G�O�G�O�B_qB��B��G�O�A��B��B��B��G�O�B�EB��B�!B��B��A���G�O�G�O�B�AB��G�O�B��B�uB��G�O�B�B�B�KB�G�O�G�O�B�yB�|G�O�B�@G�O�B��B��G�O�B��B�G�O�B�&B	 �B��B��B�	B�B+mB�UB�fB�7B��B�OB�UB��B��B��B�iB��B� G�O�G�O�B7�B�_A�[B�yB�&B��B�5B�tA�܅B�G�O�B�?G�O�A��B�B��Bc2B�B�7G�O�G�O�B��B�,B�B� B�@B��B��G�O�A�T�G�O�G�O�B��G�O�B��B�B�B{"A��B�B�5B�DB��B�B�hB�5B	_'B�JB	��B�hB	��B�mB��B��B�B��G�O�B�>B��B��B��B�qG�O�B��B�sB��G�O�B�	B��B��B��B�B�yB.G�O�A���B�B�RB�|G�O�G�O�B��B�,B�YB��B��B�pB�B�G�O�B�>B��G�O�B�<G�O�B�9B�\B��B�EB��B�FG�O�G�O�B�yB�`B��B�B��B�%B��B��G�O�G�O�A��sB��A�վA��B��B��B%B��B�7B�RB��B�~B��B�&BYvB��B
e�B�4A�m�G�O�B�qB�oB��A��#G�O�B��B�{B��G�O�B��B��B�]B�7B��B�9B��B��B�@B��B�KB�qBKNG�O�B�!G�O�B��B��A�G�B��B�]B�/B�hG�O�G�O�B	B�yB��B��G�O�B��B��B�G�O�B�?B�,B��G�O�B�4B��B��G�O�B��B��B��B�xB��B�?B�9A��G�O�B��G�O�B�5B�:B�lG�O�B��B�B	��B�B�A��xB�XB��B
-�B��B��B�RB�:G�O�B�~B��B�oA��B�rG�O�B��B��B�:B��B��B��B��B�RB��B��B��B��B�lB��B��B��B��B�AB�IB�zB��B�0B�%Ba'B�dB��B�sB�<B��B�[B�PB�vB��B�B�%B�|B��B��B�B��B�?B�zB��B�B��B��B��B�EB��B�B�zB�fB��B�B�B��B�yB��B��B��B��B�-B��B��B�AB��B��B�,B��B��B��B�vB��B�B��B��B��B�B�B�UB��B�	B�"B�"B��B��B�/B�
B��B�B��B��B�EB��B��B�GB�B�1B��B�NB�lB��B�B�	B��B�"B��B�B�LB��B��B�YB��B��B��B�
B�;B�6B�dB�
B��B�B��B�vB��B��B��B��B�dB�8B�nB�9B��B�bB��B��B��B��B�B�cB�EB�KB��B��B�PB��B��B�?B�qB�0B�<B��B��B�6B�zB��B�B�SB�B��B��B�$B�4B�fB�8B�|B��B��B��B��B��B�B��B��B��B�*B�B�RB�-B��B�B�BB��B�`B�B��B��B�:B��B��B��B�!B��B�KB��B��B��B�B�6B�.B�B�;B��B��B��B��B�|B�<B��B�zB�gB�?B�XB��B�^B�SB��B� B��B�
B�&B�B�B�JB�zB�B�B��B�RB��B�DB�PB�dB� B�B�WB�tB�AB��B��B	��B	�qB	�%B	�)B	�B	��B	�B	�yB	�B	�?B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�HB	� B	��B	�B	�B	�=B	��B	�2B	�B	�B	�~B	�uB	��B	�B	�B	�B	��B	�`B	�4B	�(B	�,B	��B	��B	�B	��B	��B	��B	�B	�B	�hB	�oB	�AB	�B	�B	�;B	�
B	��B	��B	�B	��B	��B	�GB	�XB	�lB	�B	�B	�UB	�GB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�bB	�B	��B	�B	��B	�B	�B	�B	�B	�+B	��B	�B	�B	�pB	�XB	�9B	�B	�B	� B	�hB	�B	�B	��B	��B	�rB	�B	�WB	�9B	�B	�B	��B	�IB	�B	��B	�B	��B	�B	�AB	�B�?B�zB��B�B��B��B��B�EB��B�B�zB�fB��B�B�B��B�yB��B��B��B��B�-B��B��B�AB��B��B�,B��B��B��B�vB��B�B��B��B��B�B�B�UB��B�	B�"B�"B��B��B�/B�
B��B�B��B��B�EB��B��B�GB�B�1B��B�NB�lB��B�B�	B��B�"B��B�B�LB��B��B�YB��B��B��B�
B�;B�6B�dB�
B��B�B��B�vB��B��B��B��B�dB�8B�nB�9B��B�bB��B��B��B��B�B�cB�EB�KB��B��B�PB��B��B�?B�qB�0B�<B��B��B�6B�zB��B�B�SB�B��B��B�$B�4B�fB�8B�|B��B��B��B��B��B�B��B��B��B�*B�B�RB�-B��B�B�BB��B�`B�B��B��B�:B��B��B��B�!B��B�KB��B��B��B�B�6B�.B�B�;B��B��B��B��B�|B�<B��B�zB�gB�?B�XB��B�^B�SB��B� B��B�
B�&B�B�B�JB�zB�B�B��B�RB��B�DB�PB�dB� B�B�WB�tB�AB��B��B	��B	�qB	�%B	�)B	�B	��B	�B	�yB	�B	�?B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�HB	� B	��B	�B	�B	�=B	��B	�2B	�B	�B	�~B	�uB	��B	�B	�B	�B	��B	�`B	�4B	�(B	�,B	��B	��B	�B	��B	��B	��B	�B	�B	�hB	�oB	�AB	�B	�B	�;B	�
B	��B	��B	�B	��B	��B	�GB	�XB	�lB	�B	�B	�UB	�GB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�bB	�B	��B	�B	��B	�B	�B	�B	�B	�+B	��B	�B	�B	�pB	�XB	�9B	�B	�B	� B	�hB	�B	�B	��B	��B	�rB	�B	�WB	�9B	�B	�B	��B	�IB	�B	��B	�B	��B	�B	�AB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444334444433444444444444444443444444434444444443444444444444444443444434444444444444443444444434433444434434444444434443444444444444444444443344434444444443443444444344434443344444444443444443444444443444343444444444343443444434434434433343333433333344334333433334433434334334333333333333333333344333333333343433333344333333343443433333333333333333333334333334333433333334333344333333334334343333334433333333443333333333333333333433334333433333333333334343333333443333433343334333433333333434333433333333333334333334333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455292020082814552920200828145529202008281455292020082814552920200828145529202008281455292020082814552920200828145529202008281455292020082814552920200828145529AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730472019021417304720190214173047    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730472019021417304720190214173047  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730472019021417304720190214173047  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455292020082814552920200828145529  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                