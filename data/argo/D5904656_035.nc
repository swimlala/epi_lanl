CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  Y   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:34Z creation      
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
resolution        =���   axis      Z        (,  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  m<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (,  wH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (,  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (,  Ѭ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, 6   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (, ^H   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �t   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (, ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (, ¸   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (,    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 EH   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (, OT   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � w�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   x@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �dArgo profile    3.1 1.2 19500101000000  20190214173034  20200828145457  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               #   #   #AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @ם��+�&@ם��+�&@ם��+�&111 @ם���Q�@ם���Q�@ם���Q�@6�C��%@6�C��%@6�C��%�c������c������c�����111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    #   #   #ADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C�C�C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�\D��D�_�D���D�ФD�
D�MqD���D���D�{D�G
D���D��=D��D�O\DځHD��=D�qD�L)D�}�D��\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                >L��        =���        =���        =���=���        =���        =���=���            >L��                                >L��    >L��>���        =���=���        >L��>L��        =���    =���        =���=���                =���=���        >L��            >���>���    =���        =���=���        =���=���        >L��=���            =���=���        >���>���        =���=���    >���>���=���            =���                    =���            >���>L��>L��            >L��            =���        =���=���        =���>L��        =���    =���>���        >L��>L��>���    >L��>���?��>���            >L��=���    =���    >L��>���        >L��>L��        >L��=���=���    =���>L��=���        >���>���>L��    =���=���=���        >L��>���?   >���        =���=���>L��=���=���=���    >���>L��    =���    >���>L��=���>L��    =���=���=���>���>L��=���=���    =���=���=���>���>���>L��=���=���>L��>L��        >L��>L��>L��=���>L��?   >���=���>���>L��>���>���>���>���>L��>L��>L��>���>���>���>L��>L��>���>���>���>���>L��>L��>���>���>L��>L��>���?   >���>L��>���>���>���>���>L��>���>���>���>���>���>L��>L��>L��>L��>���>L��>���>���    =���>L��>L��=���>���>L��>L��>���=���=���>���>���>���>���>���>���>L��    >L��>L��>L��>L��>���>L��=���>L��>L��>L��>���>L��>���>���>���>���>���>L��>L��>���>L��>L��>���>L��>���?   >���>���>���>���>���>L��>L��>L��>L��>���>���>���>���>���>L��>L��=���>���=���>���>L��>L��>L��>���>���>���=���=���>���>L��>L��>���>���>���>���>���>���>���?   ?   >���>���>���>���>���>���>���>���>���>���>���?   >���>���>���>���>���>���>���>���>���>���>���>���>���?   ?   >���>���>���?   >���>���>���>���>���>���>���?   >���>L��>���>L��>���>���>���>���>L��>L��>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>L��>���>L��>L��>���>���>���>���>���>���>���>���>���>���>���>���>���?   ?   >���>���?   ?   >���>���>���>���>���>���>���>���>L��>���>L��>L��>���>���>L��=���>���>���>���?��>���?   ?   ?��?333?L��?L��?333?��?333?L��?L��?L��?L��?333?fff?fff?fff?L��?fff?fff?fff?�  ?fff?fff?�  ?���?���?���?���?���?���?���?���?���?���?�ff?���?���?�ff?�ff?�ff?�33?�ff?�33?�33?�33?�33?�  ?�  ?�  ?���?ٙ�?ٙ�?ٙ�?ٙ�?ٙ�?�ff@   @   @   @   @ff@��@33@33@��@   @&ff@&ff@,��@333@9��@9��@Fff@Fff@L��@S33@Y��@`  @l��@l��@s33@y��@�  @�33@�33@�ff@���@���@�  @�33@�33@�ff@���@���@�  @�33@�33@�ff@���@���@�33@�33@�ff@���@���@���@�33@�ff@ə�@���@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@���@�33@�ff@���@���A   A33A��AffA  A	��A��AffA  A��A��AffA  A��A33A��AffA!��A#33A#33A&ffA(  A)��A+33A,��A.ffA1��A333A4��A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAH  AI��AL��ANffAP  AQ��AT��AVffAY��A[33A\��A`  Aa��Ad��Ah  Ai��Al��AnffAq��At��AvffAy��A|��A�  A���A�ffA�33A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�33A�  A���A�33A���A���A�33A���A���A�ffA�  Ař�A�33A�  Aə�A�33A���A͙�A�33A���Aљ�A�33A���Aՙ�A�33A���Aٙ�A�33A�  Aݙ�Dq9�Dq@ DqFfDqS3DqY�Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�DrffDrl�Drs3Dry�Dr�fDr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt��DtٚDt� Dt�f@9��@Fff@Fff@L��@S33@Y��@`  @l��@l��@s33@y��@�  @�33@�33@�ff@���@���@�  @�33@�33@�ff@���@���@�  @�33@�33@�ff@���@���@�33@�33@�ff@���@���@���@�33@�ff@ə�@���@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@���@���@�33@�ff@���@���A   A33A��AffA  A	��A��AffA  A��A��AffA  A��A33A��AffA!��A#33A#33A&ffA(  A)��A+33A,��A.ffA1��A333A4��A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAH  AI��AL��ANffAP  AQ��AT��AVffAY��A[33A\��A`  Aa��Ad��Ah  Ai��Al��AnffAq��At��AvffAy��A|��A�  A���A�ffA�33A���A�ffA�  A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�33A�  A���A�33A���A���A�33A���A���A�ffA�  Ař�A�33A�  Aə�A�33A���A͙�A�33A���Aљ�A�33A���Aՙ�A�33A���Aٙ�A�33A�  Aݙ�Dq9�Dq@ DqFfDqS3DqY�Dq` DqffDql�Dqy�Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�DrffDrl�Drs3Dry�Dr�fDr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  Ds�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs��Ds��Ds� Ds�fDs��DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt33Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt��DtٚDt� Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @*�H@qG�@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�\)A�(�A���A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׽pB۽pB�W
B�W
B�=B�pB�=B�=B��=B��=B��=C�C�C�C�C	�C޹C޹C�C�C�C�C�C�C�C�C�C!޹C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7޹C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDy��D�{D�XRD���D��HD��D�FD��HD���D�D�?�D��RD���D��>D�H D�y�D��D��D�D�D�vgD�� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��k��k��k��k��k��k��k��k����k��k����k��k����k��k������k��k����k��k������k��k��k����k��k��k��k��k��k��k��k����k���=�\+�k��k������k��k������k��k����k����k��k������k��k��k��k������k��k����k��k��k�=�\+=�\+�k����k��k������k��k������k��k������k��k��k������k��k�=�\+=�\+�k��k������k�=�\+=�\+���k��k��k����k��k��k��k��k����k��k��k�=�\+�����k��k��k����k��k��k����k��k������k��k������k��k����k���=�\+�k��k�����=�\+�k���>.{>�p�>.{�k��k��k������k����k���=�\+�k��k������k��k��������k��������k��k�>.{=�\+���k��������k��k���=�\+>�=q=�\+�k��k��������������k�>.{���k����k�>.{�������k�������=�\+�������k�������=�\+>.{�����������k��k�����������>�=q=�\+��=�\+��=�\+=�\+>.{=�\+������=�\+=�\+=�\+����=�\+=�\+>.{=�\+����=�\+=�\+����=�\+>�=q>.{��=�\+=�\+=�\+>.{��=�\+=�\+>.{>.{=�\+��������=�\+��=�\+=�\+�k���������>.{����=�\+����>.{=�\+=�\+=�\+=�\+=�\+���k���������=�\+����������=�\+��>.{=�\+=�\+>.{=�\+����=�\+����=�\+��>.{>�=q>.{=�\+>.{>.{>.{��������=�\+=�\+>.{>.{=�\+������=�\+��=�\+������=�\+=�\+=�\+����=�\+����=�\+=�\+=�\+=�\+>.{>.{>.{>�=q>�=q=�\+=�\+>.{>.{=�\+>.{>.{>.{=�\+=�\+>.{>�=q>.{=�\+>.{>.{>.{>.{>.{>.{>.{>.{=�\+>.{>.{>�=q>�=q>.{>.{>.{>�=q>.{>.{>.{=�\+=�\+=�\+>.{>�=q=�\+��=�\+��=�\+>.{=�\+=�\+����=�\+>.{>.{=�\+=�\+=�\+>.{>.{=�\+=�\+��=�\+=�\+=�\+��=�\+����=�\+=�\+>.{=�\+=�\+>.{=�\+>.{>.{=�\+=�\+>.{>.{>�=q>�=q=�\+>.{>�=q>�=q>.{=�\+=�\+>.{>.{>.{=�\+>.{��=�\+����=�\+=�\+����>.{>.{>.{>�p�>.{>�=q>�=q>�p�>��?�?�>��>�p�>��?�?�?�?�>��?+�?+�?+�?�?+�?+�?+�?E�?+�?+�?E�?^�R?^�R?^�R?^�R?^�R?xQ�?^�R?xQ�?xQ�?xQ�?���?xQ�?^�R?���?���?���?�?���?�?�?�?�?��\?��\?��\?�\)?�(�?�(�?�(�?�(�?�(�?���?�\?�\?�\?�\?�\(?�(�@z�@z�@
�H@G�@�@�@{@$z�@*�H@*�H@7�@7�@>{@Dz�@J�H@QG�@^{@^{@dz�@j�H@qG�@w�@w�@~z@�=q@�p�@���@��
@��
@�
=@�=q@�p�@���@��
@��
@�
=@�=q@�p�@��
@��
@�
=@�=q@�p�@�p�@��
@�
=@�=q@�p�@�p�@ȣ�@��
@�
=@�=q@�p�@أ�@��
@�
=@�=q@�p�@�p�@��
@�
=@�=q@�p�@���@�
=A�A�RAQ�A�A	�A
�RAQ�A�A�A�RAQ�A�A�A�A�RA�A�A�A"�RA$Q�A%�A'�A)�A*�RA-�A/�A1�A2�RA4Q�A5�A7�A9�A<Q�A=�A?�AA�AB�RADQ�AE�AI�AJ�RALQ�AM�AQ�AR�RAU�AW�AY�A\Q�A]�Aa�AdQ�Ae�Ai�Aj�RAm�Aq�Ar�RAu�Ay�A|Q�A}�A��\A�\)A���A��\A�(�A���A��\A�\)A���A��\A�\)A���A��\A�\)A���A��\A�\)A���A�A�\)A�(�A�A�\)A�(�A�A��\A�(�A���A��\A�(�A���A��\A�(�A���A�A�\)A���A�A�\)A�(�A�A�\)A�(�A�A�\)A���A�A�\)A���A�A��\A�(�A�A�\)A�(�A�A�\)A���A�A�\)A���A�A�\)A���A�A�\)A���A�A�\)A�(�A�Dq*�Dq1HDq7�DqD{DqJ�DqQHDqW�Dq^Dqj�DqqHDqw�Dq~Dq��Dq�HDq��Dq�Dq�{Dq�HDq��Dq�Dq�{Dq�HDq׮Dq�Dq�{Dq�HDq��Dq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrJ�DrW�Dr^Drd{Drj�Drw�Dr~Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Ds{Ds
�DsHDsDs${Ds*�Ds1HDs7�DsD{DsJ�DsQHDsW�Dsd{Dsj�DsqHDsw�Ds�{Ds��Ds�HDs��Ds�Ds��Ds�HDs��Ds�Ds��Ds�HDs׮Ds�Ds��Ds�HDs��Ds�Dt
�DtHDt�DtDt${Dt1HDt7�Dt>DtD{DtQHDtW�Dt^Dtd{DtqHDtw�Dt~Dt��Dt�HDt��Dt�Dt��Dt�HDt��Dt�Dt��Dt�HDt׮@*�H@7�@7�@>{@Dz�@J�H@QG�@^{@^{@dz�@j�H@qG�@w�@w�@~z@�=q@�p�@���@��
@��
@�
=@�=q@�p�@���@��
@��
@�
=@�=q@�p�@��
@��
@�
=@�=q@�p�@�p�@��
@�
=@�=q@�p�@�p�@ȣ�@��
@�
=@�=q@�p�@أ�@��
@�
=@�=q@�p�@�p�@��
@�
=@�=q@�p�@���@�
=A�A�RAQ�A�A	�A
�RAQ�A�A�A�RAQ�A�A�A�A�RA�A�A�A"�RA$Q�A%�A'�A)�A*�RA-�A/�A1�A2�RA4Q�A5�A7�A9�A<Q�A=�A?�AA�AB�RADQ�AE�AI�AJ�RALQ�AM�AQ�AR�RAU�AW�AY�A\Q�A]�Aa�AdQ�Ae�Ai�Aj�RAm�Aq�Ar�RAu�Ay�A|Q�A}�A��\A�\)A���A��\A�(�A���A��\A�\)A���A��\A�\)A���A��\A�\)A���A��\A�\)A���A�A�\)A�(�A�A�\)A�(�A�A��\A�(�A���A��\A�(�A���A��\A�(�A���A�A�\)A���A�A�\)A�(�A�A�\)A�(�A�A�\)A���A�A�\)A���A�A��\A�(�A�A�\)A�(�A�A�\)A���A�A�\)A���A�A�\)A���A�A�\)A���A�A�\)A�(�A�Dq*�Dq1HDq7�DqD{DqJ�DqQHDqW�Dq^Dqj�DqqHDqw�Dq~Dq��Dq�HDq��Dq�Dq�{Dq�HDq��Dq�Dq�{Dq�HDq׮Dq�Dq�{Dq�HDq��Dq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrJ�DrW�Dr^Drd{Drj�Drw�Dr~Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Ds{Ds
�DsHDsDs${Ds*�Ds1HDs7�DsD{DsJ�DsQHDsW�Dsd{Dsj�DsqHDsw�Ds�{Ds��Ds�HDs��Ds�Ds��Ds�HDs��Ds�Ds��Ds�HDs׮Ds�Ds��Ds�HDs��Ds�Dt
�DtHDt�DtDt${Dt1HDt7�Dt>DtD{DtQHDtW�Dt^Dtd{DtqHDtw�Dt~Dt��Dt�HDt��Dt�Dt��Dt�HDt��Dt�Dt��Dt�HDt׮G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA��A�bA�"�A�33A�(�A�5?A�9XA�9XA�E�A�;dA�=qA�I�A�Q�A�O�A�XA�ZA�\)A�\)A�^5A�bNA�`BA�dZA�ffA�jA�hsA�ffA�dZA�dZA�ffA�ffA�dZA�dZA�dZA�bNA�bNA�bNA�ZA�XA�`BAÅA�l�A�$�A���A��A���A¶FA�A�A�\)A�/A��A���A�ȴA�|�A�&�A�&�A�VA�A��A���A��A���A�%A��;A��TA��#A�&�A��A��A�A�A�5?A�~�A�t�A��DA���A��yA�7LA���A���A��FA��/A���A�x�A��A��wA�M�A�S�A��RA�z�A��TA��mA���A�M�A���A�ȴA� �A�t�A��A�A�v�A�-A��uA��A��
A��A�=qA���A��uA��7A�&�A��;A�VA��+A�?}A��mA��\A��!A��7A�M�A��\A��A��#A���A�
A}��A|ZA{?}AuƨAq�Aq`BAp�Ao�-Ao�7AodZAn�HAmƨAm�Ak��AkAi�Ag��Afz�Ac�Ab �Aa�TAa��Aa\)Aa"�A\��A\�A[��A[`BAX�AW�AV �AT�RAQXALA�AG�AEAChsAA
=A>��A=C�A;K�A9|�A8{A8-A6ĜA49XA2��A1
=A/A-G�A+��A+l�A*�A)VA(r�A'�#A&ȴA%��A%�A$�A$VA#�hA#G�A#G�A#;dA!�mA!�A �9A ��A A��A/AĜA�
AoA1A�yA�7A�A�/A~�AAx�A�FAI�AXA�AVA�hA��A`BAx�AA	��A5?AK�A�!A;dA�`A��AS�A1'AbA��A ��@�M�@�9X@�|�@���@��@��@��H@�ȴ@�n�@�ff@�?}@��@�~�@��
@�M�@��@�t�@�/@�h@���@�Z@�bN@���@�"�@���@�@�V@���@�7L@�t�@և+@��@�I�@ѩ�@�V@�I�@���@ʇ+@�5?@���@�z�@�(�@Ƨ�@���@�&�@ċD@�b@�l�@���@���@�bN@�ƨ@�=q@�`B@���@���@�1'@�@�E�@�&�@�"�@���@�&�@��j@�(�@��m@�|�@�
=@���@�$�@���@���@��@�?}@��@��@��!@���@��-@�`B@���@�^5@�7L@�%@��j@���@��u@�z�@��@�dZ@���@��\@�^5@��T@�?}@���@��`@�Ĝ@���@�r�@�1'@��@�1@���@�t�@��@�$�@���@�x�@�hs@�G�@��@�1'@��;@��F@���@�@���@��+@�V@�J@���@��@��@�J@��@�@���@��@�p�@�7L@�%@��@���@���@�33@��@�ȴ@��H@��!@���@�ȴ@��@��R@��@��H@���@�n�@�M�@���@���@���@�x�@�G�@�&�@���@���@�r�@�9X@�b@�ƨ@���@�@��!@���@�ff@�@��-@�hs@�`B@�G�@��u@�I�@��@��@�l�@�dZ@�\)@�S�@�+@�
=@��@�ȴ@�J@���@�@���@��7@���@��j@��j@��9@���@�z�@� �@��;@��@�\)@���@�=q@�@��#@���@�O�@���@��/@�Ĝ@��@��D@�r�@�I�@��@���@���@��P@��P@��P@��@�dZ@�dZ@�dZ@�+@��@��@���@���@���@�v�@�{@��@��#@���@�hs@�&�@���@��9@���@��D@�Q�@�  @��;@��w@�l�@�S�@�K�@�K�@�C�@�;d@�33@��@�
=@���@���@��+@�^5@��T@���@�p�@�X@�Dg@{_p@t$@l�4@d�@[�@T�?@N�"@F�@@��@;S�@4�9@-��@)m]@$l"@�F@�<@��@W?@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA���A�XA�\)A���A��FA���A���A��uA��A���A�p�A��A��9A��-A�A�O�A�E�A�ĜA�?}A�p�A��
A���A�x�A��wA��A���A��A�A�1A�K�A��9A���A�A���A���A�=qA���A�9XA��A�ĜA��wA�\)A�x�A���A��yA�33A��uA¼jA���A�A¶FA���A�1A�33A��A�(�A�A�A��A���A�p�A��mA�S�A��9A��\A�l�A���A�A��HA�K�A�ĜA��A��A��HA�VA���A�z�A���A�-A��A���A�XA���A�|�A�VA�{A��^A��uA�O�A��A�-A��;A���A�I�A�A�  A���A�bA�dZA�{A��A§�A��A�bNA�O�A��A��^A�p�A�r�A���A�;dA��RA���A��/A��\A�/A�bA��-A�A�C�A���A���A�33A���A�ĜA��A�/A�hsA�z�A�dZA��\A�A�v�A�+A�9XA�x�A�E�A���A���A���A�A�ffA��A���A�hsA�A�G�A�?}A��
A���A°!A�M�A��7A��HA��FA�~�A�(�A�~�A�hsA�=qA¶FA�I�A���A�ƨA��
A�M�A���A��A�ZA�E�A��
A�;dA�9XA�VA��uA�z�A�~�A©�A�XA�hsA�=qA�JA�r�A�jA�?}A���ADA�ƨA®A�1A���A��A�bA��A�\)A��A�E�A��A���AA�v�A��A�|�A©�A�$�A�|�A��jA�M�A�oA��A�O�A���A©�A�n�A��A��A�x�A���A�-A��A��TA¸RA��A�S�A���A��TA��;A�^5A��/A���A´9A�O�A��+A��A���A�l�A�jA��/A�S�A��HA��mA��A�ƨA�bA��`A��HA��A��
A��
A���A���A��A�A���A���A�+A��A��A��yA��A�ZA���A���A��/A�\)Aº^A�A���A��A�XA��A���A���A��Aº^A�bNA�  A�ĜA��A��A��A��`A�9XA�K�A�S�A��A���A���A²-A��A��A�hsA�E�Aº^A��A��HA��A��A��A�/A��A�9XA���A�?}A�G�A��A���A��wA�=qA�;dA�A��A��mA°!A��TA���A���A��A�ĜA���A¬A��
A��A�ƨA�33A��yA��A��mA��HA��A��`A��HA�VA�;dA�t�A���A��A��HA��yA��yA��/A�|�A\A���A��A�A�bA���A���A�ȴA�ƨA��;A�ĜA�dZA��hA���A²-A��`A��A�ȴA���A���A�ȴA���A���A���A���Aº^A�G�A°!A���A���A�ĜA���A�ƨA¾wA£�A¾wA�ffA�ƨA´9A�{A�A¾wA®Aº^A�ȴA���A���A���A���A�ƨA���A���A��A���A���A��/A��#A��HA��HA���A��#A��A��`A���A�ĜA��A��A��mA��TA��TA�ĜA��A£�A��#A��A��HA��yA��HA��;A���A���A��#A��
A�(�A®A���A¶FA���A���A��#A���A��A���A���A���A���A¸RA��;A��`A��`A��#A��;A��A��A��`A��mA��yA��#A��A��`A��yA��mA��A�G�A��TA��A��A��HA��;A���A���A���A�x�A��mA��A��/A�-A�v�A���A��A���A���A��A��A���A���A���A���A��A��A��A���A���A��A���A���A��A��A��A��A��A���A���A��A���A���A��A��A�1A��A�JA�
=A�oA�{A���A��A�bA�{A��A� �A� �A��A�{A��A�"�A��A��A��A�{A��A��A��A��A��A��A�{A� �A��A�&�A� �A�$�A�"�A�"�A��A��A�{A�oA�bA�bA�VA�bA�bA�bA�oA�oA�bA�bA�{A�oA�{A��A��A��A��A��A��A��A��A��A��A�oA�{A��A��A�{A�{A��A�{A�{A��A��A��A��A��A�VA�1A�VA�%A�bA�{A�oA�VA�JA�JA�VA�
=A�VA��A�1A�JA�
=A�JA�A�%A�A�JA�JA�{A�VA�{A� �A�1'A�33A�7LA�9XA�7LA�9XA�7LA�9XA�;dA�;dA�7LA�7LA�9XA�7LA�7LA�33A�5?A�5?A�33A�5?A�5?A�33A�33A�&�A�&�A�+A�&�A�1'A�(�A�+A�+A�&�A�-A�(�A�"�A�$�A�"�A�"�A�$�A�&�A�&�A�(�A�-A�+A�(�A�1'A�5?A�7LA�7LA�7LA�7LA�5?A�9XA�5?A�7LA�7LA�7LA�33A�;dA�7LA�7LA�7LA�=qA�A�A�A�A�9XA�33A�1'A�33A�7LA�5?A�9XA�9XA�7LA�7LA�?}A�9XA�=qA�9XA�9XA�9XA�;dA�A�A�O�A�?}A�O�A�K�A�C�A�O�A�C�A�I�A�9XA�9XA�9XA�7LA�9XA�7LA�;dA�7LA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�E�A�A�A�?}A�=qA�=qA�;dA�9XA�7LA�?}A�A�A�;dA�C�A�=qA�A�A�Q�A�K�A�M�A�K�A�I�A�O�A�Q�A�I�A�G�A�Q�A�Q�A�O�A�O�A�O�@��y@��H@��H@��H@��H@��@��@���@���@�ȴ@�ȴ@���@��R@��R@��!@���@���@���@���@���@���@���@���@�~�@��+@��+@��+@��+@�~�@�v�@�~�@�v�@�v�@�v�@�v�@�v�@�n�@�n�@�n�@�ff@�ff@�^5@�^5@�V@�V@�V@�M�@�E�@�E�@�=q@�5?@�-@�$�@��@�J@�@��@���@���@���@���@��^@��^@��^@��^@��^@��^@��-@��^@��-@��-@��-@���@���@���@���@���@���@���@���@��7@��7@��@�x�@�x�@�x�@�x�@�x�@�p�@�p�@�p�@�p�@�p�@�hs@�p�@�p�@�p�@�p�@�hs@�hs@�hs@�`B@�`B@�`B@�`B@�X@�`B@�`B@�`B@�X@�X@�O�@�X@�X@�X@�O�@�O�@�O�@�G�@�G�A�oA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A�{A��A��A��A�{A�oA��A��A��A��A��A�VA�VA�JA�%A�VA��A��A�{A�VA�JA�JA�JA�JA�oA�1A�JA�VA�bA�
=A�1A�
=A�1A�JA�bA�{A�bA��A�1'A�33A�7LA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�5?A�7LA�5?A�5?A�33A�33A�33A�33A�-A�/A�+A�+A�+A�&�A�&�A�+A�-A�/A�(�A�/A�+A�$�A�"�A�$�A�$�A�(�A�+A�(�A�+A�+A�-A�/A�1'A�7LA�7LA�5?A�5?A�7LA�9XA�7LA�9XA�7LA�7LA�7LA�7LA�;dA�7LA�9XA�9XA�A�A�?}A�?}A�5?A�5?A�33A�5?A�5?A�9XA�9XA�9XA�7LA�9XA�=qA�=qA�?}A�9XA�;dA�=qA�?}A�I�A�?}A�O�A�G�A�K�A�M�A�I�A�I�A�G�A�=qA�9XA�9XA�9XA�9XA�;dA�;dA�9XA�9XA�;dA�;dA�;dA�=qA�9XA�A�A�=qA�?}A�?}A�=qA�=qA�=qA�9XA�;dA�C�A�;dA�=qA�?}A�?}A�A�A�O�A�M�A�I�A�I�A�O�A�Q�A�M�A�I�A�E�A�O�A�M�A�Q�A�Q�A�O�@��H@��H@��@��H@��@��@��@���@�ȴ@�ȴ@���@��R@��R@��R@���@���@���@���@���@���@���@���@��\@��+@��\@��\@��+@��+@�v�@�~�@�~�@�~�@�v�@�v�@�v�@�v�@�v�@�n�@�ff@�ff@�ff@�^5@�^5@�^5@�V@�M�@�M�@�E�@�=q@�=q@�-@�$�@��@�{@�J@��@��T@���@���@���@�@��^@��^@��^@��^@��^@��^@��-@��-@��-@��-@��-@���@���@���@���@���@���@���@��h@��7@��@��@�x�@�x�@�x�@�x�@�p�@�p�@�p�@�hs@�p�@�p�@�p�@�p�@�p�@�p�@�p�@�p�@�hs@�`B@�`B@�`B@�`B@�`B@�X@�`B@�`B@�X@�X@�X@�X@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�oA��A�bA�"�A�33A�(�A�5?A�9XA�9XA�E�A�;dA�=qA�I�A�Q�A�O�A�XA�ZA�\)A�\)A�^5A�bNA�`BA�dZA�ffA�jA�hsA�ffA�dZA�dZA�ffA�ffA�dZA�dZA�dZA�bNA�bNA�bNA�ZA�XA�`BAÅA�l�A�$�A���A��A���A¶FA�A�A�\)A�/A��A���A�ȴA�|�A�&�A�&�A�VA�A��A���A��A���A�%A��;A��TA��#A�&�A��A��A�A�A�5?A�~�A�t�A��DA���A��yA�7LA���A���A��FA��/A���A�x�A��A��wA�M�A�S�A��RA�z�A��TA��mA���A�M�A���A�ȴA� �A�t�A��A�A�v�A�-A��uA��A��
A��A�=qA���A��uA��7A�&�A��;A�VA��+A�?}A��mA��\A��!A��7A�M�A��\A��A��#A���A�
A}��A|ZA{?}AuƨAq�Aq`BAp�Ao�-Ao�7AodZAn�HAmƨAm�Ak��AkAi�Ag��Afz�Ac�Ab �Aa�TAa��Aa\)Aa"�A\��A\�A[��A[`BAX�AW�AV �AT�RAQXALA�AG�AEAChsAA
=A>��A=C�A;K�A9|�A8{A8-A6ĜA49XA2��A1
=A/A-G�A+��A+l�A*�A)VA(r�A'�#A&ȴA%��A%�A$�A$VA#�hA#G�A#G�A#;dA!�mA!�A �9A ��A A��A/AĜA�
AoA1A�yA�7A�A�/A~�AAx�A�FAI�AXA�AVA�hA��A`BAx�AA	��A5?AK�A�!A;dA�`A��AS�A1'AbA��A ��@�M�@�9X@�|�@���@��@��@��H@�ȴ@�n�@�ff@�?}@��@�~�@��
@�M�@��@�t�@�/@�h@���@�Z@�bN@���@�"�@���@�@�V@���@�7L@�t�@և+@��@�I�@ѩ�@�V@�I�@���@ʇ+@�5?@���@�z�@�(�@Ƨ�@���@�&�@ċD@�b@�l�@���@���@�bN@�ƨ@�=q@�`B@���@���@�1'@�@�E�@�&�@�"�@���@�&�@��j@�(�@��m@�|�@�
=@���@�$�@���@���@��@�?}@��@��@��!@���@��-@�`B@���@�^5@�7L@�%@��j@���@��u@�z�@��@�dZ@���@��\@�^5@��T@�?}@���@��`@�Ĝ@���@�r�@�1'@��@�1@���@�t�@��@�$�@���@�x�@�hs@�G�@��@�1'@��;@��F@���@�@���@��+@�V@�J@���@��@��@�J@��@�@���@��@�p�@�7L@�%@��@���@���@�33@��@�ȴ@��H@��!@���@�ȴ@��@��R@��@��H@���@�n�@�M�@���@���@���@�x�@�G�@�&�@���@���@�r�@�9X@�b@�ƨ@���@�@��!@���@�ff@�@��-@�hs@�`B@�G�@��u@�I�@��@��@�l�@�dZ@�\)@�S�@�+@�
=@��@�ȴ@�J@���@�@���@��7@���@��j@��j@��9@���@�z�@� �@��;@��@�\)@���@�=q@�@��#@���@�O�@���@��/@�Ĝ@��@��D@�r�@�I�@��@���@���@��P@��P@��P@��@�dZ@�dZ@�dZ@�+@��@��@���@���@���@�v�@�{@��@��#@���@�hs@�&�@���@��9@���@��D@�Q�@�  @��;@��w@�l�@�S�@�K�@�K�@�C�@�;d@�33@��@�
=@���@���@��+@�^5@��T@���@�p�G�O�@�Dg@{_p@t$@l�4@d�@[�@T�?@N�"@F�@@��@;S�@4�9@-��@)m]@$l"@�F@�<@��@W?@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA���A�XA�\)A���A��FA���A���A��uA��A���A�p�A��A��9A��-A�A�O�A�E�A�ĜA�?}A�p�A��
A���A�x�A��wA��A���A��A�A�1A�K�A��9A���A�A���A���A�=qA���A�9XA��A�ĜA��wA�\)A�x�A���A��yA�33A��uA¼jA���A�A¶FA���A�1A�33A��A�(�A�A�A��A���A�p�A��mA�S�A��9A��\A�l�A���A�A��HA�K�A�ĜA��A��A��HA�VA���A�z�A���A�-A��A���A�XA���A�|�A�VA�{A��^A��uA�O�A��A�-A��;A���A�I�A�A�  A���A�bA�dZA�{A��A§�A��A�bNA�O�A��A��^A�p�A�r�A���A�;dA��RA���A��/A��\A�/A�bA��-A�A�C�A���A���A�33A���A�ĜA��A�/A�hsA�z�A�dZA��\A�A�v�A�+A�9XA�x�A�E�A���A���A���A�A�ffA��A���A�hsA�A�G�A�?}A��
A���A°!A�M�A��7A��HA��FA�~�A�(�A�~�A�hsA�=qA¶FA�I�A���A�ƨA��
A�M�A���A��A�ZA�E�A��
A�;dA�9XA�VA��uA�z�A�~�A©�A�XA�hsA�=qA�JA�r�A�jA�?}A���ADA�ƨA®A�1A���A��A�bA��A�\)A��A�E�A��A���AA�v�A��A�|�A©�A�$�A�|�A��jA�M�A�oA��A�O�A���A©�A�n�A��A��A�x�A���A�-A��A��TA¸RA��A�S�A���A��TA��;A�^5A��/A���A´9A�O�A��+A��A���A�l�A�jA��/A�S�A��HA��mA��A�ƨA�bA��`A��HA��A��
A��
A���A���A��A�A���A���A�+A��A��A��yA��A�ZA���A���A��/A�\)Aº^A�A���A��A�XA��A���A���A��Aº^A�bNA�  A�ĜA��A��A��A��`A�9XA�K�A�S�A��A���A���A²-A��A��A�hsA�E�Aº^A��A��HA��A��A��A�/A��A�9XA���A�?}A�G�A��A���A��wA�=qA�;dA�A��A��mA°!A��TA���A���A��A�ĜA���A¬A��
A��A�ƨA�33A��yA��A��mA��HA��A��`A��HA�VA�;dA�t�A���A��A��HA��yA��yA��/A�|�A\A���A��A�A�bA���A���A�ȴA�ƨA��;A�ĜA�dZA��hA���A²-A��`A��A�ȴA���A���A�ȴA���A���A���A���Aº^A�G�A°!A���A���A�ĜA���A�ƨA¾wA£�A¾wA�ffA�ƨA´9A�{A�A¾wA®Aº^A�ȴA���A���A���A���A�ƨA���A���A��A���A���A��/A��#A��HA��HA���A��#A��A��`A���A�ĜA��A��A��mA��TA��TA�ĜA��A£�A��#A��A��HA��yA��HA��;A���A���A��#A��
A�(�A®A���A¶FA���A���A��#A���A��A���A���A���A���A¸RA��;A��`A��`A��#A��;A��A��A��`A��mA��yA��#A��A��`A��yA��mA��A�G�A��TA��A��A��HA��;A���A���A���A�x�A��mA��A��/A�-A�v�A���A��A���A���A��A��A���A���A���A���A��A��A��A���A���A��A���A���A��A��A��A��A��A���A���A��A���A���A��A��A�1A��A�JA�
=A�oA�{A���A��A�bA�{A��A� �A� �A��A�{A��A�"�A��A��A��A�{A��A��A��A��A��A��A�{A� �A��A�&�A� �A�$�A�"�A�"�A��A��A�{A�oA�bA�bA�VA�bA�bA�bA�oA�oA�oA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A�{A��A��A��A�{A�oA��A��A��A��A��A�VA�VA�JA�%A�VA��A��A�{A�VA�JA�JA�JA�JA�oA�1A�JA�VA�bA�
=A�1A�
=A�1A�JA�bA�{A�bA��A�1'A�33A�7LA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�5?A�7LA�5?A�5?A�33A�33A�33A�33A�-A�/A�+A�+A�+A�&�A�&�A�+A�-A�/A�(�A�/A�+A�$�A�"�A�$�A�$�A�(�A�+A�(�A�+A�+A�-A�/A�1'A�7LA�7LA�5?A�5?A�7LA�9XA�7LA�9XA�7LA�7LA�7LA�7LA�;dA�7LA�9XA�9XA�A�A�?}A�?}A�5?A�5?A�33A�5?A�5?A�9XA�9XA�9XA�7LA�9XA�=qA�=qA�?}A�9XA�;dA�=qA�?}A�I�A�?}A�O�A�G�A�K�A�M�A�I�A�I�A�G�A�=qA�9XA�9XA�9XA�9XA�;dA�;dA�9XA�9XA�;dA�;dA�;dA�=qA�9XA�A�A�=qA�?}A�?}A�=qA�=qA�=qA�9XA�;dA�C�A�;dA�=qA�?}A�?}A�A�A�O�A�M�A�I�A�I�A�O�A�Q�A�M�A�I�A�E�A�O�A�M�A�Q�A�Q�A�O�@��H@��H@��@��H@��@��@��@���@�ȴ@�ȴ@���@��R@��R@��R@���@���@���@���@���@���@���@���@��\@��+@��\@��\@��+@��+@�v�@�~�@�~�@�~�@�v�@�v�@�v�@�v�@�v�@�n�@�ff@�ff@�ff@�^5@�^5@�^5@�V@�M�@�M�@�E�@�=q@�=q@�-@�$�@��@�{@�J@��@��T@���@���@���@�@��^@��^@��^@��^@��^@��^@��-@��-@��-@��-@��-@���@���@���@���@���@���@���@��h@��7@��@��@�x�@�x�@�x�@�x�@�p�@�p�@�p�@�hs@�p�@�p�@�p�@�p�@�p�@�p�@�p�@�p�@�hs@�`B@�`B@�`B@�`B@�`B@�X@�`B@�`B@�X@�X@�X@�X@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�G�A�oA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A�{A��A��A��A�{A�oA��A��A��A��A��A�VA�VA�JA�%A�VA��A��A�{A�VA�JA�JA�JA�JA�oA�1A�JA�VA�bA�
=A�1A�
=A�1A�JA�bA�{A�bA��A�1'A�33A�7LA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�5?A�7LA�5?A�5?A�33A�33A�33A�33A�-A�/A�+A�+A�+A�&�A�&�A�+A�-A�/A�(�A�/A�+A�$�A�"�A�$�A�$�A�(�A�+A�(�A�+A�+A�-A�/A�1'A�7LA�7LA�5?A�5?A�7LA�9XA�7LA�9XA�7LA�7LA�7LA�7LA�;dA�7LA�9XA�9XA�A�A�?}A�?}A�5?A�5?A�33A�5?A�5?A�9XA�9XA�9XA�7LA�9XA�=qA�=qA�?}A�9XA�;dA�=qA�?}A�I�A�?}A�O�A�G�A�K�A�M�A�I�A�I�A�G�A�=qA�9XA�9XA�9XA�9XA�;dA�;dA�9XA�9XA�;dA�;dA�;dA�=qA�9XA�A�A�=qA�?}A�?}A�=qA�=qA�=qA�9XA�;dA�C�A�;dA�=qA�?}A�?}A�A�A�O�A�M�A�I�A�I�A�O�A�Q�A�M�A�I�A�E�A�O�A�M�A�Q�A�Q�A�O�@��H@��H@��@��H@��@��@��@���@�ȴ@�ȴ@���@��R@��R@��R@���@���@���@���@���@���@���@���@��\@��+@��\@��\@��+@��+@�v�@�~�@�~�@�~�@�v�@�v�@�v�@�v�@�v�@�n�@�ff@�ff@�ff@�^5@�^5@�^5@�V@�M�@�M�@�E�@�=q@�=q@�-@�$�@��@�{@�J@��@��T@���@���@���@�@��^@��^@��^@��^@��^@��^@��-@��-@��-@��-@��-@���@���@���@���@���@���@���@��h@��7@��@��@�x�@�x�@�x�@�x�@�p�@�p�@�p�@�hs@�p�@�p�@�p�@�p�@�p�@�p�@�p�@�p�@�hs@�`B@�`B@�`B@�`B@�`B@�X@�`B@�`B@�X@�X@�X@�X@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=OV�=w��=��+=|�=��=���=�#>KH�@�\}=��>M�?�j+=,�=E�N=Ew�=``�=��=o�=�˼=��m>���=@�=8<�=X�=y��={�t=�r2=��$?=H�@��=7�=H�#=e��=�vK=`��=^��=��=�{�?��>:<!@�x@w��=�*�=��`>�>�=��=���>3�0@��9?��*>"��@�'g>�I(=�[->�b�=#�=5��=I��=Yjj=`��=�i�=��y=�ɰ=� 2=��>ek@	�n@G%F=���=��>�k@���>捹=&E==�=^5?=r��=�4�=��=�,R>�D@V�=�77=�x>8v�@��[=ᰊ>j-�?}qL=���@7�	>ὥ=��>��D@���?�!�=�f�>9؄=��.=��k>OF@���@~��=-�=-��=7�-=Xb=�S&=w'=r[=��I=�`�=��=���=��>\>@p��@���=��i=�y)=�p=�&>�V=�+�=b�A=oܜ=�7�=��=�-�>x2v=��)=Ӊ�=�#d>"8�@�QY?OS�=�)�?�:?=�^=��>N�@��u=Κ,>9Z�@���@��%?�{>m��@��q@��B@��?��P=��=ˡ�>6�@���=���=��H=�A�>H��@���@���=���>J5�@��x=�c�>�>N`�?fQ@p�=�U�>"�p@x2�@y =�>��@CB@��@���>��=��>;�@�<=�L�=�>>�;�@�Ѣ@��b@��?l�]=��q=���=�6>Ƙ5@C�>Q?��	=�6@��@�ݘ=�)�=���>(�M@��$@��?<��?��?=u��=��]=��w>:�>͞�@�݃@���><�?���>/��?�7">.�?�y@���@��=�h�=���>T�H@aJM?_|�>�h?���@���@���@�>��&@���@���@��4>[��@���@Q�@��@���@���@��?>��@��@��o@��f@��^@���?~V@��U@���?��e@���@��B>�V@��d@���@���@��e?(z�@��@���@���>v��@��o@�N@��@���>���@��@��@���@��@��@K�,>R�P@�?)@��@@���@��|@��o@��>!�`@��@��@��@���@��M@�@��o@��>�3@�b9@��@���@��^@��^@���@��>Ca?��@��?<��>*|[@��@���>UB?{��@���@���@���@���@���@���?l;O@��g@��@��Z@B��@��8@���@��@���@��6@���@��@���@��@��@��@��@��@Cֶ@��5@9�@��@��k@��_@��@��@��@��F@�'@��m?�rq@#h�@��g@��@���@��@���@��`@��>]�?� ?@��@��@��@��X@��@��[@��[@���@��@��@��W@��@T},@��l@��4@���@��;@��@��?@��@���@��O@��@���@���@��@���@��O@��@��`@��@��`@��@��x@��t@��@��@��o@��@��g@��g@��@���@��@��@���@��(@��(@��@��@��?!A�@���@��(@���@��x@��g?���@��S>G�@��^@���@���@��4@��4@��x@���@��@��[@��@���@���@��@��@��[@?˒@��|@U5�@��@��@��@��@��l@��@���@��@���@��E@���@��E@��E@���@���@���@��@��@���@��4@��_?~R@��8@���@��E@��@��
@��>ᓴ@���>P��@��j@���@��@g��@+(�@��H@���@��@���@���@��Y@���@��"@��+@��@���@��@���@���@���@��@���@��U@��Y@��Y@���@��@���@���@��@��]@� @���@���@��	@��@��@�@�@�:@��@��@��@��@�	�@��@�
�@�
=@��@�:@�N@��@�N@�N@�
�@�
�@�	�@�
=@�
�@��@�	�@�J@�Z@�Z@�@��@��@��@��@��@��@��@��@�)@�)@�)@�)@�)@��@�:@��@��@��@��@��@��@��@�F@�F@��@��@��@��@�F@��@��@�F@�J@��@��@��@�F@��@��@�F@��@�	W@�
R@�	�@��@��@�>@��@��@��@�
�@�	�@��@��@��@��@��@��@�	W@��@��@��@��@��@�C@�>@��@��@��@�
�@�	�@�@�@��@��@�@@�@@��@�@@�@@��@�j@�j@�n@��@�D@�D@��@��@��@�3@��@��@�3@��@��@��@�j@��@��@��@�<@��@�8@��@��@��@��@�E@�o@�@��@��@�M@��@��@�@�H@�P@��@��@�a@�3@�3@��@��@�	@�\@�	@�\@�	@�@�m@�@�*@�6@��@�K@��@��@�q@��@�~@��@�&@��@��@� G@�"�@� �@� 2@��@��@� �@�"�@�&�@�$J@�'R@�$J@�$�@�&W@�%@�#O@�"S@� �@��@��@�6@��@��@� \@��@� \@� G@�!B@� �@�!�@�"S@�#O@�#@�#d@�$@�!W@�"�@�!�@�!W@�"S@�$@�$�@�$@�%�@�%�@�'g@�+@�(�@�*�@�*0@�+,@�-8@�+�@�*�@�-M@�-�@�-8@�-�@�-�@�-�@Qo�@Qo~@Qo*@Qn�@Qn�@Qn�@QnY@Qm�@Qm	@Qla@Qk�@Qj�@Qjj@Qi�@Qh@Qg�@Qgw@Qgw@Qg�@Qgw@Qgw@Qf�@Qd�@Qd0@Qe,@Qd�@Qd0@Qc�@Qb�@Qb�@Qb�@Qb�@Qb9@Qa�@Qa�@Qa�@Qag@Q`�@Q`@Q`@Q_p@Q^�@Q^ @Q]�@Q]%@Q\)@Q[W@QZ�@QY�@QY6@QW�@QVC@QT�@QS�@QR*@QO�@QM@@QK�@QK�@QKI@QJM@QI�@QI�@QI�@QI�@QIR@QIR@QH�@QH�@QH�@QH�@QH@QG�@QGZ@QG@QGZ@QF_@QF@QE@QD=@QCB@QB�@QA�@QA�@QA�@QA�@QA�@QAJ@Q@�@QAt@QA @Q@�@QA�@QAJ@Q@�@Q@�@QAJ@QAJ@Q@�@Q@O@Q?�@Q?�@Q?�@Q?S@Q?S@Q?�@Q?S@Q?S@Q?S@Q>�@Q>�@Q>�@Q>�@Q>�@Q>@Q=�@Q>W@Q=\@Q=\@Q=@��@�S@�y@�y@�d@�y@��@�5@��@�@��@��@��@�y@�@��@��@�d@��@�y@��@�%@��@�y@�@�
(@�[@�F@�C@��@�@��@��@��@�	�@�5@�C@��@�G@��@��@�d@�u@��@�G@�C@��@�!@��@��@�@�)@�J@��@�
�@�0@�@�H@��@��@��@�]@�]@��@��@�/@��@��@��@��@�]@��@�w@��@��@��@��@�@��@��@��@��@�M@�8@��@��@�@��@��@�Z@��@�=@��@��@��@�Z@��@�@�E@�<@�f@�w@�j@�U@��@��@��@�&@��@�z@�&@�;@��@�+@�\@��@�;@�L@��@�O@��@��@�j@�U@��@��@��@��@��@�H@�v@�&@��@�K@��@�m@�~@��@�$�@��@�&�@�#y@�$�@�%�@�$_@�"h@�%�@�d@��@�C@��@��@�@�@�@��@�~@�d@�i@�u@�T@�@��@�6@� �@��@�u@��@��@��@�!B@��@� �@� �@��@�!@�'�@�&l@�%�@�%F@�'g@�)�@�(�@�'=@�$�@�)�@�(�@�)J@�*�@�(�@Qf�@QfQ@Qf'@Qf'@Qf'@Qe�@Qe�@Qe@Qd0@Qc^@Qb�@Qa�@Qa@Q`�@Q_@Q]�@Q]�@Q]�@Q^ @Q^ @Q^ @Q^J@Q\�@QY�@Q\)@Q\)@QZ�@QZ�@QY@QY6@QZ@QY6@QX�@QX:@QXd@QXd@QX�@QW�@QW?@QW@QV�@QU�@QU�@QUG@QT�@QTL@QSP@QR�@QR @QQY@QO�@QNf@QM@QK�@QJw@QH@QD�@QBF@QA�@QA�@Q@�@Q?�@Q?�@Q?S@Q?S@Q?S@Q?)@Q>�@Q>�@Q>�@Q>�@Q>�@Q>@Q=�@Q=2@Q=2@Q=@Q<`@Q;�@Q:i@Q9�@Q8�@Q7v@Q7L@Q7L@Q7v@Q6�@Q6�@Q6�@Q6z@Q6P@Q6z@Q6�@Q6z@Q6P@Q6�@Q6P@Q6�@Q6z@Q6&@Q5+@Q5~@Q4�@Q5+@Q5@Q5@Q4�@Q5@Q4�@Q4�@Q4�@Q4/@Q4/@Q4/@Q4@Q3�@Q4@Q3�@Q3	@Q3	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         4444444434444444444444444444444444444444334444443443444444444444444344434444444443444344443444344444433444444444444433444444444444444434444443443344333444434444334434444344334443344444443334444444444344433444444433444444334443444333433343333334333334334334333343334343343333334333333433433433433333334434433443333334333433333333333334343333333434433333334433333333333333333333333334333333333333333333333333343333343433333333333333343333333333333333333333343333334343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�\zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�x@w��G�O�G�O�G�O�G�O�G�O�G�O�@��:G�O�G�O�@�'kG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@G%MG�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@V�G�O�G�O�G�O�@��ZG�O�G�O�G�O�G�O�@7�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�@���@~��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@p��@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Q[G�O�G�O�G�O�G�O�G�O�G�O�@��tG�O�G�O�@���@��&G�O�G�O�@��q@��D@��G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@���@���G�O�G�O�@��xG�O�G�O�G�O�G�O�@p�G�O�G�O�@x2�@y G�O�G�O�G�O�@��@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Ѡ@��e@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ݘG�O�G�O�G�O�@��#@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�݆@���G�O�G�O�G�O�G�O�G�O�G�O�@���@��G�O�G�O�G�O�@aJKG�O�G�O�G�O�@���@���@�G�O�@���@���@��5G�O�@���@Q�@��~@���@���@��G�O�@��@��o@��f@��^@���G�O�@��U@���G�O�@���@��AG�O�@��c@���@���@��aG�O�@��@���@���G�O�@��lG�O�@��@���G�O�@��@��"@���@��!@��@K�,G�O�@�?(@��A@���@��v@��o@��G�O�@��@��G�O�@���@��KG�O�@��n@��G�O�@�b<@��@���@��]@��]@���@��G�O�G�O�@��G�O�G�O�@��@���G�O�G�O�@���@���@���@���@���@���G�O�@��g@��@��\G�O�@��:@���@��@���@��6@���@��@���@��@��@��@��@��G�O�@��4G�O�@��@��n@��_@��@��@��@��BG�O�@��oG�O�G�O�@��f@��@���@��@��@��_@��G�O�G�O�@��@��@��@��[@��@��Y@��^@���@��@��@��X@��@T}*@��k@��1@���@��9@��@��>@��@���@��L@��@���@���G�O�@���@��O@��@��a@��@��^@��@��z@��t@��@��@��n@��@��e@��e@��@���@��@��@���@��*@��&@��@��@��G�O�@���@��*@���@��z@��fG�O�@��RG�O�@��^@���@���@��2@��2@��z@���@��@��\@��@���@���@��@��
@��YG�O�@��|@U5�@��@��@��@��@��n@��~@���@��@���@��C@���@��F@��F@���@���@���@��@��@���@��6@��bG�O�@��6@���@��C@��@��
@��G�O�@���G�O�@��m@���@��@g��G�O�@��F@���@��@���@���@��W@���@��&@��+@��@���@��!@���@���@���@���@���@��T@��Z@��Z@���@��@���@���@��@��^@� @��@���@��@��@��@�@�@�9@��@��@��@��@�	�@��@�
�@�
A@��@�<@�R@��@�O@�R@�
�@�
�@�	�@�
<@�
�@��@�	�@�P@�Z@�^@�
@�@��@��@��@��@��@��@��@�'@�)@�)@�,@�*@��@�8@��@��@��@�S@�u@�|@�g@�~@��@�6@��@�	@��@��@��@�w@�@��@��@�f@��@�z@��@�"@��@�w@�@�
*@�[@�F@�F@��@�@��@��@��@�	�@�7@�C@��@�L@��@��@�c@�y@��@�K@�E@��@� @��@��@�@�,@�J@��@�
�@�0@��@�H@��@��@��@�]@�[@��@��@�0@��@��@��@��@�`@��@�x@��@��@��@��@�@��@��@��@��@�N@�5@��@��@�@��@��@�Y@��@�:@��@��@��@�_@��@�@�C@�9@�d@�v@�j@�T@��@��@��@�%@��@�y@�(@�>@��@�-@�^@��@�;@�O@��@�P@��@��@�j@�T@��@��@��@��@��@�G@�y@�&@��@�L@��@�k@�~@��@�$�@��@�&~@�#w@�$�@�%�@�$^@�"i@�%�@�i@��@�F@��@��@�@�@�@��@�~@�c@�j@�t@�T@�@��@�4@� �@��@�w@��@��@��@�!A@��@� �@� �@��@�!@�'�@�&p@�%�@�%F@�'i@�)�@�(�@�'>@�$�@�)�@�(�@�)F@�*�@�(�@Qf�@QfR@Qf&@Qf&@Qf&@Qe�@Qe�@Qe@Qd2@Qc`@Qb�@Qa�@Qa@Q`�@Q_@Q]�@Q]�@Q]�@Q^@Q^ @Q^#@Q^N@Q\�@QY�@Q\+@Q\%@QZ�@QZ�@QY
@QY5@QZ@QY:@QX�@QX:@QXf@QXf@QX�@QW�@QW>@QW@QV�@QU�@QU�@QUF@QT�@QTM@QSR@QR�@QR@QQ[@QO�@QNe@QM@QK�@QJr@QH@QD�@QBE@QA�@QA�@Q@�@Q?�@Q?�@Q?S@Q?S@Q?X@Q?(@Q? @Q>�@Q>�@Q>�@Q>�@Q>@Q=�@Q=3@Q=3@Q=
@Q<b@Q;�@Q:h@Q9�@Q8�@Q7u@Q7K@Q7J@Q7x@Q6�@Q6�@Q6�@Q6{@Q6S@Q6{@Q6�@Q6{@Q6S@Q6�@Q6P@Q6�@Q6{@Q6#@Q5.@Q5~@Q4�@Q5+@Q4�@Q4�@Q4�@Q5 @Q4�@Q4�@Q4�@Q4-@Q4*@Q40@Q4@Q3�@Q4@Q3�@Q3@Q3
@��@�S@�u@�|@�g@�~@��@�6@��@�	@��@��@��@�w@�@��@��@�f@��@�z@��@�"@��@�w@�@�
*@�[@�F@�F@��@�@��@��@��@�	�@�7@�C@��@�L@��@��@�c@�y@��@�K@�E@��@� @��@��@�@�,@�J@��@�
�@�0@��@�H@��@��@��@�]@�[@��@��@�0@��@��@��@��@�`@��@�x@��@��@��@��@�@��@��@��@��@�N@�5@��@��@�@��@��@�Y@��@�:@��@��@��@�_@��@�@�C@�9@�d@�v@�j@�T@��@��@��@�%@��@�y@�(@�>@��@�-@�^@��@�;@�O@��@�P@��@��@�j@�T@��@��@��@��@��@�G@�y@�&@��@�L@��@�k@�~@��@�$�@��@�&~@�#w@�$�@�%�@�$^@�"i@�%�@�i@��@�F@��@��@�@�@�@��@�~@�c@�j@�t@�T@�@��@�4@� �@��@�w@��@��@��@�!A@��@� �@� �@��@�!@�'�@�&p@�%�@�%F@�'i@�)�@�(�@�'>@�$�@�)�@�(�@�)F@�*�@�(�@Qf�@QfR@Qf&@Qf&@Qf&@Qe�@Qe�@Qe@Qd2@Qc`@Qb�@Qa�@Qa@Q`�@Q_@Q]�@Q]�@Q]�@Q^@Q^ @Q^#@Q^N@Q\�@QY�@Q\+@Q\%@QZ�@QZ�@QY
@QY5@QZ@QY:@QX�@QX:@QXf@QXf@QX�@QW�@QW>@QW@QV�@QU�@QU�@QUF@QT�@QTM@QSR@QR�@QR@QQ[@QO�@QNe@QM@QK�@QJr@QH@QD�@QBE@QA�@QA�@Q@�@Q?�@Q?�@Q?S@Q?S@Q?X@Q?(@Q? @Q>�@Q>�@Q>�@Q>�@Q>@Q=�@Q=3@Q=3@Q=
@Q<b@Q;�@Q:h@Q9�@Q8�@Q7u@Q7K@Q7J@Q7x@Q6�@Q6�@Q6�@Q6{@Q6S@Q6{@Q6�@Q6{@Q6S@Q6�@Q6P@Q6�@Q6{@Q6#@Q5.@Q5~@Q4�@Q5+@Q4�@Q4�@Q4�@Q5 @Q4�@Q4�@Q4�@Q4-@Q4*@Q40@Q4@Q3�@Q4@Q3�@Q3@Q3
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         4444444434444444444444444444444444444444334444443443444444444444444344434444444443444344443444344444433444444444444433444444444444444434444443443344333444434444334434444344334443344444443334444444444344433444444433444444334443444333433343333334333334334334333343334343343333334333333433433433433333334434433443333334333433333333333334343333333434433333334433333333333333333333333334333333333333333333333333343333343433333333333333343333333333333333333333343333334343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�,39�,�9�-�9�-�9�-�9�-�9�.9�.�9�.9�/E9�/
9�/
9�/
9�-�9�.^9�-Q9�.'9�-�9�.M9�-�9�.9�-�9�-d9�-�9�.a9�169�/�9�/}9�+�9�,Z9�*�9�)t9�,9�.�9�0�9�.�9�+�9�+w9�*�9�+v9�*�9�-�9�*49�*h9�*�9�+�9�++9�)�9�*~9�*~9�+�9�,�9�.�9�-S9�1�9�8�9�9�9�<@9�<|9�<�9�<�9�<S9�<Q9�<�9�<�9�=9�<�9�<�9�<�9�;9�<V9�:�9�;�9�;9�:�9�:�9�:�9�9W9�9�9�89�7�9�7A9�6�9�6�9�79�7�9�;&9�7>9�9�9�8�9�669�5�9�689�659�7�9�8�9�7@9�8a9�8�9�:`9�:�9�;�9�>29�>9�=�9�>h9�>n9�>�9�?b9�?)9�>�9�>�9�>�9�=�9�@�9�?d9�>�9�?�9�C9�C�9�CE9�?�9�>29�>9�>l9�>�9�@�9�AS9�@"9�@�9�@9�Ck9�C19�Dw9�AQ9�A�9�B�9�B�9�I�9�B�9�K9�HC9�I>9�J�9�I9�GM9�J69�C�9�AS9�A�9�AS9�A9�A�9�Bs9�Bs9�B9�B�9�C�9�B�9�D�9�B�9�D<9�D�9�Da9�E�9�D9�D�9�D�9�BM9�B�9�F?9�D*9�E�9�E�9�D�9�F9�La9�J�9�Jp9�I�9�K�9�M�9�L�9�K�9�I@9�M�9�M9�M�9�N�9�M9>�9>�y9>�Q9>�Q9>�Q9>�9>Ⱥ9>�H9>Ǌ9>��9>�X9>�$9>į9>Č9>��9>��9>��9>��9>� 9>�9>�9>�,9>��9>��9>�99>�49>�9>��9>�`9>��9>�B9>��9>�99>��9>��9>��9>��9>�V9>��9>��9>�L9>��9>�e9>��9>��9>�9>�*9>��9>��9>�_9>�9>��9>�x9>�G9>�9>��9>��9>��9>�/9>�	9>�J9>�;9>�D9>��9>��9>��9>��9>��9>�9>��9>��9>�39>��9>�P9>�9>�9>��9>�D9>��9>�w9>��9>��9>��9>��9>��9>��9>�S9>�S9>�	9>��9>��9>��9>�9>��9>��9>�	9>��9>�	9>��9>��9>��9>��9>�g9>��9>��9>��9>�c9>��9>�>9>�>9>�9>��9>��9>��9>��9>�9>��9>�/9>��9>��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BJ�BI�BI�BI�BI�BI�BJ�BJ�BK�BK�BJ�BJ�BK�BL�BL�BM�BM�BM�BM�BN�BN�BN�BH�BO�BO�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BM�BN�BT�Bk�B�%B�B�LB�XB�jB��BĜBŢB��B�B�B�/B�sB+B&�BA�B-B+B)�B'�B5?B[#Bs�Bx�Bz�B�B��B�3BB�wB�FB�B��B��B�PB}�Bs�Bo�Bm�BgmBdZBdZBe`BdZBcTBYBI�B:^BoB��B��B�!B��B��B��B��BhsBYBN�BD�B<jB"�B
=BB
��B
��B
�yB
��B
�}B
��B
�bB
k�B
YB
Q�B
L�B
D�B
:^B
33B
33B
0!B
)�B
(�B
%�B
�B
PB
B	��B	�
B	��B	�dB	�9B	�-B	�!B	�B	�B	��B	��B	��B	�uB	�DB	�B	{�B	n�B	cTB	aHB	_;B	]/B	YB	?}B	:^B	7LB	33B	"�B	�B	VB	B�B��B�RB��B��B�oB�+B�B�B�B�B�=B�hB��B�Bx�Be`B^5B`BBaHBcTBffBffBgmBhsBhsBhsBiyBiyBjBjBjBhsBk�BjBjBiyBgmBe`BcTBbNBaHB_;B]/B[#BXBW
BVBVBT�BQ�BP�BN�BL�BK�BH�BG�BF�BD�BC�BA�B?}B9XB5?B33B33B33B33B33B6FB8RB;dB:^B;dB@�B@�BG�BK�BM�BN�BP�BT�BT�BT�BP�BQ�BVBR�BO�BJ�BE�B9XB6FB49B49B1'B.B/B2-B33B33B33B6FB8RB8RB6FB6FB8RB6FB6FB7LB7LB;dB>wB?}BC�BF�BI�BJ�BK�BL�BN�BN�BR�BR�BYB\)B]/B^5B_;BbNBcTBffBn�Bt�Bx�Bz�B}�B~�B�B�B�%B�7B�DB�PB�PB�PB�oB��B��B��B��B��B��B�B�B�B�B�B�B�'B�LB�jB�}BÖBÖBƨBɺB��B��B��B��B��B��B��B��B��B��B�
B�)B�BB�`B�sB�B�B�B�B�B��B��B	B	B	B	%B	+B	1B	1B	
=B	JB	PB	\B	{B	�B	�B	�B	�B	�B	$�B	(�B	(�B	)�B	,B	,B	-B	2-B	5?B	6FB	<jB	D�B	I�B	J�B	J�B	K�B	N�B	P�B	R�B	YB	ZB	[#B	]/B	_;B	aHB	bNB	e`B	gmB	l�B	n�B	o�B	t�B	y�B	~�B	�B	�B	�B	�+B	�1B	�DB	�\B	�hB	�hB	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�9B	�?B	�LB	�RB	�RB	�XB	�^B	�^B	�jB	�jB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	B	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�B	��B
�B
 B
KB
$�B
,�B
2B
9$B
=�B
D�B
IlB
P.B
TFB
ZB
`BB
cTB
e�B
i�B
m�B
qvG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>���>��l>���>�n�>���??�țB(?(��?��U@�Y>n�>��p>�;�>��>���>�@�>��<>�y�?�#[>���>~p�>� G>�UC>��>�Dc?&g@�'AH��>���>��e>�u�>��>�c�>��>��?�|AL^K?zl+B
�^A�#5>�*c?(| @�L>��?�?{hjB	A@��g?_*�BG?ɼM?*��@L�>e�$>y��>�T�>��Y>�?�>���>���>�3?*�?
e?.�,AR�DA��>ڍe?ߠ@)�WB
�@1�~>dɆ>�4>�eA>�1�>ˈ�>�2�??EjA�u>��z?��?vI�B��?#{[?�4j@���?��A�N�@+��?-��?�3�B
�A�>�'?��H>���?$*�?��[B&�A���>p->o�>zu�>�d0>���>���>���>��>�g�>��s>�?`?�5?E%A�ԈB�>��>���?U�?,� @�'>Ֆ�>��G>��~>��J>���?,�I?�g�?2�?�?��?_0sA�l|@�I^?)�@�yJ>���?GR?���BUf?0�?w%�BOSBF�@�I?�1�B*�B3�BW�A;G�>�5?^�?A�BIS>�^�>��?�,?�z�B�BC�?*pL?�:Bޭ?3�?<hU?���@Y��A��z?�j?aaA��A�2{?]?9ѳAGx�BM�B��?LIw?5o?Q�iAt�R>�k�?5Ѕ?��`BO�B=B1�@�y>�)�>��?"��@�~Av(?>��A:��?"�7A^NNB^�>�wO>���?g5LB^�B�-@��@�z�>���>�y�>�N�?3��@�BBN�B:3?B��AIT�?k��@ҾO?O��A�BI�BW�>ߟ#?*�?�B�A�7@���?78P@�'7B89B=7A�k-?�UuBItBO�B|?���BMA�w�BLBN�BL<BP@�'�B��BO�BLBR�BV(@�C�BL�BN�AB
�BL�B߸?J^�B	*BM5BM�BY�@nKBI�BL4BT�?���BZ>AgeVBG�B� @�BN�BJ|BKBM�B[�A�ń?�/BB�BKA��BHFBI�B�?`j<B�BJ�AkV�BHYBd�AaۄBE�A��?4VA�J%BJ�BPBJBJBI~B��?�y@܁�BTU@�6?g�BLBI>?��@��^BX�B`BBG�BHB��BF�@�^�BD�BG�BV.A�6BbyBNxBFBU�BQ�BIABC#BF/BF�BC+BFRBF�B~(A��;B�CAd�BE�BLBF�BG0BH�B��B�7A|�B& A4�A}CBR�BH�BS(BSCBHpBP�Bv^?7�5@��hB] B?BG�BL!BNhBM�BO�BO�BO�BN<BN]BS�A��BZvB[[BYpBJ�BJ�BIBM�BZBQ�Br�BR�BP�A]rPBOBQ�BXBT�BO�BK�BN�BO�BP�BQZBS�BP�BN�BM�BM�BMFBM�BK�BM�BO<BOBG#BI3BM~BS�@g�BJaBJSBJ�BHuBSa@���B[f?�&�BJ�BMhBMBN�BOoBQ9BQ�BM&BJIB�BVBNBV�BM3BM�A���BSfA�7�BM�BKlBN�BL�BWFBL�BN=BLmBQ�BPfBK�BJBNBK�BKBQ�BJ*BMQBL�BL.BMA@8NBL�BKpBK�BM8BOGBT;@�tBU`?�#�BNZBM	BR�A�ȲA��_BKfBNMBMBKBLABM*BM�BMZBKzBL
BL�BM�BMdBLBL�BM�BL6BK�BMBMBLWBM�BOBMwBLBP�BP�BL�BO]BPuBQ_BH�BK�BLmBL)BL�BR�BK�BNuBN{BJ�BJ�BJYBK�BKLBODBKBLBLBKnBOjBLUBN3BNBM>BL�BNvBQ�BMBO'BL8BK�BJ�BK�BJ�BHPBJ�BJcBJ�BK�BK�BLKBKvBK�BLnBK�BK�BLBL�BL>BMBL-BK�BK�BK$BL?BJ�BKbBJ�BK1BI�BKwBLBK�BJ�BKBL"BK�BJ�BLBL�BK_BLKBJ]BI4BHBKYBMnBK�BN)BO�BL�BK!BK�BL�BM0BLdBM�BN�BF�BM�BMBL�BMDBM�BM�BOnBL�BNBMWBOBO�BR8BL�BMUBLRBK~BL�BKeBL(BK�BJ�BJ�BK:BJ�BJ6BJ�BJ�BK�BKBJ�BLBK;BJ�BJ�BH�BL�BMWBK�BNQBK9BM%BL�BM5BM;BK�BL�BM(BL�BM�BM5BM�BMBN{BL�BMABNgBNqBNBM�BL�BL�BM?BM6BN@BL�BM�BM#BL�BM
BNBBL�BM�BM�BNnBM�BL�BLQBM�BM�BN�BN[BNOBPNBNBNYBOBP�BO�BPaBNBNBO7BO�BQ=BRQBJhBS�BJGBL%BP�BJ�BM�BJ�BOHBMLBM�BN�BNBN�BM�BN�BM�BM�BN�BN<BOBN�BO�BLBM�BO3BMnBN�BNgBN�BP�BN�BN�BP]BN�BQBP�BM�BN0BN�BO\BQBP�BN	BPDBS�BO�BOsBP�BP�BP�B	�DB	�	B	ݿB	�gB	�ZB	�B	��B	�|B	��B	�oB	��B	�1B	��B	�bB	�2B	�B	߽B	ߢB	��B	߈B	�{B	��B	�;B	� B	�B	�dB	��B	ߑB	��B	�B	ߴB	�zB	�/B	��B	��B	߀B	�dB	��B	�GB	�JB	��B	�JB	ߵB	�zB	��B	�-B	ߖB	�B	�kB	��B	��B	��B	��B	��B	��B	�$B	�\B	�B	�OB	��B	�2B	��B	�B	�B	�B	�KB	�>B	�B	��B	�B	�B	�	B	��B	�wB	�,B	�mB	�B	�OB	�B	��B	�NB	��B	�AB	�DB	�7B	�B	��B	�B	�MB	�B	�DB	�B	�B	�?B	��B	��B	�B	��B	�B	�*B	�B	��B	�B	�@B	�3B	�tB	�B	��B	��B	�B	�B	�bB	�EB	�hB	�B	�B	��B	�B	�B	�BJBJ�BJ#BJBI�BJ
BJ=BI�BH�BI�BI�BJFBJ>BIBI�BJBJBJfBJBI�BI�BJBJ�BI�BI9BI�BJkBI�BJ~BJ�BJBKBJ�BIkBJ�BJ�BJ;BJ�BJ
BJ�BI�BJ�BJ�BI^BIBI+BJ�BJKBJBJ�BJgBI�BJ0BJqBIKBI)BI�BJ�BI�BI�BJ BI�BJdBJBJBJ;BI�BI�BI�BI�BJBISBI�BJ@BJBJBI�BJ�BJ`BJBJBI9BJRBJ6BH�BH�BKvBI�BJBJcBJ5BJ�BJ$BJBJ@BJ0BIvBI�BI�BJ�BJIBJtBJ�BJ�BJ�BK�BJ�BJnBK�BJ�BKBKBJ�BI�BKYBKVBJBJ�BJ�BL9BK�BL	BJ�BK*BJ�BJ�BKBK�BJ�BL
BJiBL5BK�BLcBK|BK2BKUBJ�BM^BJ�BLjBL�BLBL�BL�BJ�BNuBK�BJ�BKBJ�BJ^BJ BJ�BK�BK+BKBK�BJ�BK�BK�BI�BK�BJ�BK�BK BK�BK�BJ�BJ�BJ�BK�BLDBKpBJsBKBK�BK*BL'BK�BK BLVBL�BM2BL0BL�BL�BK�BL�BK�B	��B	�RB	�7B	�B	�B	��B	�zB	�!B	�}B	��B	�}B	؊B	�B	��B	ةB	��B	��B	خB	ؿB	زB	إB	ةB	٨B	�cB	�B	��B	�B	��B	ٺB	ػB	�:B	ؔB	�YB	��B	��B	��B	��B	�bB	��B	ٱB	�gB	��B	ٗB	�.B	��B	�{B	ٶB	�1B	ڹB	�1B	�/B	�B	�B	�+B	�9B	܂B	�B	�uB	�B	��B	�LB	�xB	�kB	�B	�B	��B	��B	ݱB	�xB	�kB	�^B	�B	ݭB	�DB	�	B	��B	��B	�;B	��B	��B	�9B	ޖB	ݓB	�wB	�jB	�nB	�B	�B	޽B	ބB	�iB	�jB	�|B	�CB	�B	�GB	��B	�-B	��B	޹B	�B	�4B	ޟB	��B	ޣB	ߦB	�]B	�oB	�4B	�B	��B	ޣB	ߦB	ߌB	�`B	�4B	�FB	��B	�vB	�iG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444434444444444444444444444444444444334444443443444444444444444344434444444443444344443444344444433444444444444433444444444444444434444443443344333444434444334434444344334443344444443334444444444344433444444433444444334443444333433343333334333334334334333343334343343333334333333433433433433333334434433443333334333433333333333334343333333434433333334433333333333333333333333334333333333333333333333333343333343433333333333333343333333333333333333333343333334343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 BJ�BI�BI�BI�BI�BI�BJ�BJ�BK�BK�BJ�BJ�BK�BL�BL�BM�BM�BM�BM�BN�BN�BN�BH�BO�BO�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BM�BN�BUBk�B�;B�0B�^B�lB�~B��BįBŴB��B�B�$B�BB�B@B' BA�B-$B+B*B(B5TB[7Bs�Bx�Bz�B�&B��B�GB¦B��B�]B�&B��B��B�hB~	Bs�Bo�Bm�Bg�BdmBdmBevBdlBchBY)BI�B:qB�B��B��B�8B��B��B��B��Bh�BY-BN�BD�B<�B"�B
RB5B
�B
��B
�B
�B
��B
�B
�yB
k�B
Y,B
RB
L�B
D�B
:sB
3GB
3JB
08B
*B
)B
%�B
�B
iB
#B	��B	�!B	��B	�|B	�OB	�FB	�9B	�3B	�B	��B	��B	��B	��B	�\B	�*B	{�B	n�B	clB	a`B	_TB	]FB	Y/B	?�B	:vB	7dB	3JB	"�B	�B	pB	*B�B��B�mB�B��B��B�EB�3B�$B�&B�3B�WB��B��B�9Bx�BeyB^MB`^BacBcnBf�Bf�Bg�Bh�Bh�Bh�Bi�Bi�Bj�Bj�Bj�Bh�Bk�Bj�Bj�Bi�Bg�BezBcmBbgBacB_UB]JB[<BX+BW#BVBVBUBRBP�BN�BL�BK�BH�BG�BF�BD�BC�BA�B?�B9uB5]B3PB3NB3NB3OB3RB6aB8oB;�B:{B;�B@�B@�BG�BK�BM�BN�BQBUBUBUBQBR
BVBSBO�BJ�BE�B9uB6cB4VB4TB1CB.3B/6B2IB3NB3PB3OB6eB8pB8lB6aB6dB8mB6dB6cB7hB7iB;�B>�B?�BC�BF�BI�BJ�BK�BL�BN�BN�BSBSBY4B\HB]KB^UB_XBbjBcsBf�Bn�Bt�Bx�Bz�B~BB�"B�6B�BB�VB�aB�nB�mB�nB��B��B��B��B��B��B�B�!B�'B�$B�)B�2B�3B�CB�iB��B��BóBôB��B��B��B��B��B��B��B��B��B��B��B�B�&B�EB�_B�~B�B�B�B�B�B�B��B�B	"B	,B	=B	DB	KB	OB	MB	
\B	iB	oB	zB	�B	�B	�B	�B	�B	�B	$�B	)B	)B	*B	,$B	,%B	-,B	2LB	5]B	6dB	<�B	D�B	I�B	J�B	J�B	K�B	N�B	QB	SB	Y6B	Z:B	[AB	]LB	_[B	ahB	blB	e�B	g�B	l�B	n�B	o�B	t�B	y�B	B	�(B	�0B	�0B	�LB	�PB	�dB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�0B	�>B	�NB	�MB	�WB	�]B	�kB	�pB	�pB	�tB	�|B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	®B	ĺB	ĺB	��B	ſB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	� B	� B	�"B	� B	� B	�'B	�'B	�'B	�.B	�5B	�:B	�EB	�LB	�SG�O�B	��B	��B
�B
>B
iB
$�B
,�B
23B
9>B
=�B
D�B
I�B
PNB
TcB
ZB
`aB
crB
e�B
i�B
m�B
q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B(+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�rA�#RG�O�G�O�G�O�G�O�G�O�G�O�B	AG�O�G�O�B[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�u'G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�A�N�G�O�G�O�G�O�BG�O�G�O�G�O�G�O�G�O�G�O�B&�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ԨB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�l�G�O�G�O�G�O�G�O�G�O�G�O�BUwG�O�G�O�BOgBF�G�O�G�O�B*�B3�BW�G�O�G�O�G�O�G�O�BIdG�O�G�O�G�O�G�O�B�BDG�O�G�O�B��G�O�G�O�G�O�G�O�A���G�O�G�O�A��=A�2�G�O�G�O�G�O�BM�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BO�B=B1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B^�G�O�G�O�G�O�B^�B�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�BN�B:DG�O�G�O�G�O�G�O�G�O�G�O�BI�BW�G�O�G�O�G�O�A�71G�O�G�O�G�O�B8MB=IA�kSG�O�BI�BPB|0G�O�BM&A�w�BL,BN�BLKBP'G�O�B��BO�BL!BR�BV=G�O�BL�BO G�O�BL�B��G�O�B	<BMJBNBY�G�O�BI�BLGBT�G�O�BZPG�O�BG�B�2G�O�BN�BJ�BK$BM�B[�A�ŜG�O�BB�BKA���BHVBI�B�G�O�B�$BJ�G�O�BHkBd�G�O�BE�A��G�O�A�JIBJ�BPBJBJBI�B��G�O�G�O�BTgG�O�G�O�BL.BIPG�O�G�O�BX�B`RBG�BH-B��BF�G�O�BEBG�BVAG�O�Bb�BN�BFBU�BQ�BISBC3BFCBGBC<BFdBGB~=G�O�B�UG�O�BE�BLBF�BGDBH�B� B�IG�O�B&G�O�G�O�BR�BIBS8BSVBH�BP�BvqG�O�G�O�B]4B?BHBL5BNzBM�BO�BO�BPBNNBNsBS�A�BZ�B[lBY�BJ�BJ�BIBM�BZ*BRBr�BSBP�G�O�BO,BRBX&BT�BO�BK�BN�BO�BP�BQmBS�BP�BN�BM�BM�BMYBM�BK�BNBOOBO/BG4BIEBM�BTG�O�BJqBJdBJ�BH�BSsG�O�B[xG�O�BJ�BM|BMBN�BO�BQLBQ�BM8BJ\B�BV/BNBV�BMIBM�G�O�BSxA�7�BM�BKBN�BL�BWYBL�BNPBLBQ�BPwBK�BJ!BNBK�BK&BQ�BJ>BM`BMBLBBMVG�O�BL�BKBK�BMJBOZBTLG�O�BUtG�O�BNoBMBSA���G�O�BKwBN`BMBKBLTBM=BM�BMnBK�BL!BL�BM�BMxBL,BL�BM�BLGBK�BM-BMBLhBM�BO&BM�BL!BP�BP�BMBOpBP�BQsBH�BK�BL�BL;BL�BR�BK�BN�BN�BJ�BK BJmBLBK`BOZBK$BL'BL BK�BOBLfBNDBN.BMSBL�BN�BQ�BM(BO<BLLBK�BKBK�BJ�BHfBJ�BJvBJ�BK�BK�BL^BK�BK�BL�BK�BK�BJ.BJ�BJ5BJ.BJBJ!BJRBI�BH�BI�BI�BJYBJRBIBI�BJBJBJzBJ.BI�BI�BJ#BJ�BI�BILBI�BJBI�BJ�BK	BJ$BK3BJ�BI�BJ�BJ�BJOBJ�BJBJ�BI�BJ�BJ�BIpBI*BI>BJ�BJ]BJ&BJ�BJxBI�BJDBJ�BI`BI<BI�BJ�BI�BJ
BJBI�BJvBJBJ$BJOBI�BI�BI�BI�BJ+BIgBJBJTBJ+BJBI�BJ�BJsBJ+BJ BILBJfBJIBH�BIBK�BI�BJBJtBJIBJ�BJ9BJ.BJVBJGBI�BI�BJBKBJ]BJ�BJ�BJ�BKBK�BJ�BJ�BK�BJ�BK$BK1BJ�BJBKkBKiBJBKBKBLJBK�BLBJ�BK>BJ�BJ�BK.BK�BJ�BLBJ|BLHBLBLvBK�BKCBKiBJ�BMsBJ�BL{BL�BL$BL�BL�BJ�BN�BK�BJ�BK1BJ�BJqBJBJ�BK�BK@BK/BK�BKBLBK�BI�BK�BJ�BK�BK3BK�BK�BJ�BJ�BJ�BK�BLXBK�BJ�BKBK�BKABL;BK�BK2BLgBL�BMDBLFBL�BL�BK�BL�BK�B	��B	�qB	�SB	�+B	�.B	��B	טB	�@B	؜B	��B	؜B	ئB	�0B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�1B	�B	�%B	��B	��B	��B	�VB	سB	�wB	��B	��B	��B	��B	فB	��B	��B	نB	��B	ٵB	�KB	��B	ڙB	��B	�OB	��B	�RB	�MB	�-B	�:B	�HB	�SB	ܡB	�/B	ܓB	�9B	� B	�lB	ݔB	݋B	�1B	�$B	�B	��B	��B	ݕB	݉B	�}B	�1B	��B	�dB	�(B	�B	��B	�ZB	��B	��B	�WB	޲B	ݰB	ޕB	ވB	ގB	�#B	�&B	��B	ޢB	߉B	މB	ޘB	�`B	�5B	�eB	�B	�KB	�B	��B	�$B	�SB	޾B	��B	��B	��B	�{B	ލB	�SB	�8B	�B	��B	��B	ߪB	�B	�SB	�cB	��B	ߓB	߇BJ.BJ�BJ5BJ.BJBJ!BJRBI�BH�BI�BI�BJYBJRBIBI�BJBJBJzBJ.BI�BI�BJ#BJ�BI�BILBI�BJBI�BJ�BK	BJ$BK3BJ�BI�BJ�BJ�BJOBJ�BJBJ�BI�BJ�BJ�BIpBI*BI>BJ�BJ]BJ&BJ�BJxBI�BJDBJ�BI`BI<BI�BJ�BI�BJ
BJBI�BJvBJBJ$BJOBI�BI�BI�BI�BJ+BIgBJBJTBJ+BJBI�BJ�BJsBJ+BJ BILBJfBJIBH�BIBK�BI�BJBJtBJIBJ�BJ9BJ.BJVBJGBI�BI�BJBKBJ]BJ�BJ�BJ�BKBK�BJ�BJ�BK�BJ�BK$BK1BJ�BJBKkBKiBJBKBKBLJBK�BLBJ�BK>BJ�BJ�BK.BK�BJ�BLBJ|BLHBLBLvBK�BKCBKiBJ�BMsBJ�BL{BL�BL$BL�BL�BJ�BN�BK�BJ�BK1BJ�BJqBJBJ�BK�BK@BK/BK�BKBLBK�BI�BK�BJ�BK�BK3BK�BK�BJ�BJ�BJ�BK�BLXBK�BJ�BKBK�BKABL;BK�BK2BLgBL�BMDBLFBL�BL�BK�BL�BK�B	��B	�qB	�SB	�+B	�.B	��B	טB	�@B	؜B	��B	؜B	ئB	�0B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�1B	�B	�%B	��B	��B	��B	�VB	سB	�wB	��B	��B	��B	��B	فB	��B	��B	نB	��B	ٵB	�KB	��B	ڙB	��B	�OB	��B	�RB	�MB	�-B	�:B	�HB	�SB	ܡB	�/B	ܓB	�9B	� B	�lB	ݔB	݋B	�1B	�$B	�B	��B	��B	ݕB	݉B	�}B	�1B	��B	�dB	�(B	�B	��B	�ZB	��B	��B	�WB	޲B	ݰB	ޕB	ވB	ގB	�#B	�&B	��B	ޢB	߉B	މB	ޘB	�`B	�5B	�eB	�B	�KB	�B	��B	�$B	�SB	޾B	��B	��B	��B	�{B	ލB	�SB	�8B	�B	��B	��B	ߪB	�B	�SB	�cB	��B	ߓB	߇G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444444434444444444444444444444444444444334444443443444444444444444344434444444443444344443444344444433444444444444433444444444444444434444443443344333444434444334434444344334443344444443334444444444344433444444433444444334443444333433343333334333334334334333343334343343333334333333433433433433333334434433443333334333433333333333334343333333434433333334433333333333333333333333334333333333333333333333333343333343433333333333333343333333333333333333333343333334343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.23 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.23 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.23 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281454582020082814545820200828145458202008281454582020082814545820200828145458202008281454582020082814545820200828145458202008281454582020082814545820200828145458AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730342019021417303420190214173034    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730342019021417303420190214173034  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730342019021417303420190214173034  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281454582020082814545820200828145458  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                