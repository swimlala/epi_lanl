CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  _   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:47Z creation      
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
resolution        =���   axis      Z        (t  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
   m�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (t  w�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
   �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (t  �8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (t  Ҭ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
   �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (t @   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  -�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (t 7�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (t `H   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (t ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �P   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (t �p   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (t ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  X   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (t  x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (t S   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � {�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   |@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                  0 �dArgo profile    3.1 1.2 19500101000000  20190219181647  20200831164801  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               9   9   9AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @��񖖷@��񖖷@��񖖷111 @��Q�s�@��Q�s�@��Q�s�@6��j~��@6��j~��@6��j~���c���o�c���o�c���o111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    9   9   9ADA BDA  DA BDA @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�)D�=D�>�D�w\D���D�\D�H�D���D��\D� �D�9�D���D���D�RD�L{D�p D�)D���D�.�D�~D�� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ��;L�;L�;L��    �L�;L�;L�;L�;L�;L�;L�;L�;L��    �L�;L�;L�;L�;L�;L�;L�;L�;L��    ���;L�;L��        �L�;L�ͽ��;L�;L��    ���;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ���    ���;L�;L��    =��ͽ��;L�;L�;L�;L�;L��    ���;L�ͽ��ͽ��;L�ͽ���=���    �L�;L��    ���;L�ͽ��ͽ��;L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�ͽ���    �L�ͽ��;L�;L��    ���;L�;L�ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��;L�;L��        �L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�;L�ͽ���    �L�;L�;L�;L�;L�;L��    ���;L�;L��    =���=��;L�;L�ͽ��ͽ��;L�;L�;L�;L�ͽ���    ���;L�ͽ��;L�;L�ͽ��;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��ͽ��;L��    ���;L�ͽ���    �L�ͽ��ͽ��;L�ͽ��;L�ͽ��ͽ��;L�ͽ��ͽ��ͽ���        ����    ���ͽ��ͽ���                            =���=���=���=���=���            ����=���        =���=���    >L��                =���        =���        =���    =���        =���=���    =���        =���=���        >L��=���        =���    =���    =���=���=���=���        =���=���            =���=���=���=���=���=���=���=���=���=���=���                =���=���        >L��    =���=���            =���    =���            =���=���=���=���=���    =���    =���=���=���=���=���=���            =���=���        =���=���=���    =���    ����    =���    >L��=���    =���=���    =���            =���    =���=���=���=���=���        =���=���=���                    =���        =���=���    =���        =���        =���=���=���=���=���=���=���    =���    =���        =���=���=���=���=���=���    =���    =���=���    =���=���=���    =���=���=���=���=���        ����        =���    =���=���=���=���=���>L��=���    =���    =���    =���=���=���    =���=���    =���=��ͽ���        =���=���>L��>L��>L��>���>���>���>���>���?   ?   ?��?��?��?333?333?333?L��?L��?L��?fff?fff?fff?fff?fff?�  ?�  ?���?���?���?���?�ff?�ff?�ff?�33?�33?�33?�  ?���?���?ٙ�?ٙ�?ٙ�?�ff?�ff?�33@   @   @   @   @ff@ff@��@��@33@33@��@��@   @&ff@&ff@&ff@,��@,��@333@9��@@  @@  @Fff@L��@L��@S33@Y��@`  @`  @fff@l��@l��@s33@y��@y��@�  @�33@�33@�ff@���@���@���@�  @�33@�33@�ff@���@���@�  @�  @�33@�ff@���@���@���@�  @�33@�ff@���@���@���@�  @�33@�33@�ff@ə�@���@�  @�33@�33@�ff@ٙ�@ٙ�@���@�  @�33@�ff@陚@���@���@�  @�33@�ff@���@���A   A   A��A33A��AffA  A	��A33A��AffA  A��A33A��AffA  A��A33A33A��A   A   A#33A$��A&ffA&ffA(  A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAH  AI��AK33ANffAP  AQ��AS33AT��AVffAY��A[33A\��A^ffA`  Aa��Ac33AfffAh  Ai��Ak33Al��Ap  Aq��As33At��Ax  Ay��A{33A|��A�  A���A���A�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  Aə�A�ffA�  A���A�ffA�33A���Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�33A�  Dq�Dq  Dq&fDq33Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3DsٚDs� Ds��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDtl�Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3DtٚDt� Dt��@,��@333@9��@@  @@  @Fff@L��@L��@S33@Y��@`  @`  @fff@l��@l��@s33@y��@y��@�  @�33@�33@�ff@���@���@���@�  @�33@�33@�ff@���@���@�  @�  @�33@�ff@���@���@���@�  @�33@�ff@���@���@���@�  @�33@�33@�ff@ə�@���@�  @�33@�33@�ff@ٙ�@ٙ�@���@�  @�33@�ff@陚@���@���@�  @�33@�ff@���@���A   A   A��A33A��AffA  A	��A33A��AffA  A��A33A��AffA  A��A33A33A��A   A   A#33A$��A&ffA&ffA(  A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A;33A<��A@  AA��AC33AD��AFffAH  AI��AK33ANffAP  AQ��AS33AT��AVffAY��A[33A\��A^ffA`  Aa��Ac33AfffAh  Ai��Ak33Al��Ap  Aq��As33At��Ax  Ay��A{33A|��A�  A���A���A�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  Aə�A�ffA�  A���A�ffA�33A���Aљ�A�ffA�  A���A�ffA�33A���Aٙ�A�33A�  Dq�Dq  Dq&fDq33Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3DsٚDs� Ds��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDtl�Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3DtٚDt� Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @3�
@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A��HA�{B 
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
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�C�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D
�>D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dp�>Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dy��D��D�?
D�w�D��)D��D�H�D�� D�ǮD�HD�9�D�� D��HD��D�L�D�pRD�{D��D�/
D�~fD��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���Q콸Q�B�\�B�\��Q�B�\�B�\�B�\�B�\�B�\��Q�B�\�B�\�B�\<#�
�B�\�B�\�B�\�B�\�B�\�B�\�B�\�B�\�B�\<#�
�B�\�B�\�B�\�B�\�B�\�B�\�B�\�B�\�B�\<#�
��Q�B�\�B�\<#�
<#�
�B�\�B�\��Q�B�\�B�\<#�
��Q�B�\��Q콸Q�B�\�B�\��Q�B�\�B�\�B�\��Q콸Q�B�\��Q�<#�
��Q�B�\�B�\<#�
=�G���Q�B�\�B�\�B�\�B�\�B�\<#�
��Q�B�\��Q콸Q�B�\��Q�=�G�<#�
�B�\�B�\<#�
��Q�B�\��Q콸Q�B�\�B�\�B�\�B�\�B�\�B�\��Q콸Q�B�\�B�\�B�\�B�\�B�\��Q�<#�
�B�\��Q�B�\�B�\<#�
��Q�B�\�B�\��Q�B�\�B�\�B�\�B�\�B�\�B�\��Q�B�\�B�\�B�\�B�\��Q�B�\�B�\<#�
<#�
�B�\�B�\�B�\�B�\�B�\�B�\�B�\�B�\�B�\�B�\��Q콸Q�B�\�B�\��Q�<#�
�B�\�B�\�B�\�B�\�B�\�B�\<#�
��Q�B�\�B�\<#�
=�G�=�G��B�\�B�\��Q콸Q�B�\�B�\�B�\�B�\��Q�<#�
��Q�B�\��Q�B�\�B�\��Q�B�\��Q�B�\�B�\�B�\��Q콸Q�B�\��Q콸Q�B�\��Q콸Q콸Q콸Q�B�\�B�\��Q콸Q콸Q콸Q�B�\<#�
��Q�B�\��Q�<#�
�B�\��Q콸Q�B�\��Q�B�\��Q콸Q�B�\��Q콸Q콸Q�<#�
<#�
��Q�<#�
��Q콸Q콸Q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
=�G�=�G�=�G�=�G�=�G�<#�
<#�
<#�
��Q�=�G�<#�
<#�
=�G�=�G�<#�
>W
><#�
<#�
<#�
<#�
=�G�<#�
<#�
=�G�<#�
<#�
=�G�<#�
=�G�<#�
<#�
=�G�=�G�<#�
=�G�<#�
<#�
=�G�=�G�<#�
<#�
>W
>=�G�<#�
<#�
=�G�<#�
=�G�<#�
=�G�=�G�=�G�=�G�<#�
<#�
=�G�=�G�<#�
<#�
<#�
=�G�=�G�=�G�=�G�=�G�=�G�=�G�=�G�=�G�=�G�=�G�<#�
<#�
<#�
<#�
=�G�=�G�<#�
<#�
>W
><#�
=�G�=�G�<#�
<#�
<#�
=�G�<#�
=�G�<#�
<#�
<#�
=�G�=�G�=�G�=�G�=�G�<#�
=�G�<#�
=�G�=�G�=�G�=�G�=�G�=�G�<#�
<#�
<#�
=�G�=�G�<#�
<#�
=�G�=�G�=�G�<#�
=�G�<#�
��Q�<#�
=�G�<#�
>W
>=�G�<#�
=�G�=�G�<#�
=�G�<#�
<#�
<#�
=�G�<#�
=�G�=�G�=�G�=�G�=�G�<#�
<#�
=�G�=�G�=�G�<#�
<#�
<#�
<#�
<#�
=�G�<#�
<#�
=�G�=�G�<#�
=�G�<#�
<#�
=�G�<#�
<#�
=�G�=�G�=�G�=�G�=�G�=�G�=�G�<#�
=�G�<#�
=�G�<#�
<#�
=�G�=�G�=�G�=�G�=�G�=�G�<#�
=�G�<#�
=�G�=�G�<#�
=�G�=�G�=�G�<#�
=�G�=�G�=�G�=�G�=�G�<#�
<#�
��Q�<#�
<#�
=�G�<#�
=�G�=�G�=�G�=�G�=�G�>W
>=�G�<#�
=�G�<#�
=�G�<#�
=�G�=�G�=�G�<#�
=�G�=�G�<#�
=�G�=�G���Q�<#�
<#�
=�G�=�G�>W
>>W
>>W
>>��R>��>��R>��>��?�\?�\?(�?(�?(�?5?5?5?O\)?O\)?O\)?h��?h��?h��?h��?h��?�G�?�G�?�{?��H?��H?��H?��?��?��?�z�?�z�?�z�?�G�?�{?�{?��H?��H?��H?�?�?�z�@ ��@ ��@ ��@ ��@
=@
=@p�@p�@�
@�
@=q@=q@ ��@'
=@'
=@'
=@-p�@-p�@3�
@:=q@@��@@��@G
=@Mp�@Mp�@S�
@Z=q@`��@`��@g
=@mp�@mp�@s�
@z=q@z=q@�Q�@��@��@��R@��@��@��@�Q�@��@��@��R@��@��@�Q�@�Q�@��@��R@��@��@��@�Q�@��@��R@��@��@��@�Q�@Å@Å@ƸR@��@��@�Q�@Ӆ@Ӆ@ָR@��@��@��@�Q�@�@�R@��@��@��@�Q�@�@��R@��@��A (�A (�AA\)A��A�\A(�A	A\)A��A�\A(�AA\)A��A�\A(�AA\)A\)A��A (�A (�A#\)A$��A&�\A&�\A((�A+\)A,��A.�\A0(�A1A3\)A4��A6�\A8(�A9A;\)A<��A@(�AAAC\)AD��AF�\AH(�AIAK\)AN�\AP(�AQAS\)AT��AV�\AYA[\)A\��A^�\A`(�AaAc\)Af�\Ah(�AiAk\)Al��Ap(�AqAs\)At��Ax(�AyA{\)A|��A�{A��HA��A�G�A�{A��HA��A�G�A�{A��HA��A�G�A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA��A�G�A�{A��HA�z�A�G�A��HA��A�G�A�{A��HA�z�A�G�A��HA��A�G�A�{A��A�z�A�G�A��HA��A�z�A�{A��HA�z�A�G�A��HA��A�z�A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA�z�A�G�A�{AɮA�z�A�{A��HA�z�A�G�A��HAѮA�z�A�{A��HA�z�A�G�A��HAٮA�G�A�{Dq>Dq �Dq'
Dq3�Dq:>Dq@�DqG
DqS�DqZ>Dq`�DqmqDqs�Dqz>Dq��Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�Dq�
Dq�qDq��Dr �Dr
DrqDr>Dr �Dr'
Dr-qDr:>Dr@�DrG
DrS�DrZ>Dr`�Drg
Drs�Drz>Dr��Dr�qDr��Dr�>Dr��Dr�qDr��Dr�>Dr��Dr�qDr��Dr�>Dr�
Dr�qDr��Dr�>Ds
DsqDs�Ds �Ds'
Ds-qDs:>Ds@�DsG
DsMqDsZ>Ds`�Dsg
DsmqDsz>Ds��Ds�
Ds��Ds�>Ds��Ds�qDs��Ds�>Ds�
Ds�qDs��Ds�>Ds�Ds�qDs��Ds�>Dt
DtqDt�Dt �Dt'
Dt-qDt:>Dt@�DtG
DtMqDtZ>Dt`�Dtg
DtmqDtz>Dt��Dt�
Dt��Dt�>Dt��Dt�
Dt��Dt�>Dt��Dt�qDt��Dt�>Dt�Dt�q@-p�@3�
@:=q@@��@@��@G
=@Mp�@Mp�@S�
@Z=q@`��@`��@g
=@mp�@mp�@s�
@z=q@z=q@�Q�@��@��@��R@��@��@��@�Q�@��@��@��R@��@��@�Q�@�Q�@��@��R@��@��@��@�Q�@��@��R@��@��@��@�Q�@Å@Å@ƸR@��@��@�Q�@Ӆ@Ӆ@ָR@��@��@��@�Q�@�@�R@��@��@��@�Q�@�@��R@��@��A (�A (�AA\)A��A�\A(�A	A\)A��A�\A(�AA\)A��A�\A(�AA\)A\)A��A (�A (�A#\)A$��A&�\A&�\A((�A+\)A,��A.�\A0(�A1A3\)A4��A6�\A8(�A9A;\)A<��A@(�AAAC\)AD��AF�\AH(�AIAK\)AN�\AP(�AQAS\)AT��AV�\AYA[\)A\��A^�\A`(�AaAc\)Af�\Ah(�AiAk\)Al��Ap(�AqAs\)At��Ax(�AyA{\)A|��A�{A��HA��A�G�A�{A��HA��A�G�A�{A��HA��A�G�A�{A��HA�z�A�G�A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA��A�G�A�{A��HA�z�A�G�A��HA��A�G�A�{A��HA�z�A�G�A��HA��A�G�A�{A��A�z�A�G�A��HA��A�z�A�{A��HA�z�A�G�A��HA��A�z�A�{A��HA�z�A�G�A�{A��A�z�A�{A��HA�z�A�G�A�{AɮA�z�A�{A��HA�z�A�G�A��HAѮA�z�A�{A��HA�z�A�G�A��HAٮA�G�A�{Dq>Dq �Dq'
Dq3�Dq:>Dq@�DqG
DqS�DqZ>Dq`�DqmqDqs�Dqz>Dq��Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�>Dq�
Dq�qDq��Dq�Dq�
Dq�qDq��Dr �Dr
DrqDr>Dr �Dr'
Dr-qDr:>Dr@�DrG
DrS�DrZ>Dr`�Drg
Drs�Drz>Dr��Dr�qDr��Dr�>Dr��Dr�qDr��Dr�>Dr��Dr�qDr��Dr�>Dr�
Dr�qDr��Dr�>Ds
DsqDs�Ds �Ds'
Ds-qDs:>Ds@�DsG
DsMqDsZ>Ds`�Dsg
DsmqDsz>Ds��Ds�
Ds��Ds�>Ds��Ds�qDs��Ds�>Ds�
Ds�qDs��Ds�>Ds�Ds�qDs��Ds�>Dt
DtqDt�Dt �Dt'
Dt-qDt:>Dt@�DtG
DtMqDtZ>Dt`�Dtg
DtmqDtz>Dt��Dt�
Dt��Dt�>Dt��Dt�
Dt��Dt�>Dt��Dt�qDt��Dt�>Dt�Dt�qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�ZA���A���A�"�A��+A�VA�A�\)A�1A���A��A�jA�bNA�VA�;dA�&�A��A�oA�JA�A���A���A��A���A��FA��uA�Q�A���A�n�A�/A���A�bA��^A�x�A�K�A��HA���A��A��RA��A�ƨA��A�A�A��A��A���A���A�~�A�XA�33A�bA��A��-A�ĜA�  A�;dA�t�A�+A�&�A���A���A�jA�E�A��A�ĜA�ZA�=qA�p�A�`BA��;A���A���A�?}A���A��\A��A�\)A��;A�ZA�%A� �A�`BA��
A�jA�&�A�;dA���A�ZA��#A���A��mA�+A�M�A��#A�G�A���A� �A�{A�1A�\)A�VA��A��HA�&�A���A��wA�  A�p�A�5?A���A�XA���A�C�A~ȴA|��A{
=AxjAv�/Aup�Ar��Ap�/AnQ�Am�AljAkdZAjĜAiVAg�;Ag|�AgAe�FAd5?Aa�7A`��A^�A[��A[;dAY��AW/AUoAT��AT�AR�HAR  AP��ALĜAI�TAGx�AEADbNAC%AA�A>�9A=��A<�uA;�FA;+A:�uA:=qA:1A9l�A7�-A6�A5�A4�9A3��A3�-A3��A2�!A2  A0VA.��A-��A,9XA*(�A(�!A'%A&9XA%A$�A$1A#��A#t�A#�7A#VA"��A jAG�AAv�A�
A;dA�Av�A��A�A��A�AdZAG�A��Ap�A�mA1A�A�RAVA�A�A
��A
A�A	�FA	33AA"�A��AĜA5?A%A��A��A5?A-Ax�A�/Av�A��A/A �+A -@�33@�$�@�hs@�dZ@���@�b@���@�l�@���@�@�@��T@���@�9X@�-@�?}@��@� �@�+@��y@柾@�j@���@�t�@��;@ڇ+@�j@�J@�z�@�l�@Ұ!@�O�@��H@��;@��@���@�hs@ȼj@���@��@��`@ļj@ċD@öF@���@�5?@��7@�9X@��P@�S�@��@�V@��@���@��#@�hs@�x�@�@�ff@��@�b@�X@��F@�C�@�
=@�-@�J@�$�@��-@�Ĝ@�A�@��\@�@��^@���@��F@�33@���@��@�Ĝ@��9@���@�j@��m@�\)@�ff@�@��@�Ĝ@��@�o@�
=@��@�+@�K�@��y@��R@�~�@�V@�-@��T@�x�@��`@���@�I�@�1@��w@�\)@��@�t�@�t�@��y@��R@��\@�ff@�-@��@��7@��D@�I�@��m@��@�\)@�S�@�;d@��@��+@�=q@��@�@���@��@��T@��T@���@�@��-@���@��h@�`B@�/@��/@��u@��@�Z@��@�  @��m@��P@�K�@��y@��\@�n�@���@��@��#@���@��@�?}@��@�Ĝ@��@�bN@��@���@���@�t�@���@��\@�5?@���@��-@�x�@��@��j@���@��@�bN@�9X@��@�1@�ƨ@�dZ@�o@�
=@���@�V@�@��T@���@���@���@�@��^@���@��7@�hs@�G�@�V@���@��@���@��D@�z�@�Z@�  @���@���@�C�@��y@���@�n�@�E�@�-@�{@��@���@��h@�p�@�V@�bN@�(�@���@���@���@��P@�t�@�+@���@���@��!@�v�@�E�@��@���@�7L@��@�V@�V@��`@�Ĝ@��j@��j@��@���@�z�@�A�@�1@��m@��@�|�@�|�@�\)@�@��H@�n�@��@��@�!�@��@xz�@n�c@d�@^��@V��@L|�@E��@A��@<�4@5�^@0��@+'�@#��@��@�	@j@V�@��@2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA���A�-A�9XA��TA��A�n�A�C�A�\)A��A���A��mA���A� �A�A�x�A�  A�r�A�33A��A�;dA�=qA�`BA�(�A�/A�bNA�A�n�A��A���A���A���A�K�A��TA�{A�p�A�A��-A�?}A�hsA�=qA��A���A�G�A���A�"�A�$�A��A�$�A���A��yA���A���A�1A�XA��A��A�|�A��A���A�oA�l�A�t�A�ffA�$�A�{A��A���A�`BA�Q�A���A�bA� �A�7LA��A�$�A�33A� �A�&�A�33A�(�A�=qA�+A�"�A�n�A�&�A�bA�t�A�1'A�p�A���A��
A��/A���A¼jA�A��uA��A�VA��+A���A��A���A�1A�$�A�n�A�G�A�$�A��PA��A�A�%A�JA�G�A�9XA��A�
=A��`A���A��A�~�A���A���A���A��A��A�"�A�9XA���A�t�A�\)A���A���A�^5A���A�M�A��A�"�A�(�A�p�A���A��A��yA���A��^A�^5A���A�z�A���A�(�A�/A���A�ȴA�%A�=qA�7LA���A�n�A���A�VA�5?A�&�A��uA�t�A�VA¶FA�?}A�bA�l�A��A��`A�ĜA��\A�Q�A�bNA��`A���A��mA�  A��A�I�A�&�A�dZA���A�33A�&�A�/A���A��-A°!A�7LA�VA�?}A¶FA��RA�/A�Q�A��A�C�A�^5A��RA�-A�ffA�C�A�
=A�"�A�G�A��A�
=A�C�A�C�A�?}A�C�A�G�A�C�A�;dA�M�A�G�A�A�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�M�A�K�A�I�A�G�A�%A�C�A�M�A�I�A�O�A�S�A�M�A�O�A�K�A�Q�A�S�A�I�A�I�A�K�A�I�A�Q�A�M�A�O�A�M�A�I�A�VA�S�A�K�A�G�A�=qA�A�A�E�A�I�A�G�A�E�A�?}A�=qA�E�A�C�A�E�A�A�A�?}A�?}A�;dA�=qA�=qA�=qA�;dA�C�A�C�A�E�A�C�A�C�A�A�A�?}A�7LA�?}A�?}A�?}A�E�A�E�A�E�A�E�A�=qA�?}A�E�A�5?A�7LA�?}A�E�A�5?A�9XA�5?A�;dA�A�A�C�A�C�A�-A�I�A�G�A�C�A�A�A�E�A�E�A�I�A�G�A�E�A�C�A�9XA�9XA�?}A�G�A�C�A�33A�=qA�=qA�33A�9XA�7LA�9XA�7LA�=qA�=qA�=qA�5?A�A�A�;dA�;dA�;dA�;dA�=qA�9XA�A�A�;dA�?}A�A�A�9XA�9XA�1'A�7LA�?}A�A�A�?}A�;dA�9XA�1'A�5?A�1'A�7LA�$�A�(�A�5?A�9XA�;dA�7LA�9XA�=qA�;dA�;dA�;dA�?}A�=qA�;dA�=qA�?}A�5?A�7LA�A�A�=qA�A�A�-A�?}A�E�A�&�A�?}A�?}A�9XA�=qA�9XA�7LA�9XA�=qA�?}A�A�A�;dA�9XA�=qA�;dA�;dA�=qA�9XA�;dA�?}A�5?A�7LA�1'A�7LA�7LA�7LA�9XA�9XA�7LA�1'A�33A�5?A�1'A�/A�9XA�+A�9XA�33A�7LA�33A�7LA�/A�&�A��A�&�A�(�A�-A�33A�1'A�+A�9XA�=qA�=qA�=qA�A�A�7LA�33A�33A�9XA�7LA�7LA�+A�-A�33A�1'A�5?A�9XA�;dA�5?A�33A�/A�A�/A�5?A�5?A�/A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�/A�1'A�7LA�/A�5?A�5?A�1'A�/A�-A�/A�-A�-A�1'A�33A�1'A�7LA�9XA�5?A�5?A�/A�/A�1'A�/A�33A�9XA�C�A�A�A�C�A�G�A�S�A�S�A�VA�O�A�O�A�Q�A�O�A�Q�A�S�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�I�A�VA�ZA�S�A�XA�ffA�dZA�`BA�bNA�^5A�\)A�`BA�`BA�`BA�^5A�\)A�ZA�^5A�^5A�\)A�^5A�\)A�^5A�bNA�\)A�Q�A�S�A�S�A�ZA�S�A�XA�ZA�I�A�C�A�I�A�C�A�=qA�;dA�"�A�
=A���A���A�7LA��A�-A�ĜA��9A���A���A���A���A�~�A�x�A��A�jA�l�A�ZA�VA�Q�A�K�A�M�A�I�A�G�A�?}A�A�A�;dA�=qA�33A�-A�+A�$�A��A�{A�VA�%A��;A��;A���A���A���A��A�n�A�jA�dZA�XA�G�A�I�A�M�A�I�A�?}A�1'A�33A�1'A�7LA�$�A� �A�{A��A�A�JA�A��A��#A��RA��FA��!A���A���A��uA��7A��DA��A�x�A�t�A�t�A�t�A�v�A�x�A�v�A�x�A�t�A�p�A�jA�l�A�jA�ffA�`BA�bNA�^5A�^5A�ZA�Q�A�O�A�O�A�O�A�K�A�G�A�C�A�A�A�?}A�7LA�(�A��A��A��A�{A�
=A���A���A��A��`A��;A��#A�A��RA���A���A��jA��A���A���A���A��uA�~�A�XA�Q�A�Q�A�$�A��A��A��`A��
A��wA���A�jA�C�A�bA���A��#A���A�ĜA��jA��RA��-A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A��uA��\A��+A��PA��+A��A�~�A�n�A�l�A�n�A�l�A�l�A�l�A�jA�jA�jA�l�A�l�A�l�A�hsA�hsA�hsA�hsA�ffA�ffA�ffA�hsA�ffA�ffA�dZ@�t�@�t�@�t�@�t�@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�S�@�;d@�33@�+@�"�@�"�@�"�@��@�o@�
=@�@�@�@�@���@���@���@��@��@��@��@��y@��y@��y@��y@��H@��H@��y@��y@��y@��y@��y@��H@��H@��H@��H@��@�ȴ@��!@���@��+@�~�@�ff@�n�@�ff@�ff@�n�@�n�@�n�@�n�@�n�@�ff@�V@�E�@�5?@�-@�-@�$�@�$�@�$�@��@��@��@�{@�{@�{@�{@�{@�J@�J@�J@�@�@���@���@���@���@��@��@��@��@��T@��#@��#@��#@��#@��#@��#@��#@���@���A�`BA�hsA�hsA�bNA�`BA�`BA�^5A�bNA�`BA�^5A�\)A�`BA�`BA�^5A�^5A�^5A�^5A�^5A�^5A�ZA�ZA�Q�A�Q�A�S�A�`BA�XA�M�A�XA�Q�A�K�A�G�A�E�A�I�A�K�A��A���A�ƨA�ĜA�XA�A��A��A��/A���A���A���A���A��hA��7A�r�A�jA�hsA�bNA�Q�A�Q�A�O�A�O�A�G�A�C�A�A�A�?}A�?}A�=qA�7LA�33A�+A�(�A��A��A��A�JA���A��HA���A���A���A�~�A�z�A�l�A�XA�S�A�Q�A�M�A�I�A�C�A�?}A�=qA�9XA�5?A�/A�-A�(�A�$�A��A�bA�  A��A��`A���A�ĜA��^A��9A���A���A���A��hA��A�~�A�~�A�x�A�t�A�t�A�x�A�x�A�x�A�x�A�v�A�p�A�l�A�hsA�hsA�ffA�bNA�\)A�\)A�XA�VA�Q�A�O�A�M�A�K�A�I�A�E�A�C�A�A�A�;dA�7LA�33A�$�A�"�A��A�oA�1A�A���A��A��yA��#A���A���A�ĜA��jA��FA���A���A���A��uA��A�XA�S�A�O�A�=qA��A��A��`A��
A�A���A��A�^5A�"�A�A��`A���A�ƨA��jA��RA��FA��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA��\A��7A��+A��7A��A�~�A�t�A�l�A�l�A�l�A�n�A�n�A�jA�jA�jA�l�A�l�A�l�A�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ff@�t�@�|�@�t�@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�|�@�t�@�|�@�t�@�|�@�t�@�t�@�t�@�t�@�t�@�|�@�t�@�t�@�t�@�dZ@�C�@�33@�+@�"�@�"�@�"�@�"�@��@�o@�@�@���@�@�@���@���@���@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��@���@��!@��!@���@�~�@�v�@�ff@�ff@�ff@�ff@�n�@�n�@�n�@�n�@�ff@�^5@�M�@�=q@�-@�-@�-@�$�@�$�@�$�@��@��@�{@�{@�{@�{@�{@�J@�J@�J@�J@�@�@�@���@���@���@��@��@��@��@��T@��#@��#@��#@��#@��#@��#@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�dZA�ZA���A���A�"�A��+A�VA�A�\)A�1A���A��A�jA�bNA�VA�;dA�&�A��A�oA�JA�A���A���A��A���A��FA��uA�Q�A���A�n�A�/A���A�bA��^A�x�A�K�A��HA���A��A��RA��A�ƨA��A�A�A��A��A���A���A�~�A�XA�33A�bA��A��-A�ĜA�  A�;dA�t�A�+A�&�A���A���A�jA�E�A��A�ĜA�ZA�=qA�p�A�`BA��;A���A���A�?}A���A��\A��A�\)A��;A�ZA�%A� �A�`BA��
A�jA�&�A�;dA���A�ZA��#A���A��mA�+A�M�A��#A�G�A���A� �A�{A�1A�\)A�VA��A��HA�&�A���A��wA�  A�p�A�5?A���A�XA���A�C�A~ȴA|��A{
=AxjAv�/Aup�Ar��Ap�/AnQ�Am�AljAkdZAjĜAiVAg�;Ag|�AgAe�FAd5?Aa�7A`��A^�A[��A[;dAY��AW/AUoAT��AT�AR�HAR  AP��ALĜAI�TAGx�AEADbNAC%AA�A>�9A=��A<�uA;�FA;+A:�uA:=qA:1A9l�A7�-A6�A5�A4�9A3��A3�-A3��A2�!A2  A0VA.��A-��A,9XA*(�A(�!A'%A&9XA%A$�A$1A#��A#t�A#�7A#VA"��A jAG�AAv�A�
A;dA�Av�A��A�A��A�AdZAG�A��Ap�A�mA1A�A�RAVA�A�A
��A
A�A	�FA	33AA"�A��AĜA5?A%A��A��A5?A-Ax�A�/Av�A��A/A �+A -@�33@�$�@�hs@�dZ@���@�b@���@�l�@���@�@�@��T@���@�9X@�-@�?}@��@� �@�+@��y@柾@�j@���@�t�@��;@ڇ+@�j@�J@�z�@�l�@Ұ!@�O�@��H@��;@��@���@�hs@ȼj@���@��@��`@ļj@ċD@öF@���@�5?@��7@�9X@��P@�S�@��@�V@��@���@��#@�hs@�x�@�@�ff@��@�b@�X@��F@�C�@�
=@�-@�J@�$�@��-@�Ĝ@�A�@��\@�@��^@���@��F@�33@���@��@�Ĝ@��9@���@�j@��m@�\)@�ff@�@��@�Ĝ@��@�o@�
=@��@�+@�K�@��y@��R@�~�@�V@�-@��T@�x�@��`@���@�I�@�1@��w@�\)@��@�t�@�t�@��y@��R@��\@�ff@�-@��@��7@��D@�I�@��m@��@�\)@�S�@�;d@��@��+@�=q@��@�@���@��@��T@��T@���@�@��-@���@��h@�`B@�/@��/@��u@��@�Z@��@�  @��m@��P@�K�@��y@��\@�n�@���@��@��#@���@��@�?}@��@�Ĝ@��@�bN@��@���@���@�t�@���@��\@�5?@���@��-@�x�@��@��j@���@��@�bN@�9X@��@�1@�ƨ@�dZ@�o@�
=@���@�V@�@��T@���@���@���@�@��^@���@��7@�hs@�G�@�V@���@��@���@��D@�z�@�Z@�  @���@���@�C�@��y@���@�n�@�E�@�-@�{@��@���@��h@�p�@�V@�bN@�(�@���@���@���@��P@�t�@�+@���@���@��!@�v�@�E�@��@���@�7L@��@�V@�V@��`@�Ĝ@��j@��j@��@���@�z�@�A�@�1@��m@��@�|�@�|�@�\)@�@��H@�n�@��G�O�@�!�@��@xz�@n�c@d�@^��@V��@L|�@E��@A��@<�4@5�^@0��@+'�@#��@��@�	@j@V�@��@2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA���A�-A�9XA��TA��A�n�A�C�A�\)A��A���A��mA���A� �A�A�x�A�  A�r�A�33A��A�;dA�=qA�`BA�(�A�/A�bNA�A�n�A��A���A���A���A�K�A��TA�{A�p�A�A��-A�?}A�hsA�=qA��A���A�G�A���A�"�A�$�A��A�$�A���A��yA���A���A�1A�XA��A��A�|�A��A���A�oA�l�A�t�A�ffA�$�A�{A��A���A�`BA�Q�A���A�bA� �A�7LA��A�$�A�33A� �A�&�A�33A�(�A�=qA�+A�"�A�n�A�&�A�bA�t�A�1'A�p�A���A��
A��/A���A¼jA�A��uA��A�VA��+A���A��A���A�1A�$�A�n�A�G�A�$�A��PA��A�A�%A�JA�G�A�9XA��A�
=A��`A���A��A�~�A���A���A���A��A��A�"�A�9XA���A�t�A�\)A���A���A�^5A���A�M�A��A�"�A�(�A�p�A���A��A��yA���A��^A�^5A���A�z�A���A�(�A�/A���A�ȴA�%A�=qA�7LA���A�n�A���A�VA�5?A�&�A��uA�t�A�VA¶FA�?}A�bA�l�A��A��`A�ĜA��\A�Q�A�bNA��`A���A��mA�  A��A�I�A�&�A�dZA���A�33A�&�A�/A���A��-A°!A�7LA�VA�?}A¶FA��RA�/A�Q�A��A�C�A�^5A��RA�-A�ffA�C�A�
=A�"�A�G�A��A�
=A�C�A�C�A�?}A�C�A�G�A�C�A�;dA�M�A�G�A�A�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�M�A�K�A�I�A�G�A�%A�C�A�M�A�I�A�O�A�S�A�M�A�O�A�K�A�Q�A�S�A�I�A�I�A�K�A�I�A�Q�A�M�A�O�A�M�A�I�A�VA�S�A�K�A�G�A�=qA�A�A�E�A�I�A�G�A�E�A�?}A�=qA�E�A�C�A�E�A�A�A�?}A�?}A�;dA�=qA�=qA�=qA�;dA�C�A�C�A�E�A�C�A�C�A�A�A�?}A�7LA�?}A�?}A�?}A�E�A�E�A�E�A�E�A�=qA�?}A�E�A�5?A�7LA�?}A�E�A�5?A�9XA�5?A�;dA�A�A�C�A�C�A�-A�I�A�G�A�C�A�A�A�E�A�E�A�I�A�G�A�E�A�C�A�9XA�9XA�?}A�G�A�C�A�33A�=qA�=qA�33A�9XA�7LA�9XA�7LA�=qA�=qA�=qA�5?A�A�A�;dA�;dA�;dA�;dA�=qA�9XA�A�A�;dA�?}A�A�A�9XA�9XA�1'A�7LA�?}A�A�A�?}A�;dA�9XA�1'A�5?A�1'A�7LA�$�A�(�A�5?A�9XA�;dA�7LA�9XA�=qA�;dA�;dA�;dA�?}A�=qA�;dA�=qA�?}A�5?A�7LA�A�A�=qA�A�A�-A�?}A�E�A�&�A�?}A�?}A�9XA�=qA�9XA�7LA�9XA�=qA�?}A�A�A�;dA�9XA�=qA�;dA�;dA�=qA�9XA�;dA�?}A�5?A�7LA�1'A�7LA�7LA�7LA�9XA�9XA�7LA�1'A�33A�5?A�1'A�/A�9XA�+A�9XA�33A�7LA�33A�7LA�/A�&�A��A�&�A�(�A�-A�33A�1'A�+A�9XA�=qA�=qA�=qA�A�A�7LA�33A�33A�9XA�7LA�7LA�+A�-A�33A�1'A�5?A�9XA�;dA�5?A�33A�/A�A�/A�5?A�5?A�/A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�/A�1'A�7LA�/A�5?A�5?A�1'A�/A�-A�/A�-A�-A�1'A�33A�1'A�7LA�9XA�5?A�5?A�/A�/A�1'A�/A�33A�9XA�C�A�A�A�C�A�G�A�S�A�S�A�VA�O�A�O�A�Q�A�O�A�Q�A�S�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�I�A�VA�ZA�S�A�XA�ffA�`BA�hsA�hsA�bNA�`BA�`BA�^5A�bNA�`BA�^5A�\)A�`BA�`BA�^5A�^5A�^5A�^5A�^5A�^5A�ZA�ZA�Q�A�Q�A�S�A�`BA�XA�M�A�XA�Q�A�K�A�G�A�E�A�I�A�K�A��A���A�ƨA�ĜA�XA�A��A��A��/A���A���A���A���A��hA��7A�r�A�jA�hsA�bNA�Q�A�Q�A�O�A�O�A�G�A�C�A�A�A�?}A�?}A�=qA�7LA�33A�+A�(�A��A��A��A�JA���A��HA���A���A���A�~�A�z�A�l�A�XA�S�A�Q�A�M�A�I�A�C�A�?}A�=qA�9XA�5?A�/A�-A�(�A�$�A��A�bA�  A��A��`A���A�ĜA��^A��9A���A���A���A��hA��A�~�A�~�A�x�A�t�A�t�A�x�A�x�A�x�A�x�A�v�A�p�A�l�A�hsA�hsA�ffA�bNA�\)A�\)A�XA�VA�Q�A�O�A�M�A�K�A�I�A�E�A�C�A�A�A�;dA�7LA�33A�$�A�"�A��A�oA�1A�A���A��A��yA��#A���A���A�ĜA��jA��FA���A���A���A��uA��A�XA�S�A�O�A�=qA��A��A��`A��
A�A���A��A�^5A�"�A�A��`A���A�ƨA��jA��RA��FA��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA��\A��7A��+A��7A��A�~�A�t�A�l�A�l�A�l�A�n�A�n�A�jA�jA�jA�l�A�l�A�l�A�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ff@�t�@�|�@�t�@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�|�@�t�@�|�@�t�@�|�@�t�@�t�@�t�@�t�@�t�@�|�@�t�@�t�@�t�@�dZ@�C�@�33@�+@�"�@�"�@�"�@�"�@��@�o@�@�@���@�@�@���@���@���@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��@���@��!@��!@���@�~�@�v�@�ff@�ff@�ff@�ff@�n�@�n�@�n�@�n�@�ff@�^5@�M�@�=q@�-@�-@�-@�$�@�$�@�$�@��@��@�{@�{@�{@�{@�{@�J@�J@�J@�J@�@�@�@���@���@���@��@��@��@��@��T@��#@��#@��#@��#@��#@��#@���@���A�`BA�hsA�hsA�bNA�`BA�`BA�^5A�bNA�`BA�^5A�\)A�`BA�`BA�^5A�^5A�^5A�^5A�^5A�^5A�ZA�ZA�Q�A�Q�A�S�A�`BA�XA�M�A�XA�Q�A�K�A�G�A�E�A�I�A�K�A��A���A�ƨA�ĜA�XA�A��A��A��/A���A���A���A���A��hA��7A�r�A�jA�hsA�bNA�Q�A�Q�A�O�A�O�A�G�A�C�A�A�A�?}A�?}A�=qA�7LA�33A�+A�(�A��A��A��A�JA���A��HA���A���A���A�~�A�z�A�l�A�XA�S�A�Q�A�M�A�I�A�C�A�?}A�=qA�9XA�5?A�/A�-A�(�A�$�A��A�bA�  A��A��`A���A�ĜA��^A��9A���A���A���A��hA��A�~�A�~�A�x�A�t�A�t�A�x�A�x�A�x�A�x�A�v�A�p�A�l�A�hsA�hsA�ffA�bNA�\)A�\)A�XA�VA�Q�A�O�A�M�A�K�A�I�A�E�A�C�A�A�A�;dA�7LA�33A�$�A�"�A��A�oA�1A�A���A��A��yA��#A���A���A�ĜA��jA��FA���A���A���A��uA��A�XA�S�A�O�A�=qA��A��A��`A��
A�A���A��A�^5A�"�A�A��`A���A�ƨA��jA��RA��FA��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA��\A��7A��+A��7A��A�~�A�t�A�l�A�l�A�l�A�n�A�n�A�jA�jA�jA�l�A�l�A�l�A�hsA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ff@�t�@�|�@�t�@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�|�@�t�@�|�@�t�@�|�@�t�@�t�@�t�@�t�@�t�@�|�@�t�@�t�@�t�@�dZ@�C�@�33@�+@�"�@�"�@�"�@�"�@��@�o@�@�@���@�@�@���@���@���@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��y@��H@��H@��@���@��!@��!@���@�~�@�v�@�ff@�ff@�ff@�ff@�n�@�n�@�n�@�n�@�ff@�^5@�M�@�=q@�-@�-@�-@�$�@�$�@�$�@��@��@�{@�{@�{@�{@�{@�J@�J@�J@�J@�@�@�@���@���@���@��@��@��@��@��T@��#@��#@��#@��#@��#@��#@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�pe>)��=쿱>gŬ>�8�=��C?S=���=��>��@�H�=�!>��?�@�~�=���=���>h4@U�c=�T=�B=���>!��@�o @�\>Z�=PR�=Zp�=�F�=��>�>�=��O=�3@�U@�|1=��=�/?��@��k@T�=��.>�	B>��	=�=�>n�@�z> �>9�a@�j�@�_�>_F?���@$\>>u�>TL@�@hZq?���>A߹@0�@�o�?�`=��?�f�@�r2@�yh=�.�?|��=�Z�>[ߏ>��?V�@�{�=��C>:�@�zx?��w>52@�s.@�|�?<��> S�@�Z�@�t�>��9>|b�@�k�@�q7=���=�B1?\�D=�c=��?�Q@�s.>A�.>ul=�wG=���=ʫ�> D(@�o?@�9m>*M+@�q"=�[�?Si�@�|1=��>5 @�U�@���=�Cl?)d�?:�?=�J�>h�@�]�@��=��>]��=��(>�X=đ*?�ҳ>�ڐ@�j�@�u�=�@>܇=��>=�L?l�=g-�=���=�^ >>Ov>bu%@�~�?6�=�&W>�@�A@�~�=\Hk=���=��;=�g=�ć>���@�~�@2�>��?\�y@���@��@���=�M�>q,(>���@`W=�j@=���>>-��@��I@��@@��i>��z@��>��?P��>�T�?��>vC>��@�ں>%�q@H,@�c�?�j�@�_?�@U�@��I@��@���>x��>�o@�SP?#7�>��q@���@���>���@���?>l�@�j+@��]@�ZG?��Y@���?��@���@+�=@�|[@���@��L>�-�@���@���@��D@��@��Y@��]@���@.@���@��]@���@���@���@��i@���@��i@��@���@���@���@���@��z@��Y@��"@��"@��i@��.@��.@���@���@���@���@���@��e@���@��e@��e@���@���@��u@��u@��@���@��u@���@��Y@��Y@���@��Y@���@��Y@��]@���@���@���@��L@���@��L@��L@���@��@@���@��<@���@���@���@���@���@���@���@���@���@��L@���@���@���@���@��@��@��]@��L@���@���@���@��@@��@��]@���@���@���@���@���@���@���@���@��@���@���@���@��Y@���@���@���@���@��L@��]@���@��L@���@���@��]@���@��]@���@��@@��<@���@���@���@���@���@���@���@��/@��@@���@���@���@���@��L@���@���@���@��/@���@��@@���@��@@��/@��@@���@��@@��/@���@���@���@���@��4@��/@��/@���@���@���@���@���@���@���@���@��i@��/@��@@��/@��@���@��@���@��L@��<@���@���@���@���@���@��L@���@���@���@��@@���@���@��<@���@���@���@��]@��@@���@���@���@��z@��@��#@���@��@���@���@���@���@���@���@���@���@���@���@���@���@��@@��#@���@���@��#@��f@��@��k@��#@���@��/@���@���@��Z@���@��@@���@���@���@��@���@��/@��/@��@@��@@���@���@���@���@���@��@@���@��f@��@��I@��{@��f@���@���@��@��k@���@��@��f@��@��I@��f@��f@��#@��f@��f@���@���@��k@��Z@���@���@���@��@��@��w@��#@���@��s@��4@���@��s@���@��w@��#@��w@���@��/@��&@��"@���@��@���@��2@��2@��u@���@��C@���@��2@���@���@���@���@���@���@��C@��C@���@���@���@��?@��@���@��@��[@��B@��@���@��O@���@���@���@���@���@���@���@���@��O@���@���@���@��@��C@��C@��@���@��@��@��	@��&@�}�@�p&@�s�@�{�@�~R@�y�@�m	@�H�@�=q@�A5@�MU@�i@��@�	�@�p@�
�@�h@�t@�	-@�	�@��3@���@� �@��j@���@���@��@��@���@��@��g@��8@��J@��@��@��W@���@��>@��	@���@���@���@���@��V@��F@��@���@���@��@���@��1@���@��d@��l@���@���@��d@��`@��[@���@���@��@��-@��[@��p@��u@��-@��9@��)@���@��K@���@���@��P@��@��h@���@��A@��{@��@���@��)@�ܱ@��v@��@���@��@��R@��M@��@��@@��	@� @��@��@�@�t@�	@�	�@�	�@�	l@�	l@�[@�[@�@��@��@�C@�G@�6@� �@��.@��@��Y@��M@��w@��Z@��1@��@��m@��@��;@���@��=@��,@��M@��^@�ԕ@���@��:@���@���@��z@��k@���@���@���@��@���@��H@��/@�~�@�ud@�ek@�Y!@�I�@�=�@�6@�/�@�+�@�(c@�'=@�%�@�#O@�"�@�!�@�!B@� �@��@�@�:@��@�T@��@��@��@�"@��@��@�j@��@�@�@��@�U@��@�@�R@�
�@�%@��@�y@�y@��@��@��@�%@�t@�_@�y@�O@��@�h@��@��@�h@��@��@��@�%@��@��@QfQ@Qf'@Qf{@Qf�@Qg#@Qf�@Qg#@Qf�@Qf�@Qf{@Qf�@Qf�@Qf�@Qg#@Qf�@Qf�@Qf�@Qf�@Qf�@Qf{@Qf�@Qf{@Qe�@Qe�@Qe�@Qe�@Qd�@Qc�@Qb9@Q]�@Q\}@Q[@QZ�@QZ�@QZ\@QY�@QX@QW@QV@QU�@QUq@QUq@QUG@QUG@QUG@QT�@QTv@QT"@QS�@QS&@QR�@QR�@QR*@QR*@QR*@QR*@QR*@QR @QR @QQ�@QQ@QP3@QP]@QN�@QM�@QK�@QGZ@QE�@QC�@Q@�@Q?S@Q>@Q>@Q>@Q>�@Q?)@Q>�@Q>�@Q>W@Q=\@Q;@Q8q@Q6z@Q5~@Q5+@Q4�@Q4/@Q3�@Q33@Q2�@Q2�@Q2�@Q28@Q28@Q1�@Q1�@Q0�@Q0�@Q0@@Q/�@Q/�@Q.�@Q.�@Q.I@Q.I@Q-�@Q,�@Q,R@Q+�@Q+V@Q+@Q*@Q*@Q)�@Q)�@Q)�@Q)�@Q)@Q(�@Q'�@��,@��/@���@��#@���@���@��j@���@���@���@��o@��j@���@���@���@��j@���@��@��Z@��@���@��x@���@��
@���@���@��@��@��c@���@���@���@���@�� @���@���@��.@��h@�n�@��@�Q�@�J�@�K
@�;�@�;%@�;�@�6&@�4�@�2a@�+@�)�@�(9@�'g@�"�@�!�@�!�@�!l@�6@�&@�&@��@�T@��@��@�&@��@��@��@�+@�	@��@��@�.@��m@���@��@���@��@�ܜ@��0@��o@��@��+@��@��+@��@��j@�٩@�٩@���@���@��@���@��e@��]@���@��1@�Б@��O@���@��e@��&@��u@�ɰ@��)@�ί@���@��^@���@��@��@� i@��@��@�#@��@�!l@�&B@�) @�+A@�,�@�.@�0U@�2a@�3	@�33@�3	@�2�@�2�@�28@�1Q@�0j@�/�@�.�@�.I@�-@�+�@�*�@�'|@�&�@�$�@�!�@�6@� @�a@�@@�w@�4@�@�
�@�	W@�O@�C@���@���@���@��@���@���@�� @���@��r@���@��e@���@��9@��p@��*@���@��a@�x@�k'@�`�@�Y`@�T�@�O�@�N�@�M�@�J�@�I�@�I=@�H�@�G�@�Ft@�E�@�E�@�Ec@�DR@�CB@�A�@�A�@�A5@�@�@�@O@�?�@�?�@�?�@�=G@�9�@�:?@�;�@�:?@�6�@�1<@�+�@�,R@�,�@�,=@�-M@�+�@�+�@�+�@�-b@�-@�,�@�+�@�+A@�+,@�+@�*�@�*�@�*�@�(�@�+@�+,@�+A@Q�@Q�@@Q�@Q�@@Q��@Q��@Q�<@Q�<@Q�<@Q�@Q��@Q��@Q�@Q�<@Q��@Q�f@Q��@Q�f@Q��@Q��@Q��@Q�@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q�j@Q�^@Q~�@Q}k@Q|�@Q|F@Q|p@Q{�@Qz�@Qy)@Qw�@Qw\@Qw@Qw\@Qw@Qv�@Qv�@Qv�@Qv@Qu�@Qu�@Qu@Qt�@Qti@Qt?@Qti@Qt@Qti@Qt�@Qt�@Qt�@Qt�@Qt�@Qt@Qs�@QsC@Qrq@Qp�@Ql�@Qj�@Qf�@Qb�@Qa�@Q_F@Q`@Q_F@Q`B@Q`�@Qa@Qa@Qa=@Q`�@Q^�@Q[@QX�@QV�@QV�@QV�@QUq@QUG@QU@QTL@QS�@QS�@QS�@QSz@QSP@QR�@QR�@QR @QR @QQ�@QP�@QP]@QP	@QO�@QO�@QO�@QNf@QM�@QM@@QL�@QLD@QKI@QKI@QJ�@QJ�@QJ�@QJ�@QJ#@QI�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               3444444444344434443444433444444444344433444443443344444434443444334444443443443344334433444444344444433434434433444443444444443344444444443444334444443444333444444443334344444434434343333443443343433343434333433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�pgG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�H}G�O�G�O�G�O�@�~�G�O�G�O�G�O�@U�eG�O�G�O�G�O�G�O�@�o@�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�|2G�O�G�O�G�O�@��k@T�G�O�G�O�G�O�G�O�G�O�@�zG�O�G�O�@�j�@�_�G�O�G�O�G�O�G�O�G�O�G�O�@hZsG�O�G�O�G�O�@�o�G�O�G�O�G�O�@�r6@�yjG�O�G�O�G�O�G�O�G�O�G�O�@�{�G�O�G�O�@�zvG�O�G�O�@�s+@�|�G�O�G�O�@�Z�@�t�G�O�G�O�@�k�@�q7G�O�G�O�G�O�G�O�G�O�G�O�@�s,G�O�G�O�G�O�G�O�G�O�G�O�@�o>@�9nG�O�@�q"G�O�G�O�@�|3G�O�G�O�@�U�@���G�O�G�O�G�O�G�O�G�O�@�]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�j�@�u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�~�G�O�G�O�G�O�@�@@�~�G�O�G�O�G�O�G�O�G�O�G�O�@�~�G�O�G�O�G�O�@���@��@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��K@��@@��jG�O�@��G�O�G�O�G�O�G�O�G�O�G�O�@�ڷG�O�G�O�@�c�G�O�@�_G�O�@U�@��J@��@���G�O�G�O�@�SNG�O�G�O�@���@���G�O�@���G�O�@�j.@��]@�ZCG�O�@���G�O�@���G�O�@�|Z@���@��KG�O�@���@���@��B@��@��[@��]@���G�O�@���@��^@���@���@���@��j@���@��k@��@���@���@���@���@��z@��X@��"@��%@��j@��,@��2@���@���@���@���@���@��f@���@��f@��f@���@���@��z@��w@��
@���@��v@���@��Z@��Z@���@��Z@���@��[@��\@���@���@���@��N@���@��N@��J@���@��<@���@��<@���@���@���@���@���@���@���@���@���@��N@���@���@���@���@��@��@��\@��N@���@���@���@��@@��	@��\@���@���@���@���@���@���@���@���@��@���@���@���@��Z@���@���@���@���@��N@��_@���@��N@���@���@��`@���@��^@���@��A@��>@���@���@���@���@���@���@���@��1@��<@���@���@���@���@��N@���@���@���@��2@���@��=@���@��@@��1@��@@���@��D@��1@���@���@���@���@��2@��/@��2@���@���@���@���@���@���@���@���@��h@��.@��D@��.@��@���@��@���@��N@��;@���@���@���@���@���@��N@���@���@���@��A@���@���@��;@���@���@���@��_@��?@���@���@���@��x@��@��&@���@��@���@���@���@���@���@���@���@���@���@���@���@���@��D@��$@���@���@��%@��j@��@��k@��!@���@��.@���@���@��]@���@��D@���@���@���@�� @���@��2@��.@��>@��@@���@���@���@���@���@��D@���@��h@�� @��K@��{@��d@���@���@��@��f@���@��@��f@��@��H@��j@��j@��&@��b@��g@���@���@��m@��[@���@���@���@���@��@��y@��$@���@��q@��9@���@��r@���@��y@��'@��{@���@��2@��&@��"@���@��@���@��1@��4@��v@���@��A@���@��2@���@���@���@���@���@���@��B@��B@���@���@���@��@@��@���@��
@��Z@��E@��(@��3@���@��!@���@���@��j@���@���@���@��p@��l@���@���@���@��j@���@��@��Z@��@���@��z@���@��
@���@���@��@��@��f@���@���@���@���@��@���@���@��-@��j@�n�@��@�Q�@�J�@�K@�;�@�;(@�;�@�6'@�4�@�2e@�*�@�)�@�(;@�'f@�"�@�" @�"@�!n@�7@�%@�&@��@�R@��@��@�&@��@�~@��@�.@�@��@��@�3@��r@���@��@���@��@�ܜ@��1@��i@��@��-@��@��*@�ـ@��l@�٩@�٪@���@���@��@���@��j@��^@���@��6@�Г@��L@���@��f@��(@��s@�ɮ@��.@�ΰ@���@��^@���@��@��@� l@��@��@�&@��@�!i@�&A@�)@�+D@�,�@�. @�0W@�2e@�3@�38@�3@�2�@�2�@�2?@�1R@�0f@�/�@�.�@�.I@�-@�+�@�*�@�'~@�&�@�$�@�!�@�9@� @�_@�B@�w@�5@�@�
�@�	W@�T@�E@���@���@���@��@���@���@�� @���@��s@���@��e@���@��:@��n@��(@���@��a@�x
@�k$@�`�@�Ye@�T�@�O�@�N�@�M�@�J�@�I�@�IB@�H�@�G�@�Fv@�E�@�E�@�Eg@�DR@�CF@�A�@�A�@�A7@�@�@�@M@�?�@�?�@�?�@�=D@�9�@�:<@�;�@�:>@�6�@�1?@�,@�,O@�,�@�,:@�-Q@�+�@�+�@�+�@�-_@�-@�,�@�+�@�+D@�+*@�*�@�*�@�*�@�*�@�(�@�+@�+*@�+@@Q�@Q�@@Q�@Q�=@Q��@Q��@Q�;@Q�@@Q�=@Q�@Q��@Q��@Q�@Q�B@Q��@Q�e@Q��@Q�f@Q��@Q��@Q��@Q�@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q�h@Q�^@Q~�@Q}k@Q|�@Q|K@Q|p@Q{�@Qz�@Qy-@Qw�@Qw`@Qw@Qw]@Qw@Qv�@Qv�@Qv�@Qv@Qu�@Qu�@Qu@Qt�@Qtj@Qt>@Qth@Qt@Qtk@Qt�@Qt�@Qt�@Qt�@Qt�@Qt@Qs�@QsE@Qrp@Qp�@Ql�@Qj�@Qf�@Qb�@Qa�@Q_E@Q`@Q_E@Q`B@Q`�@Qa@Qa@Qa>@Q`�@Q^�@Q[@QX�@QV�@QV�@QV�@QUs@QUF@QU@QTK@QS�@QS�@QS�@QS{@QSP@QR�@QR�@QR@QR @QQ�@QP�@QP]@QP@QO�@QO�@QO�@QNe@QM�@QM@@QL�@QLH@QKJ@QKJ@QJ�@QJ�@QJ�@QJ�@QJ#@QI�@��(@��3@���@��!@���@���@��j@���@���@���@��p@��l@���@���@���@��j@���@��@��Z@��@���@��z@���@��
@���@���@��@��@��f@���@���@���@���@��@���@���@��-@��j@�n�@��@�Q�@�J�@�K@�;�@�;(@�;�@�6'@�4�@�2e@�*�@�)�@�(;@�'f@�"�@�" @�"@�!n@�7@�%@�&@��@�R@��@��@�&@��@�~@��@�.@�@��@��@�3@��r@���@��@���@��@�ܜ@��1@��i@��@��-@��@��*@�ـ@��l@�٩@�٪@���@���@��@���@��j@��^@���@��6@�Г@��L@���@��f@��(@��s@�ɮ@��.@�ΰ@���@��^@���@��@��@� l@��@��@�&@��@�!i@�&A@�)@�+D@�,�@�. @�0W@�2e@�3@�38@�3@�2�@�2�@�2?@�1R@�0f@�/�@�.�@�.I@�-@�+�@�*�@�'~@�&�@�$�@�!�@�9@� @�_@�B@�w@�5@�@�
�@�	W@�T@�E@���@���@���@��@���@���@�� @���@��s@���@��e@���@��:@��n@��(@���@��a@�x
@�k$@�`�@�Ye@�T�@�O�@�N�@�M�@�J�@�I�@�IB@�H�@�G�@�Fv@�E�@�E�@�Eg@�DR@�CF@�A�@�A�@�A7@�@�@�@M@�?�@�?�@�?�@�=D@�9�@�:<@�;�@�:>@�6�@�1?@�,@�,O@�,�@�,:@�-Q@�+�@�+�@�+�@�-_@�-@�,�@�+�@�+D@�+*@�*�@�*�@�*�@�*�@�(�@�+@�+*@�+@@Q�@Q�@@Q�@Q�=@Q��@Q��@Q�;@Q�@@Q�=@Q�@Q��@Q��@Q�@Q�B@Q��@Q�e@Q��@Q�f@Q��@Q��@Q��@Q�@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q�h@Q�^@Q~�@Q}k@Q|�@Q|K@Q|p@Q{�@Qz�@Qy-@Qw�@Qw`@Qw@Qw]@Qw@Qv�@Qv�@Qv�@Qv@Qu�@Qu�@Qu@Qt�@Qtj@Qt>@Qth@Qt@Qtk@Qt�@Qt�@Qt�@Qt�@Qt�@Qt@Qs�@QsE@Qrp@Qp�@Ql�@Qj�@Qf�@Qb�@Qa�@Q_E@Q`@Q_E@Q`B@Q`�@Qa@Qa@Qa>@Q`�@Q^�@Q[@QX�@QV�@QV�@QV�@QUs@QUF@QU@QTK@QS�@QS�@QS�@QS{@QSP@QR�@QR�@QR@QR @QQ�@QP�@QP]@QP@QO�@QO�@QO�@QNe@QM�@QM@@QL�@QLH@QKJ@QKJ@QJ�@QJ�@QJ�@QJ�@QJ#@QI�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               3444444444344434443444433444444444344433444443443344444434443444334444443443443344334433444444344444433434434433444443444444443344444444443444334444443444333444444443334344444434434343333443443343433343434333433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9AT�9AY�9AZo9AW59AU�9AU�9AU;9AU�9AU�9AT�9AT9AU=9AU�9AU�9AU�9AU;9ATK9AT�9AT9AS�9ASp9AQ�9AS$9AR~9AW9AT�9AQ[9AS�9AQ�9AP 9ALD9AK�9AK<9AL�9A=,9A6�9A">9A#�9@��9A9@�69@�`9@҉9@��9@�?9@��9@�{9@��9@�&9@��9@�9@�o9@�z9@�9@�A9@�B9@��9@�9@��9@��9@�/9@��9@�c9@��9@��9@�@9@��9@��9@��9@�y9@��9@��9@��9@{m9@j�9@cj9@Yi9@W�9@SF9@N.9@No9@O!9@OQ9@O!9@OM9@O�9@O�9@O�9@O�9@P&9@Q19@R�9@T�9@V�9@R�9@L�9@Hq9@Eg9@@y9@<m9@:�9@:�9@<	9@=t9@?/9@C:9@I@9@M;9@O9@Z�9@k�9@|�9@�y9@�G9@�q9@��9@��9@�(9@�u9@��9@��9@�:9@��9@�&9@��9@�9@��9@��9@�9@��9@��9@��9@��9@�9@�j9@� 9@�59@�[9@��9@��9@�I9@��9@�9@�}9@�v9@�9@��9@��9@�9@�m9@��9@�\9@��9@z�9@w&9@v9@r�9@n+9@\�9@Z�9@X.9@S9@Cg9@59@/�9@*~9@!�9@�9@O9?�9?�[9?�}9?�<9?�	9?�p9?�%9?��9?��9?�29?�19?�o9?��9?��9?�69?�s9?�&9?��9?��9?��9?��9?��9?�*9?��9?�9?��9?�F9?�y9?��9?��9?�9?��9?�!9?��9?��9?��9?�9?��9?��9?�;9?�V9?�$9?�'9?�K9?��9?�_9?��9?��9?��9?��9?�|9?��9?�{9?� 9?��9?��9?��8�8�8�8�8�8�|8��8��8��8�8�|8�|8�8��8�@8�8�p8�8�8�8�p8��8�t8�98�p8�@8�p8�C8�8��8�x�8�v�8�ug8�tv8�t8�tF8�sS8�r28�p�8�n�8�np8�n8�nm8�n8�m�8�m�8�mz8�l�8�l�8�l�8�k�8�kj8�k8�j�8�k8�j�8�k8�k28�k28�k�8�k68�k88�j�8�jC8�i�8�h�8�g8�b$8�_�8�[�8�V�8�UN8�R�8�S�8�R�8�S�8�T�8�T�8�T�8�T�8�TZ8�RL8�M�8�K8�H�8�H�8�H�8�GV8�G"8�F�8�F8�E�8�Ev8�E?8�E8�D�8�Dx8�DR8�Cb8�C\8�B�8�B
8�Ay8�A8�@�8�@�8�@�8�?48�>x8�=�8�=S8�<�8�;�8�;�8�;>8�;>8�;A8�;8�:L8�9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�BB�BC�B�{B�fB��BB�B�B�B{B{B{BuB{B{B{B{B�B�B�B�B�B&�B1'BE�Bo�B�bB�-B�9B�?B�LB�RB�FB�B�{B�1B�1B�=B�\B��BǮB��B�B�/B�/B�)B�B�
B��B��B��B�jB�-B��B��B��B�RBBB�^B��B�hB�=B}�BO�B�B\B%B�B�)BƨB�?B��B�uB�JB�Bz�Bs�BbNBT�BK�BF�B@�B33B+B#�B�B+B��B�B�B�wB��B~�Bu�Bt�Bq�BbNBF�B.B�BVBB
�yB
ŢB
�B
��B
�=B
s�B
k�B
^5B
M�B
;dB
,B
�B
PB
B	�B	�HB	��B	��B	ǮB	��B	�dB	�-B	�B	��B	��B	��B	�\B	~�B	x�B	hsB	ZB	W
B	N�B	?}B	6FB	49B	2-B	)�B	!�B	�B	%B��B�NB�NB�B��BǮBǮBƨBƨBĜBÖBÖBÖBB�qB�LB�3B�-B�!B�'B�9B�-B�B�!B��B��B��B�PBz�Bs�Bo�Bn�Bo�Bl�Bm�Bo�Bv�Bz�B|�B|�BdZB]/BZBXBYB\)BbNBbNB[#BaHBdZBbNB`BB\)BXBS�BP�BL�BH�BG�BE�BD�BB�B@�B>wB=qB:^B6FB5?B5?B33B2-B5?B33B6FB7LB8RB9XB7LB6FB5?B5?B6FB8RB8RB8RB6FB33B0!B,B(�B(�B'�B+B0!B1'B0!B1'B5?B6FB7LB8RB;dB=qB=qB:^B5?B6FB-B'�B"�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B&�B&�B&�B)�B)�B+B+B33B5?B5?B6FB7LB8RB;dB>wBG�BL�BQ�B[#B[#B\)Bm�Bo�Bq�Bs�Bv�Bx�B~�B�VB�oB�{B�uB�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�RB�wBĜB��B��B��B��B��B��B��B��B��B��B��B�B�#B�;B�sB�B��B��B��B	B	B	B	B	1B	
=B	DB	bB	�B	�B	�B	!�B	&�B	,B	/B	2-B	5?B	9XB	<jB	<jB	>wB	@�B	B�B	C�B	C�B	C�B	C�B	D�B	F�B	G�B	H�B	M�B	O�B	Q�B	Q�B	T�B	XB	[#B	\)B	_;B	`BB	aHB	dZB	dZB	ffB	gmB	iyB	jB	l�B	n�B	o�B	p�B	s�B	w�B	z�B	|�B	� B	�B	�B	�B	�1B	�7B	�7B	�=B	�DB	�JB	�JB	�VB	�bB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�?B	�LB	�RB	�XB	�dB	�jB	�wB	��B	��B	B	B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�B	�tB
�B
�B
1B
 �B
&�B
1AB
8lB
;�B
A�B
GEB
OvB
U�B
\)B
aHB
d�B
h$B
k�B
oiB
s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��?d�\?��?���?��?�N@S�>�k�>�	�?5�B��?�??�'A�B��>��v>��?2eA���>�T�>��>�j
?T$_B�B1?�m�>���>�s�>�>�B�@>��?'xA_!TB�b?L�?��@O�B�sA��U?��?��@c|?�]?�UB��?Q�?pKMB��A�$�?)�8@�D�A�}?O�I??�Aj.LA�eLA�!?}��AZ�B��AA�?$��@�ӏB��B��?�@�v)? j�?���?,a�@Ot*B�4?��?q�lB�iA ��?j�]B��B��@��J?R.�B��B��@� ?���B�iB��>��>�y�@���>�?!_x@�-�B�V?�k{?�]�>�2�>�J�?��?P�UB�GA��N?_h�BG�?!�@��aB��?��?l�MB�A�)�>�CN@u�i@�k�?\1?Go~B�NA|r? -�?�)J?0�@5i�>���@��@'�B��B	�>�9�?<�>ֻ�?��@� �>�Qv>�%>°�?|�?�+�B�
@�4�>� �?4��A��B�y>�U�>���>�B�>�E?%Ua?��B��A���?(D@��B��B�NB�?%?�	N?���AI~�>���>�_�?-��?_�UB�B��A�	o?��uB�F?/EZ@��?��RA�?���?CM�A��?VOAj��B�A�IB��@dK�A�B�GB��B�.?�=�?.m�A�#@h�@p�B�?B�5?���B��@�PB��B�WB��A�zB��@�K�B��A�<A��OB�B��?ڽ�B��B��B��B��B��B�WB��A�wB�B�#B�#B�#B�#B�B�WB��B�rB�B��B�NB�B��B�3B��B��B��B��B�_B��B�>B��B��B��B��B�#B��B��B��B�NB�[B�B�JB�DB��B��B��B��B�iB�eB�B��B�yB�6B�B�B�FB�B�B��B�BB��B�B��B�B�)B��B��B��B��B��B��B�6B�B��B��B��B�3B�B�B�yB��B��B��B�,B�%B�|B�yB�@B��B�4B��B�iB��B��B��B��B��B��B�aB�eB��B�7B�B�B�NB�DB��B��B�SB��B��B�]B��B�[B�aB�B��B��B��B�IB��B��B��B��B��B��B��B�B��B�B�*B��B�uB�aB��B��B��B��B�'B��B��B�YB��B��B�(B��B�B��B��B�aB��B��B��B�]B�>B�*B��B�5B��B��B��B��B�B�|B��B�=B�B�B�qB��B��B�.B�6B�DB�B�B��B�aB�eB��B�B��B��B�B�wB��B�B��B�2B��B��B�BB�<B�&B�tB�tB��B��B��B�<B�yB��B�<B�B�
B�?B�YB��B�`B�\B�:B��B��B�;B��B��B�,B�xB��B�pB��B��B�B�qB��B�B�!B��B�YB�-B�%B��B��B�B�<B�]B�YB�)B�]B��B��B�B��B��B��B�lB��B�B�cB��B��B��B��B��B�JB��B�^B��B�yB�6B��B��B��B��B��B��B��B��B�B�1B�!B�B��B��B�]B�BB�UB��B��B�uB�-B��B��B�;B��B�B��B��B��B�%B��B�B� B�4B�4B��B��B�RB�RB��B��B�B��B�pB�lB� B��B�B��B��B��B��B��B��B��B�B��B��B�GB�9B��B�XB��B��B�+B��B�B��B�.B��B��B�B��B�vB�
B��B�B�4B��By)BxHB�[B�sBk�B��B��B�-B�HB�~BۚB�B�\B�B��B�B��B�B�;B��B�UB��B�#B�B�B��B�B��B�B�.B��B�B�B�)B�B�<B�B�B�}BߣB��B��B��B��B�rB�B�B	�B�BcB�B.B�BJB�B{BJB�B2B%�B$NB,�B( B%vB6B6BB;BD�BIEBT�BY�Bb�Be�Bg�Bv�B��B��B��B��B�.B��B��B��B��B�,B�zB��B��B��B�aB�B�CB�:B��BöB�{B�rB�+B·B��B�BB�B��BŹB��B��B�7B�BƤBǵBǥB�XB�B�&B�CBƕB�B�B��B��B˵B��B�	B�rB�uBʃB�B�?BωB��BиB�}B��B��B�pB�FB�kBۜB�JB�B�6B��B�tB��B��B�B�2B�JB�B�&B�}B�B�B�B��B�cB�jB�NB�FB�B��B��B�oB�B�B��B�4B�B��B�B��B�B�B�;B�9B��B�@B�GB�/B�NB�}B�B�B�qB��B�B�xB�gB��B�B�B�B�KB�B�B	ĄB	�XB	ĉB	čB	��B	àB	òB	�yB	�{B	�B	�B	�(B	��B	�KB	��B	��B	��B	àB	ÓB	�gB	�yB	�@B	��B	��B	B	B	��B	��B	��B	��B	��B	°B	�VB	�YB	�-B	�yB	�hB	òB	��B	ĵB	�jB	�]B	�$B	�'B	�B	��B	�iB	�B	��B	�?B	�#B	��B	�oB	�UB	�WB	�JB	� B	��B	��B	ÝB	�B	�qB	ÂB	�TB	��B	�B	��B	��B	�lB	�7B	�7B	�dB	�GB	�JB	ŪB	��B	ĀB	ąB	�B	�VB	B	��B	�gB	ĵB	�zB	�/B	ŷB	�mB	��B	ŝB	�RB	�8B	��B	��B	řB	�NB	��B	ŻB	�4B	��B	ŮB	�B	�B	ŔB	ŇB	��B	�zB	��B	�mB	�"B	��B	�2B	�B	��B	��B	ťB	ŘB	�B	ŷB	�B��B�yB�"B��B؈B؀B��B׾B�nB�GBؕB��B�UB�-B�-B؛B��B�DB׊B��BإBځBیB�-B�B�PBۖB�oB�CB�RBټB��B�B؈B��B��B��B�B�iB�GBBmB�BB
�B�BLBSB�BZB8B�BQBOB�BcB�B�BzB?BzB6B�B�B�BuB&B�B�B7B DB �B#-B&�B(2B(�B)XB)�B+B/VB1%B2vB41B5�B8(B:B:�B<�B>)B@�BBgBE0BHQBMBOBPCBR[BT�BX�BZ*B\�B_Be�BilBnBs�B}�B�-B��B��B� B��B��B�8B��B�B�6B�0B�~B�BܳBސB�6B�B�$B��B�B��B�B��B��B�B�B�B��B�B�!B�B�B�sB��B�#B��B�-B��B�@B��B�>B�NB�B��B��B�`B��B��B��B�fB��B�BBBB�B
WB
�BB�B�B�B0B{B2BmBB'B�B_B�BpB-BMB�BXB$BB�B�BZBB'B�B�BB�B"B�B�B�BB"B�B�B�B�B�B.B�B.B*B*B�B�B�BzB�B�B@B$B�B�B)B�BB�B�B�B	�|B	�}B	�aB	�VB	��B	۶B	��B	��B	ۿB	ܣB	�jB	�]B	�nB	܀B	ܣB	�gB	ܧB	�@B	ܟB	ہB	�fB	܉B	�>B	�B	�B	��B	��B	��B	�<B	��B	��B	��B	��B	�GB	��B	�B	�ZB	ܥB	܇B	�fB	�;B	� B	�B	��B	ݎB	�tB	�HB	��B	��B	݊B	�B	��B	�}B	�DB	�UB	�B	�.B	�?B	�2B	�bB	�B	��B	ܔB	�MB	��B	�MB	�/B	�CB	ܧB	�'B	�=B	�JB	ݯB	�<B	݈B	�3B	ݑB	ݕB	݈B	ݙB	�3B	��B	�B	�nB	�B	��B	��B	�B	��B	ݟB	�B	ݰB	ޕB	�iB	�0B	�B	ݹB	ސB	��B	��B	ݒB	��B	݅B	�:B	�B	��B	ݺB	��B	�aB	��B	ݎB	�5B	�rB	�eB	�B	� B	��B	��B	�OB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444444444344434443444433444444444344433444443443344444434443444334444443443443344334433444444344444433434434433444443444444443344444444443444334444443444333444444443334344444434434343333443443343433343434333433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   B�B�BB�BC�B�yB�dB��BB}B�B�BBzBxBvByByB{B{B�B�B�B�B�B&�B1'BE�Bo�B�aB�,B�8B�=B�MB�SB�DB�B�{B�1B�1B�=B�^B��BǬB��B�B�/B�-B�*B�B�B��B��B��B�gB�.B��B��B��B�RBBB�[B��B�fB�=B}�BO�B�B]B$B�B�(BƨB�@B��B�sB�KB�Bz�Bs�BbLBT�BK�BF�B@�B33B+ B#�B�B+B��B�}B�B�wB��B~�Bu�Bt�Bq�BbMBF�B.B�BSBB
�yB
ţB
�B
��B
�=B
s�B
k�B
^4B
M�B
;cB
,B
�B
OB
B	�B	�HB	��B	��B	ǭB	��B	�bB	�+B	�B	��B	��B	��B	�ZB	~�B	x�B	hrB	ZB	WB	N�B	?{B	6DB	48B	2,B	)�B	!�B	�B	"B��B�KB�MB� B��BǭBǬBƦBƨBĚBÕBÕB×BB�oB�JB�4B�,B� B�%B�7B�-B�B�B��B��B��B�OBz�Bs�Bo�Bn�Bo�Bl�Bm�Bo�Bv�Bz�B|�B|�BdZB]/BZBXBYB\'BbLBbMB["BaGBdYBbNB`@B\(BXBS�BP�BL�BH�BG�BE�BD�BB�B@�B>wB=oB:\B6EB5>B5>B30B2+B5>B32B6FB7KB8MB9XB7KB6EB5=B5<B6EB8OB8QB8PB6FB33B0 B,B(�B(�B'�B*�B0 B1%B0B1'B5=B6DB7LB8OB;cB=nB=pB:[B5<B6CB-B'�B"�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B&�B&�B&�B)�B)�B+B+ B31B5=B5<B6EB7JB8RB;bB>wBG�BL�BQ�B[B[$B\(Bm�Bo�Bq�Bs�Bv�Bx�B~�B�UB�mB�wB�rB�nB�lB�mB�zB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�1B�QB�vBĚB��B��B��B��B��B��B��B��B��B��B��B�B� B�:B�rB�B��B��B��B	B	B	B	B	0B	
<B	DB	_B	�B	�B	�B	!�B	&�B	,B	/B	2,B	5?B	9WB	<iB	<iB	>vB	@�B	B�B	C�B	C�B	C�B	C�B	D�B	F�B	G�B	H�B	M�B	O�B	Q�B	Q�B	T�B	XB	[ B	\'B	_9B	`@B	aGB	dZB	d[B	fdB	glB	ivB	j}B	l�B	n�B	o�B	p�B	s�B	w�B	z�B	|�B	�B	�B	�B	�B	�0B	�3B	�6B	�>B	�AB	�IB	�HB	�SB	�aB	�sB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�1B	�;B	�JB	�NB	�WB	�bB	�hB	�tB	��B	��B	B	B	ĚB	šB	ƥB	ƧB	ǫB	ȱB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�$B	�"B	�)B	�'B	�,B	�/B	�,B	�-B	�2G�O�B	�B	�rB
�B
�B
0B
 �B
&B
1AB
8lB
;�B
A�B
GDB
OwB
U�B
\'B
aFB
d�B
h"B
k�B
ohB
s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B��G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�B�B.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�aG�O�G�O�G�O�B�sA��UG�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B��A�$�G�O�G�O�G�O�G�O�G�O�G�O�A�eLG�O�G�O�G�O�B�G�O�G�O�G�O�B��B��G�O�G�O�G�O�G�O�G�O�G�O�B�5G�O�G�O�B�fG�O�G�O�B��B��G�O�G�O�B��B��G�O�G�O�B�fB��G�O�G�O�G�O�G�O�G�O�G�O�B�TG�O�G�O�G�O�G�O�G�O�G�O�B�FA��LG�O�BG�G�O�G�O�B��G�O�G�O�BA�)�G�O�G�O�G�O�G�O�G�O�B�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�A��B�wG�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B��B�KB�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B��A�	oG�O�B�GG�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�B�G�O�B��G�O�A�B�EB��B�,G�O�G�O�A�#G�O�G�O�B�9B�4G�O�B��G�O�B��B�TB��G�O�B��G�O�B��G�O�A��LB�B��G�O�B��B��B��B��B��B�TB��G�O�B�B�"B�#B�#B�#B�B�UB��B�oB�B��B�NB�B��B�1B��B��B��B��B�`B��B�=B��B��B��B��B�#B��B��B��B�MB�[B� B�HB�DB��B��B��B��B�hB�dB�B��B�xB�4B�B�B�EB�B�B��B�DB��B�B��B�B�'B��B��B��B��B��B��B�4B�B��B��B��B�2B�B�B�xB��B��B��B�*B�#B�{B�xB�>B��B�4B��B�hB��B��B��B��B��B��B�bB�dB��B�3B�B�B�NB�CB��B��B�RB��B��B�YB��B�YB�`B�B��B��B��B�GB��B��B��B��B��B��B��B�B��B�B�'B��B�rB�aB��B��B��B��B�'B��B��B�YB��B��B�(B��B�B��B��B�aB��B��B��B�^B�<B�'B��B�4B��B��B��B��B�B�{B��B�?B�B�B�pB��B��B�*B�5B�BB�B�B��B�`B�aB��B�B��B��B�B�wB��B�B��B�2B��B��B�CB�;B�$B�tB�tB��B��B��B�<B�xB��B�;B�B�	B�<B�YB��B�]B�ZB�9B��B��B�;B��B��B�*B�wB��B�pB��B��B�B�nB��B�B�B��B�TB�+B�#B��B��B�B�;B�]B�YB�'B�^B��B��B�B��B��B��B�mB��B�B�aB��B��B��B��B��B�IB��B�]B��B�yB�7B��B��B��B��B��B��B��B��B�
B�/B�#B�	B��B��B�^B�CB�UB��B��B�tB�,B��B��B�9B��B�B��B��B��B�&B��B�B�B�5B�5B��B��B�PB�PB��B��B�B��B�oB�mB� B��B�B��B�xB� B��B؅B؁B��B׼B�mB�GBؕB��B�TB�+B�+BؚB��B�CB׊B��BأBڃBۊB�,B�B�LBۓB�nB�EB�OBټB��B�B؅B��B��B��B�B�hB�FBBkB�BB
�B�BJBRB�BWB7B�BOBOB�BcB�B�ByB>B~B3B�B�B�BuB#B�B�B8B EB �B#0B&�B(/B(�B)UB)�B+}B/UB1 B2uB40B5�B8&B:	B:�B<�B>(B@�BBfBE.BHQBMBOBPDBR]BT�BX�BZ+B\�B_Be�BihBnBs�B}�B�+B��B��B� B��B��B�8B��B�B�2B�.B�|B�BܲBޏB�5B�B�%B��B�B��B�B��B��B�B�B�B��B�B�!B�B�B�tB��B�$B��B�-B��B�AB��B�?B�NB�|B��B��B�aB��B��B��B�fB��B�B~BBB�B
TB
�BB�B�B�B/B~B0BnBB%B�B`B�BnB0BPB�BXB$BB�B�BXB"B'B�B�BB�B"B�B�B�BB B�B�B�B�B�B+B�B+B+B+B�B�B�BzB�B�BAB"B�B�B&B�B�B�B�B�B	�|B	�{B	�`B	�SB	��B	۵B	��B	��B	۽B	ܢB	�hB	�ZB	�nB	܀B	ܢB	�eB	ܦB	�>B	ܜB	ۀB	�cB	܇B	�>B	�B	�B	��B	��B	��B	�<B	��B	��B	��B	��B	�EB	��B	��B	�XB	ܣB	܇B	�cB	�;B	��B	�B	��B	ݎB	�tB	�GB	��B	��B	݊B	� B	��B	�{B	�CB	�RB	�	B	�,B	�<B	�0B	�`B	�B	��B	ܐB	�LB	��B	�LB	�,B	�AB	ܥB	�'B	�;B	�IB	ݮB	�:B	݆B	�1B	ݐB	ݓB	݆B	ݘB	�1B	��B	�B	�nB	�B	��B	��B	�B	��B	ݜB	�B	ݮB	ޔB	�fB	�/B	�B	ݵB	ގB	��B	��B	ݑB	��B	݄B	�:B	�B	��B	ݹB	��B	�aB	��B	݌B	�5B	�pB	�dB	�B	��B	��B	��B	�NB	��B��B�xB� B��B؅B؁B��B׼B�mB�GBؕB��B�TB�+B�+BؚB��B�CB׊B��BأBڃBۊB�,B�B�LBۓB�nB�EB�OBټB��B�B؅B��B��B��B�B�hB�FBBkB�BB
�B�BJBRB�BWB7B�BOBOB�BcB�B�ByB>B~B3B�B�B�BuB#B�B�B8B EB �B#0B&�B(/B(�B)UB)�B+}B/UB1 B2uB40B5�B8&B:	B:�B<�B>(B@�BBfBE.BHQBMBOBPDBR]BT�BX�BZ+B\�B_Be�BihBnBs�B}�B�+B��B��B� B��B��B�8B��B�B�2B�.B�|B�BܲBޏB�5B�B�%B��B�B��B�B��B��B�B�B�B��B�B�!B�B�B�tB��B�$B��B�-B��B�AB��B�?B�NB�|B��B��B�aB��B��B��B�fB��B�B~BBB�B
TB
�BB�B�B�B/B~B0BnBB%B�B`B�BnB0BPB�BXB$BB�B�BXB"B'B�B�BB�B"B�B�B�BB B�B�B�B�B�B+B�B+B+B+B�B�B�BzB�B�BAB"B�B�B&B�B�B�B�B�B	�|B	�{B	�`B	�SB	��B	۵B	��B	��B	۽B	ܢB	�hB	�ZB	�nB	܀B	ܢB	�eB	ܦB	�>B	ܜB	ۀB	�cB	܇B	�>B	�B	�B	��B	��B	��B	�<B	��B	��B	��B	��B	�EB	��B	��B	�XB	ܣB	܇B	�cB	�;B	��B	�B	��B	ݎB	�tB	�GB	��B	��B	݊B	� B	��B	�{B	�CB	�RB	�	B	�,B	�<B	�0B	�`B	�B	��B	ܐB	�LB	��B	�LB	�,B	�AB	ܥB	�'B	�;B	�IB	ݮB	�:B	݆B	�1B	ݐB	ݓB	݆B	ݘB	�1B	��B	�B	�nB	�B	��B	��B	�B	��B	ݜB	�B	ݮB	ޔB	�fB	�/B	�B	ݵB	ގB	��B	��B	ݑB	��B	݄B	�:B	�B	��B	ݹB	��B	�aB	��B	݌B	�5B	�pB	�dB	�B	��B	��B	��B	�NB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444444444344434443444433444444444344433444443443344444434443444334444443443443344334433444444344444433434434433444443444444443344444444443444334444443444333444444443334344444434434343333443443343433343434333433333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311648012020083116480120200831164801202008311648012020083116480120200831164801202008311648012020083116480120200831164801202008311648012020083116480120200831164801AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816472019021918164720190219181647    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816472019021918164720190219181647  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816472019021918164720190219181647  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311648012020083116480120200831164801  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                