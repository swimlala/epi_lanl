CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  G   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:30Z creation      
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181630  20200831164601  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @Ս�1FM@Ս�1FM@Ս�1FM111 @Ս���@Ս���@Ս���@6W�O�;d@6W�O�;d@6W�O�;d�c��
=p��c��
=p��c��
=p�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @333@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��D�0�D��fD��
D��D�I�D���D��HD�=D�S�D��qDǷ\D�	�D�6�D�k�D�)D��D�?\D�D��{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�;L�ͽ���    �L�;L�ͽ��;L�;L�;L�;L�;L�ͽ��;L�;L��    ���;L�;L�ͽ��ͽ��;L�;L�ͽ���    �L�ͽ��;L�;L�;L�;L�ͽ���    �L�;L��        �L�ͽ���    ���;L�;L�;L�ͽ��;L�ͽ��ͽ��;L�;L�ͽ��ͽ��;L�;L�ͽ��ͽ��;L�;L�;L��=���>L�;L�;L�;L�;L�;L�ͽ��;L��=���=��;L�;L��    >L��>L�;L�ͽ���    ���;L�ͽ��;L��    ���;L�;L�ͽ��;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�ͽ��;L�;L�;L��    >������;L�;L�ͽ��;L�ͽ��ͽ��;L�ͽ��ͽ��ͽ��;L��        �L�ͽ��;L�ͽ��;L�ͽ��;L��=���    �L�ͽ��ͽ��;L�;L�ͽ��ͽ��;L�ͽ��;L�ͽ���    �L�ͽ��;L�;L�ͽ���    ���;L�ͽ��ͽ��ͽ��;L�;L�;L��    =���    �L�ͽ���    =���    �L�;L�ͽ���=��;L�ͽ��ͽ���=���    �L�ͽ��ͽ��ͽ��;L�ͽ���=���    �L�ͽ��;L�ͽ��ͽ��ͽ��;L�ͽ���    �L�ͽ��;L��    ���ͽ��ͽ��;L�ͽ��;L�ͽ��;L�;L�ͽ��ͽ���>L��    �L�ͽ���        ���ͽ��ͽ���    =���        ���ͽ���                            =���=���    =���=���=���    =���    =���=���    >L��=���    =���=���>L��        =���=���=���    =���=���=���=���=���=���=���=���=���    =���    =���=���    =���>L��=���=���>L��=���    =���        =���=���=���=���=���    >L��=���        =���=���=���    =���=���    =���    =���=���    =���=���=���    =���=���>L��    =���=���=���        =���=���=���=���>L��=���=���=���=���    =���    =���    =���=���=���=���=���    >L��=���>L��    =���        =���=���        >L��        =���=���>L��            =���=���    =���=���=���=���=���=���                =���    =���    >L��=���                >L��    =���=���>L��>L��>L��>L��>L��=���=���=���=���    ���ͽ���    =���        >L��=���    =���=���=���>L��=���=���    =���    =���=���=���=���>L��=���    =���    =���=��ͽ���=���=���=���    =���=���        >L��=���=���=���=��ͽ���=���=���    >L��=��ͽ���>L��>L��    =���=���    =���        =���=���=���        =���=���    =���=���=���=���=���=���=���=���=���=���        =���=���            =���=���    =���>L��=���    =���    =���=���>L��=���=���>L��=���=���    =���>L��=���>L��=���=���>L��>L��=���        =���        =���=���=���>L��>L��>���>���>���>���?��?��?��?L��?L��?fff?�  ?�  ?�  ?���?���?�ff?�ff?�33?�  ?���?���?ٙ�?ٙ�?�ff?�33@   @ff@ff@��@33@33@��@   @&ff@,��@333@9��@Fff@L��@L��@Y��@`  @fff@s33@y��@�  @�33@���@���@�  @�33@�ff@���@�  @�33@���@���@�  @�ff@���@���@�33@�ff@ə�@�  @�33@�ff@ٙ�@�  @�33@�ff@���@�  @�33@�ff@���A   A��A��AffA  A	��A��AffA  A33A��AffA��A33A��A   A!��A#33A&ffA(  A)��A,��A.ffA1��A333A4��A8  A9��A<��A>ffA@  AC33AD��AH  AI��AL��ANffAP  AS33AT��AX  AY��A\��A^ffAa��Ac33Ad��Ah  Ai��Al��AnffAq��As33AvffAx  Ay��A|��A�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���Ař�A�33A�  Aə�A�ffA�33A�  A͙�A�ffA�  A���Aљ�A�33A�  Aՙ�A�ffA�33A���Aٙ�A�ffA�  A���Dq� Dq�fDq�3Dq��Dq� Dq�fDq��Dq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrS3DrY�Dr` DrffDrs3Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr�fDr��Dr�3Ds  DsfDs�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt9�Dt@ DtL�DtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Du  @,��@333@9��@Fff@L��@L��@Y��@`  @fff@s33@y��@�  @�33@���@���@�  @�33@�ff@���@�  @�33@���@���@�  @�ff@���@���@�33@�ff@ə�@�  @�33@�ff@ٙ�@�  @�33@�ff@���@�  @�33@�ff@���A   A��A��AffA  A	��A��AffA  A33A��AffA��A33A��A   A!��A#33A&ffA(  A)��A,��A.ffA1��A333A4��A8  A9��A<��A>ffA@  AC33AD��AH  AI��AL��ANffAP  AS33AT��AX  AY��A\��A^ffAa��Ac33Ad��Ah  Ai��Al��AnffAq��As33AvffAx  Ay��A|��A�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���Ař�A�33A�  Aə�A�ffA�33A�  A͙�A�ffA�  A���Aљ�A�33A�  Aՙ�A�ffA�33A���Aٙ�A�ffA�  A���Dq� Dq�fDq�3Dq��Dq� Dq�fDq��Dq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr,�Dr33Dr@ DrFfDrL�DrS3DrY�Dr` DrffDrs3Dry�Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr��Dr� Dr�fDr�3DrٚDr� Dr�fDr��Dr�3Ds  DsfDs�Ds3Ds�Ds  Ds,�Ds33Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs��Ds��Dt  DtfDt�Dt3Dt  Dt&fDt,�Dt33Dt9�Dt@ DtL�DtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @<��@���@�  AffA"ffABffAbffA�33A�33A�33A�33A�33A�33A�  A�  B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC�C&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D		�D	��D
	�D
��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D	�D��D 	�D ��D!	�D!��D"	�D"��D#	�D#��D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(	�D(��D)	�D)��D*	�D*��D+	�D+��D,	�D,��D-	�D-��D.	�D.��D/	�D/��D0	�D0��D1	�D1��D2	�D2��D3	�D3��D4	�D4��D5	�D5��D6	�D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:��D;	�D;��D<	�D<��D=	�D=��D>	�D>��D?	�D?��D@	�D@��DA	�DA��DB	�DB��DC	�DC��DD	�DD��DE	�DE��DF	�DF��DG	�DG��DH	�DH��DI	�DI��DJ	�DJ��DK	�DK��DL	�DL��DM	�DM��DN	�DN��DO	�DO��DP	�DP��DQ	�DQ��DR	�DR��DS	�DS��DT	�DT��DU	�DU��DV	�DV��DW	�DW��DX	�DX��DY	�DY��DZ	�DZ��D[	�D[��D\	�D\��D]	�D]��D^	�D^��D_	�D_��D`	�D`��Da	�Da��Db	�Db��Dc	�Dc��Dd	�Dd��De	�De��Df	�Df��Dg	�Dg��Dh	�Dh��Di	�Di��Dj	�Dj��Dk	�Dk��Dl	�Dl��Dm	�Dm��Dn	�Dn��Do	�Do��Dp	�Dp��Dq	�Dq�4Dr	�Dr��Ds	�Ds��Dt	�Dt��Dy��D�qD�5�D��3D���D�RD�N�D���D��D�
D�X�D��>DǼ)D��D�;�D�pRD��D��RD�D)D�\D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�νL��=L��>���L�νL��=L�̽L�νL�νL�νL�νL��=L�̽L�νL��>��=L�̽L�νL��=L��=L�̽L�νL��=L��>���L��=L�̽L�νL�νL�νL��=L��>���L�νL��>��>���L��=L��>��=L�̽L�νL�νL��=L�̽L��=L��=L�̽L�νL��=L��=L�̽L�νL��=L��=L�̽L�νL�νL��>�  >�33�L�νL�νL�νL�νL��=L�̽L��>�  >�  �L�νL��>��>�33>�33�L��=L��>��=L�̽L��=L�̽L��>��=L�̽L�νL��=L�̽L�νL��=L��=L�̽L�νL�νL�νL�νL��=L�̽L�νL�νL��>��>�fg=L�̽L�νL��=L�̽L��=L��=L�̽L��=L��=L��=L�̽L��>��>���L��=L�̽L��=L�̽L��=L�̽L��>�  >���L��=L��=L�̽L�νL��=L��=L�̽L��=L�̽L��=L��>���L��=L�̽L�νL��=L��>��=L�̽L��=L��=L��=L�̽L�νL�νL��>��>�  >���L��=L��>��>�  >���L�νL��=L��>�  �L��=L��=L��>�  >���L��=L��=L��=L�̽L��=L��>�  >���L��=L�̽L��=L��=L��=L�̽L��=L��>���L��=L�̽L��>��=L��=L��=L�̽L��=L�̽L��=L�̽L�νL��=L��=L��>�33>���L��=L��>��>��=L��=L��=L��>��>�  >��>��=L��=L��>��>��>��>��>��>��>��>�  >�  >��>�  >�  >�  >��>�  >��>�  >�  >��>�33>�  >��>�  >�  >�33>��>��>�  >�  >�  >��>�  >�  >�  >�  >�  >�  >�  >�  >�  >��>�  >��>�  >�  >��>�  >�33>�  >�  >�33>�  >��>�  >��>��>�  >�  >�  >�  >�  >��>�33>�  >��>��>�  >�  >�  >��>�  >�  >��>�  >��>�  >�  >��>�  >�  >�  >��>�  >�  >�33>��>�  >�  >�  >��>��>�  >�  >�  >�  >�33>�  >�  >�  >�  >��>�  >��>�  >��>�  >�  >�  >�  >�  >��>�33>�  >�33>��>�  >��>��>�  >�  >��>��>�33>��>��>�  >�  >�33>��>��>��>�  >�  >��>�  >�  >�  >�  >�  >�  >��>��>��>��>�  >��>�  >��>�33>�  >��>��>��>��>�33>��>�  >�  >�33>�33>�33>�33>�33>�  >�  >�  >�  >��=L��=L��>��>�  >��>��>�33>�  >��>�  >�  >�  >�33>�  >�  >��>�  >��>�  >�  >�  >�  >�33>�  >��>�  >��>�  >�  =L��>�  >�  >�  >��>�  >�  >��>��>�33>�  >�  >�  >�  =L��>�  >�  >��>�33>�  =L��>�33>�33>��>�  >�  >��>�  >��>��>�  >�  >�  >��>��>�  >�  >��>�  >�  >�  >�  >�  >�  >�  >�  >�  >�  >��>��>�  >�  >��>��>��>�  >�  >��>�  >�33>�  >��>�  >��>�  >�  >�33>�  >�  >�33>�  >�  >��>�  >�33>�  >�33>�  >�  >�33>�33>�  >��>��>�  >��>��>�  >�  >�  >�33>�33>�fg?��?��?��?@  ?@  ?@  ?s33?s33?�ff?�33?�33?�33?�  ?���?���?���?�ff?�33?�  ?�  ?���?���?���@33@	��@  @  @fg@��@��@#34@)��@0  @6fg@<��@C34@P  @Vfg@Vfg@c34@i��@p  @|��@���@���@�  @�fg@���@���@�  @�33@���@���@�  @�fg@���@���@�33@�fg@���@�  @�33@�fg@���@�  @�33@�fg@���@�  @�33@�@���@�  @�33A ��AffA  A33A��A
ffA  A33A��AffA��A33A��A  A��A33A"ffA$  A%��A(��A*ffA,  A/33A0��A4  A5��A733A:ffA<  A?33A@��ABffAE��AG33AJffAL  AO33AP��ARffAU��AW33AZffA\  A_33A`��Ad  Ae��Ag33AjffAl  Ao33Ap��At  Au��Ax��AzffA|  A33A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���AÙ�A�ffA�  A���A�ffA�33A���A˙�A�ffA�33A���Aϙ�A�33A�  A���A�ffA�33A���Aי�A�ffA�  A���Aۙ�A�33A�  Dq��Dq� Dq��Dq�4Dq��Dq� Dq�gDq��Dq�4DqɚDq� Dq��Dq�4Dq�Dq� Dq�gDq��Dr4Dr	�DrgDr�Dr#4Dr)�Dr0 Dr6gDr<�DrI�DrP DrVgDr\�Drc4Dri�Drp Dr|�Dr�4Dr��Dr� Dr�gDr��Dr��Dr� Dr�gDr��Dr�4DrɚDr� Dr��Dr�4Dr�Dr� Dr�gDr��Ds	�Ds DsgDs�Ds#4Ds)�Ds6gDs<�DsC4DsI�DsP DsVgDsc4Dsi�Dsp DsvgDs|�Ds��Ds� Ds�gDs��Ds�4Ds��Ds�gDs��Ds�4DsɚDs� Ds��Ds�4Ds�Ds� Ds�gDt4Dt	�Dt DtgDt�Dt)�Dt0 Dt6gDt<�DtC4DtI�DtVgDt\�Dtc4Dti�Dtp Dt|�Dt�4Dt��Dt� Dt��Dt�4Dt��Dt� Dt�gDt�4DtɚDt� Dt�gDt��Dt�Dt� Dt�gDt��Du	�@6fg@<��@C34@P  @Vfg@Vfg@c34@i��@p  @|��@���@���@�  @�fg@���@���@�  @�33@���@���@�  @�fg@���@���@�33@�fg@���@�  @�33@�fg@���@�  @�33@�fg@���@�  @�33@�@���@�  @�33A ��AffA  A33A��A
ffA  A33A��AffA��A33A��A  A��A33A"ffA$  A%��A(��A*ffA,  A/33A0��A4  A5��A733A:ffA<  A?33A@��ABffAE��AG33AJffAL  AO33AP��ARffAU��AW33AZffA\  A_33A`��Ad  Ae��Ag33AjffAl  Ao33Ap��At  Au��Ax��AzffA|  A33A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���AÙ�A�ffA�  A���A�ffA�33A���A˙�A�ffA�33A���Aϙ�A�33A�  A���A�ffA�33A���Aי�A�ffA�  A���Aۙ�A�33A�  Dq��Dq� Dq��Dq�4Dq��Dq� Dq�gDq��Dq�4DqɚDq� Dq��Dq�4Dq�Dq� Dq�gDq��Dr4Dr	�DrgDr�Dr#4Dr)�Dr0 Dr6gDr<�DrI�DrP DrVgDr\�Drc4Dri�Drp Dr|�Dr�4Dr��Dr� Dr�gDr��Dr��Dr� Dr�gDr��Dr�4DrɚDr� Dr��Dr�4Dr�Dr� Dr�gDr��Ds	�Ds DsgDs�Ds#4Ds)�Ds6gDs<�DsC4DsI�DsP DsVgDsc4Dsi�Dsp DsvgDs|�Ds��Ds� Ds�gDs��Ds�4Ds��Ds�gDs��Ds�4DsɚDs� Ds��Ds�4Ds�Ds� Ds�gDt4Dt	�Dt DtgDt�Dt)�Dt0 Dt6gDt<�DtC4DtI�DtVgDt\�Dtc4Dti�Dtp Dt|�Dt�4Dt��Dt� Dt��Dt�4Dt��Dt� Dt�gDt�4DtɚDt� Dt�gDt��Dt�Dt� Dt�gDt��Du	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AžwAžwAžwAżjAżjAź^AŸRAŶFAŬAŕ�AŅA�~�A�~�A�|�A�|�A�z�A�v�A�n�A�hsA�bNA�^5A�"�A7A���A��HA�ĜA�v�A�oA��!A��A��A��A�t�A�A�?}A�5?A�-A� �A��A���A���A�bNA���A���A�7LA�A�%A��HA�E�A��FA��A�bNA�-A�VA��+A��;A�&�A���A�jA��A�VA�&�A�"�A�JA��^A��#A�M�A��A�9XA���A�x�A� �A��TA�z�A���A���A�A�ZA�JA���A���A�;dA��9A�bA���A���A�%A��A��A�G�A��A���A�VA�/A��A��A���A�{A�?}A���A��A�JA�"�A��PA��#A�5?A�G�A�+A��A���A�bA��A���A�"�A��^A���A�-A��mA��!A��DA�M�A��A�/A�ƨA�XA�E�A�7LA�A~  A}��A}"�A|ZA{�Az��Ay��Av�As�ApĜAk�Ae�Ab�DA]\)A[K�AX�ASG�AR9XARAQ/AN�`AMG�AL��AK?}AKAJ��AHE�AG
=AEXAD1'AC7LABbNAAG�A@1A>ĜA=�;A<��A;hsA:n�A9��A9/A8�A6�A4z�A3��A2-A0�!A0 �A/��A/�-A/��A/O�A.bNA-�A,�\A,jA+�mA+S�A* �A)t�A(~�A'��A'�-A'�A'��A'O�A&9XA%p�A$  A#VA"A�A!+A�AG�A+A
=A��A\)A/A�A-Av�A��A��AĜA/A�A�yA-A
-A	��A	�A	
=AQ�A�A �AA�A��A�hA�7A`BA�\A�A��AhsAA =q@���@�S�@��+@��
@�K�@��+@�&�@��m@�z�@��@�/@@@�p�@��`@�9@�|�@��H@�@���@���@�K�@旍@�E�@��@��;@�-@�?}@��@�b@�x�@ܬ@��m@���@׶F@Ցh@ҸR@щ7@�7L@υ@�$�@�`B@�@�7L@�Ĝ@�ƨ@��#@�%@�Z@�ƨ@Å@�S�@�"�@�~�@��@�Q�@��@���@���@���@�\)@�;d@�v�@��@��h@�A�@�5?@��h@���@��\@�M�@�J@���@�X@�V@��D@���@�t�@�K�@���@��+@�=q@���@�7L@��@�z�@�  @���@�v�@�@�Ĝ@�I�@��
@�;d@���@��!@�$�@�r�@���@�t�@�"�@��\@�~�@�^5@���@��T@��T@���@��@���@�I�@�9X@��m@�dZ@���@�-@��#@���@���@�G�@��@�A�@� �@�  @��
@��@�C�@��y@���@��@��T@���@�7L@�&�@�/@�/@�/@�7L@�/@�V@���@���@�\)@�+@���@�5?@��@��@���@��^@��-@�/@���@�z�@�bN@�bN@�j@�bN@� �@���@�S�@�o@�ȴ@�ȴ@���@�^5@�{@�x�@���@���@���@���@��u@�j@�Z@�I�@�A�@�ƨ@�K�@��@�ȴ@���@�~�@��@�=q@�M�@��@���@��@�&�@��j@�Z@�  @���@�|�@�"�@�ȴ@�v�@�=q@��#@�x�@�O�@�&�@�&�@���@��@�I�@�  @��;@���@�l�@�"�@�C�@�K�@�\)@�l�@�|�@��@�  @�(�@�I�@�I�@���@�S�@���@�E�@�-@�M�@��@�x�@�X@�hs@�?}@��@��/@��@��`@�Ĝ@��u@�9X@�ƨ@��P@�K�@�o@��+@�@��h@�/@��j@�r�@��m@���@�l�@�dZ@�j@|q@u��@j�M@fYK@_@X��@Q`B@H~@Aa�@:��@5p�@.Ov@)+@%�n@�]@33@6�@��@w2@	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A���A�~�A�?}A�dZA�G�A���A�C�A��FA�1'A�|�A�9XA���A�x�A¾wA�+A�JA��\A�"�A�ZA���A�1A¥�AŃA�z�A���A��-A��+A��A���A�-A�bA��/A�p�A�JAŋDA�?}A��RA�p�A�z�A���A�z�A���A�(�A��TA���AŇ+A�dZA���A�r�A�r�A�I�A�E�A�&�A� �A�1'A�l�A�A�A�Q�AœuA�=qA��;A�E�A��PAĶFA�VA�33A�bNAŏ\A��A��/AAŝ�AœuA���A��A�|�A�&�A�-A�JA�VA�A�dZA���A�t�A�%A���A��A��DA��!A�?}A�\)A��jA��HA�A�
=A��jA���A���A�r�Aŝ�A�~�A��TA���AĴ9A��A���AĲ-A�  A�1'A���A��wA��A�?}Aş�A��TA�|�A��/A�hsA�$�A�5?A���Aŏ\Aŝ�A�z�A�M�A�VA���A�ffA�ĜAŅA���A���A���A���A��mAģ�A�"�A�ȴA�?}A�oAş�Aş�A�VA�K�A�S�AŁA�A�VA��uA��;Aũ�Aš�A��-A�t�A�9XAş�AōPA�33A�&�A��A�AŅA�v�A�/Aŝ�Aũ�A� �AđhA��`A�5?A�1'A��Aŏ\Ař�A�%AüjA� �A���A�1'A�VA�
=A�r�Aś�A���A�1A��+A²-Aá�A���A���A�p�A�|�A�9XA���A�t�A�`BA��;A��A�z�Aŗ�A��;A��HA�E�Aś�Aŉ7A�^5A�\)A��Aş�Aŝ�Aţ�Aŉ7A��TAř�Aŏ\AŋDA�33Ař�AőhAŕ�AœuAŏ\Aŕ�Aŕ�Aŗ�Aŗ�AőhAŋDAœuAş�Aš�Ař�Aś�Aŝ�Aŉ7Aŉ7Aŗ�AőhAōPAŉ7AőhAŏ\Aŏ\AœuAŗ�Aŏ\Aŉ7Aŕ�Aŏ\Aŗ�AőhAœuAœuAőhAōPAōPAōPAŋDAŋDAŇ+AœuAōPAŃAōPAōPAŁA�|�A�z�AŃAŉ7A�|�AŃAŏ\Aŏ\Aŕ�Aŗ�AœuAŕ�Aŏ\Aŏ\AœuAœuAŏ\AŅAŋDAŉ7Aŉ7Aŉ7AōPAŉ7A�z�Aŉ7AŋDAŏ\AŇ+AŅAœuAōPAōPAŉ7AŇ+AŇ+AŋDAŁAŉ7AŋDAōPAŅAŃAŇ+Aŏ\AŋDAŋDAŋDAŉ7Aŉ7Aŏ\AœuAŗ�AőhAŋDAŏ\Aŗ�AōPAŅAŃAŏ\Ař�Ař�A�dZAś�Ař�Aţ�Aţ�Aŏ\AōPAţ�Aţ�Aţ�AŬAŬAţ�Aŗ�Aš�Aš�Aŝ�Aŕ�AöFAť�Aš�Aŧ�Aţ�Aš�Aţ�Aŝ�Aś�Aŕ�Aş�Aś�Aŝ�Aş�Aś�Aŝ�Aŕ�Ař�Aŝ�Ař�Aś�Aš�Aŝ�Aš�Aś�Aŝ�Aŗ�Aŕ�AœuAř�AőhAőhAŕ�Ař�A�33AōPAōPAś�Ař�AōPAœuAś�Aŗ�Aŗ�AœuAŇ+Aţ�Aŝ�AŋDAş�Aş�Aŧ�AŬAŧ�Aũ�AōPAŬAŮAť�Aš�Aũ�Aũ�AŮAũ�Aŗ�AŮAũ�Aŧ�AŮAŰ!Aũ�A�"�AŮAŮA�z�AŬAŮAť�A�^5AŬAŧ�AŬAŬAť�Aŧ�AŬAũ�AŬAũ�Aũ�AŬAũ�AŬAř�AŬAť�Aŝ�A�
=Aũ�Aŧ�Aş�Aŧ�Aŧ�Aŝ�Aŧ�AŬAũ�Ať�Aŧ�Aš�Aŝ�Aş�Aŕ�Aś�Aũ�Aŗ�AŇ+Aŕ�Aś�Aš�Aŏ\Ať�AŰ!Aţ�Aŕ�Aš�Ař�AŇ+Aś�Aŗ�Ař�Aŗ�AœuAş�Aş�Aŝ�Aś�Ať�Aţ�Aš�Aŗ�Aŗ�Aś�AŲ-AŰ!Aŝ�Aš�Aš�Aţ�Aŧ�Aũ�Ať�AŲ-AŲ-Aũ�AŮAŸRAź^AŰ!AŸRAź^AŸRAŶFAŶFAŶFAŶFAŴ9AŶFAŶFAŴ9AŶFAŲ-AŶFAź^Aũ�Ať�Aš�Aš�Aŝ�Ať�Aţ�AżjAžwAžwAžwAžwA���AžwAžwAžwAžwAżjAżjAżjAżjAź^AżjAžwAżjAżjAżjAżjAžwAžwAžwAżjA���AžwAžwAžwAžwAżjAžwAžwAžwAżjAżjAżjAź^AżjAżjAžwAžwAžwAžwAžwAžwAžwAžwAżjAżjAžwAżjAź^AžwAżjAź^Aź^AżjAżjAżjAżjAżjAź^AżjAżjAź^Aź^AżjAżjAżjAžwAżjAżjAź^AżjAżjAź^Aź^AżjAź^AżjAź^Aź^Aź^Aź^AżjAŸRAŸRAź^AŶFAź^AżjAź^AżjAżjAżjAź^AŸRAŸRAŶFAź^AŶFAŶFAŸRAŸRAŶFAŸRAź^Aź^Aź^AŶFAŲ-AŮAŮAŰ!AŲ-AŰ!AŲ-AŰ!AŰ!AŲ-AŲ-AŴ9AŮAũ�Aŧ�Aŧ�Aũ�Aţ�Aş�Aş�Aŝ�Aŝ�Aŝ�Ař�AœuAőhAŏ\AōPAōPAŋDAŉ7Aŉ7Aŉ7AŇ+AŅAŅAŃAŅAŃAŃAŁAŁAŃAŃA�~�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�|�A�~�A�|�@�/@�/@�/@�&�@��@���@���@��`@��/@���@���@���@���@���@���@���@���@���@��9@��@��9@��9@��9@��@���@���@���@��D@��D@��D@��@�z�@�z�@�r�@�r�@��D@��u@��@�bN@�I�@�I�@�A�@�I�@�I�@�A�@� �@�b@��@��m@��;@��
@���@��
@���@�ƨ@�ƨ@�ƨ@��w@��w@��F@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��P@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�S�@�S�@�K�AżjAžwAžwAżjAżjAżjAžwAžwAžwAżjAžwAżjAžwAżjAżjAžwAžwAžwAžwAžwAžwAžwAżjAžwAžwAżjAżjAżjAżjAżjAżjAżjAżjAžwA���AžwAžwAżjAžwAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAź^AżjAżjAżjAżjAżjAź^AżjAżjAżjAżjAżjAź^Aź^AżjAżjAżjAź^Aź^AżjAź^AżjAź^Aź^AżjAź^Aź^Aź^Aź^Aź^AżjAżjAżjAź^AŸRAżjAŸRAŶFAŴ9AŶFAŶFAŸRAŶFAŸRAŸRAŸRAź^AŸRAź^AŸRAŴ9AŮAŰ!AŰ!AŲ-AŲ-AŮAŰ!AŲ-AŲ-AŲ-AŲ-AŮAŧ�Aŧ�Aŧ�Aŧ�Aş�Aş�Aŝ�Aŝ�Aŝ�Aŝ�Aś�Aŕ�AőhAŏ\AōPAōPAŋDAŇ+Aŉ7Aŉ7AŇ+AŅAŅAŅAŅAŃAŁAŁAŁAŃAŃA�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�~�A�~�A�|�A�~�A�|�A�~�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�|�A�|�@�/@�/@�/@�&�@�&�@�V@���@��@���@���@���@���@���@���@���@���@���@���@�Ĝ@��@��@��@��@��@���@���@���@��u@��D@��u@��D@��@�z�@�r�@�r�@�r�@��u@��D@�j@�Q�@�I�@�A�@�I�@�I�@�I�@�9X@��@�1@��@��m@��;@��
@���@���@���@�ƨ@�ƨ@�ƨ@��w@��F@��@��@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�t�@�t�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�\)@�\)@�S�@�S�@�K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   AžwAžwAžwAżjAżjAź^AŸRAŶFAŬAŕ�AŅA�~�A�~�A�|�A�|�A�z�A�v�A�n�A�hsA�bNA�^5A�"�A7A���A��HA�ĜA�v�A�oA��!A��A��A��A�t�A�A�?}A�5?A�-A� �A��A���A���A�bNA���A���A�7LA�A�%A��HA�E�A��FA��A�bNA�-A�VA��+A��;A�&�A���A�jA��A�VA�&�A�"�A�JA��^A��#A�M�A��A�9XA���A�x�A� �A��TA�z�A���A���A�A�ZA�JA���A���A�;dA��9A�bA���A���A�%A��A��A�G�A��A���A�VA�/A��A��A���A�{A�?}A���A��A�JA�"�A��PA��#A�5?A�G�A�+A��A���A�bA��A���A�"�A��^A���A�-A��mA��!A��DA�M�A��A�/A�ƨA�XA�E�A�7LA�A~  A}��A}"�A|ZA{�Az��Ay��Av�As�ApĜAk�Ae�Ab�DA]\)A[K�AX�ASG�AR9XARAQ/AN�`AMG�AL��AK?}AKAJ��AHE�AG
=AEXAD1'AC7LABbNAAG�A@1A>ĜA=�;A<��A;hsA:n�A9��A9/A8�A6�A4z�A3��A2-A0�!A0 �A/��A/�-A/��A/O�A.bNA-�A,�\A,jA+�mA+S�A* �A)t�A(~�A'��A'�-A'�A'��A'O�A&9XA%p�A$  A#VA"A�A!+A�AG�A+A
=A��A\)A/A�A-Av�A��A��AĜA/A�A�yA-A
-A	��A	�A	
=AQ�A�A �AA�A��A�hA�7A`BA�\A�A��AhsAA =q@���@�S�@��+@��
@�K�@��+@�&�@��m@�z�@��@�/@@@�p�@��`@�9@�|�@��H@�@���@���@�K�@旍@�E�@��@��;@�-@�?}@��@�b@�x�@ܬ@��m@���@׶F@Ցh@ҸR@щ7@�7L@υ@�$�@�`B@�@�7L@�Ĝ@�ƨ@��#@�%@�Z@�ƨ@Å@�S�@�"�@�~�@��@�Q�@��@���@���@���@�\)@�;d@�v�@��@��h@�A�@�5?@��h@���@��\@�M�@�J@���@�X@�V@��D@���@�t�@�K�@���@��+@�=q@���@�7L@��@�z�@�  @���@�v�@�@�Ĝ@�I�@��
@�;d@���@��!@�$�@�r�@���@�t�@�"�@��\@�~�@�^5@���@��T@��T@���@��@���@�I�@�9X@��m@�dZ@���@�-@��#@���@���@�G�@��@�A�@� �@�  @��
@��@�C�@��y@���@��@��T@���@�7L@�&�@�/@�/@�/@�7L@�/@�V@���@���@�\)@�+@���@�5?@��@��@���@��^@��-@�/@���@�z�@�bN@�bN@�j@�bN@� �@���@�S�@�o@�ȴ@�ȴ@���@�^5@�{@�x�@���@���@���@���@��u@�j@�Z@�I�@�A�@�ƨ@�K�@��@�ȴ@���@�~�@��@�=q@�M�@��@���@��@�&�@��j@�Z@�  @���@�|�@�"�@�ȴ@�v�@�=q@��#@�x�@�O�@�&�@�&�@���@��@�I�@�  @��;@���@�l�@�"�@�C�@�K�@�\)@�l�@�|�@��@�  @�(�@�I�@�I�@���@�S�@���@�E�@�-@�M�@��@�x�@�X@�hs@�?}@��@��/@��@��`@�Ĝ@��u@�9X@�ƨ@��P@�K�@�o@��+@�@��h@�/@��j@�r�@��m@���@�l�G�O�@�j@|q@u��@j�M@fYK@_@X��@Q`B@H~@Aa�@:��@5p�@.Ov@)+@%�n@�]@33@6�@��@w2@	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A���A�~�A�?}A�dZA�G�A���A�C�A��FA�1'A�|�A�9XA���A�x�A¾wA�+A�JA��\A�"�A�ZA���A�1A¥�AŃA�z�A���A��-A��+A��A���A�-A�bA��/A�p�A�JAŋDA�?}A��RA�p�A�z�A���A�z�A���A�(�A��TA���AŇ+A�dZA���A�r�A�r�A�I�A�E�A�&�A� �A�1'A�l�A�A�A�Q�AœuA�=qA��;A�E�A��PAĶFA�VA�33A�bNAŏ\A��A��/AAŝ�AœuA���A��A�|�A�&�A�-A�JA�VA�A�dZA���A�t�A�%A���A��A��DA��!A�?}A�\)A��jA��HA�A�
=A��jA���A���A�r�Aŝ�A�~�A��TA���AĴ9A��A���AĲ-A�  A�1'A���A��wA��A�?}Aş�A��TA�|�A��/A�hsA�$�A�5?A���Aŏ\Aŝ�A�z�A�M�A�VA���A�ffA�ĜAŅA���A���A���A���A��mAģ�A�"�A�ȴA�?}A�oAş�Aş�A�VA�K�A�S�AŁA�A�VA��uA��;Aũ�Aš�A��-A�t�A�9XAş�AōPA�33A�&�A��A�AŅA�v�A�/Aŝ�Aũ�A� �AđhA��`A�5?A�1'A��Aŏ\Ař�A�%AüjA� �A���A�1'A�VA�
=A�r�Aś�A���A�1A��+A²-Aá�A���A���A�p�A�|�A�9XA���A�t�A�`BA��;A��A�z�Aŗ�A��;A��HA�E�Aś�Aŉ7A�^5A�\)A��Aş�Aŝ�Aţ�Aŉ7A��TAř�Aŏ\AŋDA�33Ař�AőhAŕ�AœuAŏ\Aŕ�Aŕ�Aŗ�Aŗ�AőhAŋDAœuAş�Aš�Ař�Aś�Aŝ�Aŉ7Aŉ7Aŗ�AőhAōPAŉ7AőhAŏ\Aŏ\AœuAŗ�Aŏ\Aŉ7Aŕ�Aŏ\Aŗ�AőhAœuAœuAőhAōPAōPAōPAŋDAŋDAŇ+AœuAōPAŃAōPAōPAŁA�|�A�z�AŃAŉ7A�|�AŃAŏ\Aŏ\Aŕ�Aŗ�AœuAŕ�Aŏ\Aŏ\AœuAœuAŏ\AŅAŋDAŉ7Aŉ7Aŉ7AōPAŉ7A�z�Aŉ7AŋDAŏ\AŇ+AŅAœuAōPAōPAŉ7AŇ+AŇ+AŋDAŁAŉ7AŋDAōPAŅAŃAŇ+Aŏ\AŋDAŋDAŋDAŉ7Aŉ7Aŏ\AœuAŗ�AőhAŋDAŏ\Aŗ�AōPAŅAŃAŏ\Ař�Ař�A�dZAś�Ař�Aţ�Aţ�Aŏ\AōPAţ�Aţ�Aţ�AŬAŬAţ�Aŗ�Aš�Aš�Aŝ�Aŕ�AöFAť�Aš�Aŧ�Aţ�Aš�Aţ�Aŝ�Aś�Aŕ�Aş�Aś�Aŝ�Aş�Aś�Aŝ�Aŕ�Ař�Aŝ�Ař�Aś�Aš�Aŝ�Aš�Aś�Aŝ�Aŗ�Aŕ�AœuAř�AőhAőhAŕ�Ař�A�33AōPAōPAś�Ař�AōPAœuAś�Aŗ�Aŗ�AœuAŇ+Aţ�Aŝ�AŋDAş�Aş�Aŧ�AŬAŧ�Aũ�AōPAŬAŮAť�Aš�Aũ�Aũ�AŮAũ�Aŗ�AŮAũ�Aŧ�AŮAŰ!Aũ�A�"�AŮAŮA�z�AŬAŮAť�A�^5AŬAŧ�AŬAŬAť�Aŧ�AŬAũ�AŬAũ�Aũ�AŬAũ�AŬAř�AŬAť�Aŝ�A�
=Aũ�Aŧ�Aş�Aŧ�Aŧ�Aŝ�Aŧ�AŬAũ�Ať�Aŧ�Aš�Aŝ�Aş�Aŕ�Aś�Aũ�Aŗ�AŇ+Aŕ�Aś�Aš�Aŏ\Ať�AŰ!Aţ�Aŕ�Aš�Ař�AŇ+Aś�Aŗ�Ař�Aŗ�AœuAş�Aş�Aŝ�Aś�Ať�Aţ�Aš�Aŗ�Aŗ�Aś�AŲ-AŰ!Aŝ�Aš�Aš�Aţ�Aŧ�Aũ�Ať�AŲ-AŲ-Aũ�AŮAŸRAź^AŰ!AŸRAź^AŸRAŶFAŶFAŶFAŶFAŴ9AŶFAŶFAŴ9AŶFAŲ-AŶFAź^Aũ�Ať�Aš�Aš�Aŝ�Ať�Aţ�AżjAžwAžwAžwAžwA���AžwAžwAžwAżjAžwAžwAżjAżjAżjAžwAžwAžwAżjAžwAżjAžwAżjAżjAžwAžwAžwAžwAžwAžwAžwAżjAžwAžwAżjAżjAżjAżjAżjAżjAżjAżjAžwA���AžwAžwAżjAžwAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAź^AżjAżjAżjAżjAżjAź^AżjAżjAżjAżjAżjAź^Aź^AżjAżjAżjAź^Aź^AżjAź^AżjAź^Aź^AżjAź^Aź^Aź^Aź^Aź^AżjAżjAżjAź^AŸRAżjAŸRAŶFAŴ9AŶFAŶFAŸRAŶFAŸRAŸRAŸRAź^AŸRAź^AŸRAŴ9AŮAŰ!AŰ!AŲ-AŲ-AŮAŰ!AŲ-AŲ-AŲ-AŲ-AŮAŧ�Aŧ�Aŧ�Aŧ�Aş�Aş�Aŝ�Aŝ�Aŝ�Aŝ�Aś�Aŕ�AőhAŏ\AōPAōPAŋDAŇ+Aŉ7Aŉ7AŇ+AŅAŅAŅAŅAŃAŁAŁAŁAŃAŃA�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�~�A�~�A�|�A�~�A�|�A�~�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�|�A�|�@�/@�/@�/@�&�@�&�@�V@���@��@���@���@���@���@���@���@���@���@���@���@�Ĝ@��@��@��@��@��@���@���@���@��u@��D@��u@��D@��@�z�@�r�@�r�@�r�@��u@��D@�j@�Q�@�I�@�A�@�I�@�I�@�I�@�9X@��@�1@��@��m@��;@��
@���@���@���@�ƨ@�ƨ@�ƨ@��w@��F@��@��@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�t�@�t�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�\)@�\)@�S�@�S�@�K�AżjAžwAžwAżjAżjAżjAžwAžwAžwAżjAžwAżjAžwAżjAżjAžwAžwAžwAžwAžwAžwAžwAżjAžwAžwAżjAżjAżjAżjAżjAżjAżjAżjAžwA���AžwAžwAżjAžwAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAżjAź^AżjAżjAżjAżjAżjAź^AżjAżjAżjAżjAżjAź^Aź^AżjAżjAżjAź^Aź^AżjAź^AżjAź^Aź^AżjAź^Aź^Aź^Aź^Aź^AżjAżjAżjAź^AŸRAżjAŸRAŶFAŴ9AŶFAŶFAŸRAŶFAŸRAŸRAŸRAź^AŸRAź^AŸRAŴ9AŮAŰ!AŰ!AŲ-AŲ-AŮAŰ!AŲ-AŲ-AŲ-AŲ-AŮAŧ�Aŧ�Aŧ�Aŧ�Aş�Aş�Aŝ�Aŝ�Aŝ�Aŝ�Aś�Aŕ�AőhAŏ\AōPAōPAŋDAŇ+Aŉ7Aŉ7AŇ+AŅAŅAŅAŅAŃAŁAŁAŁAŃAŃA�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�~�A�~�A�|�A�~�A�|�A�~�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�|�A�|�@�/@�/@�/@�&�@�&�@�V@���@��@���@���@���@���@���@���@���@���@���@���@�Ĝ@��@��@��@��@��@���@���@���@��u@��D@��u@��D@��@�z�@�r�@�r�@�r�@��u@��D@�j@�Q�@�I�@�A�@�I�@�I�@�I�@�9X@��@�1@��@��m@��;@��
@���@���@���@�ƨ@�ƨ@�ƨ@��w@��F@��@��@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�t�@�t�@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�dZ@�\)@�\)@�\)@�\)@�\)@�S�@�S�@�K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�j�>Q�+@��?���>4�y@���@��=�+�>�O"=�>+A@_m@ �=�9X?2�@��$=���=�|>/�'@���?g8=��b>]IR@��9>�NQ>7�>0Ex=D��=��?=�~�=�9�@�kf@ ��=쿱>�[@���@��j>�/@�7v@��4=��2>��?���=�9X>J��>5��@��|?�=Τ�>��@���@Z�=�p�=��@*n�=��A=���=�W >���@���@���>I"S=�k>�;@X��>H>B>�#�?�"h@��E@��e=ܦ�>I�@��J@���@/̸>b�@��/@��>8B1>g� =۫">G�@���=�@>��@���@�J8=���=ż?S�=��=��6?ԜN=��>��@VR�>�sC=�Ҟ=��p@~g@��[@��=�<>/�Q@\T7=��n>j�@L��?:�E=�	�@�Cl>��?�9C>bo�@���?W��=��j=��>i��?i�>�">%#�@j�y@��%?G�>@sX@��}=�4�=��O>d�C@��4>'�7>8:T@CV�>t ?�i�@���>M��@fQD>:�?皿@��@�ʂ>�@P�>�Կ@��=���=ƃ<>,�?4�@��r@��*?��s>Lnn@���@���@���=�3�=��D?��?{ȟ@�Ǥ>.N'@Q�-@�Ǥ@��q?~��@�X?gb@��Z?w�G>0݃@���@��}@�1<>��	@�\�>�p@�!�?��@���> �'@���?���@x��>��y>��@;��@��>@���?	��>;�@���@1��@-U�>c��>C�@+k@���@��@���> $�@^k�@��>@�ʂ@���@���>�4�@��}@��.@��)@���@��@�ȴ@�Ɠ@��.?~I�@��m@��q@��@���@�Ɠ@���@���@��\@�ȴ@���@��@��@�̣@�˒@�ȴ@��@��.@��q@��`@��P@��?@��@��.@��?@��P@��@�ȟ@��@��@��q@�Ǥ@��@��@�ȴ@���@��`@��`@�Ɠ@���@���@��@�Ɠ@��P@���@�Б@�ł@�ł@��r@���@���@��@��.@�ł@��@���@�Ǥ@�ȟ@��?@�Ǥ@��`@�ȟ@�Ǥ@���@�ł@�Ɠ@���@���@��?@���@��.@��?@���@��@��@���@��?@�Ɠ@��.@���@�Ǥ@��P@�Ɠ@��.@���@��.@��r@��v@��@�Ɠ@��P@��;@�Ɠ@���@�ȴ@��K@��?@�ł@�Ɠ@�Ǥ@�Ɠ@���@��.@��@��@��@�Ɠ@�ł@���@��.@���@��K@��>@��:@���@���@�˒@��q@��`@��@�Κ@��@���@��l@���@�ʂ@�v@��l@��6@���@��@��@�̣@�ͳ@��l@�˒@��>@��:@�ʂ@��q@��P@�̣@��%@�˒@���@���@���@��q@���@�˒@��)@���@���@��.@�ʂ@���@��.@��`@��`@��`@��@��@�ȴ@��@��q@��\@��`@�ł@���@��\@��@��`@��q@�͟@��.@��.@��e@���@��@��P@��l@��)@��}@��}@���@��9@��p@���@��%@��@���@���@���@��@��@���@��1@���@��J@��k@�ӄ@��@��?@���@���@�Ҟ@�Ӯ@�Կ@���@�Z�@�Ӯ@���@��[@���@��[@��J@���@�Ҟ@���@��J@�э@�Ҟ@��J@��l@��c@���@���@��l@��x@��J@��J@���@�л@���@�э@�Ҟ@���@�э@���@���@��1@�ͳ@��p@���@�ͳ@�ͳ@��.@�ȴ@�˒@��@��@��W@���@��@��[@��@��p@��@�ʂ@�ϫ@��@���@��:@��@���@�̣@���@��p@�ͳ@��K@��[@��:@�Б@��9@���@��J@���@���@�� @�Ҟ@�ͳ@��l@�Κ@�Ҟ@��|@��@��o@�خ@��E@��4@��^@��4@���@��4@�֡@���@�֌@�֌@�֡@���@�ם@���@���@���@���@��9@��9@���@��9@��@��@��(@�پ@���@���@��|@��@���@��@��{@��{@��{@���@���@��#@��#@��#@���@���@��H@���@���@�ی@���@�ی@�ۡ@�ی@���@���@���@��H@�ܜ@�ܜ@�ܱ@���@�ܜ@��]@��H@��H@��H@���@��@��Y@��Y@��Y@���@���@��Y@��Y@���@��@��@��@��@��Y@��Y@���@��Y@��Y@���@���@��@��@��@��@��+@��@���@���@��&@���@��z@���@��L@���@��7@��@��@���@��7@��@��@��@��@��7@�ߏ@��@��&@���@���@���@��;@���@��@��@��@��@��	@�ߏ@���@��@��@���@��~@��@���@���@��@��@��@��@��m@��)@��%@���@���@��h@��)@��}@���@��}@��}@��h@��}@��}@���@��@��y@��:@���@��%@��}@���@��)@���@��m@��\@��K@��@��P@��P@���@��?@���@���@��@��.@���@���@���@���@��@��r@��@��r@��r@���@��v@���@���@���@���@��v@��@��@��@��@��@��@��@��@��@���@��@���@��.@���@���@��.@��@��.@��C@��.@��@��@��@��@��@�� @��@�� @��@Q�@Q@Q]@Qf@Q�@Q|@Q�@Q_@Q�@Q9@Qc@Q@Q@Q
�@Q
g@Q
@Q�@Q!@Q�@Q%@Q%@QS@Q�@Q�@Q6@Q6@Q �@Q �@Q ?@P�C@P�H@P��@P��@P��@P��@P��@P�P@P�@P��@P�@P�@P�I@P��@P��@P�@P��@P�}@P�@P�@P�@P��@P�C@P�@P�H@P�@P�@Pߤ@Pީ@P�@Pܱ@P�b@Pں@P�@Pں@P�f@Pں@P�f@P�f@P�f@P�@P�@P�@P�j@P�o@P��@P��@P�$@P�$@P�$@P�$@P�$@P�$@P�$@P�w@P�$@P�$@P�(@P�(@P�(@P�|@P�(@P�|@P�|@P�|@P�|@P��@P�(@P�|@P�|@P��@P��@P�|@P�|@P�$@P��@P�$@P�$@P��@P��@P��@P�|@P��@P�|@P�R@P�(@P��@PԀ@P�@Pӄ@Pҳ@��@���@���@��#@��@��@��@���@��@��4@���@���@���@���@���@���@��s@��4@��^@��4@��s@���@���@���@��o@��o@���@���@���@���@���@���@���@���@��@@��@��@@��@��s@��+@��@��Z@���@��o@���@���@��@@���@���@���@��@���@��@��U@��U@��j@��j@��'@��'@���@��'@��<@��b@��#@��8@���@��f@��]@���@��#@���@��@���@��3@��]@���@���@��f@���@��@���@��'@���@���@��3@���@���@���@���@��b@��@���@���@���@��/@���@���@��e@��r@���@���@���@� T@� �@�@� �@� �@� �@�;@�@�y@� �@� �@���@���@��@�@��@��@�&@�e@�;@�&@�&@�&@� �@��m@��a@���@���@��P@��P@��n@���@��n@���@���@���@��Y@��H@���@���@���@��	@���@���@���@���@���@���@���@���@���@��@��#@��@��8@��8@��@��M@��w@���@���@���@���@���@���@���@��	@���@��@��@���@��H@��3@��H@��H@���@��@��@��H@Q*�@Q+@Q*�@Q*@Q*@Q'=@Q!�@Q!@Q�@Q�@Q�@Q�@Q@Q�@Q@Q�@Q�@QC@Qq@Q/@Q�@Q@Q�@Q@Q<@Q�@Q�@Qf@Q�@Q@@Qo@Q�@Q#@Q|@Q�@Q|@Q�@Q�@Q�@Q	�@Q�@QJ@Q�@Q�@Q	@Q�@Q�@P��@P��@P�8@P��@P��@P�I@P��@P��@P�@P�@P��@P�,@P�@P��@P�c@P�p@P��@P�p@P�@P�p@P�F@P��@P�F@P�@P�@P�K@P�!@P��@P�%@P�@P�@P�@P�@P�@P�@P�@P�@P�@P��@P��@P��@P�@P�6@P�`@P�`@P�@P�`@P�@P�`@P�@P�@P��@P�@P��@P��@P��@P�@P�2@P�2@P�2@P�@P�\@P�2@P��@P�@P�@P�@P�2@P�2@P�@P�@P�@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4434433444434443444344434444444344433433444444344433444444433444344433443344334444344334444444434444334434434434443444444433443444344444343443343434444334433344443433344434433343434343434443344344444333433333433334333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�@���@��G�O�G�O�G�O�G�O�@_nG�O�G�O�G�O�@��"G�O�G�O�G�O�@���G�O�G�O�G�O�@��9G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�kfG�O�G�O�G�O�@���@��gG�O�@�7t@��5G�O�G�O�G�O�G�O�G�O�G�O�@��|G�O�G�O�G�O�@���@Z�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@���G�O�G�O�G�O�@X��G�O�G�O�G�O�@��E@��fG�O�G�O�@��J@���G�O�G�O�@��5@�� G�O�G�O�G�O�G�O�@���G�O�G�O�@���@�J;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@VR�G�O�G�O�G�O�G�O�@��a@��G�O�G�O�@\T5G�O�G�O�@L��G�O�G�O�@�ClG�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@j�{@��"G�O�G�O�@��~G�O�G�O�G�O�@��2G�O�G�O�G�O�G�O�G�O�@���G�O�@fQEG�O�G�O�@��@�ʃG�O�@P�G�O�@��G�O�G�O�G�O�G�O�@��v@��)G�O�G�O�@���@���@���G�O�G�O�G�O�G�O�@�ǥG�O�@Q�&@�ǣ@��qG�O�G�O�G�O�@��]G�O�G�O�@���@��~@�1>G�O�@�\�G�O�@�!�G�O�@���G�O�@���G�O�@x��G�O�G�O�G�O�@��B@���G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�@���@��@���G�O�@^k�@��?@�ʃ@���@���G�O�@��~@��.@��)@���G�O�@�Ȳ@�ƕ@��4G�O�@��m@��t@��@���@�ƕ@���@���@��Z@�Ⱥ@���@��@��@�̢@�ˑ@�Ȳ@��@��.@��s@��_@��P@��B@��
@��/@��D@��R@��@�Ȣ@��@��@��s@�ǣ@��@��
@�ȵ@���@��f@��^@�ƕ@���@���@��@�ƕ@��T@���@�Џ@�ł@�ŀ@��q@���@���@��@��1@�ŀ@��@���@�ǥ@�Ȧ@��@@�ǣ@��^@�Ȟ@�Ǧ@��@�ł@�Ɣ@���@���@��>@���@��/@��D@���@��@��
@���@��>@�ƕ@��/@���@�ǥ@��Q@�Ƒ@��/@���@��/@��s@��x@��@�ƕ@��Q@��:@�Ɠ@���@�ȶ@��P@��>@�ň@�Ɠ@�Ǧ@�ƕ@���@��2@��@��@��@�ƕ@�ŀ@���@��1@���@��J@��B@��8@���@���@�˓@��q@��f@��@�Κ@��@���@��m@���@�ʂ@�v@��l@��7@���@��G�O�@�̥@�Ͷ@��n@�˓@��=@��>@�ʂ@��q@��Q@�̢@��(@�˕@���@���@�ʿ@��p@���@�˕@��+@���@���@��.@�ʂ@���@��.@��^@��Z@��^@��@��@�ȵ@��@��q@��`@��]@�ł@���@��^@��
@��c@��q@�͟@��/@��2@��g@���@��@��R@��n@��*@��~@��~@���@��<@��q@���@��(@��@���@���@���@��@��@���@��/@���@��J@��m@�ӂ@��@��@@���@���@�ҡ@�Ӭ@�Լ@���@�Z�@�Ӭ@���@��^@���@��\@��J@���@�Ҝ@���@��M@�ѐ@�Ң@��M@��m@��d@���@���@��p@��z@��J@��J@���@�о@���@�э@�Ң@���@�ѐ@�Ͼ@���@��2@�Ͷ@��q@���@�ʹ@�Ͷ@��2@�ȵ@�˔@��	@��@��W@���@��@��Z@��@��s@��@�ʃ@�Ϯ@��@���@��=@��	@���@�̥@���@��n@�ʹ@��M@��a@��=@�В@��;@���@��D@���@���@��@�Ң@�ͺ@��m@�Κ@�қ@�Ձ@��@��l@�ث@��E@��6@��^@��5@���@��:@�֞@���@�֏@�֑@�֤@���@�מ@���@���@���@���@��9@��<@���@��;@��@��@��*@�ٿ@���@���@��@��@���@��@��z@��}@��@���@���@��%@��@��@��@���@��@��6@���@���@���@���@���@���@��u@��7@��`@��3@��x@���@���@���@��n@��n@���@���@���@���@���@���@���@���@��G@��@��@@��@��u@��-@��@��X@���@��s@���@���@��B@���@���@���@���@���@��@��T@��W@��i@��j@��&@��'@���@��&@��=@��^@��%@��7@���@��f@��]@���@��#@���@��@���@��6@��^@���@���@��k@���@��@���@��&@���@���@��3@���@���@���@���@��f@��@���@���@���@��2@���@���@��f@��s@���@���@���@� S@� �@�@� �@� �@� �@�<@�@�z@� �@� �@���@���@��@�	@��@��@�!@�c@�6@�%@�&@�&@� �@��n@��_@���@���@��R@��R@��q@���@��j@���@���@���@��Z@��J@���@���@���@��
@���@���@���@���@���@���@���@���@���@��@��%@��@��7@��6@��@��Q@��z@���@���@���@���@���@���@���@��@���@��@��@���@��J@��8@��J@��G@���@�� @�� @��G@Q*�@Q+ @Q*�@Q*@Q*@Q'>@Q!�@Q!@Q�@Q�@Q�@Q�@Q@Q�@Q@Q�@Q�@QE@Qp@Q2@Q�@Q@Q�@Q @Q=@Q�@Q�@Qc@Q�@QC@Qm@Q�@Q%@Q{@Q�@Q}@Q�@Q�@Q�@Q	�@Q�@QM@Q�@Q�@Q	@Q�@Q�@P��@P��@P�;@P��@P��@P�H@P��@P��@P�@P�@P�@P�.@P�@P��@P�e@P�r@P��@P�r@P�@P�p@P�F@P��@P�E@P�@P�@P�N@P� @P��@P�%@P�@P�@P�@P�@P�@P�@P�@P�@P�@P��@P��@P��@P�@P�5@P�`@P�`@P�@P�`@P�@P�`@P�@P�@P��@P�@P��@P��@P��@P�@P�5@P�5@P�3@P�@P�]@P�8@P��@P�@P�@P�@P�6@P�3@P�@P�@P�@P�@��@���@���@��%@��@��@��@���@��@��6@���@���@���@���@���@���@��u@��7@��`@��3@��x@���@���@���@��n@��n@���@���@���@���@���@���@���@���@��G@��@��@@��@��u@��-@��@��X@���@��s@���@���@��B@���@���@���@���@���@��@��T@��W@��i@��j@��&@��'@���@��&@��=@��^@��%@��7@���@��f@��]@���@��#@���@��@���@��6@��^@���@���@��k@���@��@���@��&@���@���@��3@���@���@���@���@��f@��@���@���@���@��2@���@���@��f@��s@���@���@���@� S@� �@�@� �@� �@� �@�<@�@�z@� �@� �@���@���@��@�	@��@��@�!@�c@�6@�%@�&@�&@� �@��n@��_@���@���@��R@��R@��q@���@��j@���@���@���@��Z@��J@���@���@���@��
@���@���@���@���@���@���@���@���@���@��@��%@��@��7@��6@��@��Q@��z@���@���@���@���@���@���@���@��@���@��@��@���@��J@��8@��J@��G@���@�� @�� @��G@Q*�@Q+ @Q*�@Q*@Q*@Q'>@Q!�@Q!@Q�@Q�@Q�@Q�@Q@Q�@Q@Q�@Q�@QE@Qp@Q2@Q�@Q@Q�@Q @Q=@Q�@Q�@Qc@Q�@QC@Qm@Q�@Q%@Q{@Q�@Q}@Q�@Q�@Q�@Q	�@Q�@QM@Q�@Q�@Q	@Q�@Q�@P��@P��@P�;@P��@P��@P�H@P��@P��@P�@P�@P�@P�.@P�@P��@P�e@P�r@P��@P�r@P�@P�p@P�F@P��@P�E@P�@P�@P�N@P� @P��@P�%@P�@P�@P�@P�@P�@P�@P�@P�@P�@P��@P��@P��@P�@P�5@P�`@P�`@P�@P�`@P�@P�`@P�@P�@P��@P�@P��@P��@P��@P�@P�5@P�5@P�3@P�@P�]@P�8@P��@P�@P�@P�@P�6@P�3@P�@P�@P�@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       4434433444434443444344434444444344433433444444344433444444433444344433443344334444344334444444434444334434434434443444444433443444344444343443343434444334433344443433344434433343434343434443344344444333433333433334333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9F�v9F�9F�9F��9F�9F�9F�9F��9F�9F�-9F��9F�9F�9F�9F�9F�9F�w9F�.9F�^9F�*9F�z9F�9F��9F�9F�9F�9F�9F��9F��9F�9F�9F�	9F��9F�/9F�9F�H9F�9F�F9F�w9F�z9F�39F�9F��9F�9F��9F��9F�9F�9F��9F�,9F�C9F�19F�`9F�9F�9F��9F��9F�9F�9F�'9F�9F�9F�
9F��9F��9F�t9F��9F�59F�Y9F��9F�9F��9F�9F�9F�69F�u9F�U9F��9F�X9F�9F�>9F�9F�Y9F�n9F�9F�9F�9F�Y9F�9F�9F�9F�9F��9F�9F�.9F�H9F��9F��9F�(9F�9F�9F�f9F�Z9F��9F�59F�"9F��9F�9F�k9F�;9F�9F��9F�9F�9F�9F�D9F�[9F�D9F��9F�K9F�9F�d9F�P9F�Q9F�Q9F��9F�N9F��9F�r9F�X9F�9F�9F�x9F��9F�p9F��9F��9F��9F�]9F�9F�9F�q9F�Z9F��9F�9F��9F�>9F�19F�i9F�k9F�~9F�~9F�9F�9F��9F�9F��9F��9F�9F��9F�+9F�A9F�@9F�n9F�9F�9F�9F�9F��9F�9F��9F��9F�9F�9F�	9F�9F�9F�q9F��9F��9F�8�Ƥ8�� 8���8���8���8�8��t8��O8���8���8��P8��U8���8���8���8��U8��8���8���8���8��b8���8��b8���8��]8���8���8���8���8��88��>8���8���8���8��88���8��8���8��8��I8���8��98���8���8��R8��n8��{8��8�8�8��8�8��8�8�t8�
8�8�N8�V8��8�8�8�|�8�}38�|�8�|<8�|�8�|l8�|8�|k8�|�8�|>8�{J8�{8�z�8�y�8�x8�w8�w8�v�8�v�8�v�8�w8�v�8�w8�wA8�w;8�v8�u�8�uS8�u�8�u�8�u�8�u�8�u�8�u�8�u�8�u�8�v8�u�8�v8�v8�v8�vJ8�v8�v8�v|8�v�8�v�8�v�8�v8�vH8�vP8�vP8�v�8�v|8�vF8�u�8�t�8�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B  B  BB+BJBoBuB{B{B{B{B{B{BuBuBuBoBVBbB�B(�B+B/B5?B=qBB�BJ�BL�BO�BW
BZBZB[#B[#B[#BYBXBXBXBYBYBXB^5BdZBffBffBx�B�B�VB�B�B�B�B�B��B��B��B��B��B��B��B�{B�\B�JB�%B�B~�B~�B}�B{�Bs�Bw�BjB\)BYBcTB`BBVBF�B;dBA�B9XB)�B�B\BB�B�mB��B  B��B�fB�B��B�'B��B}�BS�BB�B49B"�BB�B�B�B�ZB��B��BiyBXBN�B=qB1'B+B&�B"�B�B�BB
�TB
ŢB
��B
t�B
iyB
`BB
\)B
XB
Q�B
L�B
C�B
5?B
�B
B	��B	��B	��B	~�B	`BB	Q�B	A�B	33B	0!B	33B	49B	8RB	33B	33B	/B	,B	&�B	�B	�B	�B	uB	\B	PB	PB	DB	+B	B��B��B��B�B�B�B�`B�5B�B��B��B��B��BȴBǮBŢB��B�XB�LB�?B�9B�!B��B��B��B��B��B��B��B��B��B�\B�=B�B~�By�Bp�Bo�Bo�Bn�BjBe`BdZBbNB_;BXBS�BO�BN�BM�BL�BK�BH�BH�BG�BH�BI�BJ�BJ�BJ�BO�BP�BP�BO�BN�BN�BN�BO�BN�BM�BO�BO�BO�BN�BN�BM�BL�BJ�BH�BE�BF�BG�BF�BF�BH�BH�BM�BO�BP�BP�BP�BQ�BR�BR�BR�BP�BQ�BQ�BP�BO�BN�BM�BM�BK�BO�BM�BN�BN�BN�BM�BN�BP�BQ�BYB[#B]/BaHBiyBjBk�Bk�Bk�Bl�Bk�Bl�Bm�Bs�Bu�Bu�Bu�Bu�Bu�Bu�Bw�B{�B{�B{�B�B�%B�DB�bB�hB�oB�{B��B��B��B��B��B��B��B��B�B�!B�'B�3B�LB�qB�}BĜBƨB��B��B��B��B�
B�B�;B�HB�TB�`B�sB�B�B��B��B��B��B��B��B��B	B	1B	JB	VB	hB	uB	�B	�B	�B	�B	#�B	&�B	&�B	&�B	'�B	)�B	+B	-B	1'B	33B	33B	33B	7LB	8RB	8RB	8RB	8RB	8RB	8RB	9XB	9XB	;dB	<jB	<jB	<jB	?}B	@�B	B�B	C�B	E�B	F�B	G�B	I�B	L�B	M�B	N�B	O�B	S�B	YB	]/B	^5B	_;B	`BB	`BB	`BB	bNB	cTB	gmB	iyB	jB	l�B	o�B	q�B	s�B	t�B	t�B	u�B	w�B	y�B	|�B	~�B	~�B	�B	�+B	�=B	�VB	�VB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�9B	�?B	�FB	�FB	�RB	�dB	�wB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�BB	�HB	�NB	�TB	�TB	�NB	�NB	�TB	�`B	�`B	�fB	�fB	�fB	�fB	��B	�zB
�B
^B
.B
�B
�B
(�B
2�B
;�B
CB
G_B
M�B
Q�B
V�B
[�B
`B
e�B
i_B
o5B
shG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?7H?��B�l@��?iu:B�A�]�>��@)�E?�_?;�EA���AHl�?�q@R��B�>�;�?	PU?a׈B�;@9uC?�e?�=�B��@p~?q�*?iÜ>x�M>��?>�&�? tA��A{�w?u�?��tB�WB��?D%[B _�B�;?�?)�j@�H�>澝?��?l5�B�-A�l?�?G��B��A��/>���? &�A�Ӄ>�^h?'F?�?�$�B��B �?���>Ọ?>�fA�M�?�� @/��A )5B܁BwH?&x?���B�VB֕A��?���B��B�P?t�e?� �?/@?��}B�,?��?3�eB�B�[?fI>�b�@G]�>��>�O�A$U�?��?N\A��@��>�L�?�wAI��B�UB.�>�?e�A�xY?1�?�L�A��@�\?��A�Q�?�]�A��?��B��@�S >�q�?��?���@Ua�?@F ?S��A�״B�@Nr�?x��B3>��?�?��vB�@?^q�?sm�A�Y?C�A$-�BD�?��UA���?)0A-GVB�B�Y?(�6A��s?�߱B	o�>�T? *�?)�@{
BڭB�u@��?�9A��B��B��?@�>�g@1�o@�n�B�?b'�A�Z�B�B�O@�ilA_�@��B��@��?e<�B�B�B
�+?�s�A��0?-�<A���@��wB�?LؙB�@ƿ�A�7?�uS?�$�A���B$�B2@=�A?o��B	A��A��?�%�??��Aa��B�OB�2B,�?N��A�xB�B�,A�noB��?�T�B�DB��B�[B�7Ar>�B�B�B�e@��AB�B��B�B�yB�B�B�B�tB��B�B�B�B�VB�B�	B�B��B�%B�/B�B��B�5B�3B��B��B�[B�^B�CB�pB�%B�B�pB�%B�?B��B�#B��B��B�?B�GB�B�B��B�B�*B��B��B��B�B�	B��B�B�B��B�BB�B��B�YB��B�B�B�B�zB�vB�tB�tB�B�\B��B�3B�)B�4B��B��B��B�\B�B��B�}B�sB�B��B�3B�B��B�B��B�4B�B�B�B��B�B�B�EB�\B�B�B�vB�B�pB�@B�B��B�pB��B��B��B�B��B�JB�qB�3B�?B�B�B�B��B�FB�B�B�B�B�B�B��B�"B�B�kB�AN�B��B�B�B�B�3B�QB�&B��B�`B�VB�sB�%B�B�@B�PB�TB�#B�%B�^B�@B��B��B�B�;B��B�B�MB�B�OB�B�?B��B�B�B�B��B�CB�B�1B�,B��B�nB�B�(B�^B��B��B�ZB��B�B�B�B�B�B�5B��B�2B�B��B�B�B�B�@A�WYB��B�.B�_B��B�OB�@B[B�B�dB�VB�B�BB�B��B�B��B��B�0B�+B�pB�YB��B�B�B��B�B�B�B�.B�vB��B�B%�B�B�_B�HB��B�B�B�B�:B��B��B� B��B�B��B�B��B�IB�7B�KB�ZB�>B��B�?B��B��B��B�B�7B�B��B�B��B�B�!B��B�B�MB�iB�B��B�GB�B�B�/B�1B��B�!B�B�B�B�HB�B��B�B�B�XB�8B�B��B�B�B�B�B�B�.B�B��B�xB�=B�B�B�3B�B�?B�B�B�B�B��B�BB�B�6B�B�B��B��B��B�2B�B�B�gB�_B�VB�}B�aB�B�B�cB�5B�`B�B�B�B�B�-B��B��B�B�5B��B��B�CB�B�EB�B�B�VB��B��B�B�xB�?B�BB�B�B�B��B��B�B�wB��B��B��B�B��B��B�+B�QB�B�B�0B�'B�mB�\B�TB�{B�NB�B�B�B�B�B�DB��B��B�B�B�B�B�'B�dB��B�B��B�B�)B�B�uB�<B�wB�B�B�B�B�PB�B�7B�.B�B��B��B�B�B�B��B��B�UB�DB�B��B�B�AB�B��B�RB��B��B�NB�5B�HB�B�/B�B�4B�7B�aB�B�IB��B��B�oB�B�B�QB�pB�B��B�;B��B��B�*B��B�}B��B� B��B��B�B�rB�aB�9B�[B�pB��B�$B�bB��B��B�"B�6B�WB�B�>B��B�{B�jB�0B�B�IB�@B��B��B�xB��B�_B��B��B��B�PB��B�B��B��B��B��B��B��B��B��B��B�zB��B _B	ՌB	�B	�oB	�B	�`B	֭B	��B	�lB	��B	ثB	ؼB	�TB	�GB	��B	ֲB	�wB	ՃB	�3B	׀B	ؕB	�wB	��B	ֆB	��B	בB	ׄB	�tB	�JB	��B	�9B	ׄB	�B	ׄB	�<B	��B	֠B	ӖB	ҊB	�B	��B	��B	�B	֍B	�B	�OB	�:B	�sB	�6B	��B	�B	؛B	�#B	�}B	�CB	��B	ؾB	��B	�BB	׭B	׺B	��B	�`B	ِB	�5B	��B	�B	��B	��B	��B	�aB	�dB	ٯB	��B	�B	ؑB	��B	�gB	�ZB	�LB	�?B	�%B	�B	�B	�;B	��B	��B	�B	�B	�B	�GB	��B	�B	�B	�B	��B	�(B	ړB	��B	ڶB	��B	��B	ځB	�tB	��B	ڶB	ںB	ڭB	�bB	�tB	�HB	��B	� B	��B	٪B	ڎB	�UB	��B	ڒB	�)B	څB��B�B�#B�B��B��B��B��B��B��B�jB�B�FB�B��B�7B��B��B��B��B��B��B��B��B��B�GB�{B�~B��B��B��B��B�"B��B�B��B��B�IB��B�_B�XB��B��B��B��B��B�'B��B��B�{B��B��B��B��B��B��B��B��B DB�B�^B�iB sB 'B BdB�UB"B aB �BdB �B hB �B�B �B�iB��B�B�<B��B�6B��B��B (B �BB�}B sB�BB�B�B�BB�B�B&B2B<B�B�B	*B B\B@B
'B
YBB!B
�B	�B	�B�B	�B�B�B�B}B"BLB�B�B�B�BB.B�B2B�BIBB�BbB�B�B�B�BMB=BQB6BBkBZBPB�B�B�B�B�B�B�B�B�B�B�B�BjB�B�B�B�B�B	B BB�BB�BBB�BB�B B�B]B�BtB�B	�yB	�B	�qB	��B	��B	��B	�2B	�}B	�YB	�KB	�wB	�LB	�B	�aB	�cB	�(B	��B	�B	�B	�B	�aB	�B	�GB	�wB	��B	��B	�B	��B	�B	��B	�:B	�(B	�B	�B	�B	��B	�B	��B	�`B	��B	�1B	��B	�TB	��B	�B	�WB	��B	�B	�B	�'B	�B	�B	�AB	��B	�1B	��B	��B	�aB	�B	�dB	�B	�B	�B	��B	�nB	�4B	�dB	�8B	��B	�B	�0B	��B	�AB	�B	��B	�EB	�B	�~B	�qB	�EB	�+B	�B	�/B	�B	�%B	�B	�B	�VB	�B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�zB	�B	�B	�B	�uB	�B	�zB	�NB	��B	�B	�B	��B	�B	��B	��B	�pB	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994434433444434443444344434444444344433433444444344433444444433444344433443344334444344334444444434444334434434434443444444433443444344444343443343434444334433344443433344434433343434343434443344344444333433333433334333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   B��B��B��B��B��B��B �BB<BbBjBnBnBnBnBkBkBiBkBiBaBFBSBuB(�B*�B/B54B=aBB�BJ�BL�BO�BV�BZBZB[B[B[BYBXBX BW�BYBY
BXB^*BdKBfWBfZBx�B�B�JB��B�B�B��B��B��B��B��B��B��B��B��B�lB�LB�<B�B��B~�B~�B}�B{�Bs�Bw�BjqB\BYBcJB`3BU�BF�B;TBAzB9JB)�B�BLB �B�B�_B��B��B��B�XB�BʵB�B��B}�BS�BB�B4.B"�BB�B�B�~B�KB��B��BikBX BN�B=aB1B*�B&�B"�B�BB�B
�DB
ŔB
��B
t�B
ikB
`1B
\B
XB
Q�B
L�B
C�B
5.B
�B
	B	��B	��B	��B	~�B	`1B	Q�B	AzB	3$B	0B	3"B	4'B	8@B	3#B	3#B	/
B	+�B	&�B	�B	�B	|B	fB	KB	AB	AB	5B	B	�B��B��B��B�B�B�uB�NB�$B�B��B��B˵BʭBȣBǜBőB�rB�HB�;B�-B�'B�B��B��B��B��B��B��B��B��B�qB�LB�*B�B~�By�Bp�Bo�Bo�Bn�BjnBeOBdIBb=B_+BW�BS�BO�BN�BM�BL�BK�BH�BH�BG�BH�BI�BJ�BJ�BJ�BO�BP�BP�BO�BN�BN�BN�BO�BN�BM�BO�BO�BO�BN�BN�BM�BL�BJ�BH�BE�BF�BG�BF�BF�BH�BH�BM�BO�BP�BP�BP�BQ�BR�BR�BR�BP�BQ�BQ�BP�BO�BN�BM�BM�BK�BO�BM�BN�BN�BN�BM�BN�BP�BQ�BYB[B]Ba5BigBjlBkqBkrBkrBlzBksBlyBm}Bs�Bu�Bu�Bu�Bu�Bu�Bu�Bw�B{�B{�B{�B�
B�B�1B�OB�SB�ZB�iB�mB�xB��B��B��B��B��B��B��B�B�B�B�9B�]B�kBĉBƕBʮB̸B��B��B��B��B�)B�4B�BB�OB�_B�wB�B��B��B��B��B��B��B��B	B	B	7B	AB	UB	`B	oB	oB	zB	�B	#�B	&�B	&�B	&�B	'�B	)�B	*�B	,�B	1B	3B	3B	3B	77B	8@B	8>B	8>B	8>B	8<B	8<B	9FB	9DB	;RB	<WB	<VB	<SB	?jB	@oB	B{B	C�B	E�B	F�B	G�B	I�B	L�B	M�B	N�B	O�B	S�B	YB	]B	^!B	_'B	`0B	`-B	`0B	b:B	cAB	g\B	ifB	jmB	lyB	o�B	q�B	s�B	t�B	t�B	u�B	w�B	y�B	|�B	~�B	~�B	��B	�B	�)B	�BB	�CB	�IB	�ZB	�cB	�sB	�yB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�*B	�3B	�4B	�>B	�OB	�aB	ōB	ǚB	ȢB	ɥB	ʬB	ʭB	ʮB	;B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�/B	�3B	�;B	�?B	�?B	�;B	�:B	�BB	�KB	�MB	�RB	�QB	�RG�O�B	��B	�hB
tB
IB
B
�B
�B
(�B
2�B
;�B
CB
GKB
M�B
QoB
V�B
[wB
_�B
e�B
iIB
o"B
sUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�^G�O�G�O�B�A�]}G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�B�G�O�G�O�G�O�B�+G�O�G�O�G�O�BڿG�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�B�LB��G�O�B _�B�0G�O�G�O�G�O�G�O�G�O�G�O�B� G�O�G�O�G�O�B��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�BþB �G�O�G�O�G�O�A�M�G�O�G�O�G�O�B�sBw:G�O�G�O�B�JBֈG�O�G�O�B��B�GG�O�G�O�G�O�G�O�B�G�O�G�O�B�B�PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�B�KB.�G�O�G�O�A�xIG�O�G�O�A��G�O�G�O�A�Q�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�צB�G�O�G�O�B'G�O�G�O�G�O�B�1G�O�G�O�G�O�G�O�G�O�BD�G�O�A���G�O�G�O�B��B�MG�O�A��gG�O�B	o�G�O�G�O�G�O�G�O�BڣB�iG�O�G�O�A��B��B��G�O�G�O�G�O�G�O�B�G�O�A�Z�B�uB�AG�O�G�O�G�O�B��G�O�G�O�B�yB�B
� G�O�A��G�O�A��zG�O�B�G�O�B�G�O�A�6�G�O�G�O�G�O�B$B2G�O�G�O�B	G�O�G�O�G�O�G�O�G�O�B�BB�'B,�G�O�A�eB�B�!A�nYB��G�O�B�7B��B�NB�+G�O�B�B�B�\G�O�B�B��B�B�nB�B�B�B�dB��B�B�B�B�IB�rB��B�B�B�B�"B�~B��B�&B�&B��B�B�LB�RB�7B�cB�B�B�cB�B�2B�B�B��B��B�2B�:B� B�B��B�vB�B��B��B��B�
B��B�B�B�sB��B�5B�B��B�MB��B�B�B�B�qB�hB�iB�iB�pB�MB��B�&B� B�)B��B�B��B�MB�B��B�pB�gB�B��B�&B�B��B�B��B�'B�B�B�B��B�B�B�;B�MB�B�sB�jB�B�cB�7B�~B��B�cB��B��B��B�~B��B�>B�fB�%B�0B��B�B�B�B�=B�rB��B�B�	B�B�B��B�B�B�`B�G�O�B��B�tB�B�B�$B�IB�B��B�RB�IB�hB�B�B�5B�AB�FB�B�B�RB�5B�B��B�uB�2B�B�rB�=B�B�AB�~B�2B��B�B�B�B��B�7B�B�&B�"B��B�aB�B�B�SB��B�B�OB��B�B�B�B�B�B�(B��B�'B�xB��B�B�B�B�5A�W@B��B�B�RB��B�CB�5BNB�B�VB�JB�B�2B�B��B�B��B��B�$B�B�bB�MB��B�rB�B��B�B�B�B�!B�jB��B�B%tB�B�RB�;B��B�B�B�B�/B��B�B��B��B�B��B�B��B�=B�-B�<B�PB�1B��B�2B��B��B��B�~B�+B��B��B�B��B�B�B�B�B�BB�\B�B��B�<B�B�B�$B�%B�B�B�
B�xB�B�;B�B��B�B�B�PB�,B�B��B�B�B�B�B�B�%B�B��B�lB�5B�yB�B�&B�B�0B�B��B�B�B��B�7B�B�)B�B�B��B��B��B�%B�B�B�YB�TB��B�B�B��B�}B�}B��B��B��B��B�_B�B�:B��B��B�(B��B��B��B��B��B��B��B��B�uB�<B�mB�sB��B��B��B��B�B��B�B��B��B�>B��B�TB�JB�sB��B�{B��B��B�B��B�wB�qB��B��B�zB��B��B��B��B�qB 9B��B�QB�]B dB B �BYB�IBB UB �BYB �B ]B �B�B �B�]B��B�B�1B��B�*B��B��B B �BB�qB gB�BB�B�B�B�B�B�BB%B.B�B�B	B�BLB3B
B
KBBB
�B	�B	�B�B	�B�B�B�BpBB<B�B�B�B�BB!B�B(B�B>BB�BWB�B�B�B�BBB1BEB)BB^BPBBB�B�B�B�B�B�B�B�B�BB�B�B`B�B�B�B�B�B�B�B�B�BB�BB�B�BB�B�B�BRByBgB�B	�gB	�B	�[B	��B	�B	��B	�B	�hB	�DB	�7B	�cB	�8B	�yB	�KB	�RB	�B	��B	�B	�B	�B	�NB	�}B	�3B	�cB	�B	��B	�B	�B	�B	�B	�&B	�B	�}B	��B	�B	��B	�B	�B	�LB	��B	�B	��B	�?B	��B	��B	�BB	�B	�B	�B	�B	�B	�B	�,B	��B	�B	��B	��B	�OB	�B	�PB	��B	�B	�B	��B	�[B	�B	�OB	�$B	��B	��B	�B	��B	�-B	� B	�B	�/B	�B	�lB	�\B	�0B	�B	�	B	�B	��B	�B	�B	��B	�@B	�B	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�}B	�B	�uB	�hB	�wB	�B	�oB	�cB	�B	�fB	�>B	��B	��B	��B	��B	��B	��B	�B	�\B	�B	�B��B�B�B��B�}B�}B��B��B��B��B�_B�B�:B��B��B�(B��B��B��B��B��B��B��B��B�uB�<B�mB�sB��B��B��B��B�B��B�B��B��B�>B��B�TB�JB�sB��B�{B��B��B�B��B�wB�qB��B��B�zB��B��B��B��B�qB 9B��B�QB�]B dB B �BYB�IBB UB �BYB �B ]B �B�B �B�]B��B�B�1B��B�*B��B��B B �BB�qB gB�BB�B�B�B�B�B�BB%B.B�B�B	B�BLB3B
B
KBBB
�B	�B	�B�B	�B�B�B�BpBB<B�B�B�B�BB!B�B(B�B>BB�BWB�B�B�B�BBB1BEB)BB^BPBBB�B�B�B�B�B�B�B�B�BB�B�B`B�B�B�B�B�B�B�B�B�BB�BB�B�BB�B�B�BRByBgB�B	�gB	�B	�[B	��B	�B	��B	�B	�hB	�DB	�7B	�cB	�8B	�yB	�KB	�RB	�B	��B	�B	�B	�B	�NB	�}B	�3B	�cB	�B	��B	�B	�B	�B	�B	�&B	�B	�}B	��B	�B	��B	�B	�B	�LB	��B	�B	��B	�?B	��B	��B	�BB	�B	�B	�B	�B	�B	�B	�,B	��B	�B	��B	��B	�OB	�B	�PB	��B	�B	�B	��B	�[B	�B	�OB	�$B	��B	��B	�B	��B	�-B	� B	�B	�/B	�B	�lB	�\B	�0B	�B	�	B	�B	��B	�B	�B	��B	�@B	�B	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�}B	�B	�uB	�hB	�wB	�B	�oB	�cB	�B	�fB	�>B	��B	��B	��B	��B	��B	��B	�B	�\B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994434433444434443444344434444444344433433444444344433444444433444344433443344334444344334444444434444334434434434443444444433443444344444343443343434444334433344443433344434433343434343434443344344444333433333433334333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.15 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311646022020083116460220200831164602202008311646022020083116460220200831164602202008311646022020083116460220200831164602202008311646022020083116460220200831164602AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816302019021918163020190219181630    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816302019021918163020190219181630  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816302019021918163020190219181630  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311646022020083116460220200831164602  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                