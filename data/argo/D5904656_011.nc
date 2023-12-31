CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  D   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:27Z creation      
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
resolution        =���   axis      Z        '0  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '0  v   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '0  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '0  �8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '0  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� &d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '0 00   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '0 W`   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ~�   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '0 �\   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '0 �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '0 ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '0 �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 8�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '0 B�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � i�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   jp   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   vp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173027  20200828145439  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @����He@����He@����He111 @��b���@��b���@��b���@5ܬ1&�@5ܬ1&�@5ܬ1&��c���R�c���R�c���R111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�33B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��RD�C�D��{D��=D��D�UqD�� D��D��D�O�D���D��3D��D�?\Dڈ�DએD�	�D�5qD�p G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=���    =���    ���ͽ���            ����        ���ͽ��ͽ���        ����        ����        ����=���    ����    =���    ���ͽ��ͽ���    =���        >L��=��ͽ���                ���ͽ���        ���ͽ���=���=���    ����                >L��    ����        ���ͽ���        ���ͽ���    ���ͽ��ͽ���        ����    =���    ����    >L��=��ͽ���        ����    >���>���    ���ͽ���>L��>L��    ���ͽ��ͽ���=���=��ͽ���    =���            ���ͽ���=���=���    ����        ����    ����    ����    ����    =��ͽ���        >L�ͽ���        =���    ���ͽ��ͽ���            ���ͽ���    ����    ����    =���    ����    =���=���                ����            ����        =��ͽ���    =���=���>L��    ����    =���=���>L��=���    ����=���>L��=���    ����=���        =��ͽ��ͽ���>L��>L��=��ͽ���=��ͽ���            =��ͽ���    >L��=���    >L��        =���=���    =���>L��=���>L��=���    =���=���>L��>L��>���>���=���>���>���>���>���>���>L��>L��=���=���>L��=���>L��>L��>L��=���>L��>L��>L��>���>L��>L��>���>L��>L��>L��=���=���=���>L��=���>���>L��>L��>���>���>L��>���    =���>���>L��=���>L��>L��>L��=���>���>L��=���>���>���>���>���>L��>���>���>���>L��>L��=���>L��>L��>L��>L��>L��>L��>L��=���=���>L��>L��>L��>���=���>���>���=���=���>L��>���>���>L��>L��>L��>L��=���>L��=���>L��>L��>���>L��=���>���>L��>���>L��>���>L��>���>���>L��>L��>L��>L��    =���>L��>L��>L��>L��=���=���=���>L��>L��=���>���>L��>L��>L��>L��>L��>L��>L��>L��=���=���>L��>L��>L��>���=���>L��>���>L��=���    =���>L��>L��=���>���>���>L��>���>L��>L��>L��>L��>L��>L��>L��=���>���    =���>L��>L��>L��=���>L��>L��>L��>L��>���>L��>L��>L��>L��>L��>L��=���=���=���>L��=���>L��>L��=���>L��=���>L��=���>L��>���>���>���=���>L��>���>���>���>L��=���>L��>L��>L��>L��>���=���>���>L��>���>���>L��>L��>L��>L��>L��=���>L��>���>L��>���>���>L��>L��>L��>L��>���>���=���>���>L��>L��=���>L��>���>L��>L��>L��=���>L��=���>L��>L��>L��>L��>L��=���>L��>L��=���>L��>L��>L��>L��>���>L��>L��>L��>L��=���>L��=���=���=���>L��>L��>L��>L��=���>���>���>���=���    =���=���>L��>L��>���>���>���>���?   ?   ?��?333?333?L��?fff?�  ?�  ?�  ?���?���?�ff?�33?�33?�  ?���?ٙ�?ٙ�?�ff?�33?�33@   @ff@��@33@33@��@   @&ff@,��@,��@333@@  @Fff@L��@Y��@fff@fff@s33@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@陚@�  @�33@���@���A   A33A��A  A	��A��AffA��A33A��A  A��A33AffA   A#33A$��A&ffA)��A+33A.ffA0  A1��A4��A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAP  AS33AT��AX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��As33At��Ax  Ay��A|��A~ffA���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  Aݙ�A�ffDp�fDp��DpٚDp� Dp��Dp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��DtٚDt� @333@@  @Fff@L��@Y��@fff@fff@s33@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@陚@�  @�33@���@���A   A33A��A  A	��A��AffA��A33A��A  A��A33AffA   A#33A$��A&ffA)��A+33A.ffA0  A1��A4��A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAP  AS33AT��AX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��As33At��Ax  Ay��A|��A~ffA���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  Aݙ�A�ffDp�fDp��DpٚDp� Dp��Dp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��DtٚDt� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�33B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��RD�C�D��{D��=D��D�UqD�� D��D��D�O�D���D��3D��D�?\Dڈ�DએD�	�D�5qD�p G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=���    =���    ���ͽ���            ����        ���ͽ��ͽ���        ����        ����        ����=���    ����    =���    ���ͽ��ͽ���    =���        >L��=��ͽ���                ���ͽ���        ���ͽ���=���=���    ����                >L��    ����        ���ͽ���        ���ͽ���    ���ͽ��ͽ���        ����    =���    ����    >L��=��ͽ���        ����    >���>���    ���ͽ���>L��>L��    ���ͽ��ͽ���=���=��ͽ���    =���            ���ͽ���=���=���    ����        ����    ����    ����    ����    =��ͽ���        >L�ͽ���        =���    ���ͽ��ͽ���            ���ͽ���    ����    ����    =���    ����    =���=���                ����            ����        =��ͽ���    =���=���>L��    ����    =���=���>L��=���    ����=���>L��=���    ����=���        =��ͽ��ͽ���>L��>L��=��ͽ���=��ͽ���            =��ͽ���    >L��=���    >L��        =���=���    =���>L��=���>L��=���    =���=���>L��>L��>���>���=���>���>���>���>���>���>L��>L��=���=���>L��=���>L��>L��>L��=���>L��>L��>L��>���>L��>L��>���>L��>L��>L��=���=���=���>L��=���>���>L��>L��>���>���>L��>���    =���>���>L��=���>L��>L��>L��=���>���>L��=���>���>���>���>���>L��>���>���>���>L��>L��=���>L��>L��>L��>L��>L��>L��>L��=���=���>L��>L��>L��>���=���>���>���=���=���>L��>���>���>L��>L��>L��>L��=���>L��=���>L��>L��>���>L��=���>���>L��>���>L��>���>L��>���>���>L��>L��>L��>L��    =���>L��>L��>L��>L��=���=���=���>L��>L��=���>���>L��>L��>L��>L��>L��>L��>L��>L��=���=���>L��>L��>L��>���=���>L��>���>L��=���    =���>L��>L��=���>���>���>L��>���>L��>L��>L��>L��>L��>L��>L��=���>���    =���>L��>L��>L��=���>L��>L��>L��>L��>���>L��>L��>L��>L��>L��>L��=���=���=���>L��=���>L��>L��=���>L��=���>L��=���>L��>���>���>���=���>L��>���>���>���>L��=���>L��>L��>L��>L��>���=���>���>L��>���>���>L��>L��>L��>L��>L��=���>L��>���>L��>���>���>L��>L��>L��>L��>���>���=���>���>L��>L��=���>L��>���>L��>L��>L��=���>L��=���>L��>L��>L��>L��>L��=���>L��>L��=���>L��>L��>L��>L��>���>L��>L��>L��>L��=���>L��=���=���=���>L��>L��>L��>L��=���>���>���>���=���    =���=���>L��>L��>���>���>���>���?   ?   ?��?333?333?L��?fff?�  ?�  ?�  ?���?���?�ff?�33?�33?�  ?���?ٙ�?ٙ�?�ff?�33?�33@   @ff@��@33@33@��@   @&ff@,��@,��@333@@  @Fff@L��@Y��@fff@fff@s33@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@陚@�  @�33@���@���A   A33A��A  A	��A��AffA��A33A��A  A��A33AffA   A#33A$��A&ffA)��A+33A.ffA0  A1��A4��A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAP  AS33AT��AX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��As33At��Ax  Ay��A|��A~ffA���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  Aݙ�A�ffDp�fDp��DpٚDp� Dp��Dp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��DtٚDt� @333@@  @Fff@L��@Y��@fff@fff@s33@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@陚@�  @�33@���@���A   A33A��A  A	��A��AffA��A33A��A  A��A33AffA   A#33A$��A&ffA)��A+33A.ffA0  A1��A4��A6ffA8  A;33A<��A>ffAA��AC33AFffAH  AI��AL��ANffAP  AS33AT��AX  AY��A\��A^ffA`  Ac33Ad��Ah  Ai��Al��AnffAq��As33At��Ax  Ay��A|��A~ffA���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aə�A�33A�  A���A͙�A�33A�  A���Aљ�A�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  Aݙ�A�ffDp�fDp��DpٚDp� Dp��Dp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��DtٚDt� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԡ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԡ�AԍPA�r�A�bNA�1A�E�A�|�A�  A��`A̶FA��;A˟�A���Aȕ�A�33A�hsA��yA�bA��A�&�A��FA���A���A�5?A��9A��`A���A��
A�$�A��A�(�A�bNA��yA�
=A�\)A�M�A�XA�A���A���A�oA�  A���A��yA��\A���A���A�ffA��A��`A��9A��+A��hA��A��hA�-A�?}A��A�n�A�7LA�oA�ȴA��A���A�$�A�n�A���A���A�/A��DA��A�JA��A�E�A��A�?}A�A��9A�(�A���A��FA���A��A���A���A���A�G�A~ffA|��A|ȴA{��Az�!AyoAw��AvȴAu��At �Ar1Aq
=Anv�Al~�Ak�hAiS�Ag?}Af�AedZAdZAahsA`(�A^�A]/A\1AYdZAV��AU%AS|�ARjAQ�
AP��AN��AK\)AI%AGt�AE�ADVAB��AB�AA�A?��A="�A;�A;A9ƨA7+A6VA6  A5�mA5�;A5�#A5�
A5��A4�\A3`BA3�A3�A3
=A2ȴA1�
A1�A1�A1?}A1VA0��A0ZA.jA,A*�`A(��A&ȴA&ffA&A%|�A%oA#%A!�PA!7LA�TA1A �A��A1'A��A33AM�A\)A��A��AM�A��A��A|�A%A~�A
�RAZAjA�A��A
=A�jA1A�FA��A�7AhsAG�A��A�mA �`A �A J@�o@�E�@��@�~�@�Z@�v�@��7@��u@�A�@�!@�9X@��@�?}@��@웦@�(�@�@�;d@ꟾ@��@�J@�@�p�@��@�1@睲@���@�9@�I�@��@�K�@�V@�@�I�@�$�@�b@�J@�(�@֧�@�-@ԛ�@�J@���@�z�@ύP@ΰ!@͉7@̓u@�A�@���@�|�@�ff@�hs@ȓu@��@Ǯ@ǥ�@ǥ�@��@ź^@��/@��@�|�@���@�@�E�@�Ĝ@�ƨ@��;@��m@��+@��^@��7@�%@��9@�bN@�I�@�I�@�A�@�b@�33@�r�@���@��@�hs@���@��F@���@���@�M�@���@�V@�bN@��P@��H@�@�Ĝ@�Q�@�b@�\)@�ff@���@��^@�?}@�z�@��;@��@��P@�;d@��H@��T@�Ĝ@��@�r�@�(�@�  @��w@�"�@��+@�5?@��h@��j@�1'@�o@�^5@�`B@�1'@�C�@��!@�n�@�^5@�V@�E�@�-@�J@���@�`B@�/@�&�@��@��@��@�V@��/@���@�r�@� �@��F@��@�=q@��@��@��T@���@��^@���@��7@�p�@�?}@�%@��j@��@�r�@�bN@�I�@�9X@�ƨ@�t�@�@��y@��@��@���@��!@��\@�$�@�?}@�%@��u@�(�@���@��F@�+@��y@���@���@���@��\@��\@�=q@�$�@��#@�X@���@�Z@� �@�b@��m@�ƨ@��@�+@���@���@�=q@���@��h@�x�@�x�@�x�@�p�@�%@��@��`@���@�j@�Z@�A�@���@�\)@�;d@�"�@�"�@��@�
=@�@��@��@���@���@�+@��@���@���@���@���@�ff@�=q@�-@��@�{@�J@��T@���@��T@��@��@��-@�V@��`@��@�1'@��@��
@��w@��@�dZ@�C�@�@��!@�ff@�E�@�$�@��@��^@���@�hs@�X@�7L@���@��@��/@�:@v�@l�@e�D@\~(@T�I@P��@H��@@D�@:J@/�m@)�M@&kQ@"@��@A�@$@!@  @aG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�A���A� �A���A�1A�t�A��A��yA���A��\A���A� �A��A��uA�O�A���A�JA���A��DA�$�A�A���A�~�A�v�A�1'A�"�A�33A��A���A�dZA��mAç�A�"�A�bNA�+A�ffA�jA�x�A�{A��\A�VA�O�A��FA��A��A�/A���A�~�A��\A���A�x�A�bA�
=A�z�A�VA���A���AΧ�A� �A�$�A��DA��A���A��A��jA�{A�hsA���A��mA��+A��9A�oA�ƨA���A��7A�A���A���A�A�A�A���A�C�A�A�7LAA�ĜA�1'A�z�A�\)A�5?Aǉ7A�ZA���AӮAʸRA�(�A�`BA�bAơ�AǸRAɕ�A���A�{A�`BA�Q�A�ffAȓuA��A�dZA�;dA�z�A�hsA���A�x�A��-A��9A���A��wA�O�A��A�bNA���A�=qA�l�AΑhA�E�A�=qA��TA�hsA˺^A�S�A�9XA���A�=qA�A�$�A�G�AhAƴ9A�x�AƅA�z�A��mAŸRA�hsA�%A�Q�A�dZAżjA�S�A�G�A��A�S�A˺^Aź^A�dZA�C�A��A��9AŋDAƛ�A�=qAя\A�-A�jA�O�A��
A�A��A���A���A�dZA��AΙ�A�G�A��9A�Q�Ả7A�I�A�7LA���A�l�A�9XA��/A�  Aź^A���A��AЏ\A�\)A�5?A�9XA��A�r�A���A�  A�/AӴ9A�E�A���A�C�A�O�A��mA�p�A��Aɏ\A͟�A��A��AȑhA���Aϲ-Aѕ�A�^5A�t�A�r�A�A�l�A�jA�ffA�hsA�7LA��`A�jA�ffA�jA�dZA�l�A�VA�S�A���A�ffA�JA��A��A�ZA�JA��A�dZA�\)A�n�A�v�A�l�A�p�A�l�A�A�x�A�ffA�`BA��A�hsA�hsA�v�A�n�Aҝ�A�n�A�r�A�hsA�jA�bNA˴9A�ffA�hsA�VA�`BA�`BA�VA�\)A�^5A�ZA��/A�bNA�hsA�hsA�hsA�jA�hsA�\)A�`BA�^5A�Q�A�Q�A��/A�XA�XA�^5A�bNA�XA�Q�A��A��mA�E�A�M�A�5?A�ZA��
A�Q�A�\)A�`BA�x�A�(�A�VA�ZA���A�XA�&�A�XA�K�A��#A�Q�A�;dA�VA�S�A�VA�$�A�Q�A�Q�A�O�A�Q�A�Q�A�VA�VA�VA�S�A�S�A�S�A�K�A��A��HA���A�33A�I�A�G�A�r�A�v�Aͣ�Aҕ�A�Q�A�K�A�Q�A�O�A�M�A�K�A�ĜA�M�A�S�A�O�A�K�A�I�A�ƨA�ĜA�O�A�%A�M�A�+A�A�A�M�A�=qA�=qA�ƨA��yA�A�C�A�{AҋDA�M�A�A�A�K�A�I�A�A�A��;A�E�A�ffA�dZA�C�AхA�C�A�ĜA�A�A�=qA�E�A�/A�=qA�A�A�=qA�C�A�~�A�I�A�=qA�;dA�E�A�C�A�^5A�K�A�M�AӑhA���A�7LAӶFA�;dA�M�A�
=A�I�A�"�A�-A�1'A�7LA�M�A�O�A�dZA�Q�A���A�M�A�Q�A�bNA�XA�$�A�9XA�bNA�\)A�;dA�O�A�K�A�VA�S�A��A�ZA�5?A�C�A�S�A�A�A�S�A�1'AΩ�A�XA�E�A�7LA�VA�33Aϝ�A�M�A�+A�G�A�G�A�VA�9XA�;dA�1'A�1A�x�A�G�A�G�A�5?A�5?A�(�Aӝ�A�?}A�x�A�33A�G�A��A�K�AӋDA�C�A�E�AΝ�A�  A�Q�A�(�A�C�A�XA�S�A�XA�I�A�K�A�dZA�Q�A�C�A�7LA�S�A�K�A�S�A�XA�XA�ffA�9XA�`BA�XA�"�A�x�A˺^A�l�A�ȴA��A�ZA�XA�VA�M�A�Q�A�XA�M�A�VA�ZA�l�A�ffA�l�A�jA�p�A�t�A�r�A�p�A�l�A�p�A�t�A�p�A�p�A�r�A�x�A�x�A�z�A�v�A�v�A�p�A�p�A�z�AԓuAԉ7Aԙ�Aԝ�Aԟ�Aԝ�Aԝ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԝ�Aԝ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԡ�Aԝ�Aԟ�Aԝ�Aԝ�Aԝ�Aԝ�Aԝ�Aԝ�Aԝ�Aԛ�Aԝ�Aԝ�Aԝ�Aԟ�Aԟ�Aԝ�Aԛ�Aԝ�Aԟ�Aԝ�Aԟ�Aԝ�Aԟ�Aԝ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԡ�Aԡ�Aԥ�Aԟ�Aԣ�Aԣ�Aԡ�Aԣ�Aԡ�Aԡ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԡ�Aԣ�Aԣ�Aԥ�Aԥ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԡ�Aԡ�Aԡ�Aԣ�Aԣ�Aԡ�Aԡ�Aԡ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԡ�Aԣ�Aԥ�Aԣ�Aԥ�Aԥ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԧ�Aԥ�Aԧ�Aԥ�Aԥ�Aԥ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�@���@���@���@�@�@�@��^@��^@��^@��^@��^@��-@��-@��-@��-@���@��-@���@���@���@���@��-@���@���@���@��h@��h@��7@��@�x�@�p�@�p�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�`B@�X@�`B@�`B@�`B@�X@�X@�X@�X@�X@�O�@�X@�O�@�O�@�O�@�O�@�O�@�G�@�?}@�7L@�7L@�&�@�/@�/@�/@�&�@��@�V@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��`@��/@��`@��`@��/@��/@��/@��/@���@��/@���@��/@���@��/@���@���@���@���@���@���@���Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԝ�Aԝ�Aԟ�Aԝ�Aԝ�Aԝ�Aԟ�Aԡ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԡ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԣ�Aԥ�Aԥ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԥ�@���@���@���@�@�@�@��^@��^@��^@��-@��^@��-@��-@��-@��-@���@���@��-@���@���@���@���@���@���@���@��h@��7@��@�x�@�x�@�p�@�p�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�`B@�`B@�`B@�X@�`B@�X@�X@�X@�X@�X@�X@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�?}@�7L@�7L@�/@�&�@�&�@�&�@�&�@�&�@��@���@���@���@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��`@��`@��`@��`@��`@��`@��/@��/@��/@��/@��/@��/@��/@���@���@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԡ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԡ�AԍPA�r�A�bNA�1A�E�A�|�A�  A��`A̶FA��;A˟�A���Aȕ�A�33A�hsA��yA�bA��A�&�A��FA���A���A�5?A��9A��`A���A��
A�$�A��A�(�A�bNA��yA�
=A�\)A�M�A�XA�A���A���A�oA�  A���A��yA��\A���A���A�ffA��A��`A��9A��+A��hA��A��hA�-A�?}A��A�n�A�7LA�oA�ȴA��A���A�$�A�n�A���A���A�/A��DA��A�JA��A�E�A��A�?}A�A��9A�(�A���A��FA���A��A���A���A���A�G�A~ffA|��A|ȴA{��Az�!AyoAw��AvȴAu��At �Ar1Aq
=Anv�Al~�Ak�hAiS�Ag?}Af�AedZAdZAahsA`(�A^�A]/A\1AYdZAV��AU%AS|�ARjAQ�
AP��AN��AK\)AI%AGt�AE�ADVAB��AB�AA�A?��A="�A;�A;A9ƨA7+A6VA6  A5�mA5�;A5�#A5�
A5��A4�\A3`BA3�A3�A3
=A2ȴA1�
A1�A1�A1?}A1VA0��A0ZA.jA,A*�`A(��A&ȴA&ffA&A%|�A%oA#%A!�PA!7LA�TA1A �A��A1'A��A33AM�A\)A��A��AM�A��A��A|�A%A~�A
�RAZAjA�A��A
=A�jA1A�FA��A�7AhsAG�A��A�mA �`A �A J@�o@�E�@��@�~�@�Z@�v�@��7@��u@�A�@�!@�9X@��@�?}@��@웦@�(�@�@�;d@ꟾ@��@�J@�@�p�@��@�1@睲@���@�9@�I�@��@�K�@�V@�@�I�@�$�@�b@�J@�(�@֧�@�-@ԛ�@�J@���@�z�@ύP@ΰ!@͉7@̓u@�A�@���@�|�@�ff@�hs@ȓu@��@Ǯ@ǥ�@ǥ�@��@ź^@��/@��@�|�@���@�@�E�@�Ĝ@�ƨ@��;@��m@��+@��^@��7@�%@��9@�bN@�I�@�I�@�A�@�b@�33@�r�@���@��@�hs@���@��F@���@���@�M�@���@�V@�bN@��P@��H@�@�Ĝ@�Q�@�b@�\)@�ff@���@��^@�?}@�z�@��;@��@��P@�;d@��H@��T@�Ĝ@��@�r�@�(�@�  @��w@�"�@��+@�5?@��h@��j@�1'@�o@�^5@�`B@�1'@�C�@��!@�n�@�^5@�V@�E�@�-@�J@���@�`B@�/@�&�@��@��@��@�V@��/@���@�r�@� �@��F@��@�=q@��@��@��T@���@��^@���@��7@�p�@�?}@�%@��j@��@�r�@�bN@�I�@�9X@�ƨ@�t�@�@��y@��@��@���@��!@��\@�$�@�?}@�%@��u@�(�@���@��F@�+@��y@���@���@���@��\@��\@�=q@�$�@��#@�X@���@�Z@� �@�b@��m@�ƨ@��@�+@���@���@�=q@���@��h@�x�@�x�@�x�@�p�@�%@��@��`@���@�j@�Z@�A�@���@�\)@�;d@�"�@�"�@��@�
=@�@��@��@���@���@�+@��@���@���@���@���@�ff@�=q@�-@��@�{@�J@��T@���@��T@��@��@��-@�V@��`@��@�1'@��@��
@��w@��@�dZ@�C�@�@��!@�ff@�E�@�$�@��@��^@���@�hs@�X@�7L@���@��G�O�@�:@v�@l�@e�D@\~(@T�I@P��@H��@@D�@:J@/�m@)�M@&kQ@"@��@A�@$@!@  @aG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�A���A� �A���A�1A�t�A��A��yA���A��\A���A� �A��A��uA�O�A���A�JA���A��DA�$�A�A���A�~�A�v�A�1'A�"�A�33A��A���A�dZA��mAç�A�"�A�bNA�+A�ffA�jA�x�A�{A��\A�VA�O�A��FA��A��A�/A���A�~�A��\A���A�x�A�bA�
=A�z�A�VA���A���AΧ�A� �A�$�A��DA��A���A��A��jA�{A�hsA���A��mA��+A��9A�oA�ƨA���A��7A�A���A���A�A�A�A���A�C�A�A�7LAA�ĜA�1'A�z�A�\)A�5?Aǉ7A�ZA���AӮAʸRA�(�A�`BA�bAơ�AǸRAɕ�A���A�{A�`BA�Q�A�ffAȓuA��A�dZA�;dA�z�A�hsA���A�x�A��-A��9A���A��wA�O�A��A�bNA���A�=qA�l�AΑhA�E�A�=qA��TA�hsA˺^A�S�A�9XA���A�=qA�A�$�A�G�AhAƴ9A�x�AƅA�z�A��mAŸRA�hsA�%A�Q�A�dZAżjA�S�A�G�A��A�S�A˺^Aź^A�dZA�C�A��A��9AŋDAƛ�A�=qAя\A�-A�jA�O�A��
A�A��A���A���A�dZA��AΙ�A�G�A��9A�Q�Ả7A�I�A�7LA���A�l�A�9XA��/A�  Aź^A���A��AЏ\A�\)A�5?A�9XA��A�r�A���A�  A�/AӴ9A�E�A���A�C�A�O�A��mA�p�A��Aɏ\A͟�A��A��AȑhA���Aϲ-Aѕ�A�^5A�t�A�r�A�A�l�A�jA�ffA�hsA�7LA��`A�jA�ffA�jA�dZA�l�A�VA�S�A���A�ffA�JA��A��A�ZA�JA��A�dZA�\)A�n�A�v�A�l�A�p�A�l�A�A�x�A�ffA�`BA��A�hsA�hsA�v�A�n�Aҝ�A�n�A�r�A�hsA�jA�bNA˴9A�ffA�hsA�VA�`BA�`BA�VA�\)A�^5A�ZA��/A�bNA�hsA�hsA�hsA�jA�hsA�\)A�`BA�^5A�Q�A�Q�A��/A�XA�XA�^5A�bNA�XA�Q�A��A��mA�E�A�M�A�5?A�ZA��
A�Q�A�\)A�`BA�x�A�(�A�VA�ZA���A�XA�&�A�XA�K�A��#A�Q�A�;dA�VA�S�A�VA�$�A�Q�A�Q�A�O�A�Q�A�Q�A�VA�VA�VA�S�A�S�A�S�A�K�A��A��HA���A�33A�I�A�G�A�r�A�v�Aͣ�Aҕ�A�Q�A�K�A�Q�A�O�A�M�A�K�A�ĜA�M�A�S�A�O�A�K�A�I�A�ƨA�ĜA�O�A�%A�M�A�+A�A�A�M�A�=qA�=qA�ƨA��yA�A�C�A�{AҋDA�M�A�A�A�K�A�I�A�A�A��;A�E�A�ffA�dZA�C�AхA�C�A�ĜA�A�A�=qA�E�A�/A�=qA�A�A�=qA�C�A�~�A�I�A�=qA�;dA�E�A�C�A�^5A�K�A�M�AӑhA���A�7LAӶFA�;dA�M�A�
=A�I�A�"�A�-A�1'A�7LA�M�A�O�A�dZA�Q�A���A�M�A�Q�A�bNA�XA�$�A�9XA�bNA�\)A�;dA�O�A�K�A�VA�S�A��A�ZA�5?A�C�A�S�A�A�A�S�A�1'AΩ�A�XA�E�A�7LA�VA�33Aϝ�A�M�A�+A�G�A�G�A�VA�9XA�;dA�1'A�1A�x�A�G�A�G�A�5?A�5?A�(�Aӝ�A�?}A�x�A�33A�G�A��A�K�AӋDA�C�A�E�AΝ�A�  A�Q�A�(�A�C�A�XA�S�A�XA�I�A�K�A�dZA�Q�A�C�A�7LA�S�A�K�A�S�A�XA�XA�ffA�9XA�`BA�XA�"�A�x�A˺^A�l�A�ȴA��A�ZA�XA�VA�M�A�Q�A�XA�M�A�VA�ZA�l�A�ffA�l�A�jA�p�A�t�A�r�A�p�A�l�A�p�A�t�A�p�A�p�A�r�A�x�A�x�A�z�A�v�A�v�A�p�A�p�A�z�AԓuAԉ7Aԙ�Aԝ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԝ�Aԝ�Aԟ�Aԝ�Aԝ�Aԝ�Aԟ�Aԡ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԡ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԣ�Aԥ�Aԥ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԥ�@���@���@���@�@�@�@��^@��^@��^@��-@��^@��-@��-@��-@��-@���@���@��-@���@���@���@���@���@���@���@��h@��7@��@�x�@�x�@�p�@�p�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�`B@�`B@�`B@�X@�`B@�X@�X@�X@�X@�X@�X@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�?}@�7L@�7L@�/@�&�@�&�@�&�@�&�@�&�@��@���@���@���@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��`@��`@��`@��`@��`@��`@��/@��/@��/@��/@��/@��/@��/@���@���@���@���@���@���@���@���@���Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԝ�Aԝ�Aԟ�Aԝ�Aԝ�Aԝ�Aԟ�Aԡ�Aԟ�Aԝ�Aԟ�Aԟ�Aԝ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԡ�Aԟ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԡ�Aԟ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԟ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԡ�Aԡ�Aԟ�Aԡ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԡ�Aԡ�Aԡ�Aԣ�Aԡ�Aԡ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԡ�Aԣ�Aԣ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԣ�Aԥ�Aԥ�Aԣ�Aԣ�Aԥ�Aԣ�Aԣ�Aԣ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԥ�Aԣ�Aԣ�Aԥ�Aԣ�Aԥ�Aԥ�@���@���@���@�@�@�@��^@��^@��^@��-@��^@��-@��-@��-@��-@���@���@��-@���@���@���@���@���@���@���@��h@��7@��@�x�@�x�@�p�@�p�@�p�@�p�@�p�@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�hs@�`B@�`B@�`B@�`B@�`B@�X@�`B@�X@�X@�X@�X@�X@�X@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�?}@�7L@�7L@�/@�&�@�&�@�&�@�&�@�&�@��@���@���@���@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��@��@��@��`@��`@��`@��`@��`@��`@��/@��/@��/@��/@��/@��/@��/@���@���@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��)>�L0>J͟>�Կ=>ߤ=^�z=c�P>��=���=پM?/o=��=�v=�w=�a=4�4=VC=M�=i��=r{�=P=�iY=��T=�2�>���?��Q=�$�> ��@�6=yЦ=�O�=���=�)>�@A�=�\=ՆD>-b9@���=sջ=�B�>b�\=I[�=O��=T�=Y@y=�f{=�r\=���=���>��D?�]?� �=q��=���=��h=��3=��>Q��@���<��@<�Z�<�
= �Q<�Zq<�==MU=�=��=�=1P�=;D�=DG�=m\�=e�=U{�=��=�f�=�B�=���?�w@��X=]�z=I��=�W�=���>k�?j!�@��r@��N=��=�}�>5�m@�9�@o�=0�|=T6�=f =�v�=�)�=�K�=�(�@8�=6�=T��=��F=��L=�o>�@�L�=���=�P>qv@gv�=Ǐ=��=0U2=C��=L/�=J�=o�=�;=�Z2?.�m>C@��"?	�Z=� �=�m>��@M�>F��=!��= F�=/D�=.h^=CL=�ѷ=���=�x-=�nY=���=�ֶ=��=�I�>-�S@9&�=��=�,?d�@�t ?��=��> $�?�%�=<��=H�9=X�(=���=�+=�_F=�,|?�H�=�ɛ>+��@���?��k@Q��@��!=���=���>Gi�?!�?C@��>> ��>��?�|�?�E�?
�r@��=�4�>��?�??Q[W=�"=�O�=긦?�S@��c@}Wi?�8>S�#@[&=�sm=��<>��?�@%=��7=��8?* �@���=�r�>Fk�@c�?5F>E�@��?&P>�P@k�@��w?t}�@��~>��a>7`�>��@�c�@��@��)@���@��7?=8G@��D@���@��@��K@��1@��,?�sm@�+�@��\@��)@���?ud0@���?և�?^J8@��@���@��o@��@���@���@��@��#?h&@��7@�)�?�@�ܜ@�Ԫ@	��@��@�Y`@��@��N@��@��@��>^�@�2@���@���@���@��u@�Ц@���@��q@�ɛ>�@��@��s@�׈@�ܜ@�ܜ@���@�˼@��@��B@��@�*�?*XO@�Ɠ@��@���@��c@�˧@��n@���>%�$@���@���@,�@���@�@��@��@�� >:)�?��@��@��I@���@���@���@���@��?
��@��9@��@���@��@���@��&@�+,@���@���@��@���@���@���@���@��[@���@���@��@��y?�~?�@���@��@���@]��>7h�>�?�Ĝ@��z@���@���@���@��@���@��X@��6@���@��F@��[@��9>\@5�z@���?���@��@��&@���@��@��@��t@u�j?�c@ZG@���@���?���@��~@��4@���@���@��>0��@���@��@��Y@���@(��@���@���>)��@���@���@U��?2d@�(@��y@��A@`�@���@��@�rG@��`@���?GT�@���@��X@�9�@f��@���@�F�@�RT@���@���@��.@��@���@���?0ʂ@���@���@���@��?>r@��M@��A@��@���@��1@��@���@���@���@��?@�Ȋ@��w@��@8�I@��@��c@��@��R@��@���@�`k?�@��@��4@�Ԫ@�ȴ@��@>rL�@�զ@�ł@���@��q@��@���@��
@\0+@��@�<@���@��;@���@���@���@e�c@�΅@�1@���@���@�� @��@[�b@��<@��j@0�@ "�@��@��@��@�	�@��@��C@��U@��@�Y`@��e@���>��@�
(@�:@�@�@�^@�M@�@y@��@�G@��@0�?UH�@�
@���@[��@�
@�@��@�@��@�4@��@��@�U@�Y@��@��@�P@��@�a@��@�a@�L@��@��@��@��@�d@�@� �@�!�@�"�@�"�@�&@�*@�,�@�,�@�(�@�-�@�-�@�-�@�.^@�-�@�-�@�.^@�/@�.�@�.�@�.�@�.�@�.�@�.�@�/@�/o@�/@�/@�/@�/�@�0+@�/�@�/@�/�@�/@�/@�/@�/�@�/�@�/�@�/0@�/�@�/�@�/�@�/�@�/�@�/�@�0@@�0@@�0@@�0@@�0�@�0�@�0�@�0�@�0�@�0�@�0�@�0�@�1Q@�0�@�1Q@�1Q@�/�@�1�@�1�@�1Q@�2@�2@�2@�2@�1�@�2v@�1�@�2@�2v@�2v@�2�@�2�@�2�@�2�@�33@�33@�3�@�33@�33@�3�@�33@�3�@�3�@�3�@�3�@�4Y@�3�@�4@�4Y@�4Y@�4Y@�4Y@�4Y@�4�@�4Y@�4�@�4�@�4�@�5@�4�@�5@�5i@�5i@�5i@�5i@�5i@�5�@�5�@�6&@�5�@�5�@�5�@�6�@�6&@�6&@�6&@�6�@�6�@�77@�6�@�77@�77@�7v@�77@�8	@�7�@�7L@�7�@�7�@�8	@�8	@�8�@�8�@�8\@�8�@�8	@�8	@�8	@�8\@�8\@�8�@�8q@�8q@�8�@�8q@�8�@�8�@�9@�8�@�8�@�9@�9�@�9�@�9@�9@�9X@�9@�9�@�9�@�9�@�9�@�9�@�9�@�9�@�:?@�9�@�:?@�:?@�:@�:?@�:�@�:�@�:�@�:�@�:�@�:�@�;d@�;�@�;d@�;�@�;�@�<`@�<�@�<!@�<!@�<u@�<u@�<�@�<�@�=2@�=2@P��@P�'@P�@P��@P�/@P��@P��@P��@P��@P��@P�4@P��@P��@P�8@P��@P�8@P�8@P��@P�8@P�8@P��@P�f@P�@P��@P��@P�@P��@P�(@P�,@P��@P�1@P�1@P�1@P��@P�@P��@P��@P��@P��@P�_@P�_@P�_@P��@P��@P�@P�c@P��@P�@P�c@P�9@P�@P�c@P�@P��@P��@P�h@P�@P��@P��@P�l@P�l@P�@P�@P�K@P�%@P��@P�*@P��@P��@P�X@P�@P�@P��@P�@P�"@P�v@P�v@P�v@P�@P��@P�@P�@P�@P�@P�r@P�r@P�@P�r@P�r@P��@P�r@P��@P�@P��@P��@P�r@P��@P�L@P�v@P��@P�{@P��@P��@P��@P�Q@P�Q@P��@P�Q@P��@P��@P�Q@P�Q@P��@P�{@P�Q@P�Q@P�Q@P�Q@P�{@P��@�3@�3@�3@�3r@�3�@�3]@�3r@�3r@�3�@�3�@�3�@�4@�3�@�3�@�4�@�4�@�4�@�5�@�4�@�3�@�3�@�3�@�4@�4D@�3�@�3�@�4/@�4Y@�4�@�4�@�4�@�4�@�4�@�4�@�4�@�5@�5+@�5T@�5T@�5~@�5~@�5�@�5�@�5�@�5�@�5�@�4�@�6&@�6@�6;@�6P@�6z@�6e@�6z@�6�@�6z@�6�@�6�@�6�@�6�@�6�@�7@�6�@�7"@�7L@�77@�7�@�7v@�7�@�7�@�7�@�7�@�7�@�7�@�8	@�7�@�8	@�8	@�82@�8G@�8\@�8G@�8q@�8�@�8�@�8�@�8�@�8�@�9@�8�@�9@�8�@�9.@�9.@�9.@�9m@�9m@�9m@�9�@�9�@�9�@�9�@�9�@�:*@�:*@�:i@�:?@�:i@�:~@�:�@�:�@�:�@�;%@�;:@�;:@�;d@�;%@�;y@�;d@�;y@�;�@�;�@�<!@�;�@�<`@�<`@�<�@�<`@�<6@�;�@�;�@�<`@�<`@�<`@�<`@�<`@�<u@�<u@�<u@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�=@�=@�=@�=\@�=\@�=2@�=@�=2@�=2@�=�@�=�@�=�@�=q@�=q@�=�@�=�@�=�@�=�@�=�@�=�@�>@�>W@�>�@�>�@�>�@�>�@�>�@�>�@�>�@�>�@�>�@�>�@�?@�>�@�?@P�Q@P�Q@P�+@P��@P�Y@P��@P��@P��@P��@P�
@P�4@P��@P��@P��@P��@P��@P�8@P�8@P�@P�8@P�@P��@P�k@P��@P�@P�s@P�$@P��@P��@P�[@P��@P�_@P�_@P�5@P�@P��@P��@P��@P��@P��@P��@P��@P�9@P�9@P�@P��@P��@P��@P�h@P��@P��@P��@P�h@P�@P�>@P�>@P��@P��@P��@P�B@P�B@P�@P��@P�u@P�y@P��@P� @P�.@P�X@P�@P�@P��@P�@P��@P��@P��@P��@P��@P�'@P��@P��@P��@P��@P��@P��@P�v@P��@P�L@P�"@P�L@P��@P��@P�v@P��@P�@P�v@P��@P�Q@P��@P�@P�@P��@P��@P��@P�+@P�U@P��@P��@P�@P�@P�+@P��@P�/@P�/@P�+@P��@P��@P�@P��@P�UG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    3444444444444444444444444444344444444434444444444444444444434444444444444444444444344444433444334444444444444434443444444444443444444444444444444444444344444444444444344344444344444344444444334434444444344444344434344433333433333343333434433333333433433433333334333333333433333333333433333334334333334433333334333333333333333333344333344433333333333344343333333443343333343433433433343333333334333333333333343333433333433333334333333343333343333333334333333343333333443333333333343333333333443333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��*G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��SG�O�G�O�G�O�G�O�G�O�G�O�@��s@��OG�O�G�O�G�O�@�9�@o�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�L�G�O�G�O�G�O�@gv�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@��"G�O�G�O�G�O�G�O�G�O�@��>G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��c@}WjG�O�G�O�@[&G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@��tG�O�@��|G�O�G�O�G�O�@�c�@��@��(@���@��8G�O�@��E@���@��@��K@��4@��(G�O�@�+�@��V@��*@���G�O�@���G�O�G�O�@���@���@��p@��@���@���@��
@��"G�O�@��8@�)�G�O�@�ܚ@�ԧG�O�@��@�Yb@��@��O@��
@��@��G�O�@�2@���@���@���@��u@�Х@���@��o@�ɞG�O�@��@��w@�׈@�ܞ@�ܝ@���@�˾@��@��@@��@�*�G�O�@�ƒ@��~@���@��b@�˩@��m@���G�O�@���@���G�O�@���@�@��@��@��"G�O�G�O�@��@��L@���@���@���@���@��G�O�@��:@��@���@��@���@��'@�+-@���@���@��@���@���@���@���@��[@���@���@��@��zG�O�G�O�@���@��@���@]��G�O�G�O�G�O�@��x@���@���@���@��@���@��X@��5@���@��F@��]@��6G�O�G�O�@���G�O�@��@��&@���@��@��@��u@u�pG�O�G�O�@���@���G�O�@��@��4@���@���@��G�O�@���G�O�@��X@���G�O�@���@���G�O�@���@���@U��G�O�@�(@��y@��C@`�@���@��@�rE@��_@���G�O�@���@��[@�9�@f��@���@�F�@�RW@���@���@��-@��@���@���G�O�@���@���@���@��G�O�@��K@��C@��@���@��1G�O�@���@���@���@��>@�Ȋ@��v@��G�O�@��@��f@��@��S@��@���@�`jG�O�@��@��6@�Ԫ@�ȶ@��DG�O�@�գ@�ł@���@��s@��@���@��
@\00@��G�O�@���@��9@���@���@���@e�e@�ΆG�O�@���@���@��@��@[�h@��;@��hG�O�G�O�@��@��@��@�	�@��@��A@��U@��@�Yd@��g@���G�O�@�
*@�<@�@�@�`@�L@�@|@��@�I@��G�O�G�O�@�@���@[��@�@�@��@�@��@�4@��@��@�R@�W@��@��@�T@��@�`@��@�b@�L@��@��@��@��@�e@�@� �@�"@�"�@�"�@�&@�*@�,�@�,�@�(�@�-�@�-�@�-�@�.a@�3@�3@�3!@�3u@�3�@�3c@�3r@�3r@�3�@�3�@�3�@�4@�3�@�3�@�4�@�4�@�4�@�5�@�4�@�3�@�3�@�3�@�4@�4C@�3�@�3�@�4.@�4Z@�4�@�4�@�4�@�4�@�4�@�4�@�4�@�5@�5,@�5T@�5U@�5�@�5�@�5�@�5�@�5�@�5�@�5�@�4�@�6)@�6@�6;@�6P@�6y@�6d@�6|@�6�@�6z@�6�@�6�@�6�@�6�@�6�@�7@�6�@�7#@�7L@�77@�7�@�7v@�7�@�7�@�7�@�7�@�7�@�7�@�8@�7�@�8
@�8@�82@�8F@�8]@�8G@�8r@�8�@�8�@�8�@�8�@�8�@�9@�8�@�9@�8�@�9.@�9.@�90@�9n@�9j@�9o@�9�@�9�@�9�@�9�@�9�@�:+@�:+@�:j@�:B@�:j@�:@�:�@�:�@�:�@�;&@�;:@�;<@�;g@�;(@�;}@�;e@�;v@�;�@�;�@�<%@�;�@�<b@�<^@�<�@�<^@�<4@�;�@�;�@�<e@�<b@�<b@�<b@�<b@�<v@�<v@�<v@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�=@�=	@�=@�=[@�=_@�=2@�="@�=5@�=2@�=�@�=�@�=�@�=q@�=q@�=�@�=�@�=�@�=�@�=�@�=�@�=�@�>V@�>�@�>�@�>�@�>�@�? @�>�@�>�@�>�@�>�@�? @�?@�>�@�?@P�R@P�R@P�-@P��@P�Z@P��@P��@P��@P��@P�
@P�5@P��@P��@P��@P��@P��@P�8@P�:@P�@P�:@P�@P��@P�n@P��@P�@P�p@P�&@P� @P��@P�]@P��@P�^@P�`@P�8@P�@P��@P��@P��@P��@P��@P��@P��@P�@@P�=@P�@P��@P��@P��@P�f@P��@P��@P��@P�e@P�@P�;@P�;@P��@P��@P��@P�C@P�B@P�@P��@P�v@P�z@P��@P�@P�0@P�[@P�@P�@P��@P�@P��@P��@P��@P��@P� @P�*@P��@P��@P��@P��@P��@P��@P�v@P��@P�K@P�"@P�K@P��@P��@P�v@P��@P�@P�u@P��@P�S@P��@P�~@P��@P��@P��@P��@P�*@P�U@P��@P��@P� @P� @P�0@P��@P�-@P�2@P�*@P��@P��@P�@P��@P�S@�3@�3@�3!@�3u@�3�@�3c@�3r@�3r@�3�@�3�@�3�@�4@�3�@�3�@�4�@�4�@�4�@�5�@�4�@�3�@�3�@�3�@�4@�4C@�3�@�3�@�4.@�4Z@�4�@�4�@�4�@�4�@�4�@�4�@�4�@�5@�5,@�5T@�5U@�5�@�5�@�5�@�5�@�5�@�5�@�5�@�4�@�6)@�6@�6;@�6P@�6y@�6d@�6|@�6�@�6z@�6�@�6�@�6�@�6�@�6�@�7@�6�@�7#@�7L@�77@�7�@�7v@�7�@�7�@�7�@�7�@�7�@�7�@�8@�7�@�8
@�8@�82@�8F@�8]@�8G@�8r@�8�@�8�@�8�@�8�@�8�@�9@�8�@�9@�8�@�9.@�9.@�90@�9n@�9j@�9o@�9�@�9�@�9�@�9�@�9�@�:+@�:+@�:j@�:B@�:j@�:@�:�@�:�@�:�@�;&@�;:@�;<@�;g@�;(@�;}@�;e@�;v@�;�@�;�@�<%@�;�@�<b@�<^@�<�@�<^@�<4@�;�@�;�@�<e@�<b@�<b@�<b@�<b@�<v@�<v@�<v@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�<�@�=@�=	@�=@�=[@�=_@�=2@�="@�=5@�=2@�=�@�=�@�=�@�=q@�=q@�=�@�=�@�=�@�=�@�=�@�=�@�=�@�>V@�>�@�>�@�>�@�>�@�? @�>�@�>�@�>�@�>�@�? @�?@�>�@�?@P�R@P�R@P�-@P��@P�Z@P��@P��@P��@P��@P�
@P�5@P��@P��@P��@P��@P��@P�8@P�:@P�@P�:@P�@P��@P�n@P��@P�@P�p@P�&@P� @P��@P�]@P��@P�^@P�`@P�8@P�@P��@P��@P��@P��@P��@P��@P��@P�@@P�=@P�@P��@P��@P��@P�f@P��@P��@P��@P�e@P�@P�;@P�;@P��@P��@P��@P�C@P�B@P�@P��@P�v@P�z@P��@P�@P�0@P�[@P�@P�@P��@P�@P��@P��@P��@P��@P� @P�*@P��@P��@P��@P��@P��@P��@P�v@P��@P�K@P�"@P�K@P��@P��@P�v@P��@P�@P�u@P��@P�S@P��@P�~@P��@P��@P��@P��@P�*@P�U@P��@P��@P� @P� @P�0@P��@P�-@P�2@P�*@P��@P��@P�@P��@P�SG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    3444444444444444444444444444344444444434444444444444444444434444444444444444444444344444433444334444444444444434443444444444443444444444444444444444444344444444444444344344444344444344444444334434444444344444344434344433333433333343333434433333333433433433333334333333333433333333333433333334334333334433333334333333333333333333344333344433333333333344343333333443343333343433433433343333333334333333333333343333433333433333334333333343333343333333334333333343333333443333333333343333333333443333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�P�9�P�9�P�9�P�9�Q9�P�9�P�9�P�9�Q,9�QP9�QP9�Qz9�Qe9�Qc9�R9�R9�R,9�S9�R9�Qe9�Qe9�Qc9�Q�9�Q�9�Qf9�QS9�Q�9�Q�9�Q�9�R9�R9�RB9�R?9�R@9�RV9�Ri9�R�9�R�9�R�9�R�9�R�9�R�9�S9�S29�S09�S[9�Q�9�S�9�Sm9�S�9�S�9�S�9�S�9�S�9�S�9�S�9�S�9�S�9�T9�T!9�T%9�T^9�T39�Tr9�T�9�T�9�T�9�T�9�T�9�U9�U99�U9�U9�U)9�UK9�U69�UO9�UQ9�Uu9�U�9�U�9�U�9�U�9�U�9�V9�V9�V 9�V9�V<9�V+9�V>9�V+9�Vf9�Vf9�Vh9�V�9�V�9�V�9�V�9�V�9�V�9�W9�W9�WW9�WW9�W�9�Wm9�W�9�W�9�W�9�W�9�X
9�XG9�XZ9�X\9�X�9�XI9�X�9�X�9�X�9�X�9�X�9�Y:9�X�9�Yu9�Yq9�Y�9�Yq9�YI9�Y9�X�9�Yw9�Yu9�Yu9�Yu9�Yu9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Y�9�Z)9�Z9�Z)9�Zb9�Zf9�Z;9�Z,9�Z>9�Z;9�Z�9�Z�9�Z�9�Zw9�Zw9�Z�9�Z�9�Z�9�Z�9�Z�9�Z�9�Z�9�[R9�[�9�[�9�[�9�[�9�[�9�[�9�[�9�[�9�[�9�[�9�\	9�[�9�\	9G3�9G3�9G2�9G29G1�9G1R9G1+9G19G19G0�9G0�9G0^9G09G09G09G/t9G/�9G/�9G/�9G/�9G/�9G.29G.9G-j9G,�9G,&9G*�9G)�9G(�9G(B9G'�9G'O9G'Q9G'*9G'9G&�9G&�9G&�9G&�9G&�9G&�9G&�9G&>9G&;9G&9G%�9G%�9G%�9G%m9G%�9G%�9G%�9G%l9G%9G%D9G%D9G$�9G$�9G$�9G$X9G$W9G$09G#�9G"�9G!�9G!9G H9G9G�9GW9GV9G-9G�9Gb9GZ9G�9GZ9G�9G�9GG9GQ9Gs9G�9G�9G�9G9Gb9G�9G�9G�9G>9G;9G9G:9G�9G9GJ9G�9G�9G29G49G�9G�9G�9G�9G9Gh9G�9G�9G�9G�9Gl9G�9G�9G�9G�9Gk9G�9G�9G	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B+B+B+B+B+B+B+B+B+B+B+B+B,B,B-B.B.B5?BbNB�hB�LBǮB�B�sB�B�B�B/Bx�B�BƨB�NB�B�B�sB�BB�;B�HB�sB�BB�BB�TB�ZB�HB�TB�;B�5B�)B�mB�`B�ZB��B��BÖB�!B��B��B��B��B��B��B��B�oB�\B�DB�=B�=B�\B}�Bs�Bo�BhsBZB/B$�B �B�B�B�BPB
=BB�B�ZB�/B�B�wB��Bq�B[#BM�BI�BG�BB�B8RB%�BbBB
�B
�B
�XB
��B
��B
�B
t�B
u�B
r�B
k�B
aHB
W
B
O�B
H�B
;dB
+B
�B
DB	��B	�mB	��B	ÖB	�LB	�B	��B	�+B	z�B	l�B	dZB	ZB	E�B	2-B	$�B	�B	�B	{B	oB	
=B	  B��B�B�B�`B�TB�BB�#B�B��B��B��BƨB�}B�qB�jB�jB�jB�dB�dB�^B�FB�3B�-B�-B�'B�!B�B�B�B�B�B��B��B��B��B�uB�PB�+B�B�B�B}�Bv�Bp�Bm�BgmB_;BZBVBQ�BL�BH�BF�BD�BB�B@�B=qB<jB;dB:^B9XB7LB5?B2-B2-B1'B0!B0!B/B0!B0!B/B/B/B.B.B-B-B-B-B,B+B+B)�B(�B(�B(�B(�B'�B&�B%�B%�B%�B$�B$�B$�B$�B#�B#�B$�B$�B$�B$�B$�B#�B#�B"�B"�B"�B"�B"�B!�B �B�B�B�B�B�B �B �B"�B'�B+B,B.B0!B33B6FB6FB6FB6FB:^B?}BH�BL�BO�BQ�BQ�BXBdZBe`BffBk�Bl�Bk�BiyBn�Bx�B� B�B�1B�DB�VB�uB��B��B��B��B��B��B��B��B��B�B�B�3B�?B�RB�XB�^B�jB�wBĜB��B��B��B�)B�;B�BB�ZB�B�B�B�B��B��B��B��B	B	B		7B	JB	JB	DB	
=B	
=B		7B	DB	uB	�B	�B	�B	�B	�B	%�B	&�B	%�B	&�B	)�B	+B	,B	,B	-B	.B	/B	2-B	7LB	8RB	9XB	9XB	9XB	:^B	:^B	;dB	<jB	>wB	B�B	G�B	Q�B	VB	W
B	YB	ZB	ZB	[#B	\)B	]/B	^5B	`BB	cTB	gmB	iyB	m�B	n�B	o�B	p�B	v�B	y�B	}�B	~�B	~�B	~�B	� B	�B	�B	�1B	�PB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�9B	�9B	�9B	�?B	�FB	�XB	�dB	�jB	�qB	�qB	�qB	�wB	�wB	�wB	�wB	��B	��B	��B	ÖB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�BB	�HB	�NB	�TB	�ZB	�`B	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
mB
�B
$B
!�B
)�B
.�B
6FB
="B
C{B
KDB
TB
WYB
[WB
_B
d�B
iB
m�B
r-B
v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	%�?��.?�9�@$�>n�>�O�>�6�@$�|>�8�?	5�@S�A>,ޠ>Ari>E<�>EP�>_�'>��3>}�>�Q">��3>�)>�(>ͿH?_�?�A;d�>��?E�eB��>���>�a�>�q>���?6�5AWgo>�W�?�Z?Y2�B$>�	�>��?���>z?�>��>��e>�E=>�?>��n>��?��?�~�@5̼@��{>�۶>��?��>�Cc?�4?��YB
�=��>Xm>x�>ٺ>�>��>7�>2�>0�4>=o>A��>[��>h e>t+0>��>�X:>��>>�b�>ᗢ>�
x?g�@P'B
շ>���>v�>�w�>ׯ�?"DR@�wB
�:B
�i>��?L�?b4�B
��A��V>Zn3>���>���>���?��>�YO?Q�Ais8>^t*>��v>���>�%>��?#�*A�ɯ? Nb>�A1?C��A�b>.%�>3��>Y؄>rh
>{3>y_�>�r~>�J�>�U�@oz�?8�^A�LF@52�>��z>�&_?%WyAdU�?�	5>F�q>E�>WO/>U��>p�>��>�I�>���?1�>��>�Y�>�m�>��?W��A�t�>е$>�Ǥ@� NBv@@Ů?��?G-<@�~>jQ�>x'�>��>�k�>���>�ȴ?G(A |�>�f0?V&�B��A
�A�yB �>�Z)?��?z0@YI�@SB�B
�?#?2��A5@�,�@8�B >��(?,��@G�@��d>��>ڢ?k�@)�_B
��Aü�A)��?��pA��>�$>��??]%A �!?Hw?uh@cҚBݨ?H�?{�kAV�@w��?vK�B<�@e��?/��ATB�A��@��B
�j?���?c�?E��B �B
��B
��B
��B@}~�B
�)B
��B
��B
��B
��B�@���B�xB
��BzA�jF@�ݒB
��A��@��`B
��B
�$B�B
��BB8B
��B�@�l�B
�bBZ�@D?dB
�gB
�9AH��B =A���B
�PB
�gB
��B
�GBc�?94�A�-[B
��B
��B
�6B
��B
�B
�	B
�B
�@	sCB
�SB
��B
��B
�WB
�B
�B
��B
��B
�.B
ԇA�m!@b��B
�B
�%B
�lB
�B
�HB
�B
�{?L�FB
��B
�LAd�yB
�MB�B
��B
��B
��?iX�A��B
֙B
��B
�iB
�kB
��B
�B
�s@6AWB
�B
��B
��B
�SB
�kB��B��B
ҔB
�lB
�B
ПB
�2B
�~B
�=B
סB
ۑB
��B
�B
�<@D^9@@R/B
��B
�B
�vA�o?h�R?%��A6��B
ͿB
��B
�CB
�&B
҇B
��B�B
�rB
�PB
ԢB
�?B
ٙ?�M#A��BB
ْ@��RB
��B<�B
�B
��B
�VB
�;A�F[A%�9AFbAB
��B
��A/B
�_B
�nB
��B
�DB
�p?\�1B
�xAZ'�B+B
��A{ B
��B�#?U.�B
�B
ǮA�)@o*cBǿB
��B
ċA�q�B
�,B
�2B
��B
��B
�m@�IB
��B
��A�1�A��CB�B
�MB
��B
�OB
�	B
��B
�4B
�4B
��@k�oB
�WB
�B
IB
�G@�`B
�B
�MB<B$BVAb�`B�B
�`B
��B
�B
�B
��B
��A�B>B
��B
��B
�B
�B�B
��B
�p@G.�B
��B
��B
��B
�^B
��?�63B
� B
��B
کB
�mB
�B
�(B
�A���B
�5A=P�B<B�B
��B
�B�A�pGB
�AM(B
�BB
�pBQA�>"B
�BA��qAl�B!�B�B&�B!&B$IB�B�B;B+_B{Bj?A�B#B"�B%�B%B(B&$A�B �BLB3�A�;�@��B Aۦ�A�i�B&�B&�B&*B(�B'�B'�B+B)�B(�B%�B(PB&�B'�B%�B%B' B&�B'�B(�B&�B'zB(�B)9B&�B))B)QB+�B+~B0�B4NB2�B)pB)�B(PB&�B%�B'B&�B%�B&�B&�B&�B&QB&QB'B&�B&)B'2B&�B&gB'B&PB&�B'�B&B&�B&vB&�B&�B&�B'WB'OB&�B&�B'�B'B'B'B&9B&DB'EB(B'-B&`B'iB&�B'�B&{B'�B'�B&�B&�B&�B&�B'�B&�B%&B'B&FB&�B'?B'7B&aB'B&�B'eB&�B&�B'EB'=B'xB'pB'{B&�B&�B&�B'B&�B&�B'�B&�B'CB'�B&�B'#B'qB'B')B'RB(B'�B'�B'"B'eB'	B(B'kB'[B'�B'B'sB'�B'�B&�B'�B'B'�B'�B&~B(sB&�B&�B(7B'B'�B'�B'BB'�B'�B'mB'�B'�B'�B'�B(=B'�B'{B'�B(tB'�B(�B(|B(aB'SB'�B'�B'�B'�B'�B'�B(/B'B'�B(B&�B'�B'�B(2B'B'B'MB'�B(aB(�B(�B(�B'�B(1B):B)2B(�B(	B(_B)B(�B(?B)HB({B'�B(kB(?B'�B(/B(�B(�B(�B(fB'�B(MB'�B(�B)B)nB)�B)�B)�B*�B*B)�B*JB*BB	�DB	�B	�7B	�B	�)B	�YB	��B	��B	�}B	�oB	�B	��B	��B	�:B	�B	�$B	�B	��B	��B	��B	�kB	�B	� B	��B	�>B	�B	�B	�B	�\B	�B	��B	��B	�B	�+B	�=B	��B	�B	��B	��B	�vB	�[B	�NB	��B	��B	��B	�WB	�B	�B	�"B	��B	�B	��B	��B	�zB	�NB	��B	��B	�`B	�8B	�B	��B	�B	��B	�IB	�hB	�.B	�B	�2B	�B	��B	�B	��B	�!B	�HB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�FB	��B	��B	��B	�{B	��B	��B	��B	�wB	�B	��B	�=B	�BB	��B	��B	��B	��B	��B	�"B	�B	��B	��B	�B	�RB	��B	��B	��B	��B	��B	�mB	�`B	�RB	�WB	��B*�B*|B+9B*�B*�B*�B+\B*�B*�B+�B*�B*�B*�B*�B+RB+7B+MB+OB+B*pB*`B+B+;B*�B*�B*�B+B*wB)�B*�B+LB*�B*�B+eB*�B*�B*�B*�B*�B*�B*�B*�B+B*MB+B*ZB)�B+.B+B*kB*nB+QB+.B+9B*~B+!B+,B*VB+:B+DB*oB*�B*rB*�B*�B+cB*�B+�B*�B+�B+B+�B*�B*�B+�B*�B*�B*�B*�B*�B*�B+�B*�B+!B+,B+B+B+�B+4B+B+B*<B+*B+"B*MB+CB+3B++B*�B*�B*�B*�B*�B*�B+|B+�B+B+�B*�B+�B+�B+B*|B+DB+<B+ZB+B+UB+2B+=B*�B+^B+�B*�B+B*�B+B+�B*�B+6B+B*�B*�B+mB*�B*�B+`B*�B+PB+eB+JB*�B+MB*xB+nB+fB+^B+VB+�B+wB+\B+gB+�B,MB+ZB+?B+JB+AB*�B+�B*�B+RB*�B*�B+XB+�B*�B+�B+iB+�B+B+-B+KB+UB+;B+kB+�B+.B+�B+�B+;B,B+#B+.B	��B	��B	��B	�B	�HB	��B	��B	�B	�}B	�$B	�B	��B	�B	�xB	�kB	��B	�B	��B	��B	��B	�B	�B	�]B	��B	�]B	��B	��B	�B	�0B	��B	�AB	��B	��B	�B	�B	�[B	�/B	�B	�B	��B	��B	�B	�iB	�`B	�4B	��B	��B	�B	�zB	�{B	�qB	�dB	�8B	��B	��B	��B	�~B	�qB	�8B	��B	��B	��B	�MB	�yB	��B	�.B	�B	��B	�B	��B	��B	�tB	�oB	�,B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�OB	��B	��B	��B	��B	��B	��B	��B	��B	�wB	��B	�B	�B	��B	��B	��B	��B	��B	�JB	�\B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�WB	�+B	�\B	�"B	�rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444444444444444444444444444344444444434444444444444444444434444444444444444444444344444433444334444444444444434443444444444443444444444444444444444444344444444444444344344444344444344444444334434444444344444344434344433333433333343333434433333333433433433333334333333333433333333333433333334334333334433333334333333333333333333344333344433333333333344343333333443343333343433433433343333333334333333333333343333433333433333334333333343333343333333334333333343333333443333333333343333333333443333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999B+B+B+B*�B+B+B+B+B+B*�B+B+B+B,B,B-B.B.B5=BbPB�iB�KBǮB�B�sB�B�B�B/Bx�B�BƥB�NB�B�B�tB�CB�9B�GB�tB�EB�@B�SB�XB�HB�SB�:B�9B�(B�nB�cB�XB��BʿBÙB�!B��B��B��B��B��B��B��B�oB�`B�DB�;B�>B�^B}�Bs�Bo�BhuBZB/B$�B �B�B�B�BOB
>B
B�B�ZB�.B�B�vB��Bq�B[$BM�BI�BG�BB�B8QB%�BcBB
�B
�B
�VB
��B
��B
�B
t�B
u�B
r�B
k�B
aFB
WB
O�B
H�B
;bB
+ B
�B
DB	��B	�lB	��B	ÖB	�JB	�B	��B	�+B	z�B	l�B	dYB	ZB	E�B	2*B	$�B	�B	�B	{B	oB	
=B	  B��B�B�B�`B�SB�@B�#B�B��B��B��BƨB�|B�rB�hB�kB�kB�gB�eB�]B�EB�4B�-B�,B�(B�"B�B�B�	B�B� B��B��B��B��B�vB�NB�*B�B�B�B}�Bv�Bp�Bm�BglB_;BZBVBQ�BL�BH�BF�BD�BB�B@�B=pB<jB;dB:^B9XB7LB5AB2*B2.B1&B0!B0!B/B0 B0B/B/B/B.B.B-B-B-B-B,B+B+B)�B(�B(�B(�B(�B'�B&�B%�B%�B%�B$�B$�B$�B$�B#�B#�B$�B$�B$�B$�B$�B#�B#�B"�B"�B"�B"�B"�B!�B �B�B�B�B�B�B �B �B"�B'�B+B,B.B0!B34B6FB6FB6CB6EB:`B?~BH�BL�BO�BQ�BQ�BXBd[Be`BfdBk�Bl�Bk�BiyBn�Bx�B�B�B�1B�DB�XB�uB��B��B��B��B��B��B��B��B��B�B�B�3B�?B�TB�WB�]B�jB�yBĚB��B��B��B�)B�;B�DB�ZB�~B�B�B�B��B��B��B��B	B	B		6B	IB	IB	EB	
>B	
:B		4B	AB	wB	�B	�B	�B	�B	�B	%�B	&�B	%�B	&�B	)�B	+B	,	B	,
B	-B	.B	/B	2.B	7KB	8QB	9XB	9YB	9XB	:\B	:\B	;dB	<lB	>vB	B�B	G�B	Q�B	VB	W	B	YB	ZB	ZB	["B	\,B	]/B	^5B	`BB	cVB	gnB	i{B	m�B	n�B	o�B	p�B	v�B	y�B	}�B	~�B	~�B	~�B	�B	�B	�B	�3B	�NB	�UB	�_B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�.B	�7B	�8B	�8B	�?B	�FB	�WB	�eB	�kB	�rB	�rB	�qB	�tB	�tB	�wB	�uB	��B	��B	��B	ÖB	ĝB	ĜB	šB	ǰB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�.B	�/B	�4B	�6B	�6B	�BB	�GB	�OB	�VB	�[B	�aB	�sB	�xB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��G�O�B	��B
mB
�B
#B
!�B
)�B
.�B
6FB
="B
CzB
KFB
TB
WYB
[UB
_B
d�B
iB
m�B
r,B
v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
մG�O�G�O�G�O�G�O�G�O�G�O�B
�9B
�iG�O�G�O�G�O�B
��A��WG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ɭG�O�G�O�G�O�A�b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�LEG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bv=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B �G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�B G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��Aü�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�BݩG�O�G�O�G�O�G�O�G�O�B<�G�O�G�O�G�O�A��G�O�B
�hG�O�G�O�G�O�B �B
��B
��B
��BG�O�B
�)B
��B
��B
��B
��B�G�O�B�uB
��BzA�jFG�O�B
��G�O�G�O�B
��B
�%B�B
��BB6B
��B�G�O�B
�bBZ�G�O�B
�eB
�6G�O�B ;A���B
�OB
�gB
��B
�GBc�G�O�A�-YB
��B
��B
�5B
��B
�B
�B
�B
�G�O�B
�RB
��B
��B
�WB
�B
�B
��B
��B
�,B
ԈA�m#G�O�B
�B
�#B
�mB
�B
�HB
�}B
�{G�O�B
��B
�NG�O�B
�JB�B
��B
��B
��G�O�G�O�B
֗B
��B
�gB
�kB
��B
�B
�qG�O�B
�B
��B
��B
�QB
�kB��B��B
ҕB
�kB
�B
ПB
�5B
�~B
�=B
סB
ےB
��B
�B
�;G�O�G�O�B
��B
�B
�tA�sG�O�G�O�G�O�B
ͼB
��B
�DB
�#B
҈B
��B�B
�qB
�OB
ԡB
�@B
ٖG�O�G�O�B
ّG�O�B
��B<�B
�B
��B
�WB
�<A�F]G�O�G�O�B
��B
��G�O�B
�_B
�oB
��B
�EB
�pG�O�B
�yG�O�B(B
��G�O�B
��B�"G�O�B
�B
ǮA�)
G�O�BǿB
��B
ċA�q�B
�*B
�2B
��B
��B
�kG�O�B
��B
��A�1�A��FB�B
�KB
��B
�OB
�
B
��B
�2B
�4B
��G�O�B
�VB
�B
GB
�HG�O�B
�B
�NB?B$BUG�O�B�B
�`B
��B
�}B
�B
��B
��G�O�B
��B
��B
�B
�B�B
��B
�oG�O�B
��B
��B
��B
�_B
��G�O�B
�B
��B
ګB
�nB
�B
�)B
�A���B
�7G�O�B:B�B
��B
�BA�pEB
�G�O�B
�BB
�oBNA�>$B
�
BG�O�G�O�B!�B�B&�B!'B$JB�B�B:B+aByBjG�O�B#B"�B%�B%B(B&#A�B �BLB3�G�O�G�O�B Aۦ�A�i�B&�B&�B&+B(�B'�B'�B+B)�B(�B%�B(OB&�B'�B%�B%B' B&�B'�B(�B&�B'{B(�B)9B&�B))B)QB+�B+~B0�B4KB2�B)pB)�B(OB&�B%�B'B*�B*zB+9B*�B*�B*�B+\B*�B*�B+�B*�B*�B*�B*�B+UB+6B+LB+NB+B*qB*`B+B+9B*�B*�B*�B+B*xB)�B*�B+LB*�B*�B+bB*�B*�B*�B*�B*�B*�B*�B*�B+B*MB+B*[B)�B+/B+B*jB*nB+PB+,B+:B*B+B+,B*VB+;B+EB*rB*�B*pB*�B*�B+cB*�B+�B*�B+�B+B+�B*�B*�B+�B*�B*�B*�B*�B*�B*�B+�B*�B+!B+,B+B+B+�B+3B+B+B*<B+*B+!B*MB+DB+0B++B*�B*�B*�B*�B*�B*�B+|B+�B+�B+�B*�B+�B+�B+B*|B+EB+=B+\B+B+WB+3B+;B*�B+^B+�B*}B+B*�B+B+�B*�B+4B+B*�B*�B+mB*�B*�B+_B*�B+PB+bB+JB*�B+LB*zB+pB+gB+`B+VB+�B+xB+[B+iB+�B,PB+YB+AB+LB+AB*�B+�B*�B+RB*�B*�B+VB+�B*�B+�B+iB+�B+B++B+JB+WB+:B+kB+�B+/B+�B+�B+:B,B+ B+/B	��B	��B	��B	�B	�GB	��B	��B	�B	�~B	�$B	�B	��B	�B	�xB	�jB	��B	�B	��B	��B	��B	�B	�B	�_B	��B	�[B	��B	��B	�B	�0B	��B	�BB	��B	��B	�B	�B	�[B	�.B	�B	�B	��B	��B	�B	�kB	�aB	�3B	��B	��B	�B	�zB	�{B	�qB	�bB	�7B	��B	��B	��B	�B	�oB	�8B	��B	��B	��B	�KB	�{B	��B	�.B	�B	��B	�B	��B	��B	�tB	�rB	�,B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�NB	��B	��B	��B	��B	��B	��B	��B	��B	�tB	��B	�B	�B	��B	��B	��B	��B	��B	�IB	�\B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�WB	�+B	�\B	�"B	�qB*�B*zB+9B*�B*�B*�B+\B*�B*�B+�B*�B*�B*�B*�B+UB+6B+LB+NB+B*qB*`B+B+9B*�B*�B*�B+B*xB)�B*�B+LB*�B*�B+bB*�B*�B*�B*�B*�B*�B*�B*�B+B*MB+B*[B)�B+/B+B*jB*nB+PB+,B+:B*B+B+,B*VB+;B+EB*rB*�B*pB*�B*�B+cB*�B+�B*�B+�B+B+�B*�B*�B+�B*�B*�B*�B*�B*�B*�B+�B*�B+!B+,B+B+B+�B+3B+B+B*<B+*B+!B*MB+DB+0B++B*�B*�B*�B*�B*�B*�B+|B+�B+�B+�B*�B+�B+�B+B*|B+EB+=B+\B+B+WB+3B+;B*�B+^B+�B*}B+B*�B+B+�B*�B+4B+B*�B*�B+mB*�B*�B+_B*�B+PB+bB+JB*�B+LB*zB+pB+gB+`B+VB+�B+xB+[B+iB+�B,PB+YB+AB+LB+AB*�B+�B*�B+RB*�B*�B+VB+�B*�B+�B+iB+�B+B++B+JB+WB+:B+kB+�B+/B+�B+�B+:B,B+ B+/B	��B	��B	��B	�B	�GB	��B	��B	�B	�~B	�$B	�B	��B	�B	�xB	�jB	��B	�B	��B	��B	��B	�B	�B	�_B	��B	�[B	��B	��B	�B	�0B	��B	�BB	��B	��B	�B	�B	�[B	�.B	�B	�B	��B	��B	�B	�kB	�aB	�3B	��B	��B	�B	�zB	�{B	�qB	�bB	�7B	��B	��B	��B	�B	�oB	�8B	��B	��B	��B	�KB	�{B	��B	�.B	�B	��B	�B	��B	��B	�tB	�rB	�,B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�NB	��B	��B	��B	��B	��B	��B	��B	��B	�tB	��B	�B	�B	��B	��B	��B	��B	��B	�IB	�\B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�WB	�+B	�\B	�"B	�qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444444444444444444444444444344444444434444444444444444444434444444444444444444444344444433444334444444444444434443444444444443444444444444444444444444344444444444444344344444344444344444444334434444444344444344434344433333433333343333434433333333433433433333334333333333433333333333433333334334333334433333334333333333333333333344333344433333333333344343333333443343333343433433433343333333334333333333333343333433333433333334333333343333343333333334333333343333333443333333333343333333333443333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281454392020082814543920200828145439202008281454392020082814543920200828145439202008281454392020082814543920200828145439202008281454392020082814543920200828145439AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730272019021417302720190214173027    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730272019021417302720190214173027  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730272019021417302720190214173027  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281454392020082814543920200828145439  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                