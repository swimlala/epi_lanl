CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  N   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:20Z creation      
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
resolution        =���   axis      Z        '�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  v�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  �8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� )   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 3   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� Z�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �X   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� �D   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� (   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� >�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� H�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � pP   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   q   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   }   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
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
_FillValue                  0 �4Argo profile    3.1 1.2 19500101000000  20190219181720  20200831164937  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�i���iF@�i���iF@�i���iF111 @�i�ff��@�i�ff��@�i�ff��@6@ě��T@6@ě��T@6@ě��T�cV��O�;�cV��O�;�cV��O�;111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�  @�  A   AffA@  A`  A���A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq�fDrfDr�fDs  Ds� Dt  Dt� Dy��D��D�S�D���D���D��D�F�D��3D��qD��D�C3D��=D��RD���D�@�Dڀ D���D���D�*�D��D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O������L�;��������L�;������������L�;������;��������L�;��������������������������������������������L�;L�;������������������������L�;L�;��������������������L�ͽ��;������������������������������������L�;��������������������L�;L�;������������������������������������������������������������������������������������������������������������������������L�;������������L�;����������������������������L�;������������������������L�;��������L�;L�;��������������������������������L�;L�;����L�;����L�;������������������������L�;L�;��������L�;������������L�;L�;������������������������������������L�;����������������������������L�;L�;����L�;��������L�;L�;����L�;����������������������������L�;������������L�;L�;������������������������L�;L�;��������������������L�;������;��������������������������;����������������L�;������;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��;L��    �L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ���    ���ͽ���    ���;L�;L�ͽ��ͽ��ͽ��ͽ���    ����                ����    ����    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���            ���ͽ���    ���;L�ͽ��;L�ͽ���    ����        ���ͽ���=���        ���ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ��;L�ͽ���    ���;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ���    ����    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ����    ���ͽ��ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���        ���ͽ��ͽ��;L��    ���ͽ��ͽ���    ����            ����    ����    ���ͽ��;L�ͽ��ͽ���    ���ͽ���                            ���ͽ��ͽ��ͽ���    ����    ����                        ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���            =���    =���                ���ͽ��;L�;L�;L�ͽ��ͽ��ͽ���    ����        ���ͽ��ͽ��ͽ��;L�ͽ��;L�;L��    �L�ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ��ͽ��ͽ���        ���ͽ��ͽ��ͽ���    ����    ����        ���ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ���    ���ͽ��ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��ͽ���    =���=���=���>L��>L��>���>���>���>���?   ?��?��?333?333?333?L��?fff?�  ?�  ?���?���?���?���?�ff?�ff?�33?�  ?���?���?ٙ�?ٙ�?�ff?�ff?�33@   @ff@ff@��@��@33@��@��@   @&ff@,��@333@9��@@  @Fff@L��@Y��@`  @fff@l��@s33@y��@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@���@���@�  @�33@�ff@ə�@���@�  @�ff@ٙ�@���@�  @�33@陚@���@�  @�33@���@���A   A��A��AffA  A	��A��AffA  A��A��AffA  A��A��AffA   A!��A$��A&ffA(  A)��A,��A.ffA0  A1��A4��A6ffA8  A9��A;33A>ffA@  AA��AC33AFffAH  AI��AK33AL��AP  AQ��AS33AVffAX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���Ař�A�ffA�  A���Aə�A�ffA�  A���A͙�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  Aٙ�A�ffA�33A���Dp��Dp�3Dp� Dp�fDp�3Dp��Dp�fDp��DpٚDp� Dp��Dp�3Dq  DqfDq3Dq�Dq  Dq,�Dq33Dq@ DqFfDqS3DqY�DqffDql�Dqs3Dq� Dq�fDq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq�fDq��Dq��Dr  DrfDr3Dr�Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr�3DrٚDr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds� Ds�fDs��DsٚDs� Ds��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt� Dt�fDt�3@&ff@,��@333@9��@@  @Fff@L��@Y��@`  @fff@l��@s33@y��@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@���@���@�  @�33@�ff@ə�@���@�  @�ff@ٙ�@���@�  @�33@陚@���@�  @�33@���@���A   A��A��AffA  A	��A��AffA  A��A��AffA  A��A��AffA   A!��A$��A&ffA(  A)��A,��A.ffA0  A1��A4��A6ffA8  A9��A;33A>ffA@  AA��AC33AFffAH  AI��AK33AL��AP  AQ��AS33AVffAX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���Ař�A�ffA�  A���Aə�A�ffA�  A���A͙�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  Aٙ�A�ffA�33A���Dp��Dp�3Dp� Dp�fDp�3Dp��Dp�fDp��DpٚDp� Dp��Dp�3Dq  DqfDq3Dq�Dq  Dq,�Dq33Dq@ DqFfDqS3DqY�DqffDql�Dqs3Dq� Dq�fDq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq�fDq��Dq��Dr  DrfDr3Dr�Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr�3DrٚDr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds� Ds�fDs��DsٚDs� Ds��Ds�3Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dt� Dt�fDt�3Dt��Dt� Dt��Dt�3Dt� Dt�fDt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @G
=@��@��A��A#\)AD��Ad��A�G�A�z�A�z�A�z�A�z�A�z�A�G�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De=De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�qDq�Dq�=Dr=Dr�=Ds�Ds��Dt�Dt��Dy�{D�qD�]qD��qD�ÆD��D�P�D��D��]D��D�MD��)D��>D���D�J�Dډ�D�ιD�	�D�4�D�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#��=�G�<#��<#��=�G�<#��<#��<#��=�G�<#����Q�<#��<#��=�G�<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��=�G�=�G�<#��<#��<#��<#��<#��<#��=�G�=�G�<#��<#��<#��<#��<#��=�G�>W
=<#��<#��<#��<#��<#��<#��<#��<#��<#��=�G�<#��<#��<#��<#��<#��=�G�=�G�<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��=�G�<#��<#��<#��=�G�<#��<#��<#��<#��<#��<#��<#��=�G�<#��<#��<#��<#��<#��<#��=�G�<#��<#��=�G�=�G�<#��<#��<#��<#��<#��<#��<#��<#��=�G�=�G�<#��=�G�<#��=�G�<#��<#��<#��<#��<#��<#��=�G�=�G�<#��<#��=�G�<#��<#��<#��=�G�=�G�<#��<#��<#��<#��<#��<#��<#��<#��<#��=�G�<#��<#��<#��<#��<#��<#��<#��=�G�=�G�<#��=�G�<#��<#��=�G�=�G�<#��=�G�<#��<#��<#��<#��<#��<#��<#��=�G�<#��<#��<#��=�G�=�G�<#��<#��<#��<#��<#��<#��=�G�=�G�<#��<#��<#��<#��<#��=�G�<#��>W
=<#��<#��<#��<#��<#��<#��>W
=<#��<#��<#��<#��=�G�<#��>W
==�G�=�G�=�G�=�G�=�G�=�G�>W
==�G�=�G�=�G�=�G�>W
==�G�>��R=�G�>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>W
=>W
=>��R>W
=>W
=>��R>W
==�G�=�G�>W
=>W
=>W
=>W
=>��R>W
=>��R>��R>��R>��R>W
=>��R>W
=>��R>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>��R>��R>W
=>W
=>��R>W
==�G�>W
==�G�>W
=>��R>W
=>��R>��R>W
=>W
=>��>��R>��R>W
=>W
=>W
=>W
=>W
=>��R>W
=>W
=>W
=>W
=>W
=>W
==�G�>W
==�G�>W
=>��R>W
==�G�>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>W
=>W
=>��R>W
=>��R>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>W
=>��R>W
=>W
=>W
=>��R>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>��R>W
=>W
=>W
==�G�>��R>W
=>W
=>W
=>��R>W
=>��R>��R>��R>W
=>��R>W
=>��R>W
=>W
==�G�>W
=>W
=>��R>W
=>W
=>��R>��R>��R>��R>��R>��R>��R>W
=>W
=>W
=>W
=>��R>W
=>��R>W
=>��R>��R>��R>��R>��R>��R>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>��R>��R>��>��R>��>��R>��R>��R>��R>W
=>W
==�G�=�G�=�G�>W
=>W
=>W
=>��R>W
=>��R>��R>W
=>W
=>W
=>W
==�G�>W
==�G�=�G�>��R=�G�>W
=>W
=>W
=>W
=>��R>W
=>W
=>W
=>W
=>W
=>��R>��R>W
=>W
=>W
=>W
=>��R>W
=>��R>W
=>��R>��R>W
=>W
=>W
=>W
=>W
=>��R>W
=>W
=>W
=>��R>W
=>W
=>W
=>W
==�G�=�G�>W
=>W
=>W
=>W
=>��R>��>��>��?�\?�\?(�?5?5?5?O\)?h��?h��?�G�?�G�?�G�?�{?��G?��?��?�z�?�z�?�G�?�G�?�z?�z?��G?�?�z�?�z�@ ��@ ��@
=@
=@p�@�
@=p@=p@ ��@ ��@'
=@-p�@-p�@3�
@:=p@@��@G
=@Mp�@S�
@Z=p@`��@mp�@s�
@z=p@�Q�@��@��R@��@��@�Q�@��@��@��@�Q�@��@��R@��@��@��@��R@��@��@Å@Å@��@��@�Q�@Ӆ@ָR@��@�Q�@�@�R@��@��@�@��R@��@��AA\)A��A�]A	A\)A��A�]AA\)A��A�]AA\)A��A�]A!A#\)A$��A&�]A)A+\)A,��A.�]A1A3\)A4��A6�]A9A;\)A<��A>�]A@(�AC\)AD��AF�]AH(�AK\)AL��AN�]AP(�AQAT��AV�]AX(�A[\)A\��A^�]A`(�AaAd��Af�]Ah(�AiAl��An�]Ap(�As\)At��Av�]Ax(�A{\)A|��A~�]A�{A��A�z�A�G�A��GA��A�z�A�G�A�{A��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GAŮA�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A�{AѮA�z�A�G�A��GAծA�z�A�G�A��GAٮA�z�A�{A��GAݮA�G�Dp��Dp�
Dp��Dp�=Dp�
Dp�qDp�=Dp�Dp�qDp��Dq �Dq
Dq�Dq=Dq'
Dq-qDq3�Dq@�DqG
DqS�DqZ=Dqg
DqmqDqz=Dq��Dq�
Dq��Dq�=Dq�
Dq�qDq�=Dq��Dq�qDq��Dq�=Dq�
Dq�qDq�=Dr �DrqDr�Dr=Dr'
Dr-qDr:=Dr@�DrMqDrS�DrZ=Drg
DrmqDrz=Dr��Dr�qDr��Dr�=Dr�
Dr�qDr�=Dr��Dr�
Dr��Dr�=Dr�
Dr�qDr�=Ds �Ds
Ds�Ds=Ds'
Ds-qDs3�Ds@�DsG
DsS�DsZ=Dsg
DsmqDss�Ds��Ds�
Ds��Ds�=Ds�
Ds�qDs��Ds��Ds�
Ds��Ds�=Ds�Ds�qDs��Dt �Dt
Dt�Dt=Dt'
Dt-qDt3�Dt@�DtG
DtS�DtZ=Dtg
DtmqDts�Dt��Dt�
Dt��Dt�=Dt�
Dt�qDt��Dt��Dt�
Dt��Dt�=Dt�
@:=p@@��@G
=@Mp�@S�
@Z=p@`��@mp�@s�
@z=p@�Q�@��@��R@��@��@�Q�@��@��@��@�Q�@��@��R@��@��@��@��R@��@��@Å@Å@��@��@�Q�@Ӆ@ָR@��@�Q�@�@�R@��@��@�@��R@��@��AA\)A��A�]A	A\)A��A�]AA\)A��A�]AA\)A��A�]A!A#\)A$��A&�]A)A+\)A,��A.�]A1A3\)A4��A6�]A9A;\)A<��A>�]A@(�AC\)AD��AF�]AH(�AK\)AL��AN�]AP(�AQAT��AV�]AX(�A[\)A\��A^�]A`(�AaAd��Af�]Ah(�AiAl��An�]Ap(�As\)At��Av�]Ax(�A{\)A|��A~�]A�{A��A�z�A�G�A��GA��A�z�A�G�A�{A��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A�{A��A�z�A�G�A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GAŮA�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A�{AѮA�z�A�G�A��GAծA�z�A�G�A��GAٮA�z�A�{A��GAݮA�G�Dp��Dp�
Dp��Dp�=Dp�
Dp�qDp�=Dp�Dp�qDp��Dq �Dq
Dq�Dq=Dq'
Dq-qDq3�Dq@�DqG
DqS�DqZ=Dqg
DqmqDqz=Dq��Dq�
Dq��Dq�=Dq�
Dq�qDq�=Dq��Dq�qDq��Dq�=Dq�
Dq�qDq�=Dr �DrqDr�Dr=Dr'
Dr-qDr:=Dr@�DrMqDrS�DrZ=Drg
DrmqDrz=Dr��Dr�qDr��Dr�=Dr�
Dr�qDr�=Dr��Dr�
Dr��Dr�=Dr�
Dr�qDr�=Ds �Ds
Ds�Ds=Ds'
Ds-qDs3�Ds@�DsG
DsS�DsZ=Dsg
DsmqDss�Ds��Ds�
Ds��Ds�=Ds�
Ds�qDs��Ds��Ds�
Ds��Ds�=Ds�Ds�qDs��Dt �Dt
Dt�Dt=Dt'
Dt-qDt3�Dt@�DtG
DtS�DtZ=Dtg
DtmqDts�Dt��Dt�
Dt��Dt�=Dt�
Dt�qDt��Dt��Dt�
Dt��Dt�=Dt�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�33A�VA���A��yA��TA��/A��A���A���A���A���A�ȴA�AʾwAʺ^AʸRAʲ-AʬAʥ�Aʙ�AʑhAʍPAʉ7Aʇ+AʃAʁA�|�A�%Aȧ�A�33A�oA�VA���Aǣ�A�~�AǇ+AǑhAǟ�A�ĜA�{A�l�A��`A���A�A��+A�JA�/A�+A�bA�l�A�jA���A�7LA� �A��A���A�ZA��/A�ȴA���A�1'A���A�ȴA��A���A���A�I�A�v�A�{A�A�ƨA�JA�|�A��yA�A�r�A�?}A��
A���A���A��#A��7A�?}A���A���A���A��hA��^A��wA��9A���A��7A��A��A�33A�A�S�A��A�/A��DA��+A�
=A�ZA��wA� �A�33A�=qA���A��A��DA��mA�`BA��A{�^AxffAx�uAw;dAt�+Aq�Am�Ai`BAd��AbA^�+A\ �AY/AT-ARbNAP�AOt�ANZAK&�AIS�AF�RAES�AE`BAC��AB�AA��A?��A=�A<(�A;�wA:  A8VA4��A3�wA3p�A3"�A1�A0E�A/K�A.v�A.A�A.(�A-x�A,{A+7LA*z�A)��A(�9A'��A%��A$bNA#
=A"��A!��A �jA ��A {A(�A�AXA�A?}A��AA\)AZA
=A1A;dAr�A��A�A�TA/AȴA5?AA�AAS�A	�A��A �A�7A33A�A��AA�Ap�A��A{A+A�HA�HA��A�A�`A%A�`A��AffA(�A��A I�@��@��@��T@�j@�\)@��\@���@��u@��\@�P@�n�@�h@�@�P@���@���@��`@���@��@��T@�O�@�Ĝ@�dZ@���@��@�7@��@��H@ޏ\@�V@���@�bN@�@�@�b@�V@�%@�r�@Ӿw@�33@�M�@�Q�@̼j@�I�@��H@ț�@ȋD@��@�@ř�@�t�@î@�"�@�C�@°!@���@��@�O�@��F@�n�@�=q@��7@��`@�I�@��w@�C�@��@�@���@��\@��-@���@�Q�@��@�;d@��y@�@��#@��@���@�z�@�(�@��m@���@��!@���@�33@���@���@��/@��@��j@��9@�X@���@�@��P@���@��D@�`B@��\@�`B@�33@�1@�J@���@��@�V@��@�@�K�@��F@�1@�|�@�l�@�C�@��H@���@�{@��@�G�@�
=@�E�@��@���@�t�@�ƨ@�S�@�"�@�\)@��@��/@�Ĝ@�V@�`B@��@��T@���@���@��u@��P@���@�@�\)@��w@�~�@���@��!@���@�n�@��y@�V@�@�Ĝ@��
@��;@��w@���@�/@�%@��@�V@�O�@���@��-@�hs@���@�V@��\@�^5@�=q@�hs@���@�S�@�S�@�o@���@�E�@���@�K�@�\)@��y@�@��@��m@�ȴ@�V@��@�ff@���@���@��\@�x�@��`@��@�bN@�(�@���@�
=@��@���@�9X@���@���@��u@��u@���@�I�@�l�@�33@�v�@�ff@���@�;d@�$�@���@�V@��@�1@�1'@�Ĝ@�Ĝ@�9X@�t�@���@�v�@���@�E�@��#@���@��T@���@�`B@�G�@�/@��@��u@�A�@�b@�1@���@���@���@�C�@�+@�t�@��P@�\)@�S�@�l�@��@�~�@�=q@���@��-@�X@��@�hs@��7@�hs@�/@�V@��/@���@�I�@�  @���@��@�~�@��#@��h@�N<@{]�@t*�@kt�@d��@[iD@Uj@M�@G�F@B�@<Ft@3��@.�A@)B�@$9X@�$@g�@�@��@�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�A�1A���A�(�A�1'A�A�ƨA˓uA��A���A�&�A�A�v�A���A�5?A�7LA�+A�ĜA���A���A��TA�A�A��A�;dA���A�ƨA�z�A��wA��A�VA�I�A��A�A�XA�33A�~�A�G�A�ffA��
A�G�A�^5A� �A��AƬA�`BA�?}A��PA���A�p�A�{A�A�XA�bA���A��A���A�A�Q�A��;A�ZA�ffA��A���A�A�A�A��`A���A���A��uA�r�A�p�A�z�A�VA�oA��9A��A�5?A���A��A��DA��A�r�A�E�A�v�A���A��A�JAʟ�A�A�ZA�l�AċDA�E�A�~�A��A���A��wA��A���A�~�A�Q�A�n�A�ĜA�hsA� �A��9A�ZA�z�A�A�1'A�^5A�G�A�S�A��/Aƙ�A�9XA��A���A��A�dZA�G�A�~�A�S�A�A��yA�
=A�;dA�A��A�  A�VAōPA�
=A�x�A���AĴ9A�|�A�A�A��hA��yA�^5A��A��HA��A�7LA���A�I�A�XA��;A���A�jA��A��`A�+A�-A�&�A���A�oA��HA��TA�n�A���A��ÁA��mA�&�A�z�A���A�ƨA�=qA�%A�\)A�"�A��A���A���A�JA�ȴA���A�JA�VA�v�Aˇ+A�/A��9A�`BA��A�{Aȕ�ÁAɁAˉ7A���A�{A�|�A�ZA�l�A̅Aȟ�A�v�A���Aɗ�A�jAƅA�ffA�;dA��A��A�dZA�ffA�S�A�\)A�v�Aȉ7A�n�A���A�t�A�r�A�r�A�~�A�|�A�z�A�^5A�ÁA�p�A�x�A�p�A�n�A�n�A�ffA�r�A�n�A�jA�hsA�hsA�dZA�dZA�v�A�r�A�jA�p�A�t�A�x�A�t�A�p�A�v�A�n�A�n�A�r�A�hsA�jA�p�A�z�A�v�A�r�A�r�A�`BA�ffA�l�A�ffA�~�A�x�ÁA�r�A�x�A�p�A�n�A̅A�~�ÃȦ+ÁȀ\Ả7A̍PȦ+Ȧ+A̅A�r�A�t�A�l�A�p�A�r�A�r�A�t�A�v�ÃÃȦ+A̋DȀ\Ả7Ả7A̓uA̋DA̋DȦ+ÃA̅A̅A̅Ȧ+Ȧ+A̅ȂhA̅Ả7ȂhȂhA̙�A̕�A̗�ȂhA̓uA̝�ȂhȀ\A̕�ȂhȀ\A̍PA̍PȦ+A̋DȦ+ÃA̋DÃÃẢ7Ȧ+Ả7A̍PA̋DȂhA̍PA̓uA̕�A̕�A̍PA̋DẢ7Ả7A̅ÃA̅A�~�ÃA̅ÃȦ+Ả7A̅A̅Ả7Ả7A̅ÁA̋DẢ7A̅Ȧ+Ả7Ả7ÃÃÁÃA̅ÁA̓uA̋DA̍PA̍PȂhA̓uẢ7Ȁ\A̗�A̋DA̍PA̋DȂhA̙�ȂhȂhA̍PA̍PẢ7A̍PA̕�A̓uÃȦ+A̅A̅Ȧ+Ȁ\A̍PȀ\A̓uA̛�Ȁ\A�~�A�|�A̅ÃA�|�A�|�Ả7Ả7A̋DȂhA̋DA̋DA�~�A�v�A�x�A�~�A�z�A�t�ÃȦ+A�~�A�~�Ả7Ȧ+Ȧ+A̓uÃA�v�A̅A�z�A�r�A�v�A�v�A�t�A�n�A�r�A�z�A�v�A�z�A̅ÃA̅Ȧ+A̅ÁȦ+A̋DÁA�|�ÁA̅A�|�ÃA�z�A�t�A�x�A�v�A�~�A�z�A�v�A�v�A�v�A�t�A�v�A�t�A�x�A�v�A�z�A�t�A�|�A�z�A�z�A�v�A�x�A�x�A�z�A�x�A�|�A�~�A�x�A�x�A�|�A�|�A�x�A�|�A�~�A�r�A�z�A�z�A�r�A�z�A�v�A�dZA�^5A�S�A�VA�Q�A�1'A��A��`A��TA��A���A˶FA˸RA˸RA˲-A˰!AˮA˧�Aˣ�A˟�A˟�A˝�A˓uA˕�A˗�Aˇ+AˁAˇ+A�z�A�p�A�l�A�bNA�^5A�VA�M�A�S�A�Q�A�M�A�I�A�E�A�A�A�5?A�9XA�5?A�5?A�33A�(�A�+A�&�A�$�A�"�A� �A� �A��A��A�{A�oA��A�{A�bA�JA�VA�JA�JA�JA�%A�%A�1A�A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��yA��A��yA��yA��yA��yA��A��yA��yA��yA��yA��yA��yA��mA��`A��`A��mA��`A��mA��`A��TA��`A��TA��HA��TA��TA��HA��;A��;A��HA��;A��;A��;A��HA��;A��/A��/A��/A��/A��/A��#A��#A��#A��#A��#A��/A��#A��A��A��A��#A��#A��A��#A��#A��/A��#A��A��#A��/A��A��
A��A��
A��A��A��A��
A��
A��
A��A��
A��
A��A���A���A��
A���A��
A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨ@���@���@��j@��j@��j@��j@��9@��@��@��@���@���@���@���@���@��u@��@��@�j@�bN@�Z@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�9X@�9X@�1'@��@�1@�b@�1@�1@�  @�  @�  @���@���@���@��@��m@��m@��;@��
@���@���@��w@��F@���@���@���@��P@��@��@�l�@�dZ@�\)@�\)@�S�@�C�@�C�@�;d@�33@�+@��@�
=@��@��y@��H@��@��@���@���@��R@��!@���@��\@��+@�v�@�V@�M�@�E�@�E�@�5?@�$�@�{@�J@���@���@��@��#@���@��^@��^@��-@��-@��-@���@���@��-@���@���@���@���@���@���@��h@��h@��h@��@�hs@�hs@�hs@�hs@�p�A�n�A�n�A�bNA�^5A�S�A�M�A�G�A�G�A�I�A�C�A�?}A�;dA�7LA�1'A�-A�+A�&�A�&�A�"�A� �A��A��A��A��A��A��A�oA�oA�bA�oA�VA�VA�
=A�1A�1A�1A�%A�%A�A�A�  A�  A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��mA��mA��yA��yA��yA��mA��mA��mA��mA��mA��yA��mA��TA��`A��`A��`A��TA��TA��TA��HA��HA��TA��HA��HA��HA��/A��;A��;A��/A��/A��/A��/A��/A��/A��/A��/A��#A��#A��#A��A��#A��A��A��#A��A��A��A��A��A��
A��
A��
A��
A��A��A��A��A��
A��
A��
A��
A���A���A���A���A���A��
A���A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ƨA�ĜA�A�ĜA�@���@���@�Ĝ@��j@��j@��j@��j@��9@��@��@���@���@���@���@���@��u@��D@��@�r�@�j@�bN@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�9X@�9X@�1'@� �@�b@�1@�1@�1@�1@�1@�  @�  @���@���@��@��@��m@��;@��
@��
@���@�ƨ@��w@��@���@���@��P@��P@��@�t�@�dZ@�\)@�\)@�S�@�K�@�C�@�C�@�33@�+@��@�
=@���@��y@��H@��H@��@���@���@��R@��R@���@��\@��+@�v�@�ff@�M�@�E�@�E�@�5?@�$�@��@�J@�@���@��@��#@���@�@��^@��^@��-@��-@��-@���@���@���@���@���@���@���@���@���@��h@��h@��7@�p�@�hs@�hs@�hs@�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�bNA�33A�VA���A��yA��TA��/A��A���A���A���A���A�ȴA�AʾwAʺ^AʸRAʲ-AʬAʥ�Aʙ�AʑhAʍPAʉ7Aʇ+AʃAʁA�|�A�%Aȧ�A�33A�oA�VA���Aǣ�A�~�AǇ+AǑhAǟ�A�ĜA�{A�l�A��`A���A�A��+A�JA�/A�+A�bA�l�A�jA���A�7LA� �A��A���A�ZA��/A�ȴA���A�1'A���A�ȴA��A���A���A�I�A�v�A�{A�A�ƨA�JA�|�A��yA�A�r�A�?}A��
A���A���A��#A��7A�?}A���A���A���A��hA��^A��wA��9A���A��7A��A��A�33A�A�S�A��A�/A��DA��+A�
=A�ZA��wA� �A�33A�=qA���A��A��DA��mA�`BA��A{�^AxffAx�uAw;dAt�+Aq�Am�Ai`BAd��AbA^�+A\ �AY/AT-ARbNAP�AOt�ANZAK&�AIS�AF�RAES�AE`BAC��AB�AA��A?��A=�A<(�A;�wA:  A8VA4��A3�wA3p�A3"�A1�A0E�A/K�A.v�A.A�A.(�A-x�A,{A+7LA*z�A)��A(�9A'��A%��A$bNA#
=A"��A!��A �jA ��A {A(�A�AXA�A?}A��AA\)AZA
=A1A;dAr�A��A�A�TA/AȴA5?AA�AAS�A	�A��A �A�7A33A�A��AA�Ap�A��A{A+A�HA�HA��A�A�`A%A�`A��AffA(�A��A I�@��@��@��T@�j@�\)@��\@���@��u@��\@�P@�n�@�h@�@�P@���@���@��`@���@��@��T@�O�@�Ĝ@�dZ@���@��@�7@��@��H@ޏ\@�V@���@�bN@�@�@�b@�V@�%@�r�@Ӿw@�33@�M�@�Q�@̼j@�I�@��H@ț�@ȋD@��@�@ř�@�t�@î@�"�@�C�@°!@���@��@�O�@��F@�n�@�=q@��7@��`@�I�@��w@�C�@��@�@���@��\@��-@���@�Q�@��@�;d@��y@�@��#@��@���@�z�@�(�@��m@���@��!@���@�33@���@���@��/@��@��j@��9@�X@���@�@��P@���@��D@�`B@��\@�`B@�33@�1@�J@���@��@�V@��@�@�K�@��F@�1@�|�@�l�@�C�@��H@���@�{@��@�G�@�
=@�E�@��@���@�t�@�ƨ@�S�@�"�@�\)@��@��/@�Ĝ@�V@�`B@��@��T@���@���@��u@��P@���@�@�\)@��w@�~�@���@��!@���@�n�@��y@�V@�@�Ĝ@��
@��;@��w@���@�/@�%@��@�V@�O�@���@��-@�hs@���@�V@��\@�^5@�=q@�hs@���@�S�@�S�@�o@���@�E�@���@�K�@�\)@��y@�@��@��m@�ȴ@�V@��@�ff@���@���@��\@�x�@��`@��@�bN@�(�@���@�
=@��@���@�9X@���@���@��u@��u@���@�I�@�l�@�33@�v�@�ff@���@�;d@�$�@���@�V@��@�1@�1'@�Ĝ@�Ĝ@�9X@�t�@���@�v�@���@�E�@��#@���@��T@���@�`B@�G�@�/@��@��u@�A�@�b@�1@���@���@���@�C�@�+@�t�@��P@�\)@�S�@�l�@��@�~�@�=q@���@��-@�X@��@�hs@��7@�hs@�/@�V@��/@���@�I�@�  @���@��@�~�@��#G�O�@�N<@{]�@t*�@kt�@d��@[iD@Uj@M�@G�F@B�@<Ft@3��@.�A@)B�@$9X@�$@g�@�@��@�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�A�1A���A�(�A�1'A�A�ƨA˓uA��A���A�&�A�A�v�A���A�5?A�7LA�+A�ĜA���A���A��TA�A�A��A�;dA���A�ƨA�z�A��wA��A�VA�I�A��A�A�XA�33A�~�A�G�A�ffA��
A�G�A�^5A� �A��AƬA�`BA�?}A��PA���A�p�A�{A�A�XA�bA���A��A���A�A�Q�A��;A�ZA�ffA��A���A�A�A�A��`A���A���A��uA�r�A�p�A�z�A�VA�oA��9A��A�5?A���A��A��DA��A�r�A�E�A�v�A���A��A�JAʟ�A�A�ZA�l�AċDA�E�A�~�A��A���A��wA��A���A�~�A�Q�A�n�A�ĜA�hsA� �A��9A�ZA�z�A�A�1'A�^5A�G�A�S�A��/Aƙ�A�9XA��A���A��A�dZA�G�A�~�A�S�A�A��yA�
=A�;dA�A��A�  A�VAōPA�
=A�x�A���AĴ9A�|�A�A�A��hA��yA�^5A��A��HA��A�7LA���A�I�A�XA��;A���A�jA��A��`A�+A�-A�&�A���A�oA��HA��TA�n�A���A��ÁA��mA�&�A�z�A���A�ƨA�=qA�%A�\)A�"�A��A���A���A�JA�ȴA���A�JA�VA�v�Aˇ+A�/A��9A�`BA��A�{Aȕ�ÁAɁAˉ7A���A�{A�|�A�ZA�l�A̅Aȟ�A�v�A���Aɗ�A�jAƅA�ffA�;dA��A��A�dZA�ffA�S�A�\)A�v�Aȉ7A�n�A���A�t�A�r�A�r�A�~�A�|�A�z�A�^5A�ÁA�p�A�x�A�p�A�n�A�n�A�ffA�r�A�n�A�jA�hsA�hsA�dZA�dZA�v�A�r�A�jA�p�A�t�A�x�A�t�A�p�A�v�A�n�A�n�A�r�A�hsA�jA�p�A�z�A�v�A�r�A�r�A�`BA�ffA�l�A�ffA�~�A�x�ÁA�r�A�x�A�p�A�n�A̅A�~�ÃȦ+ÁȀ\Ả7A̍PȦ+Ȧ+A̅A�r�A�t�A�l�A�p�A�r�A�r�A�t�A�v�ÃÃȦ+A̋DȀ\Ả7Ả7A̓uA̋DA̋DȦ+ÃA̅A̅A̅Ȧ+Ȧ+A̅ȂhA̅Ả7ȂhȂhA̙�A̕�A̗�ȂhA̓uA̝�ȂhȀ\A̕�ȂhȀ\A̍PA̍PȦ+A̋DȦ+ÃA̋DÃÃẢ7Ȧ+Ả7A̍PA̋DȂhA̍PA̓uA̕�A̕�A̍PA̋DẢ7Ả7A̅ÃA̅A�~�ÃA̅ÃȦ+Ả7A̅A̅Ả7Ả7A̅ÁA̋DẢ7A̅Ȧ+Ả7Ả7ÃÃÁÃA̅ÁA̓uA̋DA̍PA̍PȂhA̓uẢ7Ȁ\A̗�A̋DA̍PA̋DȂhA̙�ȂhȂhA̍PA̍PẢ7A̍PA̕�A̓uÃȦ+A̅A̅Ȧ+Ȁ\A̍PȀ\A̓uA̛�Ȁ\A�~�A�|�A̅ÃA�|�A�|�Ả7Ả7A̋DȂhA̋DA̋DA�~�A�v�A�x�A�~�A�z�A�t�ÃȦ+A�~�A�~�Ả7Ȧ+Ȧ+A̓uÃA�v�A̅A�z�A�r�A�v�A�v�A�t�A�n�A�r�A�z�A�v�A�z�A̅ÃA̅Ȧ+A̅ÁȦ+A̋DÁA�|�ÁA̅A�|�ÃA�z�A�t�A�x�A�v�A�~�A�z�A�v�A�v�A�v�A�t�A�v�A�t�A�x�A�v�A�z�A�t�A�|�A�z�A�z�A�v�A�x�A�x�A�z�A�x�A�|�A�~�A�x�A�x�A�|�A�|�A�x�A�|�A�~�A�r�A�z�A�z�A�r�A�z�A�v�A�dZA�^5A�S�A�VA�Q�A�1'A��A��`A��TA��A���A˶FA˸RA˸RA˲-A˰!AˮA˧�Aˣ�A˟�A˟�A˝�A˓uA˕�A˗�Aˇ+AˁAˇ+A�n�A�n�A�bNA�^5A�S�A�M�A�G�A�G�A�I�A�C�A�?}A�;dA�7LA�1'A�-A�+A�&�A�&�A�"�A� �A��A��A��A��A��A��A�oA�oA�bA�oA�VA�VA�
=A�1A�1A�1A�%A�%A�A�A�  A�  A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��mA��mA��yA��yA��yA��mA��mA��mA��mA��mA��yA��mA��TA��`A��`A��`A��TA��TA��TA��HA��HA��TA��HA��HA��HA��/A��;A��;A��/A��/A��/A��/A��/A��/A��/A��/A��#A��#A��#A��A��#A��A��A��#A��A��A��A��A��A��
A��
A��
A��
A��A��A��A��A��
A��
A��
A��
A���A���A���A���A���A��
A���A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ƨA�ĜA�A�ĜA�@���@���@�Ĝ@��j@��j@��j@��j@��9@��@��@���@���@���@���@���@��u@��D@��@�r�@�j@�bN@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�9X@�9X@�1'@� �@�b@�1@�1@�1@�1@�1@�  @�  @���@���@��@��@��m@��;@��
@��
@���@�ƨ@��w@��@���@���@��P@��P@��@�t�@�dZ@�\)@�\)@�S�@�K�@�C�@�C�@�33@�+@��@�
=@���@��y@��H@��H@��@���@���@��R@��R@���@��\@��+@�v�@�ff@�M�@�E�@�E�@�5?@�$�@��@�J@�@���@��@��#@���@�@��^@��^@��-@��-@��-@���@���@���@���@���@���@���@���@���@��h@��h@��7@�p�@�hs@�hs@�hs@�hsA�n�A�n�A�bNA�^5A�S�A�M�A�G�A�G�A�I�A�C�A�?}A�;dA�7LA�1'A�-A�+A�&�A�&�A�"�A� �A��A��A��A��A��A��A�oA�oA�bA�oA�VA�VA�
=A�1A�1A�1A�%A�%A�A�A�  A�  A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��mA��mA��yA��yA��yA��mA��mA��mA��mA��mA��yA��mA��TA��`A��`A��`A��TA��TA��TA��HA��HA��TA��HA��HA��HA��/A��;A��;A��/A��/A��/A��/A��/A��/A��/A��/A��#A��#A��#A��A��#A��A��A��#A��A��A��A��A��A��
A��
A��
A��
A��A��A��A��A��
A��
A��
A��
A���A���A���A���A���A��
A���A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA�ƨA�ĜA�A�ĜA�@���@���@�Ĝ@��j@��j@��j@��j@��9@��@��@���@���@���@���@���@��u@��D@��@�r�@�j@�bN@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�9X@�9X@�1'@� �@�b@�1@�1@�1@�1@�1@�  @�  @���@���@��@��@��m@��;@��
@��
@���@�ƨ@��w@��@���@���@��P@��P@��@�t�@�dZ@�\)@�\)@�S�@�K�@�C�@�C�@�33@�+@��@�
=@���@��y@��H@��H@��@���@���@��R@��R@���@��\@��+@�v�@�ff@�M�@�E�@�E�@�5?@�$�@��@�J@�@���@��@��#@���@�@��^@��^@��-@��-@��-@���@���@���@���@���@���@���@���@���@��h@��h@��7@�p�@�hs@�hs@�hs@�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Ǥ@!=��>^�u@��=�B�>��@�@�Ɠ=�#y>�@W??E��@_8�=�mH=��@\�!>�o*=�]d>��N=ʡ>�
@��6>�>�gM@Y]�>��E=��=�YK>%�@@���>ί@�[@���@��2=�$�>٩T@���=��?�s@��
@��C=�i�>���=��=�U�>S��=��>$v�>�@��?��V=���=�
==���=؃�>7Nf@�Ǥ?��=��=��@�D�=���=�-#>_1@\��=`L=��B>�=��=�=�=OV�=o4�=8�=�7�=ʡ>��@�a=�)J=�k�>��?��e>K��>i/@	x�=��>L0>�G@�1�@��`=���=�IR?���@���=��1=�<�=��>H�M=�tT>!�{@���@��)=���=�۶>I
�=�=�y�>)��@-y�?��?k=�@���@�Ԫ=�%�?$ ?�G�=�W�?3�]>w�:>j+?��@@��r?���>@�?�8�@�R�@@O=��	>9�=��@X��=�J#>z}�@��z>z��=�!�?���@��=���>v�@�ؙ@@�@B�=�Bp>�,=>�G�>�~�>:^5@(,�=�;d>�L@RV.@�ݭ=|�=��'=®�>�>�A�> ��>],|@��
>���>V��@��@U[�>���@��&>�`?/�>g�`@J=��<=��=�kQ?;G�>�>���@w;�=�#�?�>=Vm@���@���=�C�=��@=̲�>�@��5>`u@���?�:?@��=��`=әp>'b@��H@z�(@�_[@�Z�>
�`@/l7>�
�@�ی>�b�@�ܜ@�ܜ>N�w?)i�@���@��j@��Y?� G@��j>���@��Y@G4Y@�޾@��j@�޾@�ݭ@��&@��@@.w�@�� @���@��#@���@��3@���@��#@��
@��
@���@���@�׈@��E@���@���@��w@��f@��@���@��
@���@�ݭ@��j@�޾@��j@�ݭ@��&@���@��w@�ޔ@�ߤ@�ߤ@��z@�ܜ@��@���@���@��@���@�ߤ@��@@��@@��j@�ܜ@��z@��X@��X@���@��@��@��	@���@��@��@��@��z@���@���@���@��&@��@���@��;@�޾@��~@��@��&@��H@��&@���@��@��@���@��@��&@��@��@��@��~@��@���@��@��@��@��m@���@��@��\@��m@���@��@��`@��@��@��m@��@��K@��)@��@���@���@��q@��@��@��@��@��\@��)@��@��`@��@���@��m@��@��@��K@��}@��\@��@��@��@��@��@��K@��@���@��K@���@��&@��q@���@��`@��@��@��@���@��\@��\@��K@��q@��@��@��K@��@��@��@��m@��@��@��@��q@���@��@��@��@���@��@��\@���@��@��O@���@��@��@��@��@��K@��}@��@��q@��P@��@��K@���@��@��@��@��@��@���@��?@��&@��X@��@��&@��@���@��~@��@��@��@���@��X@��H@��H@���@���@��@��;@��a@��a@��&@��@��&@���@���@��&@��&@��7@��r@���@��@���@��z@�޾@��3@�ݭ@��7@���@��@@���@��P@��@��H@��H@���@��z@��@��@��@��r@��	@��7@��j@��7@�ܜ@�޾@��z@��@���@���@�ޔ@�ޔ@�ݭ@��&@��z@��j@��z@���@��j@��a@�ߤ@�ߤ@��&@��j@��Y@�ݭ@�޾@�ܜ@�ܜ@�ڥ@��<@�۶@���@���@��U@��U@��4@���@���@��@���@�¤@��P@��Q@��{@��H@���@���@��@��=@���@��7@���@���@���@��4@���@��E@���@��8@�{5@�y�@�{�@�z%@�x@�w@�t*@�r\@�k�@�g�@�fQ@�e,@�a�@�`�@�_p@�`-@�`-@�_@�[�@�[@�Y`@�W�@�V@�U@�S�@�R*@�R*@�Qn@�OL@�M�@�Ln@�L�@�Ln@�K^@�K^@�JM@�I(@�H�@�I=@�H�@�H@�G@�F_@�E�@�E�@�E�@�EN@�D�@�C�@�Cl@�B�@�@�@�@�@�?}@�>�@�>�@�>�@�>l@�>@�>@�=�@�=\@�<�@�<�@�<�@�;�@�;�@�;�@�;:@�:~@�:�@�:�@�:�@�:�@�:�@�;:@�:�@�:�@�:�@�:�@�:�@�:�@�:�@�:�@�:�@�:*@�:*@�:*@�:*@�9�@�9m@�9.@�9�@�9.@�9.@�9.@�8\@�8�@�8�@�8q@�8�@�8q@�8q@�8@�8q@�8@�8@�7�@�8@�7�@�7�@�7�@�7v@�7a@�7�@�82@�7�@�7v@�7v@�7�@�7�@�7v@�7a@�7�@�8@�8q@�8@�8@�8@�8�@�8q@�7�@�7�@�8@�8@�8q@�8�@�8�@�8�@�9.@�9�@�9�@�9.@�9.@�8�@�9.@�9.@�8�@�8�@�8�@�8�@�8�@�8�@�82@�82@�82@�82@�8�@�8�@�7�@�8G@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�8�@�82@�82@�8�@�8�@�8�@�82@�82@�7�@�82@�82@�8�@�8�@�8�@�8�@�8�@�8�@�82@�8�@�8�@�82@�8�@�8�@�82@�7�@�8G@�7�@�8G@�7�@�7�@�8G@�7�@�8G@�7�@�7�@�7�@�6�@�6;@�6;@�6@�6z@Q��@Q�A@Q�@Q��@Q��@Q��@Q��@Q��@Q�@Q�@Q��@Q�@Q�@Q�d@Q��@Q�@Q�"@Q��@Q��@Q��@Q��@Q�<@Q��@Q�@@Q��@Q�D@Q��@Q��@Q��@Q��@Q��@Q�@Q9@Q}@Q|p@Q{�@Q{t@Q{J@Qz�@Qz%@Qy�@Qx�@QxW@Qx@Qw2@Qv`@Qt�@Qs�@Qr�@QqL@QpP@Qn@Ql@Qjj@Qin@Qgw@Qf'@Qd�@Qb�@Q`@Q_@Q^t@Q]O@Q[�@QZ\@QY6@QW�@QU�@QS�@QQ@QN<@QK�@QI�@QI(@QH@QF_@QD�@QC@QA�@Q?�@Q<�@Q:i@Q8q@Q5�@Q2�@Q0�@Q/�@Q-�@Q+�@Q*@Q(�@Q&@Q%F@Q#�@Q!-@Q�@Q:@Qm@Q@Q�@Qq@Q@Qv@Q�@Qv@Q"@Qz@Q&@Q~@Q+@Q�@Q�@Q�@Q8@Q�@Q@QE@Q�@Q�@Q�@���@���@��I@��{@���@���@��$@��@���@���@��@���@���@���@��*@���@��@���@���@��@��s@���@��M@��@��{@��@��,@��@���@��o@��I@��
@�~g@�~R@�}�@�}�@�}�@�}A@�}@�|@�{J@�zN@�y�@�x�@�w�@�w@�v`@�v6@�u�@�v@�u�@�u�@�u�@�s�@�t @�s�@�sm@�r�@�s.@�r�@�q�@�q�@�q�@�q�@�r2@�r2@�r2@�r2@�rG@�r@�r@�r@�r\@�r@�q�@�q�@�q�@�q�@�qL@�q�@�q"@�p�@�p�@�q"@�p�@�pe@�p�@�o�@�oT@�o�@�o�@�oT@�oT@�oi@�oT@�o~@�oT@�n�@�n�@�n�@�n�@�n�@�nY@�nY@�nY@�n�@�n�@�n�@�n�@�nY@�nY@�n@�nD@�n@�n@�nY@�nD@�nY@�nD@�nD@�nY@�nD@�n@�m�@�m�@�m�@�n@�n@�nD@�nD@�nD@�nY@�nY@�nD@�n@�m�@�m�@�n@�m�@�m�@�n@�mH@�mH@�m3@�m	@�l�@�l�@�l�@�l�@�m	@�l�@�l�@�l�@�l�@�m	@�m	@�m	@�m3@�m	@�m@�m]@�mr@�mH@�m	@�m	@�l�@�m	@�m	@�l�@�l�@�l�@�l�@�l�@�m	@�m@�m	@�m3@�mH@�mH@�m@�m	@�m	@�l�@�l�@�l�@�lv@�la@�l"@�l"@�la@�l7@�lv@�lv@�lv@�lv@�l@�lL@�k�@�j@�i�@�j+@�i�@Q�)@Qϫ@Q��@Q�`@Q�6@Q�`@Q̸@Q�@Qʗ@Q�m@Q��@Q��@Q�q@Q�u@QǤ@Q�&@Q� @Q�3@Q�@@Q��@Q��@Q�'@Q��@Q�V@Q�Z@Q�0@Q��@Q��@Q�4@Q�
@Q�=@Q�=@Q�k@Q��@Q�@Q��@Q�`@Q�6@Q�6@Q��@Q�:@Q��@Q�>@Q��@Q�@Q��@Q��@Q��@Q� @Q��@Q��@Q��@Q��@Q�@Q��@Q��@Q�4@Q��@Q��@Q�J@Q��@Q��@Q�1@Q�6@Q��@Q��@Q�@Q��@Q�*@Q��@Q��@Q�{@Q��@Q@Q~�@Q}�@Q|�@Qy�@Qx�@Qx-@Qu�@QrG@Qpz@Qn�@Qk<@Qg�@Qe�@Qe�@Qc�@Q`�@Q_F@Q]y@QZ�@QZ@QX�@QUG@QS�@QQ�@QO�@QOa@QO7@QN�@QNf@QM�@QNf@QN@QM@@QM@@QLD@QLD@QK�@QK@QJw@QI�@QH@QF�@QD=@QD�@QD�@QE9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              344434443444434434444434434444344334434433444444443444444344434443444444444443444444444433444344444433444444444334444444434443444434434444443444444444433444444434433434444444444344433444434343444333344434334433343434333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ǦG�O�G�O�G�O�@��G�O�G�O�G�O�@�ƖG�O�G�O�G�O�G�O�@_8�G�O�G�O�@\�#G�O�G�O�G�O�G�O�G�O�@��3G�O�G�O�@Y]�G�O�G�O�G�O�G�O�@���G�O�G�O�@���@��6G�O�G�O�@���G�O�G�O�@��@��CG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�@�ǪG�O�G�O�G�O�@�D�G�O�G�O�G�O�@\��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�1�@��bG�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�@���@��(G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@�ԯG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��vG�O�G�O�G�O�@�R�G�O�G�O�G�O�G�O�@X��G�O�G�O�@��~G�O�G�O�G�O�G�O�G�O�G�O�@�ؚG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@RV5@�ݮG�O�G�O�G�O�G�O�G�O�G�O�G�O�@��
G�O�G�O�@��@U\G�O�@��*G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@w;�G�O�G�O�G�O�@���@���G�O�G�O�G�O�G�O�@��6G�O�@���G�O�@��G�O�G�O�G�O�@��I@z�.@�_]@�Z�G�O�G�O�G�O�@�ۏG�O�@�ܟ@�ܟG�O�G�O�@���@��j@��aG�O�@��jG�O�@��ZG�O�@�޼@��l@�޿@�ݯ@��)@��CG�O�@��$@���@��%@���@��3@���@��!@��
@��@���@���@�ׇ@��B@���@���@��t@��b@��@���@��@���@�ݬ@��i@�޾@��g@�ݬ@��&@���@��w@�ސ@�ߦ@�ߦ@��x@�ܞ@��@���@���@��@���@�ߦ@��>@��>@��f@�ܜ@��|@��Y@��W@���@��@��@��@���@��@��@��@��z@���@���@���@��)@��@���@��=@�޹@��|@��@��$@��J@��*@��@��@��@���@��@��$@��@��@��@��y@��@���@��@��@��@��g@���@��@��_@��r@���@��@��b@��@��@��k@��@��N@��%@��@���@���@��o@��@��@��@��@��^@��"@��@��Y@��	@���@��j@��@��	@��L@��}@��]@��@��@��{@��@��@��K@��@���@��J@���@��$@��n@���@��^@��@��@��@���@��V@��Z@��J@��s@��@��	@��G@��@��@��@��o@��	@��@��@��j@���@��@��@��	@���@��@��Z@���@��@��M@���@��@��@��@��	@��K@��}@��@��r@��S@��@��J@���@��@��@��@��@��@���@��>@��"@��V@��@��&@��@���@��~@��@��@��@���@��Y@��I@��G@���@���@��@��6@��c@��c@��'@��@��'@���@���@��)@��,@��9@��r@���@��@���@��z@�޼@��/@�ݭ@��5@���@��>@���@��P@��@��L@��F@���@��|@��@��@��@��r@��@��5@��l@��5@�ܟ@�޼@��w@��@���@���@�ސ@�ޒ@�ݬ@��!@��w@��l@��w@���@��g@��a@�ߤ@�ߤ@��!@��f@��\@�ݭ@�޼@�ܜ@�ܢ@�ڧ@��>@�۶@���@���@��V@��V@��3@���@���@��@���@�¢@��P@��R@��~@��J@���@���@��@��?@���@��9@���@���@���@��5@���@��F@���@��9@�{4@�y�@�{�@�z&@�x@�w
@�t*@�r[@�k�@�g�@���@���@��J@��@���@���@��'@��
@���@���@��@���@���@���@��+@���@��@���@���@���@��v@���@��J@��@��|@��@��+@��@���@��l@��N@��@�~j@�~T@�}�@�}�@�}�@�}B@�}@�|@�{H@�zN@�y�@�x�@�w�@�w@�vf@�v7@�u�@�v@�u�@�u�@�u�@�s�@�t@�s�@�sp@�r�@�s.@�r�@�q�@�q�@�q�@�q�@�r6@�r2@�r0@�r6@�rI@�r@�r@�r	@�rZ@�r
@�q�@�q�@�q�@�q�@�qP@�q�@�q!@�p�@�p�@�q%@�p�@�pf@�p�@�o�@�oY@�o�@�o�@�oX@�oV@�oh@�oV@�o�@�oU@�n�@�n�@�n�@�n�@�n�@�nZ@�nZ@�n\@�n�@�n�@�n�@�n�@�nY@�n^@�n@�nE@�n@�n@�nY@�nE@�nZ@�nD@�n>@�nY@�nG@�n@�m�@�m�@�m�@�m�@�n@�nB@�nD@�nB@�nZ@�n\@�nC@�n@�m�@�m�@�n@�m�@�m�@�n@�mJ@�mJ@�m5@�m	@�l�@�l�@�l�@�l�@�m	@�l�@�l�@�l�@�l�@�m@�m	@�m@�m5@�m@�m@�m^@�mr@�mN@�m@�m@�l�@�m	@�m@�l�@�l�@�l�@�l�@�l�@�m@�m@�m@�m5@�mB@�mJ@�m@�m@�m@�l�@�l�@�l�@�lu@�lb@�l!@�l&@�l^@�l9@�ls@�lw@�lw@�lw@�l@�lN@�k�@�j}@�i�@�j.@�i�@Q�+@Qϭ@Q��@Q�^@Q�6@Q�^@Q̻@Q�@Qʛ@Q�p@Q��@Q��@Q�s@Q�x@QǨ@Q�(@Q��@Q�3@Q�B@Q��@Q��@Q�+@Q��@Q�X@Q�[@Q�2@Q��@Q��@Q�5@Q�@Q�@@Q�>@Q�k@Q��@Q� @Q��@Q�]@Q�5@Q�8@Q��@Q�>@Q��@Q�B@Q��@Q�@Q��@Q��@Q� @Q�@Q��@Q��@Q��@Q��@Q�@Q��@Q��@Q�5@Q��@Q��@Q�K@Q��@Q��@Q�0@Q�8@Q��@Q��@Q�@Q��@Q�(@Q��@Q��@Q��@Q��@Q@Q~�@Q}�@Q|�@Qy�@Qy@Qx-@Qu�@QrJ@Qp{@Qn�@Qk=@Qg�@Qf@Qe�@Qc�@Q`�@Q_K@Q]z@QZ�@QZ
@QX�@QUJ@QS�@QQ�@QO�@QOc@QO5@QN�@QNe@QM�@QNb@QN@QM@@QMC@QLE@QLE@QK�@QK @QJv@QI�@QG�@QF�@QDB@QD�@QD�@QE6@���@���@��J@��@���@���@��'@��
@���@���@��@���@���@���@��+@���@��@���@���@���@��v@���@��J@��@��|@��@��+@��@���@��l@��N@��@�~j@�~T@�}�@�}�@�}�@�}B@�}@�|@�{H@�zN@�y�@�x�@�w�@�w@�vf@�v7@�u�@�v@�u�@�u�@�u�@�s�@�t@�s�@�sp@�r�@�s.@�r�@�q�@�q�@�q�@�q�@�r6@�r2@�r0@�r6@�rI@�r@�r@�r	@�rZ@�r
@�q�@�q�@�q�@�q�@�qP@�q�@�q!@�p�@�p�@�q%@�p�@�pf@�p�@�o�@�oY@�o�@�o�@�oX@�oV@�oh@�oV@�o�@�oU@�n�@�n�@�n�@�n�@�n�@�nZ@�nZ@�n\@�n�@�n�@�n�@�n�@�nY@�n^@�n@�nE@�n@�n@�nY@�nE@�nZ@�nD@�n>@�nY@�nG@�n@�m�@�m�@�m�@�m�@�n@�nB@�nD@�nB@�nZ@�n\@�nC@�n@�m�@�m�@�n@�m�@�m�@�n@�mJ@�mJ@�m5@�m	@�l�@�l�@�l�@�l�@�m	@�l�@�l�@�l�@�l�@�m@�m	@�m@�m5@�m@�m@�m^@�mr@�mN@�m@�m@�l�@�m	@�m@�l�@�l�@�l�@�l�@�l�@�m@�m@�m@�m5@�mB@�mJ@�m@�m@�m@�l�@�l�@�l�@�lu@�lb@�l!@�l&@�l^@�l9@�ls@�lw@�lw@�lw@�l@�lN@�k�@�j}@�i�@�j.@�i�@Q�+@Qϭ@Q��@Q�^@Q�6@Q�^@Q̻@Q�@Qʛ@Q�p@Q��@Q��@Q�s@Q�x@QǨ@Q�(@Q��@Q�3@Q�B@Q��@Q��@Q�+@Q��@Q�X@Q�[@Q�2@Q��@Q��@Q�5@Q�@Q�@@Q�>@Q�k@Q��@Q� @Q��@Q�]@Q�5@Q�8@Q��@Q�>@Q��@Q�B@Q��@Q�@Q��@Q��@Q� @Q�@Q��@Q��@Q��@Q��@Q�@Q��@Q��@Q�5@Q��@Q��@Q�K@Q��@Q��@Q�0@Q�8@Q��@Q��@Q�@Q��@Q�(@Q��@Q��@Q��@Q��@Q@Q~�@Q}�@Q|�@Qy�@Qy@Qx-@Qu�@QrJ@Qp{@Qn�@Qk=@Qg�@Qf@Qe�@Qc�@Q`�@Q_K@Q]z@QZ�@QZ
@QX�@QUJ@QS�@QQ�@QO�@QOc@QO5@QN�@QNe@QM�@QNb@QN@QM@@QMC@QLE@QLE@QK�@QK @QJv@QI�@QG�@QF�@QDB@QD�@QD�@QE6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              344434443444434434444434434444344334434433444444443444444344434443444444444443444444444433444344444433444444444334444444434443444434434444443444444444433444444434433434444444444344433444434343444333344434334433343434333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��'9��9���9��W9��19��9���9�֘9��G9��l9��w9�ѵ9���9��O9��!9���9��H9��9�ʇ9��9���9��w9���9�Ʃ9��49���9��'9��N9��-9�ď9�ë9��x9��)9��9���9���9���9��=9��9��A9���9���9��O9���9���9��X9���9���9��k9���9��n9��;9��9���9���9���9��h9���9��39��9��89���9��;9��99��n9��k9��i9��n9��}9��Y9��Y9��J9���9��K9���9���9���9���9���9���9���9��_9��/9���9��^9���9��O9��f9��%9���9��f9��$9��#9��19��#9��E9��"9���9���9���9���9���9��Z9��Z9��[9��}9���9���9���9��Y9��]9��$9��I9��"9��'9��Y9��I9��Z9��H9��D9��Y9��K9��'9���9���9���9��9��'9��G9��H9��G9��Z9��[9��G9��'9���9��9��9���9���9��9���9���9��p9��M9��9��,9��9��/9��M9��9��9���9��9��L9��M9��Q9��p9��P9��_9���9���9���9��O9��O9��<9��M9��O9��)9��
9���9��*9��,9��O9��[9��O9��p9��{9���9��]9��O9��O9��
9��-9���9���9���9���9���9���9���9���9���9���9���9���9���9��b9��E9���9��9���9'P�9'P�9'O�9'N�9'N�9'N�9'N?9'M�9'L�9'Lk9'K�9'K�9'K�9'J�9'J39'I�9'H�9'Gr9'E9'D9'C 9'A�9'As9'A.9'@d9'@D9'?�9'?�9'?z9'?V9'=�9'=�9'=@9':�9'8�9'89'7�9'7�9'7�9'79'6�9'6U9'5�9'5i9'5	9'4�9'3�9'2�9'1�9'1[9'0�9'/�9'.99',D9'*q9')�9'(Y9''.9'&D9'$n9'"X9'!l9'!(9' b9'9'�9'9'�9'�9'�9'r9'�9'�9'L9'+9'c9'Z9'"9'~9'
�9'�9' 9'�9'<9' �9&��9&�U9&��9&�}9&��9&��9&��9&�,9&��9&�9&� 9&��9&��9&�9&�K9&�&9&��9&�9&�9&�~9&�?9&�9&�9&��9&��9&�9&��9&�]9&��9&�e9&�\9&�k9&�9&�9&�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B[#B[#B[#B[#B[#B\)B\)B\)B\)B]/B]/B]/B]/B]/B]/B]/B]/B]/B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)BdZB�{B�dB��B�BB�B(�B2-B5?B9XBH�B"�B�B�oB� Bm�Bv�Bq�B�1B�bBz�B�=B�{B�jB�wB��B��B��B��B��B�FB��BĜB��BŢB�RB�BÖB��B�B'�BB\B=qB%BB �B�B��B�B�B��B��B��Br�Br�Bp�BQ�B=qB&�B5?B"�B��B�}Bs�B+BuB\B
��B
ȴB
�?B
�B
�!B
��B
��B
�PB
~�B
gmB
7LB
$�B
5?B
�B
  B	�B	ĜB	��B	��B	�B	��B	��B	�DB	n�B	K�B	'�B	�B	�B	bB�yB�wB�B�ZB�B��BĜB�}B�9B�B�^B�dB�9B�FB�B��B��B��B��B��B� By�B{�B|�Bv�Bp�Bp�Bq�Bs�Bu�Bt�B|�B}�B|�B|�B{�By�Bv�Bt�Bq�Bp�Bl�BjBjBgmBe`BffBdZB`BB^5B[#BT�BP�BN�BK�BI�BG�BF�BD�BC�BB�BB�BB�BA�B?}B?}B>wB=qBA�B@�B=qB=qB=qB<jB;dB=qB?}B>wB=qB>wBA�BF�BH�BN�BT�BW
BXBXBXBW
BVBR�BO�BP�BT�BW
BW
BW
BVBVBS�BS�BR�BP�BO�BO�BO�BO�BO�BO�BQ�BT�BVBT�BZB_;Be`Be`BdZBiyBm�Bn�Bt�Bw�B{�B~�B�B�%B�+B�%B�B�B�B�Bz�Bx�B~�B|�B�B�B~�B~�B�B�1B�+B�1B�+B�B~�B|�B}�B~�B~�B�B�B�1B�JB�\B�hB�uB�{B�{B��B��B��B��B��B��B�B�B�B�'B�-B�9B�9B�?B�9B�-B�!B�?B�dB�wBĜB��B��B��B�B�B�
B�)B�`B�B��B	hB	�B	&�B	!�B	 �B	 �B	#�B	/B	:^B	=qB	C�B	G�B	I�B	N�B	N�B	M�B	L�B	K�B	J�B	H�B	B�B	@�B	?}B	A�B	;dB	33B	33B	6FB	;dB	C�B	N�B	Q�B	T�B	XB	ZB	]/B	cTB	aHB	bNB	aHB	_;B	cTB	r�B	y�B	y�B	y�B	�B	�+B	�B	u�B	s�B	r�B	o�B	m�B	p�B	r�B	{�B	�B	�B	�B	�+B	�DB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�dB	��B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	B	��B	�}B	�}B	ÖB	ȴB	ƨB	ŢB	ÖB	B	��B	ĜB	ȴB	ǮB	ƨB	ĜB	B	B	ÖB	ÖB	B	B	ĜB	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�B	�B	�B	�5B	�;B	�HB	�`B	�fB	�fB	�fB	�`B	�fB	�fB	�fB	�`B	�fB	�`B	�fB	�mB	�mB	��B	��B
(B
�B
 �B
($B
-wB
5�B
9$B
>wB
E�B
L�B
S&B
Y1B
^5B
c:B
gmB
k�B
nIB
s3B
wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B"�AO?�?�8�B��>�I�?$�HAW|�BH�>�'t?@�eAt��@�TA�t3>��N>���A��?��>�f�?�s? �E?A�EB	n�?'�D?���A��z@!Gk>�^�>�9`?S�
B?7�qA?�}B�B"1?k�@6/B�?�\A&�gB�Bŵ?!O'@��?�>�2�?��?Qn?XW:?4=�B,�A��>��U?
�2>��?
AN?k�B",A��>�}�?s�Aۀ;>��?]�?G��A�Ne>��]>��[@(M'>��?u>���>�x�>�`W>˫�? ��?$D(A���>�^�>���?+��@��?�]�?6Y�AU�>�\?)�(@%��A�4�B�>�R?
A<�wB�j>�\�?��>�?��?�?K#�B�BH�>�*>��?�c�>�W�?}�?\?�A��@@F�1@��.B�DB�'?��@SRH@Ӆ%>�{�@{�L?�*�?4�<@� �B#�@�RB?u�`@�PA��AIӡ>��?ti@?�IA�Ɉ?-�?���B)?��v?�<@��Aa�?W�?A��B-
A��.AVP�?T�?��@#d?�3�?v��A�S�?��?K��A�ޘB`A>�.?)�>���?H�?��?#�?��hB)�@	�?�� B �A��Q?���B'�?Ζh@/4U?��gA�y�>�)E>�=�>�,�@�y�?G�n@!��A��z>�ӄ@D�?r�QB+8B��?	7�>�،? �?;7�A�z�?���B �A.�vA�z
?{�?Q3?S��A�|�A�)B��A�8	?-�9A���@�B��@ Q+B-�B>?�NM@i�B.�B/:B5Q@�#]B(�@.�B+A��lB)�B*vB*�B%B'3B')A��BI�B"�B(>B&�B)8B)�B) B,�B(@B(�B(�B(B(�B*�B(�B&%B&�B%�B#yB'vB!�B(�B+@B)0B,B+UB+B,B*�B+_B(_B)�B+`B(�B-�B*B)}B+%B(B)1B$�B*HB(B)�B,�B&�B)GB)B&�B(LB!�B"6B#&B%{B%�B$PB)B(JB*oB+�B):B*�B*TB)B(�B(iB'�B#�B$�B%�B%B#�B&B&�B'�B*�B*TB*�B'�B*�B,?B*LB$RB*�B*B$�B&3B"�B%MB$�B'�B$5B#3B%�B'�B$�B$�B(WB%�B'�B'�B(WB)*B)�B(�B+�B+kB*�B*B(&B'$B)�B&�B('B$�B#XB&GB'zB(�B(�B*B+�B+B)�B,QB*B)�B*B'�B) B,B)�B(�B)�B*�B*�B(CB)B)�B)�B)�B(�B*pB+�B,�B+B+�B+�B$oB*CB'�B(�B&�B$gB(�B(B$.B(;B%B(�B(�B$,B%9B&+B'�B'$B(B)fB%XB%'B)�B)"B)�B)FB*�B']B&�B&cB"�B�B#�B*�B*B(�B)[B,B*iB&cB%tB'�B$JB%�B$�B(UB+|B*B(B*>B*eB%�B$ZB&XB%SB"gB#�B#�BB$�B*�B&&B(�B,�B+8B)�B)�B)�B)�B(�B*�B'B&kB)�B'�B%.B%�B'6B#~B#�B'�B*?B'�B%�B(%B$B(�B'�B(UB)�B(�B)�B)NB)B(�B(�B)�B*�B(	B)�B)�B)�B(KB(pB(pB)�B(B'B&�B(UB$�B#�B$�B$"B#�B!RB!�B!�B �B#�BhB B�B�BBtB�B�B�B:B�B�B"�B�BwBXB&�B%�B�B�B8B ZB%BQB�B 5B�B!�B�BqB!B|B=B�B�B!B�BAB#B&0B"�B 7B!bB!YB!�B!mB%'B"�B"sB"kB"wB$lB">B"�B#�B$B#�B#�B%B%�B%�B'%B"�B%<B%�B&�B%�B& B%�B%�B'CB&�B%ZB&KB&�B&�B&OB&cB'B'�B'�B'4B'#B&�B'/B(!B(B(	B&'B(~B'�B'NB'bB'mB(�B&�B(cB([B(�B(BB'pB(EB'�B'�B(B'�B'�B(mB(�B(�B'�B(�B'�B'�B(wB(6B(^B) B(MB'�B(�B)`B)B(}B(�B(�B(�B(B({B)>B(�B)-B(�B(�B)vB)+B)B)hB)�B(�B(�B)�B*B)�B(�B(�B)�B)JB)�B(oB)1B)�B)�B(�B)|B*>B)�B*sB)�B*FB*=B*�B+<B+�B*�B+B+B)�B+�B+�B*�B+B*�B+�B*�B*B*�B+LB+DB+3B,VB*�B*B+�B,-B,�B+�B+AB+9B+0B,8B+eB*2B+TB+�B*�B+PB*�B+�B+|B+�B+B+�B+�B*�B+:B*gB,TB+B*�B+B*�B+�B*�B+TB+�B+�B,B*�B+5B+�B+B*�B*�B*�B*�B+�B+B+OB*�B*0B+-B+%B+�B,B	��B	��B	�-B	��B	��B	�B	�"B	�mB	��B	��B	��B	��B	�)B	��B	�B	��B	��B	��B	�0B	�mB	��B	�#B	��B	�TB	��B	��B	��B	��B	�,B	��B	��B	�B	�	B	��B	�3B	��B	�TB	�B	��B	�-B	��B	�-B	��B	�lB	��B	�1B	��B	�B	�[B	�[B	��B	��B	��B	�RB	��B	�/B	�/B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�jB	�:B	��B	��B	��B	��B	��B	�B	�3B	��B	��B	��B	�vB	�(B	��B	�JB	��B	��B	��B	��B	��B	�EB	��B	�|B	�zB	��B	�B	�B	�B	�B	��B	��B	�mB	�3B	��B	��B	�B	�;B	��B	��B	�!B	��B	�QB	��B	��B	��B	��B	��B	��B	�B	ÑB	�9B	çB	�|B[�B[�B[BB[&B[tB[aB[(B[�BZ�B\B[_BZ�B[tBZ�B[B[�B\_BZ�B[�BZ�B[�B[B[lB[*B[]BZ�B[�BZ�B[=BZ�B[\B[B[%B[�B[~B[<B[�B[zB\B[�B[�BZ�B[�B\}B[yB[�B[B[�B[vB[yB[]B[B[�B[�B[�B[�B[�B[_B[�B[bB[<BZ�B[�B[�B[MB[<B[4B[�B\B[�B[�B[�B[ B[�B\�B[�B[�B[�B\CB\uB\B\�B\JB[�B\sB[�B\OB\�B[�B\B\�B\OB\FB\QB\6B\LB\B[�B\cB\B\JB\�B[�B\�B\xB[�B\�B\�B\�B\FB\5B\�B\�B\�B\�B\B[�B[�B[�B\�B\�B\yB\JB\�B\�B\�B\�B\�B\6B\�B\B\(B\�B\�B\�B\VB\YB\dB\�B\�B]B]"B]B\�B]�B]OB]RB]6B]AB\�B\JB]B]�B\�B]%B]B]B]+B\1B]B\nB]<B]B]�B]�B\�B\�B]lB]4B]�B\�B]B]B])B]4B]B]/B]:B\_B\�B\�B\�B]DB]bB]B\�B]�B]UB]EB]vB]GB]qB]iB]`B]XB\�B]B]zB\�B]B\�B]B	�nB	�B	�aB	�PB	�B	�)B	�B	�B	��B	��B	�MB	�@B	��B	�3B	�B	�&B	�RB	��B	��B	��B	�B	��B	�hB	� B	�kB	�?B	��B	�B	�UB	�)B	��B	��B	�B	�B	�NB	�mB	�B	��B	�B	�HB	��B	�B	�B	�B	�1B	��B	�B	��B	� B	�B	��B	�2B	��B	��B	�TB	�B	�qB	�PB	�oB	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�xB	�LB	�B	�B	�B	��B	�VB	�B	�.B	��B	�B	�B	�B	�dB	��B	�B	�mB	�{B	�<B	�	B	�B	�B	�OB	�OB	�B	�9B	��B	��B	�{B	�B	�B	��B	�B	��B	��B	�-B	�B	��B	�1B	�B	�CB	��B	��B	�,B	�OB	�BB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344434443444434434444434434444344334434433444444443444444344434443444444444443444444444433444344444433444444444334444444434443444434434444443444444444433444444434433434444444444344433444434343444333344434334433343434333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B[	B[B[B[	B[B\B\B\B\B]B]B]B]B]B]B]B]B]B\B\B\B\B\B\B\B\B\B\Bd?B�_B�HB��B��B�B�B(�B2B5)B9?BH�B"�B��B�SB�BmvBv�Bq�B�B�HBz�B�%B�aB�OB�_B��B�|B�gB�yB��B�-B�kBāB̳BņB�8B�dB�yB��BaB'�B�BAB=VBB�B �BeB��B��B�qB��B��B�eBr�Br�Bp�BQ�B=SB&�B5"B"�B��B�^Bs�B*�BXB=B
˧B
ȕB
� B
��B
��B
��B
��B
�2B
~�B
gOB
7/B
$�B
5B
kB	��B	�pB	�|B	��B	�gB	��B	��B	��B	�$B	nxB	K�B	'�B	lB	kB	CB�VB�WB��B�7B��BϾB�{B�[B�B��B�;B�BB�B�#B��B��B��B��B��B�^B�By�B{�B|�Bv�Bp�Bp�Bq�Bs�Bu�Bt�B|�B}�B|�B|�B{�By�Bv�Bt�Bq�BpBleBjZBjZBgGBe;BfCBd9B`B^BZ�BT�BP�BN�BK�BI�BG�BF�BDwBCqBBjBBlBBiBAcB?VB?XB>NB=NBAcB@]B=LB=MB=KB<CB;?B=KB?WB>SB=KB>PBAeBF�BH�BN�BT�BV�BW�BW�BW�BV�BU�BR�BO�BP�BT�BV�BV�BV�BU�BU�BS�BS�BR�BP�BO�BO�BO�BO�BO�BO�BQ�BT�BU�BT�BY�B_Be9Be<Bd3BiTBmhBnqBt�Bw�B{�B~�B��B��B�B��B��B��B��B��Bz�Bx�B~�B|�B��B��B~�B~�B��B�B�B�B�B��B~�B|�B}�B~�B~�B��B��B�B�"B�3B�AB�KB�RB�SB�kB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B�B�;B�PB�tBʙBˠB��B��B��B��B�B�9B�pB��B	AB	�B	&�B	!�B	 �B	 �B	#�B	.�B	:4B	=JB	CnB	G�B	I�B	N�B	N�B	M�B	L�B	K�B	J�B	H�B	BhB	@ZB	?RB	AbB	;<B	3B	3B	6B	;;B	CnB	N�B	Q�B	T�B	W�B	Y�B	]B	c*B	a B	b%B	aB	_B	c+B	r�B	y�B	y�B	y�B	��B	�B	��B	u�B	s�B	r�B	ovB	mgB	p|B	r�B	{�B	��B	��B	��B	�B	�B	�:B	�GB	�IB	�YB	�|B	��B	��B	��B	�}B	�cB	�cB	�kB	�dB	�yB	�}B	��B	��B	��B	��B	��B	��B	�xB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	�YB	�pB	�mB	�mB	�mB	�mB	�mB	�fB	�bB	�RB	�SB	�mB	ȋB	ƀB	�yB	�nB	�gB	�`B	�tB	ȋB	ǄB	�B	�tB	�gB	�dB	�mB	�nB	�gB	�gB	�sB	�lB	�tB	�tB	�rB	�xB	ǅB	ȉB	ɒB	ɒB	ɑB	ʘB	ʙB	̤B	ϴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�6B	�?B	�=B	�:B	�8B	�=B	�<B	�>B	�7B	�>B	�8B	�<B	�CG�O�B	��B	��B
B
�B
 gB
'�B
-OB
5~B
8�B
>NB
E]B
LnB
R�B
YB
^B
cB
gEB
k�B
nB
sB
v�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B"�G�O�G�O�G�O�B�G�O�G�O�G�O�BH�G�O�G�O�G�O�G�O�A�tG�O�G�O�A��G�O�G�O�G�O�G�O�G�O�B	n�G�O�G�O�A��XG�O�G�O�G�O�G�O�BG�O�G�O�B�B"G�O�G�O�B~G�O�G�O�B�BśG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B,�G�O�G�O�G�O�G�O�G�O�G�O�B"G�O�G�O�G�O�AۀG�O�G�O�G�O�A�NDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�4�B�G�O�G�O�G�O�B�RG�O�G�O�G�O�G�O�G�O�G�O�B�BHvG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�*B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B#�G�O�G�O�G�O�AﴮG�O�G�O�G�O�G�O�A��eG�O�G�O�B(�G�O�G�O�G�O�G�O�G�O�G�O�B,�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��zB`'G�O�G�O�G�O�G�O�G�O�G�O�G�O�B)�G�O�G�O�B �A��6G�O�B'�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��OG�O�G�O�G�O�B+ B��G�O�G�O�G�O�G�O�A�z{G�O�B �G�O�A�y�G�O�G�O�G�O�A�|UA�(�B��A�7�G�O�G�O�G�O�B��G�O�B-zB=�G�O�G�O�B.�B/ B58G�O�B(�G�O�B*�G�O�B)�B*^B*�B$�B'B'G�O�BI�B"yB(%B&�B)B)�B(�B,�B(%B(�B(oB'�B(�B*�B(�B&	B&�B%�B#_B']B!�B(�B+(B)B+�B+8B*�B+�B*�B+CB(FB)�B+FB(�B-oB* B)cB+B'�B)B$�B*-B'�B)vB,�B&�B),B(�B&�B(.B!�B"B#B%_B%kB$7B(�B(0B*VB+�B)#B*�B*;B)B(�B(LB'�B#�B$�B%�B%B#pB%�B&�B'�B*�B*6B*�B'�B*uB,(B*1B$9B*�B)�B$tB&B"�B%6B$�B'�B$B#B%xB'�B$�B$�B(;B%�B'�B'zB(;B)B)�B(tB+�B+QB*�B* B(B'B)�B&�B(B$�B#?B&+B'_B(�B(�B)�B+�B+B)�B,9B)�B)�B)�B'�B)B+�B)�B(�B)�B*�B*�B(%B(�B)�B)�B)�B(�B*SB+�B,�B+B+�B+�B$VB**B'nB(�B&sB$MB(�B'�B$B( B%B(�B(B$B%"B&B'�B'B'�B)LB%?B%B)jB)
B)�B)*B*jB'BB&�B&GB"uB�B#�B*�B)�B(�B)@B+�B*NB&IB%]B'�B$0B%�B$pB(=B+`B)�B'�B*"B*GB%�B$AB&>B%:B"MB#�B#�BeB$�B*jB&B(�B,yB+ B)�B)�B)�B)�B(�B*�B&�B&NB)qB'�B%B%�B'B#cB#�B'�B*%B'�B%�B(
B#�B(�B'�B(;B)�B(�B)�B)7B(�B(�B(�B)jB*�B'�B)�B)�B)�B(0B(VB(VB)jB'�B&�B&�B(;B$�B#�B$tB$	B#�B!7B!�B!�B �B#�BOB�B�B�B�BZB�B�BqB#BqB�B"�B�B_B;B&tB%�B�B�BB BB$�B6B}B B�B!�B�BXB �BbB$B[�B[jB[)B[B[\B[GB[B[�BZ�B[�B[DBZ�B[[BZ�BZ�B[xB\GBZ�B[�BZ�B[�BZ�B[QB[B[BBZ�B[�BZ�B["BZ�B[DB[B[B[�B[dB[&B[�B[aB[�B[�B[�BZ�B[�B\eB[aB[�B[B[�B[[B[aB[DB[B[�B[�B[�B[�B[�B[GB[�B[IB["BZ�B[�B[�B[6B["B[B[�B[�B[�B[�B[�B[B[uB\�B[�B[�B[�B\+B\ZB[�B\xB\1B[�B\XB[�B\5B\�B[�B[�B\�B\8B\-B\8B\B\3B\B[�B\LB[�B\1B\�B[�B\fB\_B[�B\�B\uB\nB\+B\B\�B\�B\�B\�B[�B[�B[�B[�B\uB\�B\aB\-B\�B\�B\�B\�B\�B\B\�B\B\B\�B\�B\~B\=B\AB\LB\�B\�B\�B]	B] B\�B]vB]7B]7B]B])B\yB\-B\�B]�B\�B]	B]B\�B]B\B\�B\VB]B\�B]zB]lB\�B\�B]TB]B]�B\�B\�B\�B]B]B\�B]B]B\FB\�B\�B\�B])B]GB\�B\�B]~B];B]-B]ZB]+B]VB]OB]DB]@B\�B] B]_B\�B\�B\uB\�B	�FB	��B	�:B	�(B	��B	��B	�kB	��B	��B	�B	�#B	�B	��B	�B	�XB	��B	�)B	��B	�B	�B	��B	�B	�@B	��B	�BB	�B	�B	�qB	�.B	��B	�B	�B	��B	�B	�$B	�FB	��B	�B	�B	�B	��B	�^B	��B	�oB	�
B	�B	��B	�B	��B	�B	��B	�B	�B	��B	�,B	�tB	�JB	�)B	�HB	�B	��B	��B	�B	��B	�B	�[B	��B	��B	�B	��B	��B	�B	�B	�MB	�!B	�aB	�pB	�mB	��B	�,B	�eB	�B	�B	�iB	��B	�B	�=B	��B	�B	�AB	�VB	�B	��B	�B	�B	�(B	�%B	�mB	�B	��B	�B	�RB	��B	�B	��B	�{B	��B	��B	�B	��B	�B	�	B	�B	�B	�B	��B	�B	�&B	�B	�yB[�B[jB[)B[B[\B[GB[B[�BZ�B[�B[DBZ�B[[BZ�BZ�B[xB\GBZ�B[�BZ�B[�BZ�B[QB[B[BBZ�B[�BZ�B["BZ�B[DB[B[B[�B[dB[&B[�B[aB[�B[�B[�BZ�B[�B\eB[aB[�B[B[�B[[B[aB[DB[B[�B[�B[�B[�B[�B[GB[�B[IB["BZ�B[�B[�B[6B["B[B[�B[�B[�B[�B[�B[B[uB\�B[�B[�B[�B\+B\ZB[�B\xB\1B[�B\XB[�B\5B\�B[�B[�B\�B\8B\-B\8B\B\3B\B[�B\LB[�B\1B\�B[�B\fB\_B[�B\�B\uB\nB\+B\B\�B\�B\�B\�B[�B[�B[�B[�B\uB\�B\aB\-B\�B\�B\�B\�B\�B\B\�B\B\B\�B\�B\~B\=B\AB\LB\�B\�B\�B]	B] B\�B]vB]7B]7B]B])B\yB\-B\�B]�B\�B]	B]B\�B]B\B\�B\VB]B\�B]zB]lB\�B\�B]TB]B]�B\�B\�B\�B]B]B\�B]B]B\FB\�B\�B\�B])B]GB\�B\�B]~B];B]-B]ZB]+B]VB]OB]DB]@B\�B] B]_B\�B\�B\uB\�B	�FB	��B	�:B	�(B	��B	��B	�kB	��B	��B	�B	�#B	�B	��B	�B	�XB	��B	�)B	��B	�B	�B	��B	�B	�@B	��B	�BB	�B	�B	�qB	�.B	��B	�B	�B	��B	�B	�$B	�FB	��B	�B	�B	�B	��B	�^B	��B	�oB	�
B	�B	��B	�B	��B	�B	��B	�B	�B	��B	�,B	�tB	�JB	�)B	�HB	�B	��B	��B	�B	��B	�B	�[B	��B	��B	�B	��B	��B	�B	�B	�MB	�!B	�aB	�pB	�mB	��B	�,B	�eB	�B	�B	�iB	��B	�B	�=B	��B	�B	�AB	�VB	�B	��B	�B	�B	�(B	�%B	�mB	�B	��B	�B	�RB	��B	�B	��B	�{B	��B	��B	�B	��B	�B	�	B	�B	�B	�B	��B	�B	�&B	�B	�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999344434443444434434444434434444344334434433444444443444444344434443444444444443444444444433444344444433444444444334444444434443444434434444443444444444433444444434433434444444444344433444434343444333344434334433343434333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333312222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649372020083116493720200831164937202008311649372020083116493720200831164937202008311649372020083116493720200831164937202008311649372020083116493720200831164937AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817202019021918172020190219181720    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817202019021918172020190219181720  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817202019021918172020190219181720  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649372020083116493720200831164937  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                