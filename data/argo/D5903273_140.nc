CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  I   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:11Z creation      
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181711  20200831164913  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�BH��=@�BH��=@�BH��=111 @�BM��9@�BM��9@�BM��9@4ܬ1&�@4ܬ1&�@4ܬ1&��ch�9Xb�ch�9Xb�ch�9Xb111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D��D�\)D���D���D��D�]D���D��qD��D�J=D��RDǺ�D��D�D�`RD�RD��3D�${D�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����������L�;L�;������ͽ��;����������;��������L�ͽ��;L�;�����������    ���������L�;L�ͽ��;��������L�;L�;����L�;��������L�;L�;�������        ����    ���;L�;����L�;������������L�;����������ͽ��;����L�;����L�;L�;����L�;����L�;L�;������������L�;L�;����L�;L�;������������L�;������������L�;L�;������������L�;����L�;L�;����������;����L�;��������L�ͽ��;����L�;����������������L�ͽ��;L�;���������������    �������������L�ͽ��;L�;��������������������L�ͽ��;L�;����L�ͽ��;����������;������������L�ͽ��;������������������;������������L�;�����������=���>L��    �����������������L�ͽ��;��������L�;L�;����������;���������������=���    ���������L��    ���;L�;����L�;L�ͽ��;L�;��������L��=���    �������������������ͽ��;������ͽ��;L�;����L�;����L�ͽ��;L�;����L�;����L�;��������L�ͽ��;L�;������;������ͽ���        �L�;������ͽ��ͽ��ͽ���    ���;���    ���ͽ��ͽ��ͽ��;L�ͽ���    ���ͽ���        =���    ���ͽ���        =���>L�ͽ��;L�;L��                        ���;L�ͽ��ͽ��;L��        �L��    ���ͽ��ͽ��ͽ��ͽ���                ���ͽ���=���    =��ͽ���            ����    ����    ���;L��    ���;������ͽ���            ����        ����    ���ͽ��ͽ���        ����                =��ͽ��;L�ͽ���    ����        ���ͽ��ͽ��ͽ��ͽ��;L��=���    ����    ���ͽ��ͽ���            ���ͽ���    ����    =���    �L�ͽ��ͽ��ͽ���    =���    ����            ���ͽ���    ���;L��        ����        =��ͽ���=���=���=���    ���ͽ���        ���ͽ���        ����        >L��                ���ͽ��ͽ���    ����        ����        ����        =���>���=��ͽ��ͽ��ͽ��ͽ���    =���                    ���ͽ��ͽ���    ����    =���    ���ͽ���    �L�;L�ͽ���    �L��                ����                ����    ����    =���>L��=��ͽ���=���=���            ���ͽ���    ���ͽ���        =���    ���ͽ��;L�ͽ���    ����                =���>L��>L��>L��>L��>���>���?   ?   ?   ?��?333?333?333?L��?L��?fff?fff?�  ?���?���?���?���?�ff?�33?�33?�  ?���?���?ٙ�?ٙ�?�ff?�ff?�33@   @   @ff@ff@��@��@33@33@��@   @&ff@&ff@&ff@,��@333@333@@  @@  @Fff@L��@S33@Y��@Y��@`  @fff@l��@s33@y��@y��@�  @�33@�ff@���@���@���@�  @�33@�ff@���@���@�  @�33@�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�33@�ff@ə�@���@�  @�33@�ff@���@���@�  @�33@�ff@陚@���@�  @�  @�ff@���@���A   A��A33A��AffA  A33A��AffA  A��A33AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A.ffA0  A1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AFffAH  AI��AK33AL��AP  AQ��AS33AT��AVffAY��A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���Aə�A�33A�  A͙�A�ffA�33A���Aљ�A�33A�  Aՙ�A�ffA�  A���A�ffA�33Dp�3Dq  DqfDq�Dq�Dq  Dq,�Dq33Dq9�DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr�Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrS3Dr` DrffDrl�Dry�Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Ds� Ds�fDs��Ds��Ds� Ds��Ds�3Ds��Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Dt��Duf@&ff@,��@333@333@@  @@  @Fff@L��@S33@Y��@Y��@`  @fff@l��@s33@y��@y��@�  @�33@�ff@���@���@���@�  @�33@�ff@���@���@�  @�33@�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�33@�ff@ə�@���@�  @�33@�ff@���@���@�  @�33@�ff@陚@���@�  @�  @�ff@���@���A   A��A33A��AffA  A33A��AffA  A��A33AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A.ffA0  A1��A333A6ffA8  A9��A;33A<��A@  AA��AC33AFffAH  AI��AK33AL��AP  AQ��AS33AT��AVffAY��A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33AvffAx  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���Aə�A�33A�  A͙�A�ffA�33A���Aљ�A�33A�  Aՙ�A�ffA�  A���A�ffA�33Dp�3Dq  DqfDq�Dq�Dq  Dq,�Dq33Dq9�DqFfDqL�DqS3Dq` DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq�fDq�3DqٚDq� Dq��Dq�3Dr  DrfDr�Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrS3Dr` DrffDrl�Dry�Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Ds� Ds�fDs��Ds��Ds� Ds��Ds�3Ds��Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Dt��DufG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @7�@�=q@�=qA�A!�AB�RAa�A��\A��\A��\A��\A��\AЏ\A�\)A��\B G�BG�BG�BG�B G�B(G�B0�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF+�CH�CJ�CL�CN�CP�CR�CT�CV�CX+�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9��D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dp�Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Dt�{Dy�D��D�^fD���D�� D� D�_\D���D�ϮD� D�LzD���DǽD��\D� QD�b�DಏD��pD�&�D�zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��k� �k� �����k� �����k� �k� ���k� �k� �������k� �k� ��=�\)�k� �k� �������k� �k� �����k� ���k� �k� �����k� ��=�\)=�\)�k� =�\)�����k� ���k� �k� �k� ���k� �k� �����k� ���k� �����k� ���k� �����k� �k� �k� �����k� �����k� �k� �k� ���k� �k� �k� �����k� �k� �k� ���k� �����k� �k� ���k� ���k� �k� �����k� ���k� �k� �k� �k� �������k� �k� �k� �k� =�\)�k� �k� �k� �������k� �k� �k� �k� �k� �������k� �����k� �k� ���k� �k� �k� �����k� �k� �k� �k� ���k� �k� �k� ���k� �k� �k� >.{>�=q=�\)�k� �k� �k� �k� �����k� �k� �����k� �k� ���k� �k� �k� ��>.{=�\)�k� �k� ��=�\)�����k� ���������k� �k� ��>.{=�\)�k� �k� �k� �k� �����k� �������k� ���k� �������k� ���k� ���k� �k� �������k� ���k� ����=�\)=�\)���k� ��������=�\)���k� =�\)������������=�\)����=�\)=�\)>.{=�\)����=�\)=�\)>.{>�=q������=�\)=�\)=�\)=�\)=�\)=�\)����������=�\)=�\)��=�\)����������=�\)=�\)=�\)=�\)����>.{=�\)>.{��=�\)=�\)=�\)��=�\)��=�\)����=�\)���k� ����=�\)=�\)=�\)��=�\)=�\)��=�\)������=�\)=�\)��=�\)=�\)=�\)=�\)>.{������=�\)��=�\)=�\)������������>.{=�\)��=�\)������=�\)=�\)=�\)����=�\)��=�\)>.{=�\)��������=�\)>.{=�\)��=�\)=�\)=�\)����=�\)����=�\)=�\)��=�\)=�\)>.{��>.{>.{>.{=�\)����=�\)=�\)����=�\)=�\)��=�\)=�\)>�=q=�\)=�\)=�\)=�\)������=�\)��=�\)=�\)��=�\)=�\)��=�\)=�\)>.{>�p�>.{��������=�\)>.{=�\)=�\)=�\)=�\)=�\)������=�\)��=�\)>.{=�\)����=�\)������=�\)��=�\)=�\)=�\)=�\)��=�\)=�\)=�\)=�\)��=�\)��=�\)>.{>�=q>.{��>.{>.{=�\)=�\)=�\)����=�\)����=�\)=�\)>.{=�\)��������=�\)��=�\)=�\)=�\)=�\)>.{>�=q>�=q>�=q>�=q>��>�p�?�?�?�?+�?E�?E�?E�?^�R?^�R?xQ�?xQ�?���?�?�?��]?��]?�\)?�(�?�(�?���?�?�?�]?�]?�\)?�\)?�(�@z�@z�@
�G@
�G@G�@G�@�@�@{@$z�@*�G@*�G@*�G@1G�@7�@7�@Dz�@Dz�@J�G@QG�@W�@^{@^{@dz�@j�G@qG�@w�@~{@~{@�=q@�p�@���@��@�
>@�
>@�=q@�p�@���@��@��@�=q@�p�@�p�@���@��@�
>@�=q@�p�@���@��@��@�=q@�p�@�p�@ȣ�@��@�
>@�=q@�p�@أ�@�
>@�
>@�=q@�p�@��@��@�
>@�=q@�=q@���@��@�
>A�A�RAQ�A�A�A	�AQ�A�A�A�A�RAQ�A�A�A�RAQ�A�A!�A"�RA$Q�A%�A)�A*�RA,Q�A/�A1�A2�RA4Q�A7�A9�A:�RA<Q�A=�AA�AB�RADQ�AG�AI�AJ�RALQ�AM�AQ�AR�RATQ�AU�AW�AZ�RA\Q�A]�A_�Ab�RAdQ�Ae�Ai�Aj�RAlQ�Am�Aq�Ar�RAtQ�Aw�Ay�Az�RA|Q�A�A��\A�(�A���A�A��\A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A��\A�\)A���A�A��\A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A�\)A�(�A���A��\A�\)A���A�A�\)A�(�A���A��\A�\)A���A�A�\)A�(�A���Aď\A�\)A���A�A�\)A�(�A�Ȁ\A�(�A���A�A�\)A�(�A�Aԏ\A�(�A���A؏\A�\)A���A�Dp��Dq{Dq
�DqHDqDq${Dq1HDq7�Dq>DqJ�DqQHDqW�Dqd{Dqj�DqqHDq~Dq�{Dq�HDq��Dq�Dq��Dq�HDq�Dq�{Dq��Dq׮Dq�Dq�{Dq�HDq��Dr{Dr
�DrHDrDr${Dr1HDr7�Dr>DrJ�DrQHDrW�Drd{Drj�DrqHDr~Dr�{Dr�HDr��Dr�Dr��Dr�HDr��Dr�{Dr��Dr�HDr�Dr�{Dr�HDr��Dr�Ds
�DsHDs�Ds${Ds*�Ds7�Ds>DsD{DsQHDsW�Ds^Dsj�DsqHDsw�Ds�{Ds��Ds�HDs�Ds�{Ds�HDs��Ds�Ds��Ds�HDs�Ds�{Ds��Ds��Ds�Dt{DtHDt�DtDt*�Dt1HDt>DtD{DtJ�DtQHDt^Dtd{DtqHDtw�Dt~Dt��Dt�HDt��Dt�{Dt��Dt�HDt�Dt�{Dt��Dt׮Dt�Dt�{Dt�HDt��Dt�Du
�@*�G@1G�@7�@7�@Dz�@Dz�@J�G@QG�@W�@^{@^{@dz�@j�G@qG�@w�@~{@~{@�=q@�p�@���@��@�
>@�
>@�=q@�p�@���@��@��@�=q@�p�@�p�@���@��@�
>@�=q@�p�@���@��@��@�=q@�p�@�p�@ȣ�@��@�
>@�=q@�p�@أ�@�
>@�
>@�=q@�p�@��@��@�
>@�=q@�=q@���@��@�
>A�A�RAQ�A�A�A	�AQ�A�A�A�A�RAQ�A�A�A�RAQ�A�A!�A"�RA$Q�A%�A)�A*�RA,Q�A/�A1�A2�RA4Q�A7�A9�A:�RA<Q�A=�AA�AB�RADQ�AG�AI�AJ�RALQ�AM�AQ�AR�RATQ�AU�AW�AZ�RA\Q�A]�A_�Ab�RAdQ�Ae�Ai�Aj�RAlQ�Am�Aq�Ar�RAtQ�Aw�Ay�Az�RA|Q�A�A��\A�(�A���A�A��\A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A��\A�\)A���A�A��\A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A�\)A�(�A���A��\A�\)A���A�A�\)A�(�A���A��\A�\)A���A�A�\)A�(�A���Aď\A�\)A���A�A�\)A�(�A�Ȁ\A�(�A���A�A�\)A�(�A�Aԏ\A�(�A���A؏\A�\)A���A�Dp��Dq{Dq
�DqHDqDq${Dq1HDq7�Dq>DqJ�DqQHDqW�Dqd{Dqj�DqqHDq~Dq�{Dq�HDq��Dq�Dq��Dq�HDq�Dq�{Dq��Dq׮Dq�Dq�{Dq�HDq��Dr{Dr
�DrHDrDr${Dr1HDr7�Dr>DrJ�DrQHDrW�Drd{Drj�DrqHDr~Dr�{Dr�HDr��Dr�Dr��Dr�HDr��Dr�{Dr��Dr�HDr�Dr�{Dr�HDr��Dr�Ds
�DsHDs�Ds${Ds*�Ds7�Ds>DsD{DsQHDsW�Ds^Dsj�DsqHDsw�Ds�{Ds��Ds�HDs�Ds�{Ds�HDs��Ds�Ds��Ds�HDs�Ds�{Ds��Ds��Ds�Dt{DtHDt�DtDt*�Dt1HDt>DtD{DtJ�DtQHDt^Dtd{DtqHDtw�Dt~Dt��Dt�HDt��Dt�{Dt��Dt�HDt�Dt�{Dt��Dt׮Dt�Dt�{Dt�HDt��Dt�Du
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�(�A��A��A�%A���A��A���A��FA���A��PA��A�|�A�r�A�VA�?}A�9XA�5?A�&�A��A�%A��A��;A���A�I�A�$�A�%A���A�S�A�33A��^A��hA���A��A�{A�JA�A�S�A���A��A�;dA��\A�(�A��9A�1'A��jA�M�A�7LA���A��A�/A�;dA��A���A��A�7LA��HA���A�`BA�`BA��!A�VA��A�M�A��uA���A���A�ZA��HA��A��7A��yA�E�A�l�A�K�A�?}A���A�%A�ƨA�$�A�ZA��A���A��PA�VA��\A�7LA�  A�C�A�1A��A���A�`BA���A��RA�VA���A���A�r�A�A���A���A���A�l�A��HA��A�$�AK�Ax�AuAsG�Apz�An�9Am��Al��Ak%Af�yAe�hAdM�A_��A]VA\�+AZ$�AX�RAV�9AU�-AS/AM�PAK��AK%AJ��AJ$�AI��AI33AH��AG��AF^5AE�#AE�PAEO�AD�9AC��AC�AB1'AA?}A?��A>(�A=�A;|�A:�A6�/A5�A4��A2�/A1�mA0�A/�A/XA/oA.��A.(�A-�-A-�A,��A+33A)�A'XA&A�A%��A$��A#��A#+A"�/A"r�A"$�A!�mA!A �AdZA33A��A�A;dA-AȴA�A��AVA�A|�A&�A��AAI�A�;A`BA�FA��AhsA`BA��At�A	��AbNA�A��A��AdZA`BAA�A(�A�DA�A�AC�A ��@�K�@�Q�@�E�@��@��@��@��9@�l�@�M�@�%@�F@��/@�S�@�+@�/@�t�@���@�D@��m@�S�@�v�@��@�x�@��@߮@��@�v�@��@�Q�@��@��;@���@�(�@�b@��;@�S�@�5?@��T@���@��@̋D@�33@���@�@��`@ǥ�@��@�l�@��;@Ƈ+@�hs@�Z@�o@��#@�G�@�Q�@��m@��w@���@�33@�=q@��-@��@��y@�n�@�$�@�r�@��u@��/@��@��P@�@�`B@���@�l�@�|�@�@�@���@�Z@�1'@��w@���@��F@�b@��m@��!@��R@���@�~�@�^5@��@��#@�x�@�O�@�/@��@��@�r�@�Z@� �@���@��j@��;@��w@��@��@�l�@�33@��R@�@���@���@���@���@�V@�33@�-@��@���@��@��`@�%@��@��7@�O�@�Ĝ@�I�@��@�C�@���@��\@��u@��`@��!@���@���@�E�@�dZ@�E�@��@���@���@�bN@��/@��@��@�O�@�&�@��@�V@��+@���@���@���@�K�@��@���@�@��9@��u@�j@��@�
=@���@�ȴ@�ȴ@���@�ȴ@��@���@�v�@�ff@�M�@��+@��!@�ȴ@�ȴ@���@�~�@�V@�@���@�O�@�?}@�%@��u@��u@��D@��@�Q�@��
@��P@�\)@�+@���@���@���@��+@�~�@�n�@�^5@�E�@���@�/@��@��@��`@���@��u@��@��m@���@�|�@�K�@�@�ȴ@�~�@�M�@�@���@�hs@�hs@�O�@�?}@�/@��@���@���@�z�@�bN@�I�@�9X@��@���@���@���@�C�@�+@���@��R@��!@���@���@��\@�v�@�E�@�$�@�@��^@�x�@�`B@�O�@�?}@�/@�&�@��@�V@��/@��@�bN@��@��
@��@�|�@�|�@�|�@�t�@�|�@��@�S�@�ȴ@���@��\@v?@mj@d��@[��@S�k@I��@C�@;A�@6��@2q�@,Ĝ@&��@"�@��@#�@L�@�"@��@	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A��FA�JA���A���A�7LA��DA�ƨA�E�A��A���A�K�A�VA� �A�XA�^5A�&�A�9XA���A�oA���A�ƨA�9XA�t�A�|�A��A��uA�$�A�ffA�E�A�
=A�A�=qA��A�l�A�9XA�E�A��hA���A�K�A�33A�VA�K�A��
A��A�ȴA��A�A���A���A�7LA���A�A�A��yA��wA�9XA��A�-A��A��;A��TA�K�A���A�`BA�  A�VA�K�A�-A�
=A���A��A���A��A�S�A�l�A�hsA���A�-A�O�A�t�A�p�A���A� �A��DA�/A�33A�"�A�1'A���A���A���A��A�oA�?}A�1'A�ZA�A�
=A�p�A�jA��A�33A�~�A�t�A��A��+A�r�A�S�A�(�A�oA��A��A�O�A�9XA�-A�VA��RA�A�v�A��A�;dA��#A��uA�ĜA�G�A�33A��A�S�A�;dA��A�9XA�A�M�A�/A��mA���A�dZA�^5A��A���A���A�$�A��A�bNA�v�A�9XA�=qA�?}A�33A��A���A��A���A�&�A�%A��A�r�A�7LA�bNA�ffA�^5A��A��A�\)A��A�A�A�=qA�$�A�1'A���A�9XA�33A�=qA���A��mA��RA�=qA�G�A�A�^5A�1A�;dA�=qA�ĜA��uA��FA��wA��wA�7LA�-A���A�=qA�A�A��+A���A�E�A��PA�A�A�5?A�ĜA�1A�n�A�~�A�
=A��A��!A�/A�E�A��
A�"�A�ĜA�G�A�I�A�C�A�=qA�9XA�n�A�33A�?}A�;dA�?}A�A�A�A�A�7LA�A�E�A�S�A�O�A�E�A�M�A���A�A�A�I�A�G�A�E�A�K�A�I�A�?}A�S�A�S�A�K�A�G�A�I�A�A�A�A�A�C�A�A�C�A�C�A�E�A�G�A�O�A�K�A�9XA�/A�E�A�?}A�;dA�;dA�=qA�9XA�9XA�I�A�M�A�K�A�M�A�M�A�Q�A�E�A�K�A�VA�G�A�Q�A�C�A�;dA�A�A�E�A��yA�C�A�G�A�A�A�C�A�K�A�C�A�I�A�I�A�O�A�O�A�K�A�E�A�M�A�G�A�K�A�A�A�S�A�9XA�Q�A�VA�VA�M�A�S�A�K�A��A�G�A�Q�A�M�A�XA�O�A�ZA�S�A�I�A�S�A�(�A�G�A�C�A�A�A�O�A�Q�A�M�A�I�A�VA�O�A�/A�E�A�O�A�XA�Q�A�K�A�G�A�^5A�S�A�Q�A�E�A�M�A�E�A�O�A�Q�A�Q�A�I�A�G�A�ZA�M�A�Q�A�S�A�I�A�S�A�M�A�G�A�XA�K�A�O�A�K�A�C�A�G�A�E�A�I�A�G�A�M�A�XA�dZA�ZA�K�A�O�A�O�A�K�A�Q�A�G�A�VA�G�A�G�A�G�A�M�A�E�A�C�A�I�A�I�A�O�A�VA�E�A�Q�A�O�A�I�A�?}A�=qA�?}A�?}A�C�A�G�A�I�A�M�A�I�A�A�A�A�A�K�A�K�A�C�A�O�A�C�A�=qA�ZA�7LA�bNA�Q�A�\)A�O�A�M�A�I�A�VA�O�A�I�A�^5A�Q�A�VA�M�A�M�A�G�A�M�A�M�A�M�A�G�A�E�A�K�A�E�A�A�A�G�A�C�A�=qA�K�A�G�A�K�A�Q�A�G�A�E�A�E�A�C�A�=qA�C�A�VA�Q�A�S�A�K�A�E�A�VA�M�A�Q�A�Q�A�K�A�O�A�K�A��A�G�A�XA�M�A�G�A�I�A�K�A�I�A�?}A�?}A�A�x�A�A�A�K�A�K�A�C�A�5?A�=qA�9XA�9XA�A�A�G�A�K�A�K�A�K�A�E�A�G�A�O�A�M�A�K�A�A�A�K�A�A�A�K�A�E�A�C�A�=qA�=qA�E�A�=qA�9XA�9XA�7LA�1'A�1'A�5?A�1'A�/A�-A�1'A�33A�-A�/A�/A�+A�5?A�-A�1'A�-A�/A�+A��A�33A�1'A�5?A�5?A�1'A�5?A�;dA�A�A�I�A�?}A�E�A�9XA�33A�33A�33A�33A�+A�1'A�33A�1'A�33A�-A�-A�1'A�1'A�$�A�(�A�$�A�(�A�/A�+A�+A�-A�(�A�$�A�-A�&�A�(�A�"�A�(�A�-A� �A�9XA�33A�7LA�1'A�7LA�1'A�/A�33A�(�A�/A�/A�&�A�$�A�$�A�5?A�+A�+A�$�A��A�-A�1'A�$�A�&�A�$�A� �A� �A�"�A�"�A�"�A��A�$�A�$�A�oA�bA�1A��A��A��A�{A��A��A��A�JA�%A�1A�%A�A�A�A�  A�%A�%A�oA�A�A�A�1A�A�1A�A�%A�1A�%A�
=A�%A�A�VA�%A�A�
=A�A�A��A���A��A��
A���A���A���A��
A���A���A��
A���A��/A��A��A��A��
A���A��/A���A���A���A���A�A�ȴA���A��wA���A��wA��jA��jA��wA��-A��-A��RA��A��A��A��A��!A��A��!A��A���A���A���A��uA���A��hA��uA���A��uA���A��hA��hA���A���A��uA��uA��hA���A��PA��DA��DA��DA��7A��DA��DA��7A��+A��A�~�A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A��A�~�A�|�A�|�A�z�A�z�A�z�A�v�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�t�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�l�@�l�@�l�@�l�@�l�@�l�@�l�@�l�@�|�@�|�@��P@��@��@��P@��P@��P@��P@��P@��P@��P@��P@��P@���@���@��P@��P@���@��P@�t�@�l�@�l�@�dZ@�dZ@�\)@�S�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�C�@�;d@��y@��@�ȴ@��R@��R@��!@��!@���@���@���@���@���@���@���@���@���@���@���@��\@��\@���@��\A�1'A�1'A�1'A�33A�1'A�5?A�5?A�5?A�1'A�5?A�/A�(�A�(�A�+A�-A�(�A�+A�(�A�&�A�$�A�"�A�&�A�$�A�$�A�&�A�$�A�"�A��A��A�"�A��A��A��A��A��A��A��A��A��A��A��A��A�$�A�"�A�$�A� �A� �A� �A� �A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A�{A��A�{A��A��A��A��A��A�JA�%A�%A���A�%A�%A�1A�VA�JA�
=A�A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�A�%A�A�  A�  A���A��TA���A�ȴA���A�ȴA���A���A���A���A���A���A���A��
A���A���A�ȴA���A���A�ȴA���A��jA�ĜA��RA��FA��jA��RA��^A��^A��RA��RA��RA��FA��-A��A��A���A���A���A���A���A���A��A���A���A���A���A��PA��\A��PA��hA��uA��uA���A��uA��\A��PA��hA��\A��hA��PA��DA��7A��7A��+A��+A��7A��7A��+A��A��A�~�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�|�A�|�A�~�A�|�A�~�A�|�A�~�A�|�A�|�A�z�A�x�A�x�A�x�A�v�A�t�A�t�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�t�@�t�@�l�@�l�@�t�@�|�@�|�@��@��@��P@��@��P@��P@��P@��P@���@��P@��P@��P@���@���@���@���@���@��P@��@�l�@�l�@�l�@�dZ@�dZ@�\)@�S�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�C�@�
=@��@���@���@��R@��R@��!@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�1'A�(�A��A��A�%A���A��A���A��FA���A��PA��A�|�A�r�A�VA�?}A�9XA�5?A�&�A��A�%A��A��;A���A�I�A�$�A�%A���A�S�A�33A��^A��hA���A��A�{A�JA�A�S�A���A��A�;dA��\A�(�A��9A�1'A��jA�M�A�7LA���A��A�/A�;dA��A���A��A�7LA��HA���A�`BA�`BA��!A�VA��A�M�A��uA���A���A�ZA��HA��A��7A��yA�E�A�l�A�K�A�?}A���A�%A�ƨA�$�A�ZA��A���A��PA�VA��\A�7LA�  A�C�A�1A��A���A�`BA���A��RA�VA���A���A�r�A�A���A���A���A�l�A��HA��A�$�AK�Ax�AuAsG�Apz�An�9Am��Al��Ak%Af�yAe�hAdM�A_��A]VA\�+AZ$�AX�RAV�9AU�-AS/AM�PAK��AK%AJ��AJ$�AI��AI33AH��AG��AF^5AE�#AE�PAEO�AD�9AC��AC�AB1'AA?}A?��A>(�A=�A;|�A:�A6�/A5�A4��A2�/A1�mA0�A/�A/XA/oA.��A.(�A-�-A-�A,��A+33A)�A'XA&A�A%��A$��A#��A#+A"�/A"r�A"$�A!�mA!A �AdZA33A��A�A;dA-AȴA�A��AVA�A|�A&�A��AAI�A�;A`BA�FA��AhsA`BA��At�A	��AbNA�A��A��AdZA`BAA�A(�A�DA�A�AC�A ��@�K�@�Q�@�E�@��@��@��@��9@�l�@�M�@�%@�F@��/@�S�@�+@�/@�t�@���@�D@��m@�S�@�v�@��@�x�@��@߮@��@�v�@��@�Q�@��@��;@���@�(�@�b@��;@�S�@�5?@��T@���@��@̋D@�33@���@�@��`@ǥ�@��@�l�@��;@Ƈ+@�hs@�Z@�o@��#@�G�@�Q�@��m@��w@���@�33@�=q@��-@��@��y@�n�@�$�@�r�@��u@��/@��@��P@�@�`B@���@�l�@�|�@�@�@���@�Z@�1'@��w@���@��F@�b@��m@��!@��R@���@�~�@�^5@��@��#@�x�@�O�@�/@��@��@�r�@�Z@� �@���@��j@��;@��w@��@��@�l�@�33@��R@�@���@���@���@���@�V@�33@�-@��@���@��@��`@�%@��@��7@�O�@�Ĝ@�I�@��@�C�@���@��\@��u@��`@��!@���@���@�E�@�dZ@�E�@��@���@���@�bN@��/@��@��@�O�@�&�@��@�V@��+@���@���@���@�K�@��@���@�@��9@��u@�j@��@�
=@���@�ȴ@�ȴ@���@�ȴ@��@���@�v�@�ff@�M�@��+@��!@�ȴ@�ȴ@���@�~�@�V@�@���@�O�@�?}@�%@��u@��u@��D@��@�Q�@��
@��P@�\)@�+@���@���@���@��+@�~�@�n�@�^5@�E�@���@�/@��@��@��`@���@��u@��@��m@���@�|�@�K�@�@�ȴ@�~�@�M�@�@���@�hs@�hs@�O�@�?}@�/@��@���@���@�z�@�bN@�I�@�9X@��@���@���@���@�C�@�+@���@��R@��!@���@���@��\@�v�@�E�@�$�@�@��^@�x�@�`B@�O�@�?}@�/@�&�@��@�V@��/@��@�bN@��@��
@��@�|�@�|�@�|�@�t�@�|�@��@�S�@�ȴG�O�@��\@v?@mj@d��@[��@S�k@I��@C�@;A�@6��@2q�@,Ĝ@&��@"�@��@#�@L�@�"@��@	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A��FA�JA���A���A�7LA��DA�ƨA�E�A��A���A�K�A�VA� �A�XA�^5A�&�A�9XA���A�oA���A�ƨA�9XA�t�A�|�A��A��uA�$�A�ffA�E�A�
=A�A�=qA��A�l�A�9XA�E�A��hA���A�K�A�33A�VA�K�A��
A��A�ȴA��A�A���A���A�7LA���A�A�A��yA��wA�9XA��A�-A��A��;A��TA�K�A���A�`BA�  A�VA�K�A�-A�
=A���A��A���A��A�S�A�l�A�hsA���A�-A�O�A�t�A�p�A���A� �A��DA�/A�33A�"�A�1'A���A���A���A��A�oA�?}A�1'A�ZA�A�
=A�p�A�jA��A�33A�~�A�t�A��A��+A�r�A�S�A�(�A�oA��A��A�O�A�9XA�-A�VA��RA�A�v�A��A�;dA��#A��uA�ĜA�G�A�33A��A�S�A�;dA��A�9XA�A�M�A�/A��mA���A�dZA�^5A��A���A���A�$�A��A�bNA�v�A�9XA�=qA�?}A�33A��A���A��A���A�&�A�%A��A�r�A�7LA�bNA�ffA�^5A��A��A�\)A��A�A�A�=qA�$�A�1'A���A�9XA�33A�=qA���A��mA��RA�=qA�G�A�A�^5A�1A�;dA�=qA�ĜA��uA��FA��wA��wA�7LA�-A���A�=qA�A�A��+A���A�E�A��PA�A�A�5?A�ĜA�1A�n�A�~�A�
=A��A��!A�/A�E�A��
A�"�A�ĜA�G�A�I�A�C�A�=qA�9XA�n�A�33A�?}A�;dA�?}A�A�A�A�A�7LA�A�E�A�S�A�O�A�E�A�M�A���A�A�A�I�A�G�A�E�A�K�A�I�A�?}A�S�A�S�A�K�A�G�A�I�A�A�A�A�A�C�A�A�C�A�C�A�E�A�G�A�O�A�K�A�9XA�/A�E�A�?}A�;dA�;dA�=qA�9XA�9XA�I�A�M�A�K�A�M�A�M�A�Q�A�E�A�K�A�VA�G�A�Q�A�C�A�;dA�A�A�E�A��yA�C�A�G�A�A�A�C�A�K�A�C�A�I�A�I�A�O�A�O�A�K�A�E�A�M�A�G�A�K�A�A�A�S�A�9XA�Q�A�VA�VA�M�A�S�A�K�A��A�G�A�Q�A�M�A�XA�O�A�ZA�S�A�I�A�S�A�(�A�G�A�C�A�A�A�O�A�Q�A�M�A�I�A�VA�O�A�/A�E�A�O�A�XA�Q�A�K�A�G�A�^5A�S�A�Q�A�E�A�M�A�E�A�O�A�Q�A�Q�A�I�A�G�A�ZA�M�A�Q�A�S�A�I�A�S�A�M�A�G�A�XA�K�A�O�A�K�A�C�A�G�A�E�A�I�A�G�A�M�A�XA�dZA�ZA�K�A�O�A�O�A�K�A�Q�A�G�A�VA�G�A�G�A�G�A�M�A�E�A�C�A�I�A�I�A�O�A�VA�E�A�Q�A�O�A�I�A�?}A�=qA�?}A�?}A�C�A�G�A�I�A�M�A�I�A�A�A�A�A�K�A�K�A�C�A�O�A�C�A�=qA�ZA�7LA�bNA�Q�A�\)A�O�A�M�A�I�A�VA�O�A�I�A�^5A�Q�A�VA�M�A�M�A�G�A�M�A�M�A�M�A�G�A�E�A�K�A�E�A�A�A�G�A�C�A�=qA�K�A�G�A�K�A�Q�A�G�A�E�A�E�A�C�A�=qA�C�A�VA�Q�A�S�A�K�A�E�A�VA�M�A�Q�A�Q�A�K�A�O�A�K�A��A�G�A�XA�M�A�G�A�I�A�K�A�I�A�?}A�?}A�A�x�A�A�A�K�A�K�A�C�A�5?A�=qA�9XA�9XA�A�A�G�A�K�A�K�A�K�A�E�A�G�A�O�A�M�A�K�A�A�A�K�A�A�A�K�A�E�A�C�A�=qA�=qA�E�A�=qA�9XA�9XA�7LA�1'A�1'A�5?A�1'A�/A�-A�1'A�33A�-A�/A�/A�+A�5?A�-A�1'A�-A�/A�+A��A�33A�1'A�5?A�1'A�1'A�1'A�33A�1'A�5?A�5?A�5?A�1'A�5?A�/A�(�A�(�A�+A�-A�(�A�+A�(�A�&�A�$�A�"�A�&�A�$�A�$�A�&�A�$�A�"�A��A��A�"�A��A��A��A��A��A��A��A��A��A��A��A��A�$�A�"�A�$�A� �A� �A� �A� �A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A�{A��A�{A��A��A��A��A��A�JA�%A�%A���A�%A�%A�1A�VA�JA�
=A�A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�A�%A�A�  A�  A���A��TA���A�ȴA���A�ȴA���A���A���A���A���A���A���A��
A���A���A�ȴA���A���A�ȴA���A��jA�ĜA��RA��FA��jA��RA��^A��^A��RA��RA��RA��FA��-A��A��A���A���A���A���A���A���A��A���A���A���A���A��PA��\A��PA��hA��uA��uA���A��uA��\A��PA��hA��\A��hA��PA��DA��7A��7A��+A��+A��7A��7A��+A��A��A�~�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�|�A�|�A�~�A�|�A�~�A�|�A�~�A�|�A�|�A�z�A�x�A�x�A�x�A�v�A�t�A�t�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�t�@�t�@�l�@�l�@�t�@�|�@�|�@��@��@��P@��@��P@��P@��P@��P@���@��P@��P@��P@���@���@���@���@���@��P@��@�l�@�l�@�l�@�dZ@�dZ@�\)@�S�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�C�@�
=@��@���@���@��R@��R@��!@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���A�1'A�1'A�1'A�33A�1'A�5?A�5?A�5?A�1'A�5?A�/A�(�A�(�A�+A�-A�(�A�+A�(�A�&�A�$�A�"�A�&�A�$�A�$�A�&�A�$�A�"�A��A��A�"�A��A��A��A��A��A��A��A��A��A��A��A��A�$�A�"�A�$�A� �A� �A� �A� �A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A�{A��A�{A��A��A��A��A��A�JA�%A�%A���A�%A�%A�1A�VA�JA�
=A�A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�A�%A�A�  A�  A���A��TA���A�ȴA���A�ȴA���A���A���A���A���A���A���A��
A���A���A�ȴA���A���A�ȴA���A��jA�ĜA��RA��FA��jA��RA��^A��^A��RA��RA��RA��FA��-A��A��A���A���A���A���A���A���A��A���A���A���A���A��PA��\A��PA��hA��uA��uA���A��uA��\A��PA��hA��\A��hA��PA��DA��7A��7A��+A��+A��7A��7A��+A��A��A�~�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�|�A�|�A�~�A�|�A�~�A�|�A�~�A�|�A�|�A�z�A�x�A�x�A�x�A�v�A�t�A�t�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�l�@�t�@�t�@�l�@�l�@�t�@�|�@�|�@��@��@��P@��@��P@��P@��P@��P@���@��P@��P@��P@���@���@���@���@���@��P@��@�l�@�l�@�l�@�dZ@�dZ@�\)@�S�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�K�@�C�@�
=@��@���@���@��R@��R@��!@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�bN>�X@�Y�>��?&j@�cs@X>BmH@�g�@>8�=�g=�Ц@w�,@�f�=�=�5>p��@�hI@�K�=�v�=��>M�@�f{?� �=˱�>	,�@8=�Zq>�H�=� T=�~�>K�b@�M�?�>>W}�@�f{@�f�@�L>ȫ@�i�@�do=��>^J?���=��> S�@�ek=�=��4@$�s@�c�?tZ>:�r@�aR>%b�@�d>��@�a�=�}>[dZ@�z:>�$t=��=�E>T�@@R�f?�>
�`@$�?kz�=���>1�?�LY?�$=��,>�F5?z�Y?ሐ>���>]�? �,@/��?���>�B�@�d>�s>Ȋ@��@�c�>.`�@�]�=�:?P�@�c�@�b>\(�@f<=�p�=�9@u��>X�m@�`�@�cs>�L�>�w?�\>#�?���@�a=���=�[�>D=?���@�`=�:~>-�>��=�w>U��>%!@�h�@�_�=�b�?��o@�j@@�$5> If>�,�@�i�=��>=��>Dڐ@�i�@�f�=�x=�If?	�>u�;@ޔ?��>�?@��@�fQ=ͳ}>��@�82@�f'@�f{@�+A>�p@�4?,�>g��@�[�@�f�=��[>k�@�e,=���>
�?\K�@�f�=���?���?�!@�d�@�d�@��#>�@8��@�f�@�d@�f'=�AJ?��6>dE$@�f�@�f{@Ff�>�N>I<�@�bc@�a�@�`=���=��p=��!@�@�]�@�s>O�C@�aR@�_F>�M@\��@��>�}@�c@�c�@�d?��@�7�@|�@�eV@��@���?GY�@�_�?��@=�@�g8>�<!@�h�@�g8@�e�@�f'@9L�?��\@�h�@�h4@�g8@�j�@�h4@�f{?��@�do@�g#@�j@@�g8@�e�@b�C@�f�@�g�@�f'@�d@�f@�iD@�f{@�g#@�h�@�h�@�f�@�g�@�g8@�f{@�f'@k�<@�h4@�g8@�g�@�f{@�e�@�h�@�h�@�iD@�i/@�iD@�h�@�f'@�iD@�iD@���@�j�@�kQ@�l@�h�@�h�@�j@@�j�@�g�@�kQ@�g�@�f{@�f{@�i�@�h�@�kQ@�j�@�j@@�j�@�j@@�k�@�i�@�g�@�g8@�iD@�j�@�g�@�j@�o�@�g�@�j�@�j�@�iD@�iD@n�@�m]@�k�@�m	@�m�@�m�@�l�@?`�@�i�@�k�@�m	@�k�@�mH@�o~@�m�@�kQ@�n�@�n@�m]@�k�@�l@�h�@�m	@�kQ@�m]@�n@�l�@�l@�m	@�j�@�k�@�m	@�m	@�m	@�l@�j�@�la@�n�@�m	@�l�@�l�@�m	@�l�@�n@�m]@�m]@�m�@�m�@�m]@�n@�n@�m�@�m]@�m	@�m	@�l@�m]@�la@�kQ@�n@�n@�m�@�k�@�p&@�nn@�m	@�m	@�h�@�n@�m	@�kQ@�j�@�m�@�kQ@�i�@�h�@�l�@�kQ@�iD@�j�@�i�@�h�@�k�@�m	@�nD@�k�@�j@@�j@@�j@@�j�@�i�@�h�@�j�@�j�@�l�@�l�@�m	@�m	@�n�@�m]@�iD@�j�@�la@�i�@�i�@��@�l@�h�@�j�@�j�@�j@@�iD@�kQ@�n@�j�@�nn@�nn@�l@�k�@�h�@�j�@�m]@�i�@�h�@�i�@�m	@�la@�i�@�h�@�iD@�j�@�j�@�k�@�kQ@�j@@�l@�m]@�j�@�l�@�k�@�kQ@�l�@�j@@�j@@�j@@�k<@�i�@�i�@�m	@�i�@�j@@�l@�nn@�l�@��@�h�@�j@@�i�@�k�@�i�@�iD@�h�@�h�@�j�@�g�?5%�@�i�@�g�@�h4@�h�@�hI@�g8@�i�@�hI@�i�@�j�@�kQ@�i�@�i�@�kQ@�h�@�h�@�j+@�hI@�iD@�g�@�i�@�jU@�jU@�h�@�g�@�j@�h�@�h�@�g8@�d@�d�@�d�@�d�@�d@�cs@�c�@�c@�c�@�d0@�d@�d@�d�@�d�@�d@�d@�cs@�bc@�c�@�cs@�f<@�f<@�h4@�g�@�f�@�g�@�h�@�h�@�g#@�hI@�j@�d�@�d�@�f�@�e�@�f<@�f�@�e,@�f<@�f�@�f�@�d�@�c�@�d0@�cs@�c @�c�@�d�@�d�@�d0@�c @�c @�bx@�bx@�`�@�`W@�a�@�bx@�a@�bx@�b@�^�@�cs@�f�@�f�@�f�@�g�@�f<@�d�@�c�@�b�@�d�@�e�@�a�@�a�@�b$@�b$@�b�@�c�@�bx@�d0@�c�@�c�@�c @�b$@�ag@�`�@�`k@�bx@�a�@�a@�`k@�_[@�_�@�]O@�Y6@�Y6@�Y�@�[B@�[�@�Z�@�Z�@�[�@�Z�@�X�@�V@�U@�V@�U�@�U�@�T�@�U�@�U\@�U@�U@�T�@�U�@�U�@�T�@�T�@�V�@�Vm@�W*@�W*@�Vm@�W~@�V.@�T�@�V.@�V�@�U�@�Se@�S@�QD@�L@�E9@�B�@�C�@�B�@�B@�C-@�D�@�D=@�C�@�D=@�D=@�E�@�E�@�D=@�CB@�D=@�C�@�B�@�At@�>�@�A @�=@�;�@�=@�;�@�:�@�:�@�:�@�:�@�:?@�9C@�82@�6&@�6�@�6�@�5~@�4�@�4�@�4n@�4�@�4@�4@�1�@�0U@�1�@�+�@�+A@�+�@�+�@�,�@�,(@�,(@�+�@�+�@�+,@�,(@�+�@�+�@�+,@�+,@�)t@�) @�(�@�(c@�) @�(x@�) @�(x@�'�@�'g@�&@�%�@�&@�&@�&�@�&@�&l@�&l@�&W@�&@�&�@�'@�'@�'@�'g@�'g@�'@�&�@�&�@�&l@�%@�%[@�%[@�%@�%@�%�@P��@P��@P�z@P��@P�"@P�v@P�v@P�v@P�@P��@P�@P�@P��@P��@P�r@P��@P�@P��@P�m@P�D@P�m@P�m@P�m@P�@P��@P�@P�@P��@P�i@P��@P�@P��@P��@P�?@P��@P�i@P��@P��@P�?@P�?@P�i@P��@P�;@P�;@P�;@P�e@P��@P��@P��@P��@P�@P��@P�`@P��@P��@P�@P��@P�O@P��@P��@P��@P�p@P��@P��@P��@P�l@P��@P��@P�@P�h@P�@P�c@P��@P�c@P�9@P��@P�>@P��@P��@P�}@P�)@P��@P�2@P��@P�6@P��@P��@P��@P��@P�?@P�?@P��@P��@P��@P�H@P��@P�@P�$@P��@P�@P��@P��@P�O@P�%@P�}@P�S@P�*@P��@P�S@P�*@P� @P��@P�.@P��@P�.@P��@P�@P��@P��@P�@���@���@���@�� @���@��@���@���@��J@���@���@��B@���@��}@���@��@���@��l@��p@���@���@��F@���@��K@��p@��6@���@��h@���@���@���@���@��7@���@���@���@���@���@��@���@���@���@���@���@���@���@��y@��@��d@���@��X@��?@��@���@��@��\@���@��X@���@��@��X@���@��X@���@���@���@��\@���@���@���@���@���@��@��"@���@��Q@���@���@��/@��@���@���@��U@���@��{@��b@��8@��Q@���@���@���@���@���@���@���@��b@��Q@���@��@��'@���@��I@��n@��@���@��D@��v@���@���@��n@��b@�}�@�t�@�pP@�q�@�o�@�p�@�pz@�r�@�r�@�r@�t@�rG@�u�@�vu@�t@�p;@�rG@�t�@�s@�q�@�k@�oi@�jU@�h�@�kQ@�j�@�j�@�kQ@�jU@�j@@�jU@�i�@�h�@�e�@�e�@�d�@�e@�d�@�d�@�d@�do@�e@�d�@�c�@�`-@�b�@�Y�@�[@�Zq@�[@�\h@�[�@�\�@�\>@�Z�@�Y�@�[B@�[�@�[-@�Z2@�Z2@�Y@�X�@�W�@�X:@�X�@�Yu@�X:@�W�@�W�@�V�@�UG@�U@�U\@�U�@�U�@�U\@�U�@�U�@�U�@�U�@�U�@�VC@�U�@�VC@�Vm@�VX@�VC@�VX@�U�@�T�@�UG@�Tv@�T7@�S�@�S�@P�@Pӄ@Pӄ@PӮ@P�@P�,@P�V@P�V@PԀ@P��@P��@P�R@P�R@P�R@Pզ@Pզ@P��@P��@P�$@P�$@P�M@P�w@P֡@P��@P��@P��@P�@P�I@P�I@P�s@P�I@P�@P�s@P�s@P�s@P�I@P�I@P�I@P�@P�I@P�I@P�@P�I@P��@Pם@P��@P��@P��@P�@P�@P�@P�E@P�o@Pؙ@P��@P��@P�j@P�<@P�
@P�Y@P�U@P�z@P�z@Pߤ@P�"@P�"@P�L@P�r@P�r@P�@P�r@P��@P�?@P�@P�@P�@P�;@P�@P��@P�+@P݃@P�Y@P��@P�
@P�@P��@P�j@P�j@P��@P�@@P�@@P�j@P�@@P�j@P�@@P�@P��@P�s@P��@P��@P��@P�P@P�@P�Y@P�Y@P�4@P�4@P�]@P�
@P�4@P�
@P��@P�b@P�b@P�8@P�8@P�@P��@P�@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         44344344344433444334443444444444344333433444443444344343434434444344444444444444444434433434433444434334444434444344444433443344344433444444433443333444433443444344433344333444334443334444334334334333434333434434333344333333433333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Y�G�O�G�O�@�clG�O�G�O�@�g�G�O�G�O�G�O�@w�3@�f�G�O�G�O�G�O�@�hH@�K�G�O�G�O�G�O�@�f~G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�N G�O�G�O�@�f}@�f�@�LG�O�@�i�@�dmG�O�G�O�G�O�G�O�G�O�@�emG�O�G�O�G�O�@�c�G�O�G�O�@�aRG�O�@�dG�O�@�a�G�O�G�O�@�z8G�O�G�O�G�O�G�O�@R�cG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�dG�O�G�O�@��@�c�G�O�@�]�G�O�G�O�@�c�@�bG�O�G�O�G�O�G�O�@u��G�O�@�a @�cqG�O�G�O�G�O�G�O�G�O�@�aG�O�G�O�G�O�G�O�@�`G�O�G�O�G�O�G�O�G�O�G�O�@�h�@�_�G�O�G�O�@�jD@�$6G�O�G�O�@�i�G�O�G�O�G�O�@�i�@�f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��"@�fUG�O�G�O�@�82@�f(@�f{@�+AG�O�G�O�G�O�G�O�@�[�@�f�G�O�G�O�@�e*G�O�G�O�G�O�@�f�G�O�G�O�G�O�@�d�@�d�@��%G�O�G�O�@�f�@�d@�f&G�O�G�O�G�O�@�f�@�fzG�O�G�O�G�O�@�bd@�a�@�`G�O�G�O�G�O�G�O�@�]�@�rG�O�@�aR@�_FG�O�@\��@��G�O�@�c
@�c�@�dG�O�@�7�G�O�@�eW@��@���G�O�@�_�G�O�G�O�@�g:G�O�@�h�@�g7@�e�@�f&G�O�G�O�@�h�@�h4@�g4@�j�@�h6@�fuG�O�@�dp@�g!@�j?@�g7@�e�@b�H@�f�@�g�@�f&@�d@�f@�iF@�fz@�g"@�h�@�h�@�f�@�g�@�g7@�fw@�f+@k�@@�h6@�g5@�g�@�fz@�e�@�h�@�h�@�iG@�i/@�iG@�h�@�f+@�iD@�iF@���@�j�@�kO@�l@�h�@�h�@�j?@�j�@�g�@�kR@�g�@�f~@�f}@�i�@�h�@�kS@�j�@�j@@�j�@�jA@�k�@�i�@�g�@�g;@�iD@�j�@�g�@�jz@�o�@�g�@�j�@�j�@�iB@�iD@n� @�m[@�k�@�m
@�m�@�m�@�l�G�O�@�i�@�k�@�m@�k�@�mI@�o@�m�@�kP@�n�@�n@�mZ@�k�@�l@�h�@�m
@�kO@�mZ@�n@�l�@�l@�m@�j�@�k�@�m@�m@�m@�l@�j�@�lc@�n�@�m@�l�@�l�@�m
@�l�@�n@�mb@�m^@�m�@�m�@�mc@�n@�n@�m�@�m[@�m@�m@�l@�m\@�lZ@�kO@�n@�n@�m�@�k�@�p'@�nn@�m@�m@�h�@�n@�m@�kO@�j�@�m�@�kO@�i�@�h�@�l�@�kS@�iE@�j�@�i�@�h�@�k�@�m@�nF@�k�@�j:@�j>@�j@@�j�@�i�@�h�@�j�@�j�@�l�@�l�@�m@�m
@�n�@�m_@�iH@�j�@�la@�i�@�i�@��@�l
@�h�@�j�@�j�@�j@@�iF@�kR@�n@�j�@�nn@�nm@�l@�k�@�h�@�j�@�m_@�i�@�h�@�i�@�m@�ld@�i�@�h�@�iD@�j�@�k @�k�@�kU@�jC@�l@�mZ@�k @�l�@�k�@�kY@�l�@�jA@�j?@�jB@�k=@�i�@�i�@�m@�i�@�j?@�l@�nk@�l�G�O�@�h�@�jC@�i�@�k�@�i�@�iH@�h�@�h�@�j�@�g�G�O�@�i�@�g�@�h3@�h�@�hM@�g;@�i�@�hK@�i�@�j�@�kO@�i�@�i�@�kO@�h�@�h�@�j.@�hK@�iH@�g�@�i�@�jV@�jV@�h�@�g�@�i�@�h�@�h�@�g6@�d@�d�@�d�@�d�@�d@�cw@�c�@�c@�c�@�d2@�d@�d@�d�@�d�@�d@�d@�ct@�bd@�c�@�cr@�f<@�f:@�h9@�g�@���@���@���@��%@���@��@���@���@��I@���@���@��D@���@��|@���@��@���@��n@��o@���@���@��F@���@��N@��p@��6@���@��f@���@���@���@���@��7@���@���@���@���@���@��@���@���@���@���@���@���@���@��x@��@��h@���@��X@��=@��@���@��@��Z@���@��X@���@��@��Z@���@��X@���@���@���@��]@���@���@���@���@���@���@��%@���@��N@���@���@��/@��@���@���@��P@���@��z@��d@��8@��O@���@���@���@���@���@���@���@��b@��O@���@��@��&@���@��L@��s@��@���@��B@��w@���@���@��p@��b@�}�@�t�@�pO@�q�@�o�@�p�@�px@�r�@�r�@�r@�t@�rJ@�u�@�vx@�t@�p<@�rF@�t�@�s@�q�@�k@�oi@�jY@�h�@�kP@�j�@�j�@�kR@�jV@�j@@�jV@�i�@�h�@�e�@�e�@�d�@�e@�d�@�d�@�d@�dp@�e@�d�@�c�@�`1@�b�@�Y�@�[@�Zq@�[@�\h@�[�@�\�@�\<@�Z�@�Y�@�[F@�[�@�[-@�Z5@�Z0@�Y@�X�@�W�@�X7@�X�@�Yv@�X:@�W�@�W�@�V�@�UJ@�U @�UZ@�U�@�U�@�UY@�U�@�U�@�U�@�U�@�U�@�VB@�U�@�VE@�Vq@�VZ@�VF@�VX@�U�@�T�@�UJ@�Ts@�T8@�S�@�S�@P�@PӅ@Pӆ@Pӫ@P� @P�0@P�X@P�V@Pԃ@P��@P��@P�P@P�U@P�N@Pը@Pը@P��@P��@P�#@P�"@P�M@P�u@P֥@P��@P��@P��@P� @P�M@P�J@P�r@P�M@P� @P�s@P�v@P�s@P�K@P�H@P�H@P�"@P�K@P�E@P� @P�H@P��@Pך@P��@P��@P��@P�@P�@P�@P�E@P�m@Pؚ@P��@P��@P�k@P�8@P�
@P�Z@P�U@P�z@P�z@Pߦ@P�%@P� @P�K@P�s@P�s@P�@P�r@P��@P�@@P�@P�@P�@P�;@P�@P��@P�&@P݆@P�Z@P��@P�
@P�@P��@P�k@P�h@P��@P�E@P�B@P�k@P�B@P�k@P�B@P�@P��@P�v@P��@P��@P��@P�P@P�@P�Z@P�V@P�2@P�5@P�`@P�@P�5@P�@P��@P�e@P�e@P�6@P�6@P�@P��@P�@P�@���@���@���@��%@���@��@���@���@��I@���@���@��D@���@��|@���@��@���@��n@��o@���@���@��F@���@��N@��p@��6@���@��f@���@���@���@���@��7@���@���@���@���@���@��@���@���@���@���@���@���@���@��x@��@��h@���@��X@��=@��@���@��@��Z@���@��X@���@��@��Z@���@��X@���@���@���@��]@���@���@���@���@���@���@��%@���@��N@���@���@��/@��@���@���@��P@���@��z@��d@��8@��O@���@���@���@���@���@���@���@��b@��O@���@��@��&@���@��L@��s@��@���@��B@��w@���@���@��p@��b@�}�@�t�@�pO@�q�@�o�@�p�@�px@�r�@�r�@�r@�t@�rJ@�u�@�vx@�t@�p<@�rF@�t�@�s@�q�@�k@�oi@�jY@�h�@�kP@�j�@�j�@�kR@�jV@�j@@�jV@�i�@�h�@�e�@�e�@�d�@�e@�d�@�d�@�d@�dp@�e@�d�@�c�@�`1@�b�@�Y�@�[@�Zq@�[@�\h@�[�@�\�@�\<@�Z�@�Y�@�[F@�[�@�[-@�Z5@�Z0@�Y@�X�@�W�@�X7@�X�@�Yv@�X:@�W�@�W�@�V�@�UJ@�U @�UZ@�U�@�U�@�UY@�U�@�U�@�U�@�U�@�U�@�VB@�U�@�VE@�Vq@�VZ@�VF@�VX@�U�@�T�@�UJ@�Ts@�T8@�S�@�S�@P�@PӅ@Pӆ@Pӫ@P� @P�0@P�X@P�V@Pԃ@P��@P��@P�P@P�U@P�N@Pը@Pը@P��@P��@P�#@P�"@P�M@P�u@P֥@P��@P��@P��@P� @P�M@P�J@P�r@P�M@P� @P�s@P�v@P�s@P�K@P�H@P�H@P�"@P�K@P�E@P� @P�H@P��@Pך@P��@P��@P��@P�@P�@P�@P�E@P�m@Pؚ@P��@P��@P�k@P�8@P�
@P�Z@P�U@P�z@P�z@Pߦ@P�%@P� @P�K@P�s@P�s@P�@P�r@P��@P�@@P�@P�@P�@P�;@P�@P��@P�&@P݆@P�Z@P��@P�
@P�@P��@P�k@P�h@P��@P�E@P�B@P�k@P�B@P�k@P�B@P�@P��@P�v@P��@P��@P��@P�P@P�@P�Z@P�V@P�2@P�5@P�`@P�@P�5@P�@P��@P�e@P�e@P�6@P�6@P�@P��@P�@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         44344344344433444334443444444444344333433444443444344343434434444344444444444444444434433434433444434334444434444344444433443344344433444444433443333444433443444344433344333444334443334444334334334333434333434434333344333333433333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�b�9�b(9�c>9�b�9�b)9�dB9�d9�c�9�b�9�c�9�c09�`9�`]9�a9�ap9�_�9�`�9�`&9�_F9�_~9�]�9�_"9�^�9�^H9�_G9�^39�]�9�\�9�[,9�]�9�\�9�[�9�Y�9�Y�9�Y<9�Zt9�Zb9�Z)9�Z�9�Z9�Y�9�Y>9�_l9�]�9�_�9�]�9�]�9�]19�]~9�[>9�[�9�\w9�[e9�ZN9�Z�9�Z�9�Yr9�[�9�[B9�Z�9�[�9�\9�[�9�YM9�[9�Yn9�Z�9�[�9�[B9�[,9�Z>9�X�9�S�9�UX9�RP9�T�9�T�9�U29�W$9�V�9�U�9�U9�S�9�P�9�P\9�Q*9�Q9�P79�O�9�O�9�O�9�P�9�Q�9�Qo9�P�9�Q(9�P79�P�9�P9�P9�Q�9�Q�9�R�9�R�9�Q�9�R�9�U�9�T9�S.9�R�9�Q(9�Kz9�Ch9�?�9�@�9�?9�@9�?�9�A�9�A�9�A9�B�9�A?9�Dx9�D�9�B�9�?p9�A;9�C�9�A�9�@�9�:�9�>�9�:C9�8�9�;9�:�9�:�9�;9�:A9�:-9�:A9�9�9�8�9�6&9�6o9�5~9�5�9�5l9�5$9�4�9�59�5�9�5H9�4j9�1U9�3�9�+y9�,�9�,F9�,�9�.9�-o9�.s9�-�9�,�9�+�9�-9�-69�,�9�,9�,9�+9�*�9�*9�*Q9�*�9�+i9�*T9�)�9�)�9�(�9�'�9�'�9�'�9�'�9�'�9�'�9�(9�(*9�(@9�(;9�(9�(�9�(;9�(�9�(�9�(�9�(�9�(�9�((9�'9�'�9�'9�&�9�&�9�&�97��97�97�97�897��97��97��97��97��97�<97�<97��97��97��97��97��97�97�@97�d97�c97��97��97��97��97��97�97�B97�j97�g97��97�j97�B97��97��97��97�h97�f97�f97�D97�h97�c97�B97�f97��97��97��97��97��97�97�!97�97�D97�g97��97��97��97�G97��97��97��97��97��97��97��97�197�-97�R97�W97�W97�97�V97��97��97�797�497�W97��97�397��97�p97��97��97�M97��97��97��97�G97�D97��97�%97�#97�G97�#97�G97�#97��97��97��97��97�D97�h97��97��97�<97�897�797�:97�`97�97�:97�97��97��97��97�Z97�Z97�697��97�697�6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�hB�hB�hB�hB�hB�bB�bB�bB�bB�bB�bB�hB�oB�oB��B��B��B��B��B��B��B��B��B�'B�dB�jB�dB�dB�dB�XB�LB��B�)B�/B��B��B��B��B!�B�B�B/BD�BE�B?}B(�B!�B�B!�B)�B?}BO�BT�BS�BN�BG�B=qB33B&�B�BÖB�}B�-B��B��B�7B�1B�VB�%Bt�BdZBR�BC�B:^B9XB8RB5?B/B!�B�BDB�fB��B��BǮB�XB��Bz�Bl�BgmB`BBI�B1'B{BB
�B
�yB
�#B
�qB
��B
��B
��B
y�B
m�B
aHB
M�B
!�B	��B	�RB	��B	�B	p�B	dZB	ZB	N�B	=qB	�B	{B	1B�yB�B��BĜB�dB�B��B��B�{B�PB�1B�%B�B�DB��B�{B�bB�DB�1B�+B�B�B�B�B�B�B�PB�{B�PB� Bu�BgmBbNB_;B_;BbNBk�Bn�Bp�Bp�Bo�Bm�Bk�BiyBgmBhsBiyBp�Bp�B�B~�B~�B�B�B�B� B� B~�Bz�Bu�Bx�Bz�Bu�Bp�Bm�Bk�BjBgmB_;B[#BZBYBXBW
BS�BYBw�B~�B|�Bx�B~�Bu�BiyB_;BS�BI�BJ�BVBffBk�Bm�Bq�Bv�Bt�Bp�Bk�BgmBgmBdZBbNBaHB`BB`BB`BB^5B\)B[#BYBW
BW
BVBS�BP�BN�BN�BQ�BW
BXBXBW
BW
BVBT�BVBVBS�BT�BVBVBYB\)B\)B\)B\)B\)B[#B[#B\)BZBYBYBYBYBXBYB_;BdZBcTB`BBcTBm�Bm�Bn�Bo�Bo�Bp�Bs�B}�B|�Bu�Br�Bu�Bv�Bx�B|�B�%B��B��B��B�B�-B�-B�RB�^B�^B�dB�qB�qB�qB�}B��B��BǮB��B�B�5B�BB�ZB�sB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B	B	B	B	  B	  B	B	B	B	B	B��B��B��B��B	B		7B	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	(�B	33B	;dB	;dB	;dB	B�B	>wB	:^B	8RB	:^B	B�B	F�B	H�B	I�B	I�B	J�B	M�B	S�B	T�B	W
B	R�B	Q�B	Q�B	P�B	Q�B	S�B	O�B	P�B	VB	XB	XB	ZB	\)B	^5B	bNB	cTB	ffB	gmB	k�B	n�B	p�B	r�B	t�B	u�B	v�B	w�B	x�B	w�B	v�B	w�B	y�B	y�B	z�B	{�B	� B	� B	�B	�B	�1B	�=B	�DB	�JB	�PB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�XB	�XB	�XB	�XB	�^B	�^B	�dB	�qB	�}B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	��B	��B
KB
�B
"hB
-�B
6�B
<�B
DMB
I�B
MPB
RTB
YKB
\�B
a�B
e�B
i�B
mB
q�B
v+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�*�?>��B
��?.{�@g��B^AL�?|�B\YA� ^>�FN? (_AȪ�Bj*>ۻb?�,?�R�Ba�BoR>�? �"?�\�B`$@��S?)t?0T�AkY�?s?�92?��?s�?�V�BGnA2��?��B`B[�B�8@`WB[�B`�?\�?KK@A	�3?�?Nu�Bjs?��?�nA��B^]@���?q�MBz�?U��B]�?< qB`s?��?�jbB�?̼s>�cR?�?��8A�@F@Z�j?2гA�]@��p>趷?(�	A2F@�o?�@!y@�ŢA,��@�?3�@b�VA��dA0B?�RYBa�?,C�?9f�Aӻ�B�x?am]B[�?B�@XD�B[B_(?�]WAe��?�?=hA��K?���B]aB��@8�?H�3@9�A?,A 4�B�Z>�K�>�MI?)�@�BZ>ɭ�?6��?��~?>%?�V?U�UBaXB~�?#P<@���B^A��?$!\@�HBb=>�]?��?U�B[UBd�>�F�>�u6@@�?���As�@]�?9Z�A�WB��?�m?OQYB4�B^B]�A��?6��A_�[@q�9?�p;B]XBt�?��?�s�B_�>�7]?2�$@�p�B~Q>č�@��o@Uq?B[DB\�B�V?O�A���B`�B`@B^0?�@��E?���B^�BZyA��(?K�?��B[yBY�B��>ʵ=>���?$B1AoP'BX~BQ+?�&�BY�BV?��9A��LB$�?ئ;BY�B_+B��@��JA�+�AVS�Bq�A��A�#D@�c�BUA3A��B��?�E�B[�B\�B]�B_�A��[@���B`B`�B^cB`�B^zB`�@�?BYPBVBBZ�B[�BWpA��fB]2BZ�BZ*BY	BXqB\BB]�BV:BW�B[.BZ�BZ�B]�B\�B[�A��'B]�B\�B\�BZpBV�BZ�BbBf�B]�B`RBa�B_BaBb�B��B]�B\�B^BZBZ`BZ B_/BZBY\B[qBVrB\Bb{B^�B_�B�}B_�B^�B`hB`�B[�B]BZTB\KB[9BX�B\�B��BY^B^�B\�B_{BX<A�~!B\�BY�B[B^�B\�B^�A��B]`B[4B^<BX�B]�B[�B\{B^(B]qBm�B`�B`�Bb$BY:B\�B\�B`(B\B]BidBa}B[oBX�B\�B_B`�BV�BY�B[�Bb�B^4Ba&B]B\�B\QB`�B`�BY�B^�B]RB\!B`�B\�B^�B`�BZ6B_B\xB_QBa�B_BbnB`�BaaB\�B]BV�BY_B_BY�B^oB^�BZ�B^XB[�B_B]hB\�B]�B_�B^�B]�B\�BY�BY�BalB]�B\B]0Ba7BbBa�B`�B^B^�B]�B]�B_�BcBc	B`�B_ZB^�B[(Ba�Ba�BVgB��BUBBX�BV�B[wB[�B\BBY\B^gB]�BYB]�BZB\�BZWB^iB^�B[<BZ/B]�BauB^dB^?B_=B]B_�Bb�B]�B^�B\bB[�B`�B_~BaB`�BcBa�BX[BZ BY)B]FB^BW�B^<BYPBY�B^B^�B^�A\��B\�BW�B[MB_WB\�B[tB[�B_�Ba�Bv@~N�B`"BY�BZ|B^^BccB_)BcBa�B`B^�B]QB\B[�B_�B\HBYB[UBZTB_?BY�B_�B\1B^�B^B_+BatB]2B`jB`_B]oB^�B`�B`�B^�B_�BaBaB`.B_�BbBa;Ba�Bc1B^�Ba�B_�B`PB`�BbBi�BamBdBa�Ba5Bc�BcB`�B\yBZUB_�BX�B]BapB`�BaBa�Bc>Ba�BaEBbbB_�B`�BaXB_B^�Bd1Bc*Bd�Bb�B_VB`�B`CB_lB_UB`�B^�Ba�B_�BcHB`rB[�Bd�B^vB`�B_PBbPB^�B_OB_wB\�Bb�B`�B]fB`�Ba�Ba�B[�B`�B_mBcxBe�B_�B]Ba^B_�B_�BaBBc(Ba�B`�B`IB`�B^�B\qB_�B`�BdB_-B`BB^�B`XB_RB\�B]�B^�B`8B`cB`�BbeB`�Ba�Bb�B_�B_�BZ�BaRBaABaB^�Bb B`Bc-Ba�B_�Ba�B^�B_Ba5B]�B_�B_AB[�B\_BX=BX�Bb�B`B_�Ba�BbBb�BagBb�BbBaEBd*B`iB`UB__B`;B`OBbB[�B^�Bb�B[�Ba�Ba�B^xB`�Ba1B`�BaBarB`|B^�BayBbB_�BbVBa�Ba�Ba2B`�B`�B_�B_4Bb�B_�B_�Bb�Bb	Bc�Bc�Ba�BcCB`�BcYBcBbJBbBb�Bb Bb�B^�BbZBb�Bb\BcBc%Bb�BbEBbdBb�Bc�BdGBc�Bc�Bc�Bc�Bc�Bc�Bc�Bd0BdBdIBdABd0BdwBdnBc?Bc�Bb�BdBc�Bc�Bd�Bd4Bd#BfjB	��B	�pB	�%B	�UB	�yB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�-B	�B	�B	��B	��B	�LB	� B	�$B	�B	��B	�-B	�?B	��B	�eB	�B	��B	��B	��B	��B	��B	�fB	�YB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�EB	�B	�B	�NB	�B	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�9B	�B	�OB	��B	��B	�B	�B	��B	��B	�TB	��B	��B	��B	�mB	�"B	��B	��B	�_B	��B	��B	��B	��B	��B	�*B	�B	��B	��B	��B	�\B	�KB	��B	�B	��B	��B	��B	��B	��B	�pB	��B	��B	��B	��B	��B	�^B	�5B	�
B	��B	��B	�jB	�B	�4B	��B	��B	��B��B��B��B�dB��B�_B�B��B�/B��B��B�|B��B��B�QB�2B�B�yB�QB�SB�>B�B�|B��B�'B��B�B�sB�tB�!B��B�nB��B�;B��B�zB�^B��B�`B��B��B��B��B��B��B�)B�B��B��B��B�SB�VB��B�_B��B�
B�/B�B��B��B�/B�lB��B��B��B��B��B�	B�	B�B��B��B�>B��B��B��B�1B��B�CB��B�WB��B�MB�B��B��B�lB��B��B��B��B��B�B��B�oB�6B��B�9B��B��B��B�.B�mB��B�mB�]B��B��B�lB�(B��B�PB�B��B��B�UB�B�B��B�_B��B�B��B�&B�[B��B�8B��B��B��B��B��B��B��B�B�B��B�7B��B��B�dB�pB��B�B��B�B��B��B��B�	B��B��B��B�B��B��B��B��B�XB��B��B��B�9B�xB��B��B��B�UB�WB�(B��B��B�=B��B��B�$B��B�eB�B�\B�B��B�B��B�:B�(B�B��B�BB�aB�3B�*B��B��B��B��B�yB��B�8B�:B�vB�
B��B��B�hB��B��B	ܹB	��B	��B	��B	�"B	�4B	�8B	�+B	�<B	�`B	�RB	ݢB	݇B	�zB	ݪB	ݐB	ݢB	ݦB	ݸB	ݪB	ݯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ݯB	݃B	ݳB	ݙB	݌B	�cB	�VB	�HB	�B	�!B	�B	��B	��B	�;B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�.B	�CB	ߒB	�B	�6B	�B	߭B	�wB	�YB	�nB	߭B	ߒB	ߤB	�nB	�DB	�	B	�:B	�4B	�TB	��B	�jB	�|B	��B	�FB	��B	�hB	��B	�B	�NB	�B	��B	�B	�B	�B	�3B	�dB	�IB	�[B	�/B	�3B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�KB	�BB	�mB	�`B	�eB	�B	�+B	��B	��B	�mB	�SB	�7B	�*B	��B	�B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944344344344433444334443444444444344333433444443444344343434434444344444444444444444434433434433444434334444434444344444433443344344433444444433443333444433443444344433344333444334443334444334334334333434333434434333344333333433333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B�aB�cB�]B�aB�aB�]B�]B�]B�]B�]B�^B�bB�iB�gB�yB��B��B��B��B��B��B��B��B�B�]B�gB�`B�^B�`B�QB�HBʼB�B�,B��B��B��B��B!�B{B�B/BD�BE�B?xB(�B!�B�B!�B)�B?vBO�BT�BS�BN�BG�B=mB3/B&�B�BÎB�vB�'B��B�|B�/B�+B�NB�!Bt�BdTBR�BC�B:VB9QB8MB58B/B!�B�B:B�_B��B��BǨB�PB��Bz�Bl�BgiB`<BI�B1!BrBB
�B
�sB
�B
�jB
��B
��B
��B
y�B
m�B
aAB
M�B
!�B	��B	�KB	��B	�B	p�B	dQB	ZB	N�B	=iB	�B	rB	+B�rB�B��BĔB�\B�B��B��B�sB�HB�,B�B�B�<B�xB�tB�ZB�=B�(B�#B�B�
B�B�B�B�
B�HB�sB�JB�Bu�BgdBbIB_2B_3BbFBk~Bn�Bp�Bp�Bo�Bm�Bk|BirBgfBhkBioBp�Bp�B�B~�B~�B�B�B��B�B�B~�Bz�Bu�Bx�Bz�Bu�Bp�Bm�Bk~BjuBgfB_3B[BZBYBXBWBS�BYBw�B~�B|�Bx�B~�Bu�BioB_3BS�BI�BJ�BU�Bf_Bk}Bm�Bq�Bv�Bt�Bp�Bk|BgeBgdBdTBbFBa@B`9B`:B`:B^-B\"B[BYBW BWBU�BS�BP�BN�BN�BQ�BWBXBX
BV�BWBU�BT�BU�BU�BS�BT�BU�BU�BYB\B\"B\B\ B\B[B[B\"BZBYBYBYBYBXBYB_2BdQBcLB`9BcIBm�Bm�Bn�Bo�Bo�Bp�Bs�B}�B|�Bu�Br�Bu�Bv�Bx�B|�B�B��B��B��B�B�%B�%B�IB�SB�VB�[B�jB�fB�gB�uB�yB�zBǤB��B�B�,B�8B�SB�iB�B�B�B�B�B��B��B��B��B��B	 �B	 �B	 �B	 �B	 �B	 �B	 �B	 �B��B��B	B	B	B	
B	 �B��B��B��B��B	 �B		/B	RB	rB	�B	�B	�B	~B	B	�B	�B	�B	�B	(�B	3*B	;\B	;XB	;ZB	B�B	>nB	:TB	8JB	:UB	B�B	F�B	H�B	I�B	I�B	J�B	M�B	S�B	T�B	WB	R�B	Q�B	Q�B	P�B	Q�B	S�B	O�B	P�B	U�B	X
B	XB	ZB	\ B	^,B	bFB	cKB	f^B	gdB	k|B	n�B	p�B	r�B	t�B	u�B	v�B	w�B	x�B	w�B	v�B	w�B	y�B	y�B	z�B	{�B	�B	�B	�B	�B	�*B	�3B	�:B	�@B	�HB	�hB	�lB	�rB	�qB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�*B	�7B	�>B	�OB	�LB	�OB	�OB	�SB	�TB	�[B	�gB	�sB	�yB	��B	�~B	B	B	ËB	ĔB	ŘB	ƛB	ǥB	ȨB	ɱB	ɰB	ɱB	ɰB	ʷB	˽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�!B	�&B	�&B	�,B	�,B	�1B	�8B	�>B	�EG�O�B	��B	��B
BB
xB
"^B
-�B
6�B
<�B
DDB
I�B
MGB
RLB
Y@B
\�B
a�B
e�B
i�B
mB
q�B
v!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��G�O�G�O�B^G�O�G�O�B\TG�O�G�O�G�O�AȪ�Bj$G�O�G�O�G�O�Ba�BoMG�O�G�O�G�O�B`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BGhG�O�G�O�B`B[�B�2G�O�B[�B`�G�O�G�O�G�O�G�O�G�O�BjnG�O�G�O�G�O�B^UG�O�G�O�Bz�G�O�B]�G�O�B`nG�O�G�O�B�G�O�G�O�G�O�G�O�A�@:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ba�G�O�G�O�Aӻ�B�qG�O�B[�G�O�G�O�B[B_!G�O�G�O�G�O�G�O�A��AG�O�B]\B��G�O�G�O�G�O�G�O�G�O�B�SG�O�G�O�G�O�G�O�BY�G�O�G�O�G�O�G�O�G�O�G�O�BaQB~�G�O�G�O�B^A��G�O�G�O�Bb6G�O�G�O�G�O�B[QBd�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�OB��G�O�G�O�B4~B^B]�A��G�O�G�O�G�O�G�O�B]QBt�G�O�G�O�B_�G�O�G�O�G�O�B~MG�O�G�O�G�O�B[@B\�B�PG�O�G�O�B`�B`:B^(G�O�G�O�G�O�B^�BZrG�O�G�O�G�O�B[tBY�B��G�O�G�O�G�O�G�O�BXxBQ&G�O�BY�BVG�O�A��AB$�G�O�BY�B_#B��G�O�A�+}G�O�Bq�A��A�#5G�O�BUG�O�G�O�B��G�O�B[�B\�B]�B_�G�O�G�O�B_�B`�B^\B`�B^tB`�G�O�BYJBV:BZ�B[�BWhA��_B]+BZ�BZ#BYBXiB\?B]�BV3BW�B[(BZ�BZ�B]~B\�B[�A��B]�B\�B\�BZgBV�BZ�BbBf�B]�B`MBa�B_BaBb�B��B]�B\�B^BZBZ\BY�B_*BZ
BYUB[lBVmB\BbsB^�B_�B�rB_�B^�B`dB`�B[�B]BZNB\CB[4BX�B\�B��BYSB^�B\�B_sBX6A�~B\�BY�BZ�B^�B\~B^�G�O�B]ZB[,B^5BX�B]�B[�B\wB^"B]hBm�B`�B`�BbBY4B\�B\�B`B\ B]Bi^BauB[kBX�B\�B^�B`�BV�BY�B[�Bb�B^/Ba B]B\�B\KB`�B`�BY�B^�B]JB\B`�B\�B^�B`�BZ1B^�B\rB_IBa�B_BbgB`�Ba\B\�B]BV�BYUB^�BY{B^fB^�BZ�B^RB[�B_B]cB\�B]�B_�B^�B]�B\�BY�BY�BacB]�B\B]'Ba.Ba�Ba�B`�B^B^�B]�B]�B_}Bc
BcB`�B_UB^�B[#Ba�Ba�BVbB��BU:BX�BV�B[rB[�B\?BYUB^`B]�BYB]�BZB\�BZNB^`B^|B[6BZ(B]�BaqB^^B^7B_7B]B_�Bb�B]�B^�B\_B[�B`�B_zBaB`�BcBa�BXUBY�BY$B]?B^BW�B^5BYLBY�B^B^�B^�G�O�B\�BW�B[GB_PB\�B[oB[�B_�Ba�BvG�O�B`BY�BZvB^WBc`B_#Bc Ba�B`B^�B]JB[�B[�B_�B\FBYB[QBZNB_=BY�B_�B\*B^�B^B_&BalB]+B`fB`XB]hB^�B`�B`�B^�B_�B`�BaB`,B_�BbBa5Ba�Bc,B^�Ba�B_�B`KB`�BbBi}BafBd
Ba�B��B��B��B�`B��B�[B�B��B�'B��B�}B�vB��B��B�KB�*B�B�sB�MB�MB�7B�
B�sB��B�!B��B�
B�mB�mB�B��B�hB��B�5B��B�tB�YB��B�XB��B��B��B��B��B��B�#B�B��B��B��B�MB�OB��B�XB��B�B�'B�B��B��B�+B�fB��B��B��B��B��B�B�B�B��B��B�=B��B��B��B�&B��B�?B��B�TB��B�BB��B��B��B�hB��B��B��B��B��B�B��B�hB�/B��B�2B��B��B��B�)B�jB��B�gB�UB��B��B�gB�"B��B�JB�B��B��B�TB�yB�B��B�ZB��B�
B��B�B�WB�yB�3B��B�B��B��B��B��B��B�B��B��B�0B��B��B�]B�jB��B�	B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�QB��B�B��B�4B�uB��B��B��B�PB�QB�!B��B��B�7B��B��B�B��B�`B��B�UB�B�B�B��B�3B�B�B��B�>B�ZB�/B�!B��B��B��B��B�vB��B�3B�3B�mB�B��B��B�bB��B��B	ܰB	��B	��B	��B	�B	�,B	�/B	�"B	�4B	�UB	�GB	ݖB	�B	�oB	ݡB	݇B	ݖB	ݝB	ݯB	ݟB	ݦB	ݵB	ݽB	��B	ݾB	��B	��B	��B	��B	��B	ݨB	�zB	ݩB	ݒB	݂B	�\B	�LB	�?B	�B	�B	�B	��B	��B	�4B	��B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�#B	�:B	߉B	��B	�.B	�B	ߥB	�mB	�OB	�fB	ߥB	߈B	ߛB	�fB	�;B	��B	�/B	�+B	�LB	�zB	�`B	�rB	��B	�;B	��B	�[B	��B	�B	�EB	��B	��B	�B	�B	�B	�(B	�^B	�@B	�RB	�&B	�*B	��B	��B	�{B	��B	�B	��B	��B	�B	��B	�CB	�7B	�cB	�XB	�\B	�B	�#B	��B	�B	�fB	�LB	�-B	� B	��B	�B	��B	�B��B��B��B�`B��B�[B�B��B�'B��B�}B�vB��B��B�KB�*B�B�sB�MB�MB�7B�
B�sB��B�!B��B�
B�mB�mB�B��B�hB��B�5B��B�tB�YB��B�XB��B��B��B��B��B��B�#B�B��B��B��B�MB�OB��B�XB��B�B�'B�B��B��B�+B�fB��B��B��B��B��B�B�B�B��B��B�=B��B��B��B�&B��B�?B��B�TB��B�BB��B��B��B�hB��B��B��B��B��B�B��B�hB�/B��B�2B��B��B��B�)B�jB��B�gB�UB��B��B�gB�"B��B�JB�B��B��B�TB�yB�B��B�ZB��B�
B��B�B�WB�yB�3B��B�B��B��B��B��B��B�B��B��B�0B��B��B�]B�jB��B�	B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�QB��B�B��B�4B�uB��B��B��B�PB�QB�!B��B��B�7B��B��B�B��B�`B��B�UB�B�B�B��B�3B�B�B��B�>B�ZB�/B�!B��B��B��B��B�vB��B�3B�3B�mB�B��B��B�bB��B��B	ܰB	��B	��B	��B	�B	�,B	�/B	�"B	�4B	�UB	�GB	ݖB	�B	�oB	ݡB	݇B	ݖB	ݝB	ݯB	ݟB	ݦB	ݵB	ݽB	��B	ݾB	��B	��B	��B	��B	��B	ݨB	�zB	ݩB	ݒB	݂B	�\B	�LB	�?B	�B	�B	�B	��B	��B	�4B	��B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�#B	�:B	߉B	��B	�.B	�B	ߥB	�mB	�OB	�fB	ߥB	߈B	ߛB	�fB	�;B	��B	�/B	�+B	�LB	�zB	�`B	�rB	��B	�;B	��B	�[B	��B	�B	�EB	��B	��B	�B	�B	�B	�(B	�^B	�@B	�RB	�&B	�*B	��B	��B	�{B	��B	�B	��B	��B	�B	��B	�CB	�7B	�cB	�XB	�\B	�B	�#B	��B	�B	�fB	�LB	�-B	� B	��B	�B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944344344344433444334443444444444344333433444443444344343434434444344444444444444444434433434433444434334444434444344444433443344344433444444433443333444433443444344433344333444334443334444334334334333434333434434333344333333433333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.07 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.07 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.07 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649132020083116491320200831164913202008311649132020083116491320200831164913202008311649132020083116491320200831164913202008311649132020083116491320200831164913AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817112019021918171120190219181711    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817112019021918171120190219181711  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817112019021918171120190219181711  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649132020083116491320200831164913  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                