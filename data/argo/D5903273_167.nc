CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  I   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:19Z creation      
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181719  20200831164934  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�e�&N$�@�e�&N$�@�e�&N$�111 @�eȎ8��@�eȎ8��@�eȎ8��@5O�;@5O�;@5O�;�co+I��co+I��co+I�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @,��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�33B�33B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D� RD�7
D��HD��)D�� D�O\D��)D���D�fD�X�D��\D�θD�3D�@ D�y�D�HD���D�${D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O������L�;L�;��������������������L�;��������������������L�;�������    ���;��������L�;������������L�ͽ��;������������L�;L�;���������������    �L�;����L�;����L�;������������������������L�;L�;�������        ���������������������L�;����������ͽ��;������������������������������������L�;L�;��������������������L�;��������������;L�;����������������������������������������L�;����������������������������L�;������ͽ��;����������������������������L�;�����������=��;L�;����L�ͽ���    �������������������;����L�;������������������������L�;��������L�;����L�ͽ��;��������L�;L�;��������������;L�;��������L�;L�;������������L�ͽ��ͽ��;������������L�;��������������������������������L�;����L�;��������L�ͽ��;L�;�����������        ���������L�;L��=���    �L�;��������L�ͽ��;L�;L�;����������;L�;L�;L�;L�;����L�ͽ��ͽ��;L�;L�;������ͽ���    �L�;L�ͽ��ͽ��ͽ��;L�;L�ͽ���    ���ͽ���    ���ͽ��ͽ��ͽ��ͽ��ͽ��;L�;L��    ���ͽ��ͽ���        ���ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ��;������;L�ͽ���        ����    ���ͽ��ͽ���    �L�ͽ��;L�ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ���        ����        ���ͽ��ͽ��ͽ��ͽ���    �L�ͽ��ͽ��ͽ���                            ���ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ���    �L��        ���ͽ��;L�ͽ���    �L�ͽ��ͽ��ͽ��ͽ���    ���;L��            ����        ����    ���ͽ��ͽ��ͽ��ͽ���            =���        ���ͽ��;L�;L�ͽ���    ���ͽ��ͽ��ͽ���                    ���ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    >L��    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���            �L�;L�ͽ��ͽ���    ���ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���=��;L�;L��        ���;L�ͽ���    =��ͽ��ͽ��;L�;L�;L�ͽ��ͽ��ͽ��ͽ��ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ��ͽ���    ����        ���ͽ��;L�ͽ���    ���ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ����            ���ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ���=��ͽ���        =��ͽ���    ����    =���        �L�ͽ��;L�ͽ��;L�ͽ��ͽ��;L�;L�;L�ͽ��ͽ��ͽ��ͽ���        =���=���>���>���>���>���?��?333?333?L��?fff?fff?�  ?���?���?�ff?�ff?�33?���?�  ?���?���?ٙ�?�ff?�33@   @   @ff@��@��@33@��@   @   @&ff@,��@9��@9��@@  @Fff@L��@S33@Y��@`  @fff@l��@s33@y��@�  @�33@�ff@���@���@���@�  @�33@�ff@���@�  @�  @�33@�ff@���@���@�33@�ff@���@���@���@�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@陚@���@�  @�33@�ff@���@���A   A33A��AffA  A	��A��AffA  A��A33AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A4��A8  A9��A<��A>ffA@  AC33AD��AFffAI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A^ffA`  Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33AvffAx  A{33A|��A~ffA���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  Ař�A�ffA�33A�  A���A�ffA�33A�  A���A͙�A�ffA�  A���Aљ�A�ffA�33A�  Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  Dq  Dq&fDq,�Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrS3Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt�3Dt��Du  @   @&ff@,��@9��@9��@@  @Fff@L��@S33@Y��@`  @fff@l��@s33@y��@�  @�33@�ff@���@���@���@�  @�33@�ff@���@�  @�  @�33@�ff@���@���@�33@�ff@���@���@���@�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@陚@���@�  @�33@�ff@���@���A   A33A��AffA  A	��A��AffA  A��A33AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A4��A8  A9��A<��A>ffA@  AC33AD��AFffAI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A^ffA`  Ac33Ad��Ah  Ai��Ak33Al��Ap  Aq��As33AvffAx  A{33A|��A~ffA���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  Ař�A�ffA�33A�  A���A�ffA�33A�  A���A͙�A�ffA�  A���Aљ�A�ffA�33A�  Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  Dq  Dq&fDq,�Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrS3Dr` DrffDrl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` DsffDss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt�3Dt��Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @?\)@�G�@�G�A
>A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ�\BY(�Ba(�Bi(�Bq(�By(�B�ǮB�ǮB�aHB�aHB��{B��{B�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq)Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy�fD�	�D�@RD���D��qD�HD�X�D��qD��D��D�a�D���D�� D�{D�IHDڂ�D�D� D�-�D�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��#�=�Q�=�Q�#��#��#��#��#�=�Q�#��#��#��#��#�=�Q�#��#�>�z�>B�\�#��#�=�Q�#��#��#�=�Q�>B�\�#��#��#�=�Q�=�Q�#��#��#�>B�\>�z�=�Q�#�=�Q�#�=�Q�#��#��#��#��#��#�=�Q�=�Q�#��#�>�z�>�z�#��#��#��#��#�=�Q�#��#�>B�\>B�\�#��#��#��#��#��#��#��#��#�=�Q�=�Q�#��#��#��#��#�=�Q�#��#��#�>B�\=�Q�#��#��#��#��#��#��#��#��#��#�=�Q�#��#��#��#��#��#��#�=�Q�#�>B�\>B�\�#��#��#��#��#��#��#�=�Q�#��#�>B�\>Ǯ=�Q�#�=�Q�>B�\>�z�#��#��#��#�>B�\�#�=�Q�#��#��#��#��#��#�=�Q�#��#�=�Q�#�=�Q�>B�\�#��#�=�Q�=�Q�#��#��#�>B�\=�Q�#��#�=�Q�=�Q�#��#��#�=�Q�>B�\>B�\�#��#��#�=�Q�#��#��#��#��#��#��#��#�=�Q�#�=�Q�#��#�=�Q�>B�\=�Q�#��#�>B�\>�z�>�z�#��#�=�Q�=�Q�>Ǯ>�z�=�Q�#��#�=�Q�>B�\=�Q�=�Q�#��#�>B�\=�Q�=�Q�=�Q�=�Q�#�=�Q�>B�\>B�\=�Q�=�Q�#�>B�\>B�\>�z�=�Q�=�Q�>B�\>B�\>B�\=�Q�=�Q�>B�\>�z�>B�\>B�\>�z�>B�\>B�\>B�\>B�\>B�\>B�\=�Q�=�Q�>�z�>B�\>B�\>B�\>�z�>�z�>B�\>B�\>B�\>B�\=�Q�>B�\>B�\>B�\�#�>B�\=�Q�>B�\>�z�>�z�>B�\>�z�>B�\>B�\>B�\>�z�=�Q�>B�\=�Q�>B�\>B�\>B�\=�Q�>B�\>B�\>B�\>�z�>�z�>B�\>�z�>�z�>B�\>B�\>B�\>B�\>B�\>�z�=�Q�>B�\>B�\>B�\>�z�>�z�>�z�>�z�>�z�>�z�>�z�>B�\>B�\=�Q�>B�\=�Q�>B�\>B�\>B�\>�z�=�Q�>�z�>�z�>B�\>B�\=�Q�>B�\>�z�=�Q�>B�\>B�\>B�\>B�\>�z�>B�\=�Q�>�z�>�z�>�z�>B�\>�z�>�z�>B�\>�z�>B�\>B�\>B�\>B�\>B�\>�z�>�z�>�z�>Ǯ>�z�>�z�>B�\>B�\=�Q�=�Q�>B�\>�z�>B�\>B�\>B�\>B�\>�z�>�z�>�z�>�z�>�z�>B�\>B�\>B�\>B�\=�Q�>B�\>B�\>B�\>B�\>B�\>B�\>B�\>B�\>�z�>��H>�z�>B�\>B�\>B�\>B�\>B�\>B�\>B�\>�z�>�z�>�z�=�Q�=�Q�>B�\>B�\>�z�>B�\>B�\=�Q�>B�\>B�\>B�\>B�\>B�\>B�\>Ǯ=�Q�=�Q�>�z�>�z�>B�\=�Q�>B�\>�z�>Ǯ>B�\>B�\=�Q�=�Q�=�Q�>B�\>B�\>B�\>B�\>B�\=�Q�>B�\=�Q�>B�\>B�\>B�\>B�\>�z�>B�\>�z�>�z�>B�\>B�\=�Q�>B�\>�z�>B�\>B�\=�Q�>B�\>B�\>B�\>B�\>B�\>B�\>�z�>B�\>�z�>�z�>�z�>B�\>B�\=�Q�>B�\=�Q�>B�\>B�\>B�\>Ǯ>B�\>�z�>�z�>Ǯ>B�\>�z�>B�\>�z�>Ǯ>�z�>�z�=�Q�>B�\=�Q�>B�\=�Q�>B�\>B�\=�Q�=�Q�=�Q�>B�\>B�\>B�\>B�\>�z�>�z�>Ǯ>Ǯ?
>?
>?0��?0��?c�?}p�?}p�?��?�Q�?�Q�?��?��?��R?˅?˅?�Q�?��?��?��?��?��R@@(�@�\@�\@��@\)@\)@%@,(�@2�\@2�\@8��@?\)@L(�@L(�@R�\@X��@_\)@e@l(�@r�\@x��@\)@��H@�{@�G�@�z�@��@��H@��H@�{@�G�@�z�@��@�{@�G�@�G�@�z�@��@�{@�{@�z�@��@��H@�{@�{@�z�@Ϯ@��H@�{@�G�@�z�@߮@��H@�{@�G�@�z�@��H@�{@�G�@�z�@��Ap�A
>A��A�
A	p�A
=A��A=qAp�A
=A��A=qA�
A
=A��A=qA�
A!p�A$��A&=qA'�
A)p�A,��A.=qA/�
A1p�A4��A6=qA7�
A9p�A<��A>=qAAp�AC
=AD��AG�
AIp�AK
=AN=qAO�
AQp�AT��AV=qAW�
AYp�A\��A^=qA_�
Ac
=Ad��Ag�
Aip�Al��An=qAo�
Aqp�At��Av=qAw�
A{
=A|��A�
A��RA��A��A��A��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��RA��A�Q�A��A��RA��A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��A�Q�A��A��A��RA�Q�A��A��AĸRAŅA�Q�A��AȸRAɅA�Q�A��A̸RAͅA�Q�A��A��AиRA�Q�A��A��AԸRAՅA�Q�A��AظRAمA�Q�A��AܸRA݅A�Q�Dq2�Dq8�Dq?\DqL)DqR�DqX�Dqe�Dql)Dqr�Dq\Dq��Dq�)Dq��Dq�\Dq��Dq�)Dq��Dq�\Dq��DqҏDq��Dq�\Dq�)Dq�Dq��Dq�\Dr)Dr�Dr\Dr%�Dr,)Dr2�Dr?\DrE�DrL)DrX�Dr_\Dre�Drr�Drx�Dr\Dr��Dr��Dr��Dr�\Dr�)Dr��Dr��Dr��Dr�)DrҏDr�\Dr��Dr�)Dr��Dr�\Ds�Ds)Ds�Ds\Ds%�Ds2�Ds8�Ds?\DsL)DsR�DsX�Dse�Dsl)Dsr�Dsx�Ds��Ds�)Ds��Ds�\Ds��Ds�)Ds��Ds�\Ds��Ds�)Ds��Ds�\Ds��Ds�Ds��Ds�\Dt)Dt�Dt�Dt\Dt,)Dt2�Dt8�DtE�DtL)DtR�DtX�Dte�Dtl)Dtx�Dt\Dt��Dt�)Dt��Dt�\Dt��Dt��Dt��Dt�\Dt�)DtҏDt��Dt�\Dt�)Dt�Dt��Du�Du)Du�@2�\@8��@?\)@L(�@L(�@R�\@X��@_\)@e@l(�@r�\@x��@\)@��H@�{@�G�@�z�@��@��H@��H@�{@�G�@�z�@��@�{@�G�@�G�@�z�@��@�{@�{@�z�@��@��H@�{@�{@�z�@Ϯ@��H@�{@�G�@�z�@߮@��H@�{@�G�@�z�@��H@�{@�G�@�z�@��Ap�A
>A��A�
A	p�A
=A��A=qAp�A
=A��A=qA�
A
=A��A=qA�
A!p�A$��A&=qA'�
A)p�A,��A.=qA/�
A1p�A4��A6=qA7�
A9p�A<��A>=qAAp�AC
=AD��AG�
AIp�AK
=AN=qAO�
AQp�AT��AV=qAW�
AYp�A\��A^=qA_�
Ac
=Ad��Ag�
Aip�Al��An=qAo�
Aqp�At��Av=qAw�
A{
=A|��A�
A��RA��A��A��A��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��RA�Q�A��A��A��A�Q�A��A��RA��A��A��RA��A�Q�A��A��RA��A��A��A��A�Q�A��A��A��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��RA��A�Q�A��A��A��A�Q�A��A��A��RA�Q�A��A��AĸRAŅA�Q�A��AȸRAɅA�Q�A��A̸RAͅA�Q�A��A��AиRA�Q�A��A��AԸRAՅA�Q�A��AظRAمA�Q�A��AܸRA݅A�Q�Dq2�Dq8�Dq?\DqL)DqR�DqX�Dqe�Dql)Dqr�Dq\Dq��Dq�)Dq��Dq�\Dq��Dq�)Dq��Dq�\Dq��DqҏDq��Dq�\Dq�)Dq�Dq��Dq�\Dr)Dr�Dr\Dr%�Dr,)Dr2�Dr?\DrE�DrL)DrX�Dr_\Dre�Drr�Drx�Dr\Dr��Dr��Dr��Dr�\Dr�)Dr��Dr��Dr��Dr�)DrҏDr�\Dr��Dr�)Dr��Dr�\Ds�Ds)Ds�Ds\Ds%�Ds2�Ds8�Ds?\DsL)DsR�DsX�Dse�Dsl)Dsr�Dsx�Ds��Ds�)Ds��Ds�\Ds��Ds�)Ds��Ds�\Ds��Ds�)Ds��Ds�\Ds��Ds�Ds��Ds�\Dt)Dt�Dt�Dt\Dt,)Dt2�Dt8�DtE�DtL)DtR�DtX�Dte�Dtl)Dtx�Dt\Dt��Dt�)Dt��Dt�\Dt��Dt��Dt��Dt�\Dt�)DtҏDt��Dt�\Dt�)Dt�Dt��Du�Du)Du�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�(�A�+A�-A�/A�-A�%A��A��
A˴9A˙�AˑhAˍPAˇ+AˁA�jA�5?A��#A�n�A���Aə�A�x�A�-A��A�x�A��A��
A�x�A���A��/A�;dA�A��A��A�9XA���A��HA��A��jA�  A�  A�-A���A���A�S�A�A��A�n�A�$�A�\)A��\A��HA��A���A���A�~�A���A�n�A���A�33A�33A��A�&�A�^5A��DA��A�A���A�JA� �A�S�A���A��
A���A�"�A��A��FA�A�5?A�ffA�G�A��A�-A�VA���A��RA���A���A���A�|�A�`BA��A���A��FA��PA�/A�~�A�t�A��A�`BA���A��uA���A���A��A���A�9XA�JA��A�A�A��A��+A�FA}hsAz��Aw�AtjArbNAp�uAn  Ak
=AgC�Aa��A]��AZ�yAW��AVE�ATv�AQ�AN~�ALbAI/AG��AF�9AE%AA�hA>JA<�uA:��A9��A8�\A7O�A6�A4��A3��A2{A0�/A/�mA/�hA/VA,�yA*��A(  A&bNA$�`A$9XA#�A"1A!;dA!%A bNAC�AVA�A&�AbNA�A�An�A-A��A��A(�A�hA��A �A��A�`Ar�A-A��AA1A�AjA �AQ�AA33A
��A��A�A��A�A�hA��AffAv�A {@���@�-@�`B@��@�r�@� �@��@��@��@���@�t�@���@�ƨ@�R@�h@�V@���@�z�@@@�`B@�bN@�@�v�@�(�@�!@���@�E�@�@�Z@�Q�@�9X@㝲@�{@�9@ߥ�@ޟ�@�-@ݩ�@�r�@�M�@؛�@�dZ@ղ-@ԃ@ҟ�@�@мj@�Z@�|�@�^5@���@�?}@���@�7L@�7L@��`@�Ĝ@ʗ�@��#@�ff@�ff@ɡ�@��/@�o@�Ĝ@���@�S�@�X@�7L@��@��9@�n�@���@��h@�?}@���@��
@�ȴ@�J@��^@��`@�(�@��@���@�@�ff@��@���@�bN@��
@��F@���@�l�@�+@�V@���@�p�@�9X@���@�K�@�o@���@��+@���@���@���@��@�1@�\)@�~�@�`B@��@�j@���@�t�@�l�@�ȴ@�ff@���@��h@��@�G�@�%@�`B@��^@���@�$�@�$�@�A�@��w@��
@��/@�?}@�p�@�Q�@��@��+@���@��-@�G�@���@��/@��`@��9@��9@��9@��u@�Z@��m@��F@���@�\)@�
=@��y@��@���@�n�@�^5@�V@�M�@�5?@�-@�$�@�@��#@���@��^@���@��h@�hs@�/@���@��@���@�&�@��@��@�%@�bN@� �@�+@��@���@��H@��@���@��!@��@���@�|�@�dZ@���@�C�@���@��P@�+@�
=@�K�@�t�@��@�;d@�"�@�o@���@�{@�hs@�Ĝ@��;@�S�@���@��!@�ȴ@��@�n�@���@��7@��^@���@��h@�Ĝ@�Z@�(�@�A�@�bN@�Q�@�I�@�b@��@��@��w@��F@�t�@�K�@��@���@���@�V@�n�@�v�@�J@��T@���@�?}@�/@�V@��`@�Ĝ@��9@��
@�|�@���@�\)@��;@��w@�@���@��@�K�@�l�@�S�@�"�@���@��R@���@�v�@�v�@�~�@�J@��@�bN@��@�Q�@��P@��@���@�\)@��H@���@�n�@���@���@�p�@���@���@�J@���@��@�I�@��D@�/@�C-@z}V@r�H@i^�@a�@Z�@T��@L�P@E��@?��@9�j@5q@-ԕ@'�@#t�@��@}V@�@��@ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A�7LA��#A���A�ȴA�jA��;A�^5A���A��A���A�"�Aˡ�A��A�  A�^5A���A�A��\AɍPA��AŅA�~�A��A��A��
A�A�A�\)A�"�A�33A�=qA���A�x�A�VA���A��A�33A�t�A�A��A�oA���A��\A��jA�|�A�&�AȃA�  A˼jAǝ�A���A���A���A�A��A��uA�C�Aė�A�x�A�{A��mA�A�ĜA�VA�1A���A�n�A�VAȧ�A��#A��^A��HA���A��TA�VA���A��A�A�A�A�I�A���A���A�l�A�AÍPA�S�A��#A�M�A�r�A���A��A��uA�z�A���Aɕ�A��A�`BA�bNA�G�A���AìA���AʸRAˣ�A�M�A�  A˸RA��A�=qAĬAȉ7A˓uA��^A�n�A���Aô9A��yA�
=A���A�`BA�`BA�%A�A˲-A��+A�1'A��mA��A�(�A�  A�=qA�n�A��A�`BA��AǇ+A�oA���AƼjAˏ\A��AÕ�A�JAɗ�A��DA�/A��yA��A��A�oA�oA�1AÏ\A�;dA���A�VA�XA�bNAƧ�A�VA�
=A�bA�1'A�r�A���AɼjA�G�A��`A�VA�1A��A���A�r�A�hsA�  AȲ-A�ffA�ĜAǅA�
=A�
=A�1A� �A�%A˝�A��A�VA��yA�XA�\)A�JA�bA��A�1AȅA�|�A�$�A�VA�VA�A�=qA�`BA���A�{A�%A���A�1A��
A�\)A�A�bAˏ\A���Aʺ^A�%A�bA�VA�bA�
=A�VA˃A�JA�A�1A�%A�oA�{A�
=A�
=A�JA�JA�bA�VA�bA��A�bA�VA�oA��A�bA�VA�{A��A�oA��A��A��A�oA�oA�JA�
=A���A�VA�VA�oA��A��A��A�oA�{A�bA�bA�oA��A�oA�oA�{A�bA�bA�oA�VA�oA�
=A�oA�oA�bA�{A�oA��Aʰ!A�JA�
=A�VA�{A�oA�bA�VA�oA�bA�{A�{A�oA�oA�{A��A�{A�{A�oA�JA�oA�JA�oA�oA�oA�bA�{A�oA�oA��A��A�ĜA��A�VA�oA�{A�{A�
=A�
=A�bA�1A�1A�{A�bA�JA�bA�bA�oA�
=A�oA�bA�oA�oA�oA�{A�{A�{A�{A�oA��A�oA�VA�bA�1A�
=A��A�oA�{A�{A�{A��A�{A��A��A��A��A��A�{A�{A�oA�oA�oA�{A�JA�JA��A��A�bA��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A�{A�bA��A��A��A�{A�{A��A��A�{A��A��A�{A�oA��A��A�VAˉ7A��A��A��A��A��A��A��A�%A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A�oA��A��A�oA�bA�oA�bA�oA��A��A��A��A��A��A��A��A��A��A�bA��A��A�{A��A�$�A�"�A��A�"�A�$�A�&�A�"�A�"�A�$�A�"�A�$�A� �A�oA���A� �A��A��A��A�$�A˶FA��A�&�A�"�A�VA�&�A�$�A�&�A�$�A�$�A�&�A�&�A�&�A�(�A�(�A�&�A�(�A�(�A�(�A�$�A�&�A�&�A�&�A�$�A�$�A�$�A�(�A�(�A�(�A�(�A�&�A�&�A�(�A�$�A�(�A�&�A�&�A�"�A� �A�$�A� �A� �A� �A�"�A�(�A�+A�&�A�(�A�+A�(�A�+A�-A�+A�-A�-A�-A�-A�+A�+A�(�A�-A�(�A�&�A�(�A�&�A�&�A�(�A�(�A�&�A�(�A�&�A�&�A�+A�-A�/A�1'A�/A�/A�/A�/A�-A�+A�+A�(�A�+A�+A�-A�/A�-A�/A�-A�-A�/A�+A�1'A�+A�(�A�+A�(�A�-A�+A�+A�-A�+A�+A�+A�-A�-A�-A�-A�/A�-A�/A�-A�-A�/A�/A�/A�/A�1'A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�/A�&�A�/A�1'A�/A�/A�/A�1'A�1A�%A�1A�
=A�1A�1A�%A�1A�  A�  A���A�
=A���A���A���A��A��A��A��A��A��A��A���A���A���A���A���A��A��A��;A��TA��yA��A���A���A�ƨA���A�A�ƨA���A���A���A���A�A˺^A˺^A˺^A˶FA˴9A˴9A˴9AˮAˬAˮAˬAˬA˩�A˧�Aˣ�A˛�A˛�A˗�A˗�A˕�A˗�A˕�A˗�A˗�A˗�A˗�A˕�A˕�A˕�A˕�A˓uA˓uA˓uA˓uA˓uAˑhAˑhAˑhAˑhAˏ\AˑhAˏ\Aˏ\AˍPAˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\AˍPAˍPAˏ\Aˏ\Aˏ\AˍPAˍPAˉ7AˋDAˉ7Aˉ7Aˇ+Aˉ7Aˇ+Aˉ7Aˉ7Aˇ+@�X@�`B@�hs@�p�@��h@�$�@�M�@��\@��@�33@�33@�;d@�;d@�S�@�S�@�K�@�;d@�33@��@�
=@��@���@�
=@�
=@�
=@���@��y@��H@��H@��@���@�^5@�=q@�-@�5?@��@�{@��@�{@�@�@���@���@���@���@�@�@�@�@�@���@���@���@��@��@��@��@��T@��T@��#@���@���@���@�x�@�/@���@��@�Ĝ@��@��j@��9@��9@��9@��@��9@��j@��9@���@�z�@�z�@�bN@�bN@�Z@�Q�@�I�@�A�@�9X@�1'@�9X@�1'@�9X@�A�@�Q�@�I�@�Q�@�Q�@�bN@�r�@�r�@��@��u@��u@��u@��u@��u@��D@��D@��D@��u@��u@��9@���@��@�&�@�7L@�/@�/@�7L@�/@�/A� �A� �A� �A�&�A�&�A�"�A�&�A�+A�+A�-A�-A�+A�-A�-A�-A�+A�-A�(�A�&�A�&�A�+A�$�A�(�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�-A�1'A�/A�-A�1'A�-A�/A�+A�(�A�+A�+A�+A�+A�-A�-A�/A�/A�/A�-A�-A�/A�-A�-A�(�A�(�A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�-A�-A�/A�/A�-A�1'A�1'A�/A�/A�1'A�1'A�/A�1'A�1'A�/A�$�A�$�A�$�A�-A�1'A�1'A�1'A�-A��A�JA�%A�1A�bA�
=A�%A�A�1A�1A�%A���A���A���A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��`A��HA��`A��A��A��/A���A�ȴA���A�ȴA�ȴA���A���A���A���A���A˺^A˺^A˺^A˶FA˶FA˲-A˲-AˮA˰!AˮAˬAˬA˩�A˧�A˥�A˝�A˙�A˙�A˗�A˗�A˗�A˗�A˗�A˗�A˗�A˗�A˕�A˕�A˕�A˓uA˓uA˓uA˕�A˓uA˓uA˓uA˓uA˓uAˑhAˑhAˏ\Aˏ\AˍPAˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\AˍPAˍPAˏ\AˍPAˏ\Aˏ\AˍPAˋDAˋDAˉ7Aˉ7Aˉ7Aˉ7Aˉ7Aˉ7Aˇ+Aˇ+@�`B@�X@�hs@�`B@�p�@�@�5?@�^5@���@�@�;d@�;d@�;d@�S�@�S�@�K�@�C�@�33@�+@�o@��@��@�
=@�o@�
=@�@��@��y@��@���@���@�n�@�M�@�E�@�-@�$�@�{@��@��@�J@�@�@���@���@���@�@�@�@�@�@�@�@���@���@��@��@��@��@��T@��#@��#@���@��-@�x�@�?}@���@��@���@��@��9@��9@��9@��9@��@��@��9@��9@���@��@�z�@�r�@�j@�bN@�Z@�Q�@�A�@�9X@�1'@�1'@�1'@�9X@�A�@�Q�@�Q�@�Q�@�Q�@�Z@�j@�r�@��@��D@��u@��u@��u@��u@��u@��D@��D@��D@��u@��@��j@�%@�&�@�7L@�7L@�/@�7L@�7L@�7LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�"�A�(�A�+A�-A�/A�-A�%A��A��
A˴9A˙�AˑhAˍPAˇ+AˁA�jA�5?A��#A�n�A���Aə�A�x�A�-A��A�x�A��A��
A�x�A���A��/A�;dA�A��A��A�9XA���A��HA��A��jA�  A�  A�-A���A���A�S�A�A��A�n�A�$�A�\)A��\A��HA��A���A���A�~�A���A�n�A���A�33A�33A��A�&�A�^5A��DA��A�A���A�JA� �A�S�A���A��
A���A�"�A��A��FA�A�5?A�ffA�G�A��A�-A�VA���A��RA���A���A���A�|�A�`BA��A���A��FA��PA�/A�~�A�t�A��A�`BA���A��uA���A���A��A���A�9XA�JA��A�A�A��A��+A�FA}hsAz��Aw�AtjArbNAp�uAn  Ak
=AgC�Aa��A]��AZ�yAW��AVE�ATv�AQ�AN~�ALbAI/AG��AF�9AE%AA�hA>JA<�uA:��A9��A8�\A7O�A6�A4��A3��A2{A0�/A/�mA/�hA/VA,�yA*��A(  A&bNA$�`A$9XA#�A"1A!;dA!%A bNAC�AVA�A&�AbNA�A�An�A-A��A��A(�A�hA��A �A��A�`Ar�A-A��AA1A�AjA �AQ�AA33A
��A��A�A��A�A�hA��AffAv�A {@���@�-@�`B@��@�r�@� �@��@��@��@���@�t�@���@�ƨ@�R@�h@�V@���@�z�@@@�`B@�bN@�@�v�@�(�@�!@���@�E�@�@�Z@�Q�@�9X@㝲@�{@�9@ߥ�@ޟ�@�-@ݩ�@�r�@�M�@؛�@�dZ@ղ-@ԃ@ҟ�@�@мj@�Z@�|�@�^5@���@�?}@���@�7L@�7L@��`@�Ĝ@ʗ�@��#@�ff@�ff@ɡ�@��/@�o@�Ĝ@���@�S�@�X@�7L@��@��9@�n�@���@��h@�?}@���@��
@�ȴ@�J@��^@��`@�(�@��@���@�@�ff@��@���@�bN@��
@��F@���@�l�@�+@�V@���@�p�@�9X@���@�K�@�o@���@��+@���@���@���@��@�1@�\)@�~�@�`B@��@�j@���@�t�@�l�@�ȴ@�ff@���@��h@��@�G�@�%@�`B@��^@���@�$�@�$�@�A�@��w@��
@��/@�?}@�p�@�Q�@��@��+@���@��-@�G�@���@��/@��`@��9@��9@��9@��u@�Z@��m@��F@���@�\)@�
=@��y@��@���@�n�@�^5@�V@�M�@�5?@�-@�$�@�@��#@���@��^@���@��h@�hs@�/@���@��@���@�&�@��@��@�%@�bN@� �@�+@��@���@��H@��@���@��!@��@���@�|�@�dZ@���@�C�@���@��P@�+@�
=@�K�@�t�@��@�;d@�"�@�o@���@�{@�hs@�Ĝ@��;@�S�@���@��!@�ȴ@��@�n�@���@��7@��^@���@��h@�Ĝ@�Z@�(�@�A�@�bN@�Q�@�I�@�b@��@��@��w@��F@�t�@�K�@��@���@���@�V@�n�@�v�@�J@��T@���@�?}@�/@�V@��`@�Ĝ@��9@��
@�|�@���@�\)@��;@��w@�@���@��@�K�@�l�@�S�@�"�@���@��R@���@�v�@�v�@�~�@�J@��@�bN@��@�Q�@��P@��@���@�\)@��H@���@�n�@���@���@�p�@���@���@�J@���@��@�I�@��DG�O�@�C-@z}V@r�H@i^�@a�@Z�@T��@L�P@E��@?��@9�j@5q@-ԕ@'�@#t�@��@}V@�@��@ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A�7LA��#A���A�ȴA�jA��;A�^5A���A��A���A�"�Aˡ�A��A�  A�^5A���A�A��\AɍPA��AŅA�~�A��A��A��
A�A�A�\)A�"�A�33A�=qA���A�x�A�VA���A��A�33A�t�A�A��A�oA���A��\A��jA�|�A�&�AȃA�  A˼jAǝ�A���A���A���A�A��A��uA�C�Aė�A�x�A�{A��mA�A�ĜA�VA�1A���A�n�A�VAȧ�A��#A��^A��HA���A��TA�VA���A��A�A�A�A�I�A���A���A�l�A�AÍPA�S�A��#A�M�A�r�A���A��A��uA�z�A���Aɕ�A��A�`BA�bNA�G�A���AìA���AʸRAˣ�A�M�A�  A˸RA��A�=qAĬAȉ7A˓uA��^A�n�A���Aô9A��yA�
=A���A�`BA�`BA�%A�A˲-A��+A�1'A��mA��A�(�A�  A�=qA�n�A��A�`BA��AǇ+A�oA���AƼjAˏ\A��AÕ�A�JAɗ�A��DA�/A��yA��A��A�oA�oA�1AÏ\A�;dA���A�VA�XA�bNAƧ�A�VA�
=A�bA�1'A�r�A���AɼjA�G�A��`A�VA�1A��A���A�r�A�hsA�  AȲ-A�ffA�ĜAǅA�
=A�
=A�1A� �A�%A˝�A��A�VA��yA�XA�\)A�JA�bA��A�1AȅA�|�A�$�A�VA�VA�A�=qA�`BA���A�{A�%A���A�1A��
A�\)A�A�bAˏ\A���Aʺ^A�%A�bA�VA�bA�
=A�VA˃A�JA�A�1A�%A�oA�{A�
=A�
=A�JA�JA�bA�VA�bA��A�bA�VA�oA��A�bA�VA�{A��A�oA��A��A��A�oA�oA�JA�
=A���A�VA�VA�oA��A��A��A�oA�{A�bA�bA�oA��A�oA�oA�{A�bA�bA�oA�VA�oA�
=A�oA�oA�bA�{A�oA��Aʰ!A�JA�
=A�VA�{A�oA�bA�VA�oA�bA�{A�{A�oA�oA�{A��A�{A�{A�oA�JA�oA�JA�oA�oA�oA�bA�{A�oA�oA��A��A�ĜA��A�VA�oA�{A�{A�
=A�
=A�bA�1A�1A�{A�bA�JA�bA�bA�oA�
=A�oA�bA�oA�oA�oA�{A�{A�{A�{A�oA��A�oA�VA�bA�1A�
=A��A�oA�{A�{A�{A��A�{A��A��A��A��A��A�{A�{A�oA�oA�oA�{A�JA�JA��A��A�bA��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A�{A�bA��A��A��A�{A�{A��A��A�{A��A��A�{A�oA��A��A�VAˉ7A��A��A��A��A��A��A��A�%A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A�oA��A��A�oA�bA�oA�bA�oA��A��A��A��A��A��A��A��A��A��A�bA��A��A�{A��A�$�A�"�A��A�"�A�$�A�&�A�"�A�"�A�$�A�"�A�$�A� �A�oA���A� �A��A��A��A�$�A˶FA��A�&�A�"�A�VA�&�A�$�A�&�A�$�A�$�A�&�A�&�A�&�A�(�A�(�A�&�A�(�A�(�A�(�A�$�A�&�A�&�A�&�A�$�A�$�A�$�A�(�A�(�A�(�A�(�A�&�A�&�A�(�A�$�A�(�A�&�A�&�A�"�A� �A�$�A� �A� �A� �A� �A� �A� �A�&�A�&�A�"�A�&�A�+A�+A�-A�-A�+A�-A�-A�-A�+A�-A�(�A�&�A�&�A�+A�$�A�(�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�-A�1'A�/A�-A�1'A�-A�/A�+A�(�A�+A�+A�+A�+A�-A�-A�/A�/A�/A�-A�-A�/A�-A�-A�(�A�(�A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�-A�-A�/A�/A�-A�1'A�1'A�/A�/A�1'A�1'A�/A�1'A�1'A�/A�$�A�$�A�$�A�-A�1'A�1'A�1'A�-A��A�JA�%A�1A�bA�
=A�%A�A�1A�1A�%A���A���A���A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��`A��HA��`A��A��A��/A���A�ȴA���A�ȴA�ȴA���A���A���A���A���A˺^A˺^A˺^A˶FA˶FA˲-A˲-AˮA˰!AˮAˬAˬA˩�A˧�A˥�A˝�A˙�A˙�A˗�A˗�A˗�A˗�A˗�A˗�A˗�A˗�A˕�A˕�A˕�A˓uA˓uA˓uA˕�A˓uA˓uA˓uA˓uA˓uAˑhAˑhAˏ\Aˏ\AˍPAˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\AˍPAˍPAˏ\AˍPAˏ\Aˏ\AˍPAˋDAˋDAˉ7Aˉ7Aˉ7Aˉ7Aˉ7Aˉ7Aˇ+Aˇ+@�`B@�X@�hs@�`B@�p�@�@�5?@�^5@���@�@�;d@�;d@�;d@�S�@�S�@�K�@�C�@�33@�+@�o@��@��@�
=@�o@�
=@�@��@��y@��@���@���@�n�@�M�@�E�@�-@�$�@�{@��@��@�J@�@�@���@���@���@�@�@�@�@�@�@�@���@���@��@��@��@��@��T@��#@��#@���@��-@�x�@�?}@���@��@���@��@��9@��9@��9@��9@��@��@��9@��9@���@��@�z�@�r�@�j@�bN@�Z@�Q�@�A�@�9X@�1'@�1'@�1'@�9X@�A�@�Q�@�Q�@�Q�@�Q�@�Z@�j@�r�@��@��D@��u@��u@��u@��u@��u@��D@��D@��D@��u@��@��j@�%@�&�@�7L@�7L@�/@�7L@�7L@�7LA� �A� �A� �A�&�A�&�A�"�A�&�A�+A�+A�-A�-A�+A�-A�-A�-A�+A�-A�(�A�&�A�&�A�+A�$�A�(�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�-A�1'A�/A�-A�1'A�-A�/A�+A�(�A�+A�+A�+A�+A�-A�-A�/A�/A�/A�-A�-A�/A�-A�-A�(�A�(�A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�-A�-A�/A�/A�-A�1'A�1'A�/A�/A�1'A�1'A�/A�1'A�1'A�/A�$�A�$�A�$�A�-A�1'A�1'A�1'A�-A��A�JA�%A�1A�bA�
=A�%A�A�1A�1A�%A���A���A���A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��`A��HA��`A��A��A��/A���A�ȴA���A�ȴA�ȴA���A���A���A���A���A˺^A˺^A˺^A˶FA˶FA˲-A˲-AˮA˰!AˮAˬAˬA˩�A˧�A˥�A˝�A˙�A˙�A˗�A˗�A˗�A˗�A˗�A˗�A˗�A˗�A˕�A˕�A˕�A˓uA˓uA˓uA˕�A˓uA˓uA˓uA˓uA˓uAˑhAˑhAˏ\Aˏ\AˍPAˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\Aˏ\AˍPAˍPAˏ\AˍPAˏ\Aˏ\AˍPAˋDAˋDAˉ7Aˉ7Aˉ7Aˉ7Aˉ7Aˉ7Aˇ+Aˇ+@�`B@�X@�hs@�`B@�p�@�@�5?@�^5@���@�@�;d@�;d@�;d@�S�@�S�@�K�@�C�@�33@�+@�o@��@��@�
=@�o@�
=@�@��@��y@��@���@���@�n�@�M�@�E�@�-@�$�@�{@��@��@�J@�@�@���@���@���@�@�@�@�@�@�@�@���@���@��@��@��@��@��T@��#@��#@���@��-@�x�@�?}@���@��@���@��@��9@��9@��9@��9@��@��@��9@��9@���@��@�z�@�r�@�j@�bN@�Z@�Q�@�A�@�9X@�1'@�1'@�1'@�9X@�A�@�Q�@�Q�@�Q�@�Q�@�Z@�j@�r�@��@��D@��u@��u@��u@��u@��u@��D@��D@��D@��u@��@��j@�%@�&�@�7L@�7L@�/@�7L@�7L@�7LG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�@�Q�@-��=��[>.�@�xl>.��@�Y`=�f{>��@"�=��p>
�"@S@�N�>CB@E�@�\)@�`=�Vm>�͟@�T�@�R>!�`@�Sz@�T�@�BF=�1�> 7@e@�ܱ@=�z�>�?�@�[�@�a|>"h>�"�@�M+@i/@�`W=�e�>+ud=�[�=��=�y�>���@�_@�`>F�@��@�[@�`�>t@���=���=�*o>s�> )�>@@si�@�a(@�]�=���>�n/?�8=���>Kƨ>��3?f`�>��@�@�W@�Ri>@��Q>y�=�q�>R�?�3	=��>=D>�L�@�`?�>=@�W*=�%�=�:?=��~>[/�=��P>Y_�=�|>ŕ�?�=jt~>���=��n=���?�=�{�?wU�@�?h>tk'@�d@�b�=�ܱ=�-�=�k�@�c@�_�=���=�0�@��$>�n@�@�b�@�e�>��>�{�@�_@�b�@�h
=���=���>b�@��?5O7>�>W=�=�!@t=�¤=��>L�.@�i�?�9�=��@y�0@� �><�o@�f<@�h�>��@.��?Zd@�Zq=�0U>2<�@�i@�k<>��>+�q@���@L>.��>*>��@ ��@�do@�h^@�hs=�D>��>n��@�h^>	��@E#�=�j?7^ @�~@Ik�=�?}?�O�@I��>mՑ@&�D>"�b@���@�f<@�b�>q�G>DR@�U@�k�@�k�@�h�=�X>�t@�jj@�i�@�i@�i@�g�>9�>� @�jj@�jj@�g�>��>SB�@�j+@�j�@�j�?av�@�jj?���@�w�@�j@�i�@�i�@�gM?lڐ@�in@�k�@�jj@�j�@�iY@�i�@B�5@�j@�iY@�h�@�k'@�l7@�l�@�h^@�in@�i�@�j�@�k<@�j�@�l�@�lL@�j+@�h�@�m	@�m	@�k<@�lL@�m	@�mr@�m	@�mr@�m	@�l�@�k�@�j�@�j�@�j�@�j+@�j+@�k�@�n@�l�@�lL@�l�@�k�@�l�@�l�@�m	@�k�@�m�@�j�@�i@�k�@�m	@�j�@�l�@�k�@�l7@�l�@�l�@�k�@�i�@�l�@�j�@�mH@W�,@�j+@�k<@�k�@�lL@�j�@�j�@�k�@�k�@�l�@�l�@�lL@�lL@�k�@�l�@�l�@�lL@�l�@�lL@�lL@�lL@�j�@�lL@�lL@�lL@�lL@�lL@�k�@�l�@�m	@�m	@�m	@�n@�lL@�l7@�l7@�k�@�k'@�k�@�j�@�j@�j�@�k�@�k�@�k'@�j@�j�@�i�@�i@�m	@�l�@�k�@�mr@�m	@�l�@�lL@�k�@�lL@�k�@�k�@�lL@�k<@�j�@�i�@�k�@�m	@�m	@�n�@�lL@�l�@�mr@�m�@�o�@�n�@�mr@�m	@�m	@�k�@�lL@�k�@�k�@�k�@�lL@�j+@�i@�m�@�l�@�mH@�oi@�p&@�pz@�nY@�mH@�oi@�o@�o@�n�@�oi@�o�@�o�@�o?@�n�@�lL@�m	@�n�@�l�@�lL@�o?@�n@�mr@�mr@�m�@�m�@�m�@�n@�n@�l�@�k�@�k�@�n@�n@�l�@�mr@�n�@�n�@�n�@�m�@�n@�o?@�n�@�n�@�n�@�n�@�n�@�m�@�n�@�n�@�n�@�nY@�m�@�nY@�m�@�m�@�n�@�n�@�l�@�l�@�l�@�m�@�n�@�l�@�l�@�l�@�m	@�k�@�k�@�k�@�k�@�l�@�mr@�m�@�m�@�o�@�p�@�p�@�s.@�o?@�o?@�pP@�qa@�pP@�q�@�rq@�rq@�rq@�rq@�r�@�s.@�s.@�rq@�r�@�s�@�sX@�s@�s@�p�@���@�pz@�q7@�p�@�s@�sX@��@�rG@�s�@�sX@�s@�s�@�s�@�s�@�s�@�s.@�rq@�r�@�t�@�t�@�t?@�t?@�t�@�t�@�t�@�t�@�t?@�t?@�t?@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�u�@�t?@�t�@�s@�sX@�sX@�sX@�r�@�sX@�sX@�u�@�v@�vu@�vu@�v@�v�@�vu@�w�@�w�@�w2@�w2@�w2@�w�@�v�@�w�@�w2@�v�@�v�@�v�@�v6@�v!@�v�@�v!@�vK@�v�@�v�@�v�@�vu@�v�@�w�@�xB@�yS@�y�@�z@�x�@�xB@�x�@�x�@�xB@�x@�x@�x@�x�@�y)@�y}@�z%@�z:@�z%@�yh@�yh@�yh@�yh@�yh@�x�@�x�@�x�@�yh@�y�@�y�@�y�@�z%@�z%@�y�@�z%@�z:@�z:@�z�@�z�@�zx@�z�@�{5@�z�@�{5@�z�@�{5@�{5@�{�@�{5@�{�@�{�@�{�@�{�@�{�@�{�@�{�@�z�@�zN@�xW@�x-@�y�@�zN@�y�@�y@�wG@�u�@�kf@�kQ@�lv@�lv@�k@�j�@�k@�i�@�g�@�f�@�f�@�g#@�d�@�dE@�c�@�b�@�c5@�b�@�c5@�c�@�c�@�dE@�ff@�f@�e@�eV@�d�@�b�@�]�@�\S@�^�@�^�@�^5@�\h@�R~@�T�@�T7@�R@�R~@�U�@�U�@�T�@�R@�M�@�M+@�Ln@�L@�J�@�JM@�I�@�H�@�H,@�H,@�Go@�G@�F@�E�@�DR@�B1@�A@�@d@�@d@�@d@�@d@�@d@�@@�@d@�@@�@�@�@@�@@�?�@�@@�?�@�?S@�?�@�?S@�?S@�?S@�>�@�>�@�>�@�>�@�>�@�>B@�=�@�=�@�>B@�>B@�>B@�>B@�>B@�>B@�>B@�>B@�>B@�>�@�>B@�>B@�=�@�=�@�=2@�=2@�<�@�<�@�<�@�<�@�=G@�=G@�=�@�=�@�=�@Q6@QS@Q@Q_@Q�@Q/�@Q6�@QH@QR*@QZ\@QZ2@QZ�@Q]O@Q^ @Q\�@QZ�@QX:@QV@QR�@QO7@QO�@QR*@QR*@QP�@QN�@QL�@QI�@QG�@QD�@Q=\@Q4�@Q.�@Q,�@Q+@Q(�@Q(@Q(9@Q'�@Q&B@Q%F@Q$t@Q$J@Q$J@Q$J@Q$�@Q$�@Q%@Q$J@Q$ @Q#O@Q"�@Q"S@Q!-@Q�@Q:@Q?@QC@Q�@QU@Q@Q@Q@Q�@P�L@P��@P��@P�@P�@P�X@P�S@P��@P�@P�X@P�.@P��@P��@P�;@P�P@P�Y@P�
@P�@P�@P�j@P�@P�w@P�|@P��@P�$@P��@P�@P�j@P�]@Pܱ@Pݭ@P�P@P�H@P�@P��@P��@P��@P�@P�B@P�@P��@P��@P��@P�@P��@P�|@P�f@P��@Q�@Q0@Q|@Q�@Q(@Q�@QR@Q(@Q(@��u@��"@���@��@��S@���@��S@��d@���@���@���@���@���@���@���@���@���@��@��S@��)@���@��@��)@��>@���@��S@���@���@���@���@��%@��[@���@��@��@��@��@���@���@��!@���@��@���@���@���@��p@���@���@��-@��@���@��-@���@��p@��K@���@���@��p@��@��-@��@��@���@���@��-@���@��-@��l@���@��l@���@���@��@��)@��h@��>@���@���@��$@���@���@��9@��N@���@��N@��h@���@���@���@��>@��c@���@��}@���@�� @��e@���@���@���@��"@���@���@��n@��n@��n@��@���@���@��N@���@���@��(@���@��g@���@��R@���@��Q@���@���@��@���@���@���@���@���@��@���@���@���@���@��@���@���@��@��q@��@���@���@���@��s@���@��'@���@��@��^@��@���@��4@��@�9@�~(@�}k@�y�@�x�@�xB@�x@�x@�w�@�w�@�w�@�w�@�w�@�w�@�w�@�w�@�v�@�wG@�w@�v�@�w@�v�@�v�@�v�@�vu@�vK@�v6@�u�@�u�@�u�@�t�@�t�@�u@�uy@�uy@�uO@�u:@�u:@�uO@�u:@�uy@�uO@�uO@�uy@�u:@�t�@�t*@�s�@�s�@�sX@�sX@�sC@�sX@�s�@�s�@�s�@Q*�@Q)@Q,|@Q+,@Q/�@Q:�@QXd@Q`�@Qti@Q�
@Q��@Q�"@Q�@Q�@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q�#@Q��@Q�3@Q��@Q�@Q�I@Q��@Q9@Q@QxW@Qm	@Qe�@Qe,@Q`�@Q]�@Q[�@Q[�@Q]%@Q[@QY6@QX�@QX@QX@QXd@QY@QZ\@QZ�@QY�@QY�@QY`@QY�@QX�@QX�@QW�@QW@QV@QU�@QT�@QR~@QR~@QN�@QKI@QA�@Q6P@Q*@Q'=@Q#�@Q�@Qq@QC@Q�@Q�@Qq@Q�@Qm@Q@Q�@Q@@Qs@Q|@Q0@Q�@Q�@Q
=@QF@Q%@Q}@Q�@Q%@QJ@QF@Q@Q5@Q�@Q�@Q(@Q�@Q�@Q�@Q"@QC@Q�@Q�@QC@Q�@Qm@Q@QC@Q�@Q"S@Q&�@Q5�@Q>�@QA�@QB�@QB@QB�@QB�@QBpG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         43444343444443344334434433344434444334434344444433433343444443334444444443343444444434434444444444444444343344443443443344333444344444444344334334443443344344444333444344443444444433344333344333334433344333434333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Q�G�O�G�O�G�O�@�xjG�O�@�YbG�O�G�O�G�O�G�O�G�O�@S@�N�G�O�G�O�@�\*@�`G�O�G�O�@�T�G�O�G�O�@�Sx@�T�@�BDG�O�G�O�G�O�@�ܵG�O�G�O�G�O�G�O�@�[�@�azG�O�G�O�@�M.G�O�@�`\G�O�G�O�G�O�G�O�G�O�G�O�@�_@�`G�O�@��@�[@�`�G�O�@���G�O�G�O�G�O�G�O�G�O�@si�@�a%@�]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�W@�RkG�O�@��TG�O�G�O�G�O�G�O�G�O�G�O�G�O�@�`G�O�G�O�@�W)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�?gG�O�@�d@�b�G�O�G�O�G�O�G�O�@�_�G�O�G�O�@��&G�O�G�O�@�b�@�e�G�O�G�O�@�_@�b�@�h
G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�i�G�O�G�O�@y�2@� �G�O�@�f>@�h�G�O�G�O�G�O�@�ZtG�O�G�O�@�i@�k>G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�@�dn@�h[@�hqG�O�G�O�G�O�@�h`G�O�G�O�G�O�G�O�@�|G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@�f=@�b�G�O�G�O�@�U@�k�@�k�@�h�G�O�G�O�@�jn@�i�@�i@�i@�g�G�O�G�O�@�jn@�ji@�g�G�O�G�O�@�j*@�j�@�j�G�O�@�jkG�O�@�w�@�j@�i�@�i�@�gNG�O�@�ir@�k�@�ji@�j�@�i^@�i�G�O�@�j@�iW@�h�@�k&@�l:@�l�@�h]@�ip@�i�@�j�@�k=@�j�@�l�@�lN@�j,@�h�@�m@�m@�k=@�lJ@�m@�ms@�m	@�ms@�m@�l�@�k�@�j�@�j�@�j�@�j+@�j-@�k�@�n@�l�@�lN@�l�@�k�@�l�@�l�@�m	@�k�@�m�@�j�@�i@�k�@�m	@�j�@�l�@�k�@�l:@�l�@�l�@�k�@�i�@�l�@�j�@�mN@W�*@�j)@�k:@�k�@�lN@�j�@�j�@�k�@�k�@�l�@�l�@�lN@�lL@�k�@�l�@�l�@�lN@�l�@�lN@�lM@�lN@�j�@�lN@�lN@�lN@�lL@�lN@�k�@�l�@�m@�m@�m	@�n@�lI@�l7@�l6@�k�@�k&@�k�@�j�@�j@�j�@�k�@�k�@�k'@�j@�j�@�i�@�i@�m	@�l�@�k�@�ms@�m
@�l�@�lN@�k�@�lN@�k�@�k�@�lL@�k>@�j�@�i�@�k�@�m@�m	@�n�@�lN@�l�@�ms@�m�@�o�@�n�@�ms@�m@�m@�k�@�lN@�k�@�k�@�k�@�lN@�j)@�i@�m�@�l�@�mH@�of@�p'@�p}@�nY@�mK@�of@�o@�o@�n�@�oo@�o�@�o�@�o>@�n�@�lN@�m@�n�@�l�@�lL@�o<@�n@�mn@�mp@�m�@�m�@�m�@�n@�n@�l�@�k�@�k�@�n@�n@�l�@�mv@�n�@�n�@�n�@�m�@�n@�o?@�n�@�n�@�n�@�n�@�n�@�m�@�n�@�n�@�n�@�n]@�m�@�n\@�m�@�m�@�n�@�n�@�l�@�l�@�l�@�m�@�n�@�l�@�l�@�l�@�m
@�k�@�k�@�k�@�k�@�l�@�ms@�m�@�m�@�o�@�p�@�p�@�s+@�o<@�o<@�pS@�q^@�pQ@�q�@�rr@�rr@�ru@�rr@�r�@�s.@�s.@�ru@�r�@�s�@�sZ@�s@�s@�p�@���@�p|@�q:@�p�@�s@�sS@��@�rK@�s�@�sZ@�s@�s�@�s�@�s�@�s�@�s.@�rq@�r�@�t�@�t�@�tB@�tE@�t�@�t�@�t�@�t�@�t@@�t@@�tB@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�t�@�u�@�t@@�t�@�s@�sZ@�sV@�sV@�r�@�sY@�sZ@�u�@��w@��"@���@��@��O@���@��V@��d@���@���@���@���@���@���@���@���@���@��@��V@��)@���@��@��*@��>@���@��U@���@���@���@���@��&@��^@���@��@��@��@��@���@���@��!@���@��@���@���@���@��m@���@���@��2@��@���@��1@���@��n@��M@���@���@��p@��@��*@��@��@���@���@��1@���@��1@��n@���@��r@���@���@��@��+@��e@��>@���@���@��%@���@���@��7@��T@���@��J@��h@���@���@���@��>@��f@���@���@���@�� @��l@���@���@���@��#@���@���@��n@��n@��q@��@���@���@��O@���@���@��*@���@��h@���@��V@���@��V@���@���@��@���@���@���@���@���@��@���@���@���@���@��@���@���@��@��q@��"@���@���@���@��p@���@��*@���@��@��b@��@���@��6@��@�:@�~%@�}n@�y�@�x�@�xD@�x@�x@�w�@�w�@�w�@�w�@�w�@�w�@�w�@�w�@�v�@�wH@�w@�v�@�w@�v�@�v�@�v�@�vv@�vJ@�v3@�u�@�u�@�u�@�t�@�t�@�u@�uz@�uv@�uN@�u<@�u<@�uN@�u>@�uz@�uN@�uR@�uw@�u:@�t�@�t-@�s�@�s�@�sZ@�sZ@�sE@�s^@�s�@�s�@�s�@Q*�@Q)
@Q,�@Q+.@Q/�@Q:�@QXc@Q`�@Qtk@Q�@Q��@Q�"@Q�@Q�
@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q�"@Q��@Q�3@Q��@Q�@Q�H@Q��@Q6@Q@QxU@Qm@Qe~@Qe*@Q`�@Q]�@Q\@Q\ @Q]#@Q[@QY6@QX�@QX@QX@QXf@QY
@QZ]@QZ�@QY�@QY�@QYc@QY�@QX�@QX�@QW�@QW@QV@QU�@QT�@QRz@QR~@QN�@QKH@QA�@Q6P@Q*@Q':@Q#�@Q�@Qr@QB@Q�@Q�@Qp@Q�@Qj@Q@Q�@Q>@Qn@Q{@Q0@Q�@Q�@Q
:@QH@Q"@Q�@Q�@Q%@QM@QH@Q@Q3@Q�@Q�@Q(@Q�@Q�@Q�@Q#@QC@Q�@Q�@QE@Q�@Qp@Q@QF@Q�@Q"Z@Q&�@Q5�@Q>�@QA�@QB�@QB@QB�@QB�@QBp@��w@��"@���@��@��O@���@��V@��d@���@���@���@���@���@���@���@���@���@��@��V@��)@���@��@��*@��>@���@��U@���@���@���@���@��&@��^@���@��@��@��@��@���@���@��!@���@��@���@���@���@��m@���@���@��2@��@���@��1@���@��n@��M@���@���@��p@��@��*@��@��@���@���@��1@���@��1@��n@���@��r@���@���@��@��+@��e@��>@���@���@��%@���@���@��7@��T@���@��J@��h@���@���@���@��>@��f@���@���@���@�� @��l@���@���@���@��#@���@���@��n@��n@��q@��@���@���@��O@���@���@��*@���@��h@���@��V@���@��V@���@���@��@���@���@���@���@���@��@���@���@���@���@��@���@���@��@��q@��"@���@���@���@��p@���@��*@���@��@��b@��@���@��6@��@�:@�~%@�}n@�y�@�x�@�xD@�x@�x@�w�@�w�@�w�@�w�@�w�@�w�@�w�@�w�@�v�@�wH@�w@�v�@�w@�v�@�v�@�v�@�vv@�vJ@�v3@�u�@�u�@�u�@�t�@�t�@�u@�uz@�uv@�uN@�u<@�u<@�uN@�u>@�uz@�uN@�uR@�uw@�u:@�t�@�t-@�s�@�s�@�sZ@�sZ@�sE@�s^@�s�@�s�@�s�@Q*�@Q)
@Q,�@Q+.@Q/�@Q:�@QXc@Q`�@Qtk@Q�@Q��@Q�"@Q�@Q�
@Q��@Q��@Q��@Q��@Q��@Q��@Q��@Q�"@Q��@Q�3@Q��@Q�@Q�H@Q��@Q6@Q@QxU@Qm@Qe~@Qe*@Q`�@Q]�@Q\@Q\ @Q]#@Q[@QY6@QX�@QX@QX@QXf@QY
@QZ]@QZ�@QY�@QY�@QYc@QY�@QX�@QX�@QW�@QW@QV@QU�@QT�@QRz@QR~@QN�@QKH@QA�@Q6P@Q*@Q':@Q#�@Q�@Qr@QB@Q�@Q�@Qp@Q�@Qj@Q@Q�@Q>@Qn@Q{@Q0@Q�@Q�@Q
:@QH@Q"@Q�@Q�@Q%@QM@QH@Q@Q3@Q�@Q�@Q(@Q�@Q�@Q�@Q#@QC@Q�@Q�@QE@Q�@Qp@Q@QF@Q�@Q"Z@Q&�@Q5�@Q>�@QA�@QB�@QB@QB�@QB�@QBpG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         43444343444443344334434433344434444334434344444433433343444443334444444443343444444434434444444444444444343344443443443344333444344444444344334334443443344344444333444344443444444433344333344333334433344333434333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��9�r9��9�9��9�B9��9��9�$9��9�#9�89�(9�69�I9��9�59��9��9��9�'9��9��9��9��9��9�^9�^9�N9�.9��9�c9�K9��9�9��9�"9��9�9�h9��9�[9��9��9��9�o9��9�[9�9��9��9�
9��9�p9��9��9��9�q9��9�9��9��9��9��9�
9��9�
9�:9�[9�>9�n9�m9��9��9��9��9�X9�x9��9�C9�f9��9��9��9��9�9�E9�#9�E9��9��9�f9�9�29��9��9��9��9�	9�|9�{9��9�W9�W9�Z9��9�c9� �9� �9� -9��w9���9���9���9� +9���9� q9��9�K9��9�!9��9��I9���9���9���9���9���9��q9��9��&9���9��9��'9��?9���9��A9��~9��9��9��9���9���9��c9���9��9��&9��9��n9���9��9���9��:9��p9��9��"9��9���9���9���9���9��9��9��9��9��9��9��Z9��&9��	9��&9���9���9���9��9��9��9��9��9���9��h9��x9��9���9���9���9��9��9���9��9���9���9���9���9��9��W9���9��9��[9��=9��=9��,9��@9��^9��n9��`9%�	9%��9%ś9%ď9%�R9%��9%�b9%��9%��9&�9&�9&�9&�9&�9&N9&f9&�9&g9&�9&Y9&�9&9&�9&�9&y9&+9&�9&
9&'9&9&�9%��9%��9%�9%��9%��9%�A9%�?9%�&9%�y9%�
9%�9%�9%�9%�e9%��9%��9%�9%�9%�9%�-9%�r9%��9%�9%��9%�Z9%�9%�N9%�9%�9%�9%�9%� 9%�w9%�a9%ã9%�m9%��9%��9%�9%��9%�"9%�X9%�9%�X9%��9%�e9%��9%��9%�&9%��9%��9%�F9%��9%�q9%��9%�39%��9%��9%�59%� 9%��9%��9%�9%��9%�.9%�Y9%�69%��9%�u9%�9%��9%�X9%�!9%��9%��9%��9%��9%��9%�9%��9%��9%��9%�B9%֜9%�A9%ּ9%�9%�?9%��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBJB+B  B��B��B��B��B��B��B��B��B��B��B�B��B�BJB��B�B��B+BVB"�BC�Be`Bw�By�Bn�Bp�B�1B�bB��B�bB�oB��B�JB�7B��B�uB~�BffBdZBm�B�DB��B�yB��BBW
BaHB>wB'�BPB��B�B2-B/B8RBN�BL�B;dB#�B�`B��B�HB�B�jBq�B^5BI�B7LB%�BVB��B��BĜB��B�\B�Bs�B[#B+B1B  B
�B
��B
��B
��B
�JB
{�B
l�B
ZB
L�B
?}B
,B
uB
  B	�B	��B	�-B	��B	�JB	s�B	T�B	2-B	B�ZB��B�XB�B��B��B�{B�VB�JB�=B�7B�%B�B� B~�Bx�Bw�Bv�Bu�Bs�Br�Bo�Bn�Bm�By�B�B{�Br�BiyBaHB^5B^5B^5B_;B]/B[#BZBYBXBVBXBXBXBW
BS�BQ�BM�BH�BI�BJ�BW
BZB[#B[#B\)B\)B\)B]/BYBXBXBYB^5Be`BffBbNB^5BYBS�BP�BL�BJ�BG�BB�B<jB=qB@�B@�BD�BF�BH�BH�BH�BH�BJ�BL�BL�BL�BR�BVBYBZBZB[#B[#B^5BaHBbNBdZBe`BhsBjBiyBiyBo�Bq�Br�Br�Bw�B|�B~�B� B� B�B�B�B�B�B�B�B�B�B�B�%B�+B�DB�uB��B��B��B��B��B��B��B��B��B�B�-B�B��B��B��B�hB�1B�B�B�+B�B|�B~�B�B�B�%B�7B�\B�oB�{B��B��B��B��B��B��B�B�'B�?B�LB�RB�LB�RB�^B�jB��B��B��B��B��B��B��B��B�B�#B�)B�/B�5B�5B�BB�BB�ZB�B�B�B�B��B�B��B��B��B	  B	B	DB	hB	�B	�B	�B	�B	�B	�B	'�B	1'B	33B	.B	+B	+B	+B	+B	.B	/B	0!B	1'B	2-B	49B	5?B	7LB	;dB	=qB	?}B	B�B	G�B	I�B	K�B	N�B	S�B	VB	YB	[#B	]/B	_;B	aHB	e`B	hsB	iyB	iyB	hsB	l�B	p�B	q�B	s�B	x�B	z�B	}�B	�B	�B	�%B	�+B	�%B	�+B	�B	�+B	�=B	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�!B	�B	�B	�-B	�3B	�9B	�?B	�FB	�FB	�RB	�dB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�)B	�#B	�/B	�;B	�;B	�;B	�;B	�5B	�#B	�/B	�HB	�;B	�5B	�5B	�5B	�/B	�)B	�)B	�)B	�#B	�#B	�/B	�mB	�yB	�sB	�mB	�ZB	�ZB	�fB	�B	�B	�$B
�B
&B
=B
&�B
+B
3�B
9�B
A B
J�B
N<B
U�B
\]B
a-B
d�B
i�B
nB
s�B
x8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���B�ZA��>��1?:�6B�r?]SB�>���?22A{�#>��?-��A�E�B�%?K?uAl�[B�~B�3? 7�?���B/!AH	J?K�=B��B�B9�>�9�?!�>AD�BE�AE�?/j?;�A*�B�]B��?2��?�`�B�JAc��Aތ>���?`�>���>݅{?��@"B�B��?=�0B
��BާBF[?,��A�t�>��_?aA?�Pn?$�r?B��A��zB��B�t>��'?�ם@KO�>�4�?���?���@��?){SA_�8B�B�?4�A��??9�<?��?��S@�y�?��?r;6?�0QB��@H��?F|(B�>�ֲ>�� ?"?�8�>��~?�.^? �Y@�A��>��?�V.>��|>�G�@L؀?�@�;CB�?�7PB�@B z>�r�>�3�?�rAIp�B>��?�B��?;Z�Af�@B��B�?1�*?�3�B�AB�iB�>��>��?E�fB�4@y�A@�?	�>���AM��>�E_?	3�?�QB�k@�]�?	}A��!B�?rjB�B��?4�9A��@�wB�f?Kw?`_�B��B�?�IV?XɸB}�A��2?`�?0��?4D�A>��B�B��B9�?�D?8��?�^�B12?/�A�	�>�G�@z�4B=A���?�@�ǛA�zS?���A}ѳ?L�mB�JB�MB�?��?(9�A�iEB�B�B�,?jM?�zB�ZB�-B�
B�BC,?H�-?�KB�B�B��@E|?��B�8B�B��@��B��A&%�BLB�3B�$B�B�@�eA�B��B�B�B�)B��A�T�B�B�GB�YB�dB�B�?B�:B�4B��B�xB�B�B��B�B�B�B�eB��B�B�CB�B�"B�UB�*B�B�}B�cB�qB�xB�BB�eB�_B�B�`B�B�B�uB�cB�?B��B�'B�B�oB�qB��B�B�'B��B��B�B�B�#B��B�GB�B�B�MB��A��UB�B��B�B��B�`B�:B�B�B��B�?B��B�B�[B�?B�B��B�GB�B�B�B��B�B�B�B�qB��B�[B�B�B��BWB�1B�;B�B��B�9B��B�)B�B�B�NB�~B�B��B�rB�B�SB��B�UB��B�cB�B�]B�GB��B�B��B�RB�B�B�IB�2B�fB�DB��B�UB��B��B�GB�*B�9B�B��B�"B��B��B�B��B�B�B�cB��B�B�B�wB�B�aB�B�AB�B�,B�B�B�B�B�B�fB��B�B�mB��B��B��B�B�GB�qB�uB��B�3B��B�AB�B�wB�B�1B�mB�B�kB�)B��B�B�B�rB��B�KB�B��B�B�dB�zB��B�[B�B�B�B��B�B�DB��B�kB�B��B��B��B�B�bB�}B�wB�-B�B��B�B�]B��B�cB��B�cB�}B�*B�B�oB�VB�B�B�@B�uB�uB�1B�B�xB��B�B�>B��B�B�MB��B�B�B�&B�_B��B�B�PB��A��)B�B�B��B�B�B�"B�B�B��B�oB�B�B�B�}B��B�dB�B�IB�B�*B��B�fB�^B�^B��B��B��B��B��B��B� B�B�$B�tB�B�>B�>B�B�JB�B�OB�B�&B��B�B��B��B��B�B�|B�B��B�B�B�NB�|B�]B�B�MB�B��B�B��B�B�uB�B�B�mB�B�\B�{B�B�B��B�B��B��B�tB�B�+B�B�mB�B�B�TB�iB�B��B�B�B��B�XB� B��B�B�B�B�BB��B�gB�\B��B�KB�dB�B��B��B�^B�B�B�B�HB�@B�B�B�B��B�,B�B��B�B�B��B�JB�B�:B�)B�!B�OB�GB� B�.B�qB��B��B��B�*B�B�B�HB�B�>B�B�;B�wB�B�B��B��B�B�B�B�fB�^B��B�B�B�B�B�B��B�B�B��B�B�B�B�fB�zB�UB��B�+B�B�*B�BB��B�lB�B�B�nB�+B�"B�~B��B�B�MB�B� B�B��B�GB��B�B�B�GB��B�<B�9B�B�B�B�B�B�B�B�WB�B��B�uB�B�B�B��B�WB�B�GB�B�B�B�B�B�B��B��B�B�FB�'B�B�B�B��B��B�B�B�B�B�B�OB�B�uB�eB�B��B�B�B�B�AB�8B�B�B�AB�B�B�vB	�GB	�rB	�_B	�TB	�NB	��B	ǢB	ˏB	�bB	ØB	�lB	��B	�|B	��B	��B	�+B	��B	��B	��B	�B	òB	�CB	�	B	�B	�KB	��B	��B	�|B	�B	��B	��B	��B	��B	�TB	��B	�#B	�EB	��B	��B	�B	�`B	�DB	�*B	�B	�MB	�`B	�rB	��B	��B	��B	��B	�B	�8B	�B	��B	�BB	�|B	��B	��B	��B	��B	��B	�"B	�7B	��B	��B	�vB	�OB	�TB	��B	��B	�.B	�B	��B	�7B	�aB	�xB	��B	��B	��B	�dB	�JB	��B	��B	��B	��B	�,B	�_B	��B	¶B	B	ÎB	��B	�YB	�bB	��B	� B	��B	�B	�:B	��B	��B	�uB	�IB	��B	��B	�vB	�3B	�B	ˏB	ʑB	�OB	�QB	��B	ǶB	�]B	ȬB	�DB	�(B	�BXBB�B[B|B�BlB�B"BBHBB7BBBMB�B)B�BB�B�B�B�B�BwB�ByBpBUBB�BBzB�B�B	B�B�B8B9BBLB�B�B�B�BJB�BkBB�BRB�B^B�BB�B�B�B�B�B�BdBoB�BVB�B�B�B�BB�B"BcB�B'BB0BB�B�BABB�B+BB�B�B�B�B�B�BB�B�B7B�B1B�B�B�B�B�B�B[BGB0BRB�BB
B$BB�B�BWBBB\BB�BXB�BB�B�B3B�B�BB�BeB�BjBBRB�BB�B�B`B#B�B�B@B7B
BEB�B*B�BJB^B9B�B;B�B�B�B{B�BB BB�B�B'B6B�B�BB�B�BuB3B�B�B.B�B�B�BB,B�B|BMB*B!B-B�BBB�B"B�B-BhBBBPBHB-B/BNB#BB	ݿB	ݭB	�B	�B	�RB	܏B	�KB	��B	�B	�B	��B	�MB	��B	�B	��B	��B	�B	�FB	�B	�B	�kB	��B	��B	��B	�B	�bB	�^B	�B	��B	��B	�4B	�QB	��B	�B	�B	�B	�-B	�B	��B	�QB	�B	�zB	�B	�B	�7B	�B	�pB	�B	��B	��B	�uB	�B	�B	�B	�B	�B	��B	�B	��B	�B	�B	�UB	��B	�bB	�RB	��B	��B	�AB	�(B	�gB	��B	�6B	�pB	�6B	�YB	�B	�$B	��B	�*B	��B	�mB	�B	�<B	��B	�B	�*B	�B	�B	�HB	�yB	�2B	��B	�UB	�gB	��B	�TB	�LB	��B	�	B	�jB	�B	�B	�B	�[B	��B	�B	��B	�B	�B	��B	�B	�B	�7B	�B	�B	�B	��B	��B	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943444343444443344334434433344434444334434344444433433343444443334444444443343444444434434444444444444444343344443443443344333444344444444344334334443443344344444333444344443444444433344333344333334433344333434333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB\B1BB��B��B��B��B��B��B��B��B��B��B��B�B��B�B/B��B�B��BB;B"�BC|BeEBw�By�BnBp�B�B�HB�hB�IB�WB�{B�3B�B�}B�\B~�BfKBdCBmxB�(B��B�^B��B�BV�Ba-B>_B'�B7B��BlB2B/B86BN�BL�B;IB#�B�GBˬB�-B��B�RBq�B^BI�B70B%�B:B��B��B�B��B�BB�Bs�B[B*�BB
��B
�B
��B
��B
��B
�+B
{�B
lnB
Z B
L�B
?bB
+�B
VB	��B	�iB	̭B	�B	��B	�.B	s�B	T�B	2B	B�:BκB�8B��B��B�{B�YB�6B�)B�B�B�B��B�B~�Bx�Bw�Bv�Bu�Bs�Br�Bo}BnwBmqBy�B��B{�Br�BiZBa'B^B^B^B_B]B[BY�BX�BW�BU�BW�BW�BW�BV�BS�BQ�BM�BH�BI�BJ�BV�BY�BZ�BZ�B\B\B\B]BX�BW�BW�BX�B^Be=BfCBb,B^BX�BS�BP�BL�BJ�BG�BBlB<FB=OB@bB@_BDwBF�BH�BH�BH�BH�BJ�BL�BL�BL�BR�BU�BX�BY�BY�B[ B[B^Ba%Bb*Bd8Be<BhQBj\BiTBiVBo{Bq�Br�Br�Bw�B|�B~�B�B�B��B��B��B��B��B��B��B��B��B��B� B�B�B�PB�ZB�hB�{B��B��B��B��B��B��B��B�B��B��B��B�|B�BB�B��B��B�B��B|�B~�B��B��B��B�B�6B�JB�TB�yB��B��B��B��B��B��B�B�B�'B�-B�'B�,B�9B�EB�]B�cBˢBͭBδBγB��B��B��B��B�B�B�B�B�B�B�3B�YB�eB�~B�B��B�B��B��B��B��B	�B	B	BB	{B	�B	�B	{B	zB	�B	'�B	1B	3B	-�B	*�B	*�B	*�B	*�B	-�B	.�B	/�B	1B	2B	4B	5B	7'B	;>B	=KB	?WB	BjB	G�B	I�B	K�B	N�B	S�B	U�B	X�B	Z�B	]B	_B	a$B	e<B	hLB	iRB	iRB	hKB	leB	p~B	q�B	s�B	x�B	z�B	}�B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�+B	�5B	�<B	�HB	�VB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�/B	�=B	�KB	�RB	�QB	�VB	�UB	�]B	�eB	�bB	�gB	�wB	�{B	�|B	�~B	ƂB	ȌB	ʛB	ͫB	ͬB	̧B	ϺB	ϷB	ϸB	оB	пB	ϸB	ϸB	��B	нB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�
B	�B	�B	�B	�B	�B	��B	�B	�!B	�B	�B	�B	�B	�	B	�B	�B	�B	��B	��B	�B	�EB	�SB	�KB	�GB	�3B	�5B	�@G�O�B	�wB	� B
�B
 B
B
&�B
*�B
3�B
9B
@�B
J�B
NB
U�B
\8B
aB
d�B
i�B
m�B
sZB
xG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BG�O�G�O�G�O�B�ZG�O�B��G�O�G�O�G�O�G�O�G�O�A�EjB�G�O�G�O�B�gB�G�O�G�O�B/G�O�G�O�B۫B��B9xG�O�G�O�G�O�BE�G�O�G�O�G�O�G�O�B�GB��G�O�G�O�B�2G�O�Aދ�G�O�G�O�G�O�G�O�G�O�G�O�B�B��G�O�B
��BތBFDG�O�A�t�G�O�G�O�G�O�G�O�G�O�A��XB��B�]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B��G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�B�(B cG�O�G�O�G�O�G�O�B G�O�G�O�B��G�O�G�O�B��B�xG�O�G�O�B�+B�SB�G�O�G�O�G�O�B� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�SG�O�G�O�A���B��G�O�B�mBؿG�O�G�O�G�O�B�PG�O�G�O�B�B�G�O�G�O�B}�G�O�G�O�G�O�G�O�G�O�B�B��B9�G�O�G�O�G�O�B1G�O�G�O�G�O�G�O�B=G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�5B�6B��G�O�G�O�A�iB��B�B�G�O�G�O�B�EB�B��B�|BCG�O�G�O�B�yB�pB��G�O�G�O�B�B�B��G�O�B��G�O�BL B�B�
B�B�|G�O�A��B�B�pB�B�B��G�O�B��B�-B�BB�KB�B�(B� B�B�B�_B�eB�B��B�B�lB��B�MB�B�eB�)B�~B�
B�=B�B��B�fB�JB�[B�_B�)B�LB�HB�B�IB�B��B�]B�JB�(B��B�B� B�XB�[B�B�B�B��B��B�B�B�
B��B�1B�B�gB�7B��A��8B��B��B�B��B�HB�"B�B� B��B�(B��B�B�AB�(B�B��B�.B�B��B�B�B�B�B�B�XB��B�AB��B��B�B>B�B� B�B�B�"B�B�B�B�B�8B�fB��B��B�[B�B�;B�B�=B��B�JB�B�EB�.B��B�wB��B�=B�B�B�1B�B�NB�/B�B�=B��B��B�.B�B� B�B��B�
B�B�B�B��B� B�	B�JB��B��B�B�^B�B�GB�B�*B�B�B��B�B�B�B�|B�QB�B��B�UB��B��B�B�B�.B�XB�[B�B�B��B�*B�B�^B�uB�B�VB�B�TB�B��B�B�B�[B��B�4B�B�B��B�LB�eB��B�AB�|B��B��B�B�|B�-B�B�SB��B�B�B�B�B�KB�fB�^B�B��B��B�B�EB��B�JB��B�JB�fB�B�B�XB�=B��B�~B�'B�[B�[B�B�B�bB��B�B�'B��B�B�4B��B��B��B�B�FB�B�B�:B��A��B��B�lB��B�B��B�B�B�B��B�WB�}B�pB�B�eB��B�MB�B�-B�fB�B��B�OB�HB�HB��B�B�B�B�B�B�
B�uB�B�^B�B�%B�%B�B�4B��B�6B�B�B��B�B��B�B��BAB�B�BCBbB�BUB�B	B�B/BB"B-B6B�BB�B�B�B�BqB�B�B^B�BbBXBABB�B�BdB�B�B�B}BkB"B"B�B8B�B�B�B�B1B�BUBBtB:BvBEB�B�BiB�B�B�B�BBLBXB�BABtB�B�B�B�B�BBLBtBB�BB�B�B�B(B�BtBB�B�B�B�B{B�BlBB�B�B"B�BB�BnB�B�B�B�BEB1BB:B�BB�BB�B�B�BCBBBDB�BkB?BkB�B�B�BBnB�B�B�BLB�BUB�B:B�B�BkBvBGBB�B�B)B!B�B,B�BBkB0BGB$B�B$B�B�BBdBmB�BB�B�B�BB B�B�B�B�BxB`BB�B�BB�B�B�B�BBmBbB5BBBB�B�B�B�BB�BBRB�BgB9B0BBB7BB�B	ݙB	݇B	��B	��B	�-B	�hB	�#B	��B	�{B	�cB	��B	�)B	��B	�gB	�B	��B	�{B	� B	�vB	�B	�CB	�B	��B	��B	�nB	�:B	�7B	�mB	��B	�B	�B	�,B	��B	�B	�]B	�gB	�	B	��B	�B	�,B	��B	�WB	��B	��B	�B	�_B	�KB	�ZB	��B	�B	�QB	�uB	��B	�B	��B	�wB	�B	�eB	�B	��B	��B	�.B	��B	�8B	�+B	�B	�B	�B	�B	�@B	��B	�B	�JB	�B	�3B	�B	��B	�B	�B	�B	�GB	�B	�B	�B	�^B	�B	�uB	��B	�!B	�SB	�B	�B	�/B	�@B	�B	�-B	�'B	�B	��B	�DB	�B	��B	�B	�6B	�B	��B	��B	�zB	�B	�B	�B	�B	�B	�{B	�lB	��B	�B	�B	��B	�yBAB�B�BCBbB�BUB�B	B�B/BB"B-B6B�BB�B�B�B�BqB�B�B^B�BbBXBABB�B�BdB�B�B�B}BkB"B"B�B8B�B�B�B�B1B�BUBBtB:BvBEB�B�BiB�B�B�B�BBLBXB�BABtB�B�B�B�B�BBLBtBB�BB�B�B�B(B�BtBB�B�B�B�B{B�BlBB�B�B"B�BB�BnB�B�B�B�BEB1BB:B�BB�BB�B�B�BCBBBDB�BkB?BkB�B�B�BBnB�B�B�BLB�BUB�B:B�B�BkBvBGBB�B�B)B!B�B,B�BBkB0BGB$B�B$B�B�BBdBmB�BB�B�B�BB B�B�B�B�BxB`BB�B�BB�B�B�B�BBmBbB5BBBB�B�B�B�BB�BBRB�BgB9B0BBB7BB�B	ݙB	݇B	��B	��B	�-B	�hB	�#B	��B	�{B	�cB	��B	�)B	��B	�gB	�B	��B	�{B	� B	�vB	�B	�CB	�B	��B	��B	�nB	�:B	�7B	�mB	��B	�B	�B	�,B	��B	�B	�]B	�gB	�	B	��B	�B	�,B	��B	�WB	��B	��B	�B	�_B	�KB	�ZB	��B	�B	�QB	�uB	��B	�B	��B	�wB	�B	�eB	�B	��B	��B	�.B	��B	�8B	�+B	�B	�B	�B	�B	�@B	��B	�B	�JB	�B	�3B	�B	��B	�B	�B	�B	�GB	�B	�B	�B	�^B	�B	�uB	��B	�!B	�SB	�B	�B	�/B	�@B	�B	�-B	�'B	�B	��B	�DB	�B	��B	�B	�6B	�B	��B	��B	�zB	�B	�B	�B	�B	�B	�{B	�lB	��B	�B	�B	��B	�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943444343444443344334434433344434444334434344444433433343444443334444444443343444444434434444444444444444343344443443443344333444344444444344334334443443344344444333444344443444444433344333344333334433344333434333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649342020083116493420200831164934202008311649342020083116493420200831164934202008311649342020083116493420200831164934202008311649342020083116493420200831164934AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817192019021918171920190219181719    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817192019021918171920190219181719  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817192019021918171920190219181719  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649342020083116493420200831164934  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                