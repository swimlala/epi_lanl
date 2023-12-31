CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  I   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:18Z creation      
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181718  20200831164932  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�a�h|n3@�a�h|n3@�a�h|n3111 @�a���B@�a���B@�a���B@6E�Q�@6E�Q�@6E�Q��cwl�C���cwl�C���cwl�C��111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D� D�K3D��D��
D��
D�=D���D�ÅD�D�@ D���DǺ=D��D�Q�Dڅ�D໅D� �D�J=D�u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���������    �����������������L�;������������L�;������������������;������������L�;������������L�ͽ��;������������������������L�;����������;��������������ͽ��;��������������������L�;L�;����L�;����������������L�;����������������������������L�;������;��������L�;��������������������������������L�;������������L�;��������L�ͽ��;����������������L�;����������ͽ��;����������;L�;������������L�ͽ��;��������L�;������������������������������������������������������ͽ��;L�;����������������������������L�;������������L�;L�;��������L�;��������L�;������������L�;L�;����L�;L�;��������L�;������������������������������������L�;L�;��������L�;L�ͽ��;������������L�ͽ��ͽ��;������������L�ͽ��ͽ��;����L�;����L�;����������������L�ͽ��;L�;����L�;��������������ͽ��;������������L�;L�;L�ͽ��;������;L�;������;L�;L�ͽ��ͽ��ͽ��;L�;L�;������ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L��    �L�;L��    ����    ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���        =���    ���ͽ��;L��    ���ͽ���    ���ͽ��ͽ��ͽ��ͽ���        =��ͽ��ͽ��ͽ��ͽ��ͽ���                ���ͽ��ͽ���            ���ͽ���    �L�ͽ��ͽ��ͽ���    ���ͽ��;L�ͽ��ͽ���        ���;L�;L�ͽ��ͽ���                        �L�;L�;L�ͽ��ͽ��ͽ��ͽ���    ����    ���ͽ��ͽ��ͽ��ͽ���        �L�ͽ���        ���ͽ���        =��ͽ��ͽ���    �L�ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ���                ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���            ���ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ��;L��=��;L�ͽ���            ���;L��=��ͽ��ͽ���        ���ͽ��ͽ���                    �L�ͽ��ͽ��ͽ���        ���ͽ��ͽ��ͽ���            ���ͽ��;L�;L�ͽ��ͽ���    ���ͽ���        ���ͽ���    ����    �L�ͽ���    ���;L��    ���;L�ͽ��ͽ��ͽ��ͽ��;L�ͽ���    �L��    ���ͽ��ͽ���            ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���        �L�ͽ���    ����=��ͽ��ͽ���            ���ͽ��ͽ��ͽ��;L�ͽ���    ���ͽ��ͽ��ͽ��ͽ���    ���ͽ���        ����    ���ͽ��ͽ��;L�ͽ��ͽ��;L�ͽ��;L�ͽ���        =���>L��>���>���>���>���?   ?��?333?L��?fff?fff?�  ?�  ?���?���?���?�ff?�33?�  ?�  ?���?ٙ�?ٙ�?�ff?�33?�33@   @ff@��@33@33@��@   @&ff@,��@333@9��@@  @Fff@S33@Y��@`  @fff@l��@y��@�  @�  @�33@���@���@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�33@���@���@�  @�ff@ə�@���@�  @�33@�ff@���@�  @�33@陚@���@�  @�33@�ff@���A   A��A33A��A  A	��A33A��AffA��A33A��AffA��A33A��AffA   A#33A$��A&ffA(  A)��A,��A.ffA0  A1��A4��A6ffA8  A9��A;33A>ffA@  AA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��As33AvffAx  Ay��A|��A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  Ař�A�ffA�33A�  Aə�A�ffA�33A���A͙�A�ffA�  A���Aљ�A�ffA�33A���Aՙ�A�ffA�33A�  A���A�ffA�33A�  Dq9�Dq@ DqFfDqL�DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3DqٚDq� Dq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�DrffDrl�Drs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Dt��Du  @&ff@,��@333@9��@@  @Fff@S33@Y��@`  @fff@l��@y��@�  @�  @�33@���@���@�  @�33@�ff@���@�  @�33@�ff@���@���@�  @�33@���@���@�  @�ff@ə�@���@�  @�33@�ff@���@�  @�33@陚@���@�  @�33@�ff@���A   A��A33A��A  A	��A33A��AffA��A33A��AffA��A33A��AffA   A#33A$��A&ffA(  A)��A,��A.ffA0  A1��A4��A6ffA8  A9��A;33A>ffA@  AA��AC33AD��AH  AI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Ak33AnffAp  Aq��As33AvffAx  Ay��A|��A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  Ař�A�ffA�33A�  Aə�A�ffA�33A���A͙�A�ffA�  A���Aљ�A�ffA�33A���Aՙ�A�ffA�33A�  A���A�ffA�33A�  Dq9�Dq@ DqFfDqL�DqY�Dq` DqffDqs3Dqy�Dq� Dq�fDq�3Dq��Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3DqٚDq� Dq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�DrffDrl�Drs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsFfDsL�DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Dt��Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @G
=@��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBX�Ba=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4=D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D��D�UD��D���D� �D�GD���D��qD� D�I�D���D��)D��D�[�Dڏ�D��qD�
�D�T)D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#��<#��>��R<#��<#��<#��<#��=�G�<#��<#��<#��=�G�<#��<#��<#��<#��>W
=<#��<#��<#��=�G�<#��<#��<#��=�G�>W
=<#��<#��<#��<#��<#��<#��=�G�<#��<#��>W
=<#��<#��<#��>W
=>W
=<#��<#��<#��<#��<#��=�G�=�G�<#��=�G�<#��<#��<#��<#��=�G�<#��<#��<#��<#��<#��<#��<#��=�G�<#��>W
=<#��<#��=�G�<#��<#��<#��<#��<#��<#��<#��<#��=�G�<#��<#��<#��=�G�<#��<#��=�G�>W
=<#��<#��<#��<#��=�G�<#��<#��>W
=>W
=<#��<#��>W
==�G�<#��<#��<#��=�G�>W
=<#��<#��=�G�<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��<#��>W
=>W
==�G�<#��<#��<#��<#��<#��<#��<#��=�G�<#��<#��<#��=�G�=�G�<#��<#��=�G�<#��<#��=�G�<#��<#��<#��=�G�=�G�<#��=�G�=�G�<#��<#��=�G�<#��<#��<#��<#��<#��<#��<#��<#��<#��=�G�=�G�<#��<#��=�G�=�G�>W
=<#��<#��<#��=�G�>W
=>W
=<#��<#��<#��=�G�>W
=>W
=<#��=�G�<#��=�G�<#��<#��<#��<#��=�G�>W
==�G�<#��=�G�<#��<#��<#��>W
=>W
=<#��<#��<#��=�G�=�G�=�G�>W
=<#��>W
==�G�<#��>W
==�G�=�G�>W
=>W
=>W
==�G�=�G�<#��>W
=>W
=>W
==�G�>W
=>W
=>W
=>W
=>W
=>W
==�G�>��R=�G�=�G�>��R>W
=>��R>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>��R>��>��R>W
=>W
==�G�>��R>W
=>W
=>��R>W
=>W
=>W
=>W
=>W
=>��R>��R>��>W
=>W
=>W
=>W
=>W
=>��R>��R>��R>��R>W
=>W
=>W
=>��R>��R>��R>W
=>W
=>��R=�G�>W
=>W
=>W
=>��R>W
=>W
==�G�>W
=>W
=>��R>��R>W
==�G�=�G�>W
=>W
=>��R>��R>��R>��R>��R>��R=�G�=�G�=�G�>W
=>W
=>W
=>W
=>��R>W
=>��R>W
=>W
=>W
=>W
=>W
=>��R>��R=�G�>W
=>��R>��R>W
=>W
=>��R>��R>��>W
=>W
=>��R=�G�>W
==�G�>W
=>W
=>W
=>W
=>W
=>��R>��R>��R>��R>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>��R>��R>W
=>W
=>W
=>W
=>W
=>��R>W
=>W
=>W
==�G�>��=�G�>W
=>��R>��R>��R>W
==�G�>��>W
=>W
=>��R>��R>W
=>W
=>W
=>��R>��R>��R>��R>��R=�G�>W
=>W
=>W
=>��R>��R>W
=>W
=>W
=>W
=>��R>��R>��R>W
=>W
==�G�=�G�>W
=>W
=>��R>W
=>W
=>��R>��R>W
=>W
=>��R>W
=>��R=�G�>W
=>��R>W
==�G�>��R>W
==�G�>W
=>W
=>W
=>W
==�G�>W
=>��R=�G�>��R>W
=>W
=>W
=>��R>��R>��R>W
=>W
=>W
=>W
=>W
=>W
=>W
=>W
=>��R>��R=�G�>W
=>��R>W
=>��>W
=>W
=>��R>��R>��R>W
=>W
=>W
=>W
==�G�>W
=>��R>W
=>W
=>W
=>W
=>W
=>��R>W
=>W
=>��R>��R>W
=>��R>W
=>W
=>W
==�G�>W
=>W
==�G�>W
==�G�>W
=>��R>��R>��?�\?(�?(�?5?5?O\)?h��?�G�?�{?��G?��G?��?��?�z�?�G�?�G�?�z?��G?�?�?�z�@ ��@ ��@
=@p�@p�@�
@=p@ ��@'
=@'
=@-p�@3�
@:=p@@��@G
=@Mp�@S�
@Z=p@g
=@mp�@s�
@z=p@�Q�@��R@��@��@��@��@��R@��@��@�Q�@��@��@��@�Q�@��@��R@��@��@Å@ƸR@��@�Q�@Ӆ@ָR@��@��@�Q�@�R@��@��@�@��R@��@��A (�A\)A��A�]A(�A	A��A�]A(�AA\)A�]A(�AA\)A�]A (�A!A#\)A$��A((�A)A+\)A,��A.�]A1A3\)A4��A6�]A9A;\)A<��A>�]A@(�AC\)AD��AF�]AH(�AIAL��AN�]AP(�AQAT��AV�]AX(�AYA\��A^�]A`(�AaAd��Af�]Ah(�AiAl��An�]Ap(�As\)At��Av�]Ax(�A{\)A|��A~�]A��HA��A�z�A�G�A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GA�z�A�G�A�{A��A�z�A�G�A��GA��A�z�A�{A��GA��A�G�A�{A��A�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A�{A��A�z�A�G�A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A��GAŮA�z�A�{A��GAɮA�z�A�{A��GAͮA�G�A�{A��GA�z�A�G�A�{A��GAծA�G�A�{A��GAٮA�z�A�G�A��GAݮA�z�DqMqDqS�DqZ=Dq`�DqmqDqs�Dqz=Dq�
Dq�qDq��Dq�=Dq�
Dq�qDq��Dq�=Dq�
Dq�qDq��Dq�Dq�
Dq�qDq��Dr �Dr
DrqDr�Dr �Dr'
Dr-qDr:=Dr@�DrG
DrMqDrZ=Dr`�Drg
DrmqDrz=Dr��Dr�
Dr�qDr�=Dr��Dr�
Dr��Dr�=Dr��Dr�
Dr��Dr�=Dr�Dr�
Dr��Dr�=Ds �DsqDs�Ds=Ds �Ds'
Ds3�Ds:=Ds@�DsMqDsS�DsZ=Ds`�DsmqDss�Dsz=Ds�
Ds�qDs��Ds�=Ds�
Ds�qDs��Ds�=Ds�
Ds�qDs��Ds�Ds�
Ds�qDs��Dt �Dt
DtqDt�Dt �Dt'
Dt-qDt:=Dt@�DtG
DtMqDtZ=Dt`�Dtg
Dts�Dtz=Dt��Dt�qDt��Dt�=Dt��Dt�qDt��Dt�=Dt�
Dt�qDt��Dt�=Dt�
Dt�qDt��Du �Du
DuqDu�@:=p@@��@G
=@Mp�@S�
@Z=p@g
=@mp�@s�
@z=p@�Q�@��R@��@��@��@��@��R@��@��@�Q�@��@��@��@�Q�@��@��R@��@��@Å@ƸR@��@�Q�@Ӆ@ָR@��@��@�Q�@�R@��@��@�@��R@��@��A (�A\)A��A�]A(�A	A��A�]A(�AA\)A�]A(�AA\)A�]A (�A!A#\)A$��A((�A)A+\)A,��A.�]A1A3\)A4��A6�]A9A;\)A<��A>�]A@(�AC\)AD��AF�]AH(�AIAL��AN�]AP(�AQAT��AV�]AX(�AYA\��A^�]A`(�AaAd��Af�]Ah(�AiAl��An�]Ap(�As\)At��Av�]Ax(�A{\)A|��A~�]A��HA��A�z�A�G�A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GA�z�A�G�A�{A��A�z�A�G�A��GA��A�z�A�{A��GA��A�G�A�{A��A�z�A�G�A�{A��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A�{A��A�z�A�G�A��GA��A�G�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A��GAŮA�z�A�{A��GAɮA�z�A�{A��GAͮA�G�A�{A��GA�z�A�G�A�{A��GAծA�G�A�{A��GAٮA�z�A�G�A��GAݮA�z�DqMqDqS�DqZ=Dq`�DqmqDqs�Dqz=Dq�
Dq�qDq��Dq�=Dq�
Dq�qDq��Dq�=Dq�
Dq�qDq��Dq�Dq�
Dq�qDq��Dr �Dr
DrqDr�Dr �Dr'
Dr-qDr:=Dr@�DrG
DrMqDrZ=Dr`�Drg
DrmqDrz=Dr��Dr�
Dr�qDr�=Dr��Dr�
Dr��Dr�=Dr��Dr�
Dr��Dr�=Dr�Dr�
Dr��Dr�=Ds �DsqDs�Ds=Ds �Ds'
Ds3�Ds:=Ds@�DsMqDsS�DsZ=Ds`�DsmqDss�Dsz=Ds�
Ds�qDs��Ds�=Ds�
Ds�qDs��Ds�=Ds�
Ds�qDs��Ds�Ds�
Ds�qDs��Dt �Dt
DtqDt�Dt �Dt'
Dt-qDt:=Dt@�DtG
DtMqDtZ=Dt`�Dtg
Dts�Dtz=Dt��Dt�qDt��Dt�=Dt��Dt�qDt��Dt�=Dt�
Dt�qDt��Dt�=Dt�
Dt�qDt��Du �Du
DuqDu�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˙�A˧�A˥�AˬAˬA˩�AˬAˮAˋDA�^5A�Q�A�%A��A��mA��#A���A���A�ƨA�ĜA���Aʴ9AʮAʰ!Aʥ�Aʗ�A��A�ȴA�  A��DA��A�;dA�I�A�JA��A�r�A���A���A�v�A�(�A��jA�S�A��\A�l�A�ƨA�ZA�ZA�(�A�$�A��`A�t�A��;A�(�A��A��7A�7LA��9A�JA��A�x�A�t�A�ZA��A��+A�9XA�^5A�t�A�x�A���A�JA�oA�jA�%A�I�A��`A�O�A�&�A�M�A�=qA���A��wA��A�I�A�1'A� �A���A���A��hA�`BA�;dA��#A���A�S�A��A���A��7A�?}A���A���A��A���A�hsA���A�p�A�dZA�  A��A�v�A��jA���A��A�ȴA�^5A�ȴA��A�jA��A�%A�1'A�$�A��A�VA�l�A�ȴA�%A~n�Ay7LAu�#Arv�Ap��An�9Aj�Ag��Acp�A^�A\�9A[|�AZbNAY�PAX�RAW��AV�AU��AQ�AMVAJ(�AH��AG�hAFjAC|�A@=qA<�RA;�A9C�A8A�A6�A4 �A2�A2�+A1��A1��A2-A2(�A1�#A1�#A1G�A0�A.�RA.9XA-x�A,��A+ƨA*�A(~�A'��A&��A&�+A&bNA%�TA%�PA$�A"��A ��A`BAZA�A�FA"�A��A�A~�A�#A��A=qA��AƨAx�A/A�wA1'Az�AG�A��A�DA�A��A
��A	C�AI�A�A�hA^5AA"�AI�A��Ax�A��A-A�FA`BA�A ��A 9XA @��
@�o@��h@��D@��F@�o@���@��
@��@�&�@�z�@�  @�\)@�{@��`@�F@�R@���@�o@�@�`B@���@�G�@�P@�=q@�@�l�@�+@�I�@� �@�5?@�t�@�=q@���@؛�@�5?@��@�33@��@�|�@���@�n�@�O�@ˍP@�ȴ@���@ț�@�o@�M�@�Ĝ@�(�@î@�"�@�ȴ@���@�@�X@�%@�Ĝ@�1@�\)@���@��\@���@�X@���@�A�@���@�+@��@�=q@��#@��#@��7@���@�|�@�l�@��@�~�@���@�%@�Q�@�1@���@��F@���@�@� �@�
=@��^@���@��D@�z�@�Q�@�  @�ƨ@�\)@��@�o@�o@��\@�v�@�v�@�ff@��@���@��H@�{@�A�@�Ĝ@���@�5?@�+@�j@��@�|�@�dZ@�|�@��@��@���@���@�v�@�5?@��#@���@�$�@���@�&�@��@��
@�bN@�z�@���@��@���@��u@��@�(�@�x�@�-@��@��!@�o@���@�A�@�l�@�?}@�~�@��@��h@�X@�x�@�  @��P@���@�o@�"�@�33@�;d@�"�@�S�@��@���@��#@�z�@��@��@�o@���@��R@�V@��T@�@��7@��@�A�@���@���@�l�@�;d@���@���@���@�^5@�=q@��@��#@���@�x�@�%@�Ĝ@�  @��m@��w@�l�@��y@�n�@�E�@�E�@�E�@�E�@��@��@��^@�x�@�&�@��@���@� �@��@���@���@�;d@�o@��y@��H@��@���@���@�-@��T@��@�V@���@���@�Z@�9X@���@��F@���@��P@�l�@�33@�"�@�@��H@��!@�ff@�M�@�@���@��@��@��@�X@��@��@���@��D@�Q�@�9X@��@�  @��@��@�S�@���@�n�@�M�@��@��T@��-@���@�hs@�?}@��@�F@v�6@m0�@e��@^��@W��@Q��@G��@@Q�@9��@5<6@0��@)�d@$��@�2@J@��@�p@[WG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�jA�$�A��yA�VAüjA�K�A���A��yA�+AǼjA� �A��PA��A�jAȧ�A�ffA�oA�JAĩ�A���A�/Aã�A��A�ffA�1'A�`BA�O�A���A��A�bA��
A��mA�p�A�G�A���A�p�A���A��#A�`BA�JA��A�jA°!A��Aɧ�A�E�A��`A�A��TA��A�z�A�oA�I�A�&�A�~�A�(�A��-A��RAº^Aś�Aʏ\A���AˁA��;A��`A���A��/A�E�A�ZA��A�ȴA�M�AōPA�E�A�&�A�`BA�l�A��A��#A�+A�~�A���A�x�A�(�A���AȓuA�^5A�7LA�\)A�1'A�jA�~�A��AA�A�hsA���A�E�A� �AąAɅA��TA��!A�  A��A�jA��
A�%A�;dA�t�A�ĜA��#Aʙ�A���A��#A�AÁA�K�A�r�A�\)A�p�A�=qA���AȁA�l�A�AǇ+A�l�Aĝ�A�+A��A�ȴA�t�A�l�A�C�A��A�E�A�A��/A�Q�A��wA�I�A�+A�v�A�7LA��A�p�Aũ�A���A�ffA�VA�t�A�7LA��A�G�A�x�A�"�A�A�S�A�9XAȺ^A�1A��AȾwA˃AˁAʬA�jAPAǼjA�~�A�x�A�JA���A���A��A��
AˁA�1'Aũ�A�p�A��Aɣ�A�(�A��A���AÑhAˁA�v�A®Aš�A���A��A�-A�Q�A�x�Aʲ-A��
A��A�t�A�=qA�t�A�A�{Aʉ7A�^5A�VA�z�A�t�A�t�A�r�A�&�A�v�A��AčPA�=qA���AˁA�~�A�x�A�x�A�r�A�x�A�r�A�p�A�t�A�x�A�jA�r�A�n�A�x�A�x�A�z�A�t�A�v�A�x�A�r�A�v�A�p�A�v�A�|�A�r�A�z�A�x�A�n�A�t�A�z�A�v�A�|�AˁA�~�A�t�A�t�A�~�A�r�A�x�A˅A�~�A˃A�~�A�v�A�I�A˅A˃AˁAˉ7Aˉ7A˅A˅A˃AˋDAˉ7AˋDAˍPAˋDA�~�A˃AˁAˉ7A�|�Aˇ+Aˇ+AˋDA˅A�~�AˍPAˍPAˍPAˍPA˃Aˇ+A˃AˋDAˋDAˍPAˏ\A�|�AˍPAˑhAˋDAˏ\A˅AˍPAˋDAˇ+AˋDA�p�A˃A˕�AˑhA˓uA˓uA˕�AˍPAˍPAˏ\AˍPAˉ7A��AˋDAˍPAˋDAˉ7A˅AˑhAˍPAˋDA˅A˅A˃A�bA�t�A�n�A˃A�|�A˃A�~�Aˉ7A˃AˋDAˋDA˃Aˇ+A˃A�~�A�x�A�z�A�~�A˃A�|�Aˇ+AˁA�z�A�p�A�~�A�x�A�r�A˃A�r�A�v�A�|�Aˉ7Aˉ7A�~�A˅AˋDA˅A˅A�|�Aˇ+A˅AˁAˇ+Aˉ7A˅A˃Aˇ+Aˇ+Aˇ+Aˉ7AˋDAˉ7Aˇ+AˋDAˇ+AˋDAˋDAˏ\A˃AˋDAˉ7A˃Aˇ+Aˉ7Aˇ+Aˇ+A˃Aˇ+Aˇ+Aˇ+Aˇ+Aˉ7Aˇ+A�~�A˃AˋDAˇ+A�~�AˋDAˋDAˉ7Aˉ7Aˇ+Aˇ+Aˉ7A�x�Aˇ+Aˇ+Aˇ+AˋDAˍPAˋDAˇ+Aˇ+AˁA˅A�z�AˋDAˋDAˉ7Aˇ+AˍPAˇ+AˍPAˋDAˇ+Aˇ+Aˉ7Aˇ+AˋDAˇ+A�z�AˍPAˉ7A�hsAˋDAˋDAˍPAˉ7AˋDAˍPAˋDAˏ\Aˏ\Aˉ7AˋDAˉ7AˍPAˋDAˍPAˍPAˇ+Aˉ7Aˉ7Aˇ+AˋDAˋDAˍPAˋDAˋDAˋDAˍPAˉ7AˍPAˍPAˇ+A˅A˅AˍPA�-A˅A˅AˋDAˏ\Aˏ\AˍPAˑhA˓uA˕�A˕�A˓uA˓uA˗�A˕�A˙�A˓uA˓uA˓uA˙�A˙�A˙�A˙�A˙�A˛�A˙�A˙�A˗�A˙�A˛�A˛�A˝�A˛�Aˡ�A˗�A˙�A˗�A˗�A˗�A˗�A˕�A˗�A˗�A˙�A˛�A˙�A˛�A˛�Aˣ�A˧�A˩�A˥�A˥�A˩�AˬA˩�A˩�AˬAˬA˩�A˩�AˮAˬA˩�A˥�A˥�Aˡ�Aˣ�Aˣ�Aˣ�A˥�Aˣ�A˥�A˥�A˩�A˥�A˧�A˧�AˬA˰!A˰!AˬAˬAˮAˬAˬAˬA˩�A˧�A˩�A˩�A˩�AˬA˩�A˧�A˩�A˩�A˩�A˩�A˩�AˬAˬAˮA˰!AˮAˮAˮAˬAˬA˩�A˥�A˥�A˧�A˥�A˥�A˧�A˥�A˧�A˧�AˬAˬA˩�A˩�AˮAˬAˬAˬA˩�AˬA˩�A˩�A˩�A˧�A˧�A˩�A˧�A˧�A˩�A˩�A˩�A˩�A˩�AˮAˮAˮA˰!A˰!A˰!AˮA˰!AˮAˮAˮAˮAˮA˧�AˮAˬAˬA˗�A˕�AˑhAˋDAˍPA˕�AˑhAˍPAˋDAˋDA˕�A˓uAˋDA˅A�hsA�`BA�^5A�`BA�`BA�`BA�`BA�bNA�`BA�`BA�ZA�bNA�\)A�VA�S�A�XA�^5A�`BA�^5A�ZA�O�A�K�A�O�A�S�A�M�A�I�A�Q�A�Q�A�S�A�Q�A�/A�5?A�9XA�33A�JA�
=A�
=A�%A�  A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��@��\@��\@��+@��\@��+@��+@�v�@�n�@�n�@�ff@�^5@�ff@�^5@�^5@�V@�V@�V@�M�@�V@�V@�M�@�M�@�M�@�M�@�V@�V@�M�@�E�@�E�@�=q@�=q@�5?@�5?@�5?@�5?@�-@�-@�-@�-@�$�@�$�@�$�@��@�{@�{@��@��@��@��@��@��@��@��@��@��@��@��T@��@��T@��T@��T@���@��-@���@���@���@���@���@���@��-@��-@��-@��-@��-@��-@��^@��^@��^@��^@��^@��-@���@���@���@���@��h@��h@���@��h@��h@��h@��h@��h@��7@��h@��7@��7@��@�x�@�x�@�p�@�hs@�hs@�hs@�hs@�`B@�X@�O�@�O�@�G�@�?}@�7L@�7L@�?}@�7L@�?}@�7L@�?}@�?}@�?}A˗�A˕�A˛�A˝�A˟�A˛�A˟�A˥�A˧�A˧�A˧�A˧�A˧�A˩�AˬAˬAˬAˬAˬAˬAˬA˩�AˬAˬA˥�Aˣ�A˥�A˥�A˥�A˥�A˥�Aˣ�Aˣ�A˥�A˧�A˧�A˥�A˧�AˮA˰!AˮAˮAˮAˮAˬAˬAˮAˬA˩�A˩�AˬAˬA˩�AˬA˧�A˩�A˩�A˩�A˩�A˩�AˬAˬAˮAˮAˮA˰!AˮAˮA˩�A˧�A˥�A˧�A˧�A˧�A˧�A˥�A˧�A˧�A˩�A˩�AˮAˬA˩�AˮAˬA˩�A˩�A˩�AˬA˩�AˬA˩�A˩�A˩�A˩�A˩�A˩�AˬA˩�AˬAˬA˰!AˮAˮA˰!A˰!A˲-A˰!A˰!A˰!A˰!A˰!AˮAˮAˮAˮAˬAˮA˗�A˕�AˑhAˋDAˇ+Aˏ\A˗�Aˏ\AˑhAˏ\AˍPA˗�AˋDAˇ+A�|�A�`BA�^5A�^5A�`BA�bNA�`BA�`BA�bNA�^5A�`BA�ZA�^5A�\)A�`BA�XA�`BA�^5A�^5A�VA�S�A�O�A�S�A�Q�A�O�A�M�A�M�A�Q�A�S�A�VA�G�A�9XA�;dA�5?A��A�
=A�
=A�
=A�A���A���A��A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��@��\@��\@��\@��\@��\@��\@�~�@�n�@�n�@�n�@�^5@�^5@�^5@�^5@�V@�V@�V@�V@�V@�V@�M�@�M�@�M�@�M�@�V@�V@�V@�M�@�E�@�E�@�=q@�=q@�=q@�5?@�5?@�5?@�-@�-@�-@�-@�-@�$�@�$�@��@�{@�@��@��@��@��@��@��@��@��@��@��@��@��@��@��T@��@��T@��^@���@���@���@���@���@���@��-@���@��-@��^@��-@��-@��^@��^@��^@��^@��^@��-@��-@���@���@���@��h@��h@���@���@���@��h@��h@��h@��h@��h@��h@��7@��7@��@��@�x�@�p�@�hs@�hs@�hs@�hs@�`B@�X@�X@�O�@�G�@�?}@�?}@�?}@�?}@�?}@�7L@�?}@�?}@�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A˙�A˧�A˥�AˬAˬA˩�AˬAˮAˋDA�^5A�Q�A�%A��A��mA��#A���A���A�ƨA�ĜA���Aʴ9AʮAʰ!Aʥ�Aʗ�A��A�ȴA�  A��DA��A�;dA�I�A�JA��A�r�A���A���A�v�A�(�A��jA�S�A��\A�l�A�ƨA�ZA�ZA�(�A�$�A��`A�t�A��;A�(�A��A��7A�7LA��9A�JA��A�x�A�t�A�ZA��A��+A�9XA�^5A�t�A�x�A���A�JA�oA�jA�%A�I�A��`A�O�A�&�A�M�A�=qA���A��wA��A�I�A�1'A� �A���A���A��hA�`BA�;dA��#A���A�S�A��A���A��7A�?}A���A���A��A���A�hsA���A�p�A�dZA�  A��A�v�A��jA���A��A�ȴA�^5A�ȴA��A�jA��A�%A�1'A�$�A��A�VA�l�A�ȴA�%A~n�Ay7LAu�#Arv�Ap��An�9Aj�Ag��Acp�A^�A\�9A[|�AZbNAY�PAX�RAW��AV�AU��AQ�AMVAJ(�AH��AG�hAFjAC|�A@=qA<�RA;�A9C�A8A�A6�A4 �A2�A2�+A1��A1��A2-A2(�A1�#A1�#A1G�A0�A.�RA.9XA-x�A,��A+ƨA*�A(~�A'��A&��A&�+A&bNA%�TA%�PA$�A"��A ��A`BAZA�A�FA"�A��A�A~�A�#A��A=qA��AƨAx�A/A�wA1'Az�AG�A��A�DA�A��A
��A	C�AI�A�A�hA^5AA"�AI�A��Ax�A��A-A�FA`BA�A ��A 9XA @��
@�o@��h@��D@��F@�o@���@��
@��@�&�@�z�@�  @�\)@�{@��`@�F@�R@���@�o@�@�`B@���@�G�@�P@�=q@�@�l�@�+@�I�@� �@�5?@�t�@�=q@���@؛�@�5?@��@�33@��@�|�@���@�n�@�O�@ˍP@�ȴ@���@ț�@�o@�M�@�Ĝ@�(�@î@�"�@�ȴ@���@�@�X@�%@�Ĝ@�1@�\)@���@��\@���@�X@���@�A�@���@�+@��@�=q@��#@��#@��7@���@�|�@�l�@��@�~�@���@�%@�Q�@�1@���@��F@���@�@� �@�
=@��^@���@��D@�z�@�Q�@�  @�ƨ@�\)@��@�o@�o@��\@�v�@�v�@�ff@��@���@��H@�{@�A�@�Ĝ@���@�5?@�+@�j@��@�|�@�dZ@�|�@��@��@���@���@�v�@�5?@��#@���@�$�@���@�&�@��@��
@�bN@�z�@���@��@���@��u@��@�(�@�x�@�-@��@��!@�o@���@�A�@�l�@�?}@�~�@��@��h@�X@�x�@�  @��P@���@�o@�"�@�33@�;d@�"�@�S�@��@���@��#@�z�@��@��@�o@���@��R@�V@��T@�@��7@��@�A�@���@���@�l�@�;d@���@���@���@�^5@�=q@��@��#@���@�x�@�%@�Ĝ@�  @��m@��w@�l�@��y@�n�@�E�@�E�@�E�@�E�@��@��@��^@�x�@�&�@��@���@� �@��@���@���@�;d@�o@��y@��H@��@���@���@�-@��T@��@�V@���@���@�Z@�9X@���@��F@���@��P@�l�@�33@�"�@�@��H@��!@�ff@�M�@�@���@��@��@��@�X@��@��@���@��D@�Q�@�9X@��@�  @��@��@�S�@���@�n�@�M�@��@��T@��-@���@�hsG�O�@��@�F@v�6@m0�@e��@^��@W��@Q��@G��@@Q�@9��@5<6@0��@)�d@$��@�2@J@��@�p@[WG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�jA�$�A��yA�VAüjA�K�A���A��yA�+AǼjA� �A��PA��A�jAȧ�A�ffA�oA�JAĩ�A���A�/Aã�A��A�ffA�1'A�`BA�O�A���A��A�bA��
A��mA�p�A�G�A���A�p�A���A��#A�`BA�JA��A�jA°!A��Aɧ�A�E�A��`A�A��TA��A�z�A�oA�I�A�&�A�~�A�(�A��-A��RAº^Aś�Aʏ\A���AˁA��;A��`A���A��/A�E�A�ZA��A�ȴA�M�AōPA�E�A�&�A�`BA�l�A��A��#A�+A�~�A���A�x�A�(�A���AȓuA�^5A�7LA�\)A�1'A�jA�~�A��AA�A�hsA���A�E�A� �AąAɅA��TA��!A�  A��A�jA��
A�%A�;dA�t�A�ĜA��#Aʙ�A���A��#A�AÁA�K�A�r�A�\)A�p�A�=qA���AȁA�l�A�AǇ+A�l�Aĝ�A�+A��A�ȴA�t�A�l�A�C�A��A�E�A�A��/A�Q�A��wA�I�A�+A�v�A�7LA��A�p�Aũ�A���A�ffA�VA�t�A�7LA��A�G�A�x�A�"�A�A�S�A�9XAȺ^A�1A��AȾwA˃AˁAʬA�jAPAǼjA�~�A�x�A�JA���A���A��A��
AˁA�1'Aũ�A�p�A��Aɣ�A�(�A��A���AÑhAˁA�v�A®Aš�A���A��A�-A�Q�A�x�Aʲ-A��
A��A�t�A�=qA�t�A�A�{Aʉ7A�^5A�VA�z�A�t�A�t�A�r�A�&�A�v�A��AčPA�=qA���AˁA�~�A�x�A�x�A�r�A�x�A�r�A�p�A�t�A�x�A�jA�r�A�n�A�x�A�x�A�z�A�t�A�v�A�x�A�r�A�v�A�p�A�v�A�|�A�r�A�z�A�x�A�n�A�t�A�z�A�v�A�|�AˁA�~�A�t�A�t�A�~�A�r�A�x�A˅A�~�A˃A�~�A�v�A�I�A˅A˃AˁAˉ7Aˉ7A˅A˅A˃AˋDAˉ7AˋDAˍPAˋDA�~�A˃AˁAˉ7A�|�Aˇ+Aˇ+AˋDA˅A�~�AˍPAˍPAˍPAˍPA˃Aˇ+A˃AˋDAˋDAˍPAˏ\A�|�AˍPAˑhAˋDAˏ\A˅AˍPAˋDAˇ+AˋDA�p�A˃A˕�AˑhA˓uA˓uA˕�AˍPAˍPAˏ\AˍPAˉ7A��AˋDAˍPAˋDAˉ7A˅AˑhAˍPAˋDA˅A˅A˃A�bA�t�A�n�A˃A�|�A˃A�~�Aˉ7A˃AˋDAˋDA˃Aˇ+A˃A�~�A�x�A�z�A�~�A˃A�|�Aˇ+AˁA�z�A�p�A�~�A�x�A�r�A˃A�r�A�v�A�|�Aˉ7Aˉ7A�~�A˅AˋDA˅A˅A�|�Aˇ+A˅AˁAˇ+Aˉ7A˅A˃Aˇ+Aˇ+Aˇ+Aˉ7AˋDAˉ7Aˇ+AˋDAˇ+AˋDAˋDAˏ\A˃AˋDAˉ7A˃Aˇ+Aˉ7Aˇ+Aˇ+A˃Aˇ+Aˇ+Aˇ+Aˇ+Aˉ7Aˇ+A�~�A˃AˋDAˇ+A�~�AˋDAˋDAˉ7Aˉ7Aˇ+Aˇ+Aˉ7A�x�Aˇ+Aˇ+Aˇ+AˋDAˍPAˋDAˇ+Aˇ+AˁA˅A�z�AˋDAˋDAˉ7Aˇ+AˍPAˇ+AˍPAˋDAˇ+Aˇ+Aˉ7Aˇ+AˋDAˇ+A�z�AˍPAˉ7A�hsAˋDAˋDAˍPAˉ7AˋDAˍPAˋDAˏ\Aˏ\Aˉ7AˋDAˉ7AˍPAˋDAˍPAˍPAˇ+Aˉ7Aˉ7Aˇ+AˋDAˋDAˍPAˋDAˋDAˋDAˍPAˉ7AˍPAˍPAˇ+A˅A˅AˍPA�-A˅A˅AˋDAˏ\Aˏ\AˍPAˑhA˓uA˕�A˕�A˓uA˓uA˗�A˕�A˙�A˓uA˓uA˓uA˙�A˙�A˙�A˙�A˙�A˛�A˙�A˙�A˗�A˙�A˛�A˛�A˝�A˛�Aˡ�A˗�A˙�A˗�A˗�A˗�A˗�A˕�A˗�A˕�A˛�A˝�A˟�A˛�A˟�A˥�A˧�A˧�A˧�A˧�A˧�A˩�AˬAˬAˬAˬAˬAˬAˬA˩�AˬAˬA˥�Aˣ�A˥�A˥�A˥�A˥�A˥�Aˣ�Aˣ�A˥�A˧�A˧�A˥�A˧�AˮA˰!AˮAˮAˮAˮAˬAˬAˮAˬA˩�A˩�AˬAˬA˩�AˬA˧�A˩�A˩�A˩�A˩�A˩�AˬAˬAˮAˮAˮA˰!AˮAˮA˩�A˧�A˥�A˧�A˧�A˧�A˧�A˥�A˧�A˧�A˩�A˩�AˮAˬA˩�AˮAˬA˩�A˩�A˩�AˬA˩�AˬA˩�A˩�A˩�A˩�A˩�A˩�AˬA˩�AˬAˬA˰!AˮAˮA˰!A˰!A˲-A˰!A˰!A˰!A˰!A˰!AˮAˮAˮAˮAˬAˮA˗�A˕�AˑhAˋDAˇ+Aˏ\A˗�Aˏ\AˑhAˏ\AˍPA˗�AˋDAˇ+A�|�A�`BA�^5A�^5A�`BA�bNA�`BA�`BA�bNA�^5A�`BA�ZA�^5A�\)A�`BA�XA�`BA�^5A�^5A�VA�S�A�O�A�S�A�Q�A�O�A�M�A�M�A�Q�A�S�A�VA�G�A�9XA�;dA�5?A��A�
=A�
=A�
=A�A���A���A��A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��@��\@��\@��\@��\@��\@��\@�~�@�n�@�n�@�n�@�^5@�^5@�^5@�^5@�V@�V@�V@�V@�V@�V@�M�@�M�@�M�@�M�@�V@�V@�V@�M�@�E�@�E�@�=q@�=q@�=q@�5?@�5?@�5?@�-@�-@�-@�-@�-@�$�@�$�@��@�{@�@��@��@��@��@��@��@��@��@��@��@��@��@��@��T@��@��T@��^@���@���@���@���@���@���@��-@���@��-@��^@��-@��-@��^@��^@��^@��^@��^@��-@��-@���@���@���@��h@��h@���@���@���@��h@��h@��h@��h@��h@��h@��7@��7@��@��@�x�@�p�@�hs@�hs@�hs@�hs@�`B@�X@�X@�O�@�G�@�?}@�?}@�?}@�?}@�?}@�7L@�?}@�?}@�?}A˗�A˕�A˛�A˝�A˟�A˛�A˟�A˥�A˧�A˧�A˧�A˧�A˧�A˩�AˬAˬAˬAˬAˬAˬAˬA˩�AˬAˬA˥�Aˣ�A˥�A˥�A˥�A˥�A˥�Aˣ�Aˣ�A˥�A˧�A˧�A˥�A˧�AˮA˰!AˮAˮAˮAˮAˬAˬAˮAˬA˩�A˩�AˬAˬA˩�AˬA˧�A˩�A˩�A˩�A˩�A˩�AˬAˬAˮAˮAˮA˰!AˮAˮA˩�A˧�A˥�A˧�A˧�A˧�A˧�A˥�A˧�A˧�A˩�A˩�AˮAˬA˩�AˮAˬA˩�A˩�A˩�AˬA˩�AˬA˩�A˩�A˩�A˩�A˩�A˩�AˬA˩�AˬAˬA˰!AˮAˮA˰!A˰!A˲-A˰!A˰!A˰!A˰!A˰!AˮAˮAˮAˮAˬAˮA˗�A˕�AˑhAˋDAˇ+Aˏ\A˗�Aˏ\AˑhAˏ\AˍPA˗�AˋDAˇ+A�|�A�`BA�^5A�^5A�`BA�bNA�`BA�`BA�bNA�^5A�`BA�ZA�^5A�\)A�`BA�XA�`BA�^5A�^5A�VA�S�A�O�A�S�A�Q�A�O�A�M�A�M�A�Q�A�S�A�VA�G�A�9XA�;dA�5?A��A�
=A�
=A�
=A�A���A���A��A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��@��\@��\@��\@��\@��\@��\@�~�@�n�@�n�@�n�@�^5@�^5@�^5@�^5@�V@�V@�V@�V@�V@�V@�M�@�M�@�M�@�M�@�V@�V@�V@�M�@�E�@�E�@�=q@�=q@�=q@�5?@�5?@�5?@�-@�-@�-@�-@�-@�$�@�$�@��@�{@�@��@��@��@��@��@��@��@��@��@��@��@��@��@��T@��@��T@��^@���@���@���@���@���@���@��-@���@��-@��^@��-@��-@��^@��^@��^@��^@��^@��-@��-@���@���@���@��h@��h@���@���@���@��h@��h@��h@��h@��h@��h@��7@��7@��@��@�x�@�p�@�hs@�hs@�hs@�hs@�`B@�X@�X@�O�@�G�@�?}@�?}@�?}@�?}@�?}@�7L@�?}@�?}@�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>p�@�fQ@�*�=ݒ�?^�[>(��@�=�=�{�>E;�=��?��@�%�=��=�M>`�@4�a@�)�=�'�@��>0j@@>=���=꣭=�m@�"�@�"=��+>]��?� ~?���=�˒>*�/>��h>h4@��@�.�=�л>qL?��@� �@�F_>V>D�?��>QY?N)t@��>��?}��@��'=��
@8�D=��n>]��@���=y|�=�3r?�H=��]=�0>K4?���>^��@�+@��>��?��?�7�>K4>�%�=�mH=�1{=�ff>#
�@��?���@�/�>�@9M@>T4>�i�>@�M@�"@�$t>�i@?}>E�@���@�&->8�?Bzc@�,�@���>	�@(�@��?9=�^=�pe?Cp�@M@�%1>�q@��T@�*>�@e�S=�^>�s?�t*@�#%>(��@�(x=���>�7@pK
>�n?��]@�(�@�*�>+X�=�x>�g�@��?��x=�?�>���@�&-?Nff>r2@�.>D�@�%�?�o�>��@��<@�'R>>B@g��@vީ=��>iv6?��@��p?b*o>*�@�&-@D�f>(@�%�@��?=�uy>�r�=��
?F(�>�@���=��x>��@S��@�/?��>a>��@�1�@�28@�/Z=��l=��>V��@�+�@�.�@B��=�d�?�5>�@+]y@��r@�2�>l�@�(�>��s@�e�@�.�=�4D>Z�
>c�@�0�@�)�?�?<6=�?S>)@��s@�K�@�*0@�-�?��<>��>��@:S�@�-�@�iD@���@�x�@�)�@�&B?gm3@�-�@�,(@�-w@�.4@�.I@�,|>Hh4@�0�@���@�1@�1�@�0j@�0j@�.�@�.4@�-�@�-�@�-�@�-�@�,�@�.�@�-#@�,�@�0@�/Z@�.�@�/�@�0j@�/Z@�.�@�/@�-�@�/Z@�0�@�0�@�1'@�.I@�0@�0j@�0j@�2�@�28@�28@�0j@�/�@�.�@�.4@�2�@�4@�0�@�3�@�2�@�1{@�-�@�4�@�5@�4Y@�5~@�4�@�4�@�5@�5~@�6z@�6&@�6�@�7L@�6�@�5~@�77@�6z@�6�@�5~@�5@�6�@�6z@�6&@�6�@�8	@�6�@�7�@�8	@�3H@�6�@�7�@�7�@�8\@�8�@�4�@�6�@�9m@�9@�9�@�8\@�8	@�8�@�9m@�9@�9m@�;O@�8\@�9m@�9�@�9@�8�@�8�@�8�@�8\@�8�@�8G@�6�@�8	@�9@�:�@�7�@�6�@�6�@�7�@�5�@�5~@�4Y@�3�@�5@� @�/�@�0j@�2�@�4@�4�@�4�@�5@�5~@�6;@�6;@�5@�5@�4�@�3H@�4@�3�@�4@�5�@�5@�5~@�4@�3�@�(c@�4@�2�@��@�5~@�1�@�2�@�28@�6&@�5@�3�@�6�@�7L@�5@�4Y@�4Y@�6�@�5�@�5@�6;@�6�@�5�@�6�@�6�@�6;@�6�@�7L@�7�@�6�@�6�@�5�@�6;@�7L@�8G@�8G@�6�@�6�@�6;@�5@�5�@�6z@�6&@�5�@�5�@�5�@�5�@�6z@�5�@�6�@�6;@�2�@�6�@�6&@�6&@�6�@�7�@�6�@�6�@�6�@�77@�77@�7L@�7�@�6;@�6�@�6�@�6�@�77@�7�@�5�@�6�@�5�@�7�@�4Y@�6�@�6�@�5@�5@�7L@�7L@�7�@�77@�6;@�6;@�7L@�7�@�7�@�5�@�5@�6�@�7L@�6�@�6�@�7�@�8\@�7�@�7�@�77@�8�@�8\@�6�@�6�@�7�@�7�@�7�@�7�@�8	@�8	@�6�@�6�@�6&@�8	@�7�@�7�@�8	@�7�@�7�@�8\@�8\@�8	@�8\@�8\@�7�@�8G@�8	@�9@�8\@�77@�7�@�9@�9@�9m@�9�@�;�@�;:@�;:@�:�@�;�@�;:@�;�@�<�@�;�@�;�@�<`@�<`@�=q@�=@�>@�=�@�=�@�=q@�>-@�=�@�=\@�=�@�=q@�>-@�>-@�=�@�=�@�=q@�=�@�=�@�=q@�=@�<�@�=@�=@�>-@�?�@�@@�@@�A @�A_@�D@�Dg@�Dg@�Dg@�D�@�Ex@�Ex@�E�@�E�@�E�@�E�@�Ec@�Ec@�E�@�D@�Dg@�D�@�CW@�C�@�C�@�C�@�Dg@�D@�D�@�Dg@�E�@�E�@�E�@�Ft@�GZ@�G�@�HA@�G�@�G�@�H@�HA@�G�@�GZ@�G@�G@�FJ@�G@�FJ@�F�@�G0@�G@�GZ@�GZ@�F�@�F�@�GZ@�G�@�H@�H�@�H�@�I=@�H�@�H�@�H�@�H�@�G�@�G@�F_@�Ft@�F�@�G@�Go@�G@�Go@�Go@�H�@�H�@�If@�H,@�H�@�I�@�I@�H�@�H�@�I@�If@�H�@�H�@�H�@�H�@�H�@�H�@�H�@�I@�H�@�IR@�IR@�I@�I�@�J�@�Jw@�J�@�J�@�K@�Jb@�Jb@�Jb@�J@�I�@�I�@�I�@�G�@�H@�G�@�F�@�@:@�?�@�?S@�<�@�;y@�>@�>�@�<6@�:�@�:�@�<!@�<�@�:i@�6�@�0�@�)@�(�@�(N@�)�@�)�@�)�@�)�@�)@�(@�&�@�&�@�'=@�%1@�$t@�$�@�%p@�&�@�&�@�%�@�#:@�")@��@�"�@�"h@� \@��@�!@� 2@�u@�H@�@��@��@�	-@�@��@��@���@��a@��j@���@��@���@���@���@��@��@��j@��j@��U@��j@��j@���@��&@���@��e@���@���@���@���@���@���@��v@���@��&@��@��j@���@Q'�@Q'�@Q'g@Q'=@Q&@Q$�@Q"}@Q!�@Q!�@Q!-@Q!-@Q �@Q �@Q 2@Q�@Q�@Q�@Q�@Q�@Q�@Q6@Q6@Q�@Q�@Q�@Q6@Q:@Q�@Q�@Q�@QC@QC@Q�@QH@Q�@Q�@Q�@QL@Q�@Q�@Q�@Q@QY@Q	@Qj@Q@Q@Q�@Qs@Q�@Q�@Qs@Q@Qs@Q�@Qw@Q�@Q#@Q�@Q�@Q�@Q	�@Qp@Q@Qp@Q�@Q	@Q	�@Q	l@Q	�@Q
�@Q�@Q@Q�@Q@Q5@Q�@Q�@Q_@Q9@Q
g@Q	B@Q�@QF@Qt@Q�@Q�@Q�@Q	@Q�@Q	@Q�@Q�@Q�@Qp@Q�@Q�@Q�@Q%@Q�@Q�@Q@Q\@Q�@Q�@Q`@Q@Q �@P��@P�@P�H@P�r@P��@P�r@P��@P�@P��@P�m@P��@P�m@�u�@�u@�u�@�w�@�y)@�v�@�y@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�}�@�~(@�~�@�~�@�~@�~(@�~�@�~R@�~g@�~�@�|�@�{�@�{�@�|@�{�@�|@�|�@�{�@�{�@�|�@�}�@�}�@�}@�}k@���@���@���@���@��
@���@��@��@��I@��
@�$@�~�@��@�N@��@�N@�N@�~�@�x@��@��@��@���@��s@���@��@��Z@��Z@��@��Z@��@�@�~�@�~�@�~�@�~�@�~�@�~�@�@��@��
@��E@���@���@��s@��,@��0@���@���@���@���@���@���@���@���@��s@���@���@���@��@��0@���@���@���@���@���@��<@��{@���@���@���@���@��{@��f@��<@��f@���@���@���@���@�x�@�xl@�xl@�t�@�s.@�uO@�z�@�u�@�vu@�w�@�t?@�y�@�t�@�r�@�q@�b9@�`W@�`@�`�@�a�@�b@�a�@�a�@�`�@�`�@�^�@�a�@�_F@�bN@�^t@�`�@�`�@�`�@�\�@�]y@�[B@�[�@�Z�@�Z�@�Z�@�YK@�Z@�[�@�\�@�X�@�R @�P	@�P@�Ex@�;�@�<6@�;�@�82@�4n@�3H@�1�@�33@�2a@�3�@�1�@�1�@�28@�2@�2#@�28@�2�@�2v@�2�@�2�@�3H@�3]@�3]@�3�@�3�@�3�@�3�@�4/@�4�@�4Y@�3�@�3�@�2�@Q[�@Q[�@Q\)@Q[�@Q[�@Q[W@QY�@QWi@QW?@QVm@QT�@QTv@QTL@QT�@QS�@QS&@QS&@QS&@QR�@QR�@QS&@QR�@QR�@QR�@QR�@QS�@QS�@QRT@QQ�@QQ/@QP�@QP]@QP3@QO�@QOa@QO@QN�@QN�@QN�@QN@QM�@QM�@QL�@QL�@QKs@QI{@QEc@QE�@QE9@QE�@QEc@QEc@QE@QE@QE@QE@QD�@QD�@QD=@QD=@QD�@QD=@Q>W@Q;@Q:�@Q:�@Q:�@Q;:@Q;�@Q<6@Q;�@Q<`@Q>-@Q>W@Q>-@Q?S@Q?}@Q@y@Q@O@Q@O@Q>�@Q>-@Q<`@Q;�@Q;d@Q9�@Q:@Q;�@Q<@Q<@Q;�@Q;�@Q<@Q<6@Q<6@Q<�@Q<6@Q;�@Q:�@Q:@Q9�@Q8G@Q6�@Q6�@Q6�@Q6�@Q5�@Q5T@Q3�@Q3�@Q1�@Q0�@Q0�@Q0�@Q0�@Q1<@Q1f@Q1f@Q1�@Q1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         43344434444344443444444433444444444344433444443443444434444444433444444444343444444334443344334434444434334344434344344334444444344443443343344434434433444443443344433344433444443343433444334444333344443333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�fQ@�*�G�O�G�O�G�O�@�=�G�O�G�O�G�O�G�O�@�%�G�O�G�O�G�O�G�O�@�)�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�"�@�"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�.�G�O�G�O�G�O�@� �@�F^G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�@��)G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�+@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�@�/�G�O�G�O�G�O�G�O�G�O�G�O�@�"@�$yG�O�G�O�G�O�@���@�&5G�O�G�O�@�,�@���G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�@�%/G�O�@��R@�+G�O�@e�SG�O�G�O�G�O�@�#'G�O�@�(|G�O�G�O�@pKG�O�G�O�@�(�@�*�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&-G�O�G�O�G�O�G�O�@�%�G�O�G�O�@��>@�'SG�O�@g��@vޭG�O�G�O�G�O�@��qG�O�G�O�@�&.G�O�G�O�@�%�@��@G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@S��@�/G�O�G�O�G�O�@�1�@�27@�/]G�O�G�O�G�O�@�+�@�.�G�O�G�O�G�O�G�O�G�O�@��r@�2�G�O�@�(�G�O�@�e�@�.�G�O�G�O�G�O�@�0�@�)�G�O�G�O�G�O�G�O�@��r@�K�@�*2@�-�G�O�G�O�G�O�G�O�@�-�@�iF@���@�x�@�)�@�&EG�O�@�-�@�,%@�-~@�.5@�.J@�,G�O�@�0�@���@�1@�1�@�0k@�0k@�.�@�.6@�-�@�-�@�-�@�-�@�,�@�.�@�-&@�,�@�0@�/Z@�.�@�/�@�0k@�/Z@�.�@�/@�-�@�/Z@�0�@�0�@�1)@�.H@�0@�0q@�0l@�2�@�29@�2:@�0n@�/�@�.�@�.6@�2�@�4@�0�@�3�@�2�@�1@�-�@�4�@�5@�4\@�5@�4�@�4�@�5@�5~@�6z@�6&@�6�@�7O@�6�@�5{@�78@�6~@�6�@�5�@�5@�6�@�6{@�6(@�6�@�8@�6�@�7�@�8
@�3G@�6�@�7�@�7�@�8X@�8�@�4�@�6�@�9o@�9@�9�@�8^@�8	@�8�@�9n@�9@�9q@�;Q@�8a@�9q@�9�@�9@�8�@�8�@�8�@�8_@�8�@�8H@�6�@�8@�9@�:�@�7�@�6�@�6�@�7�@�5�@�5@�4]@�3�@�5@� @�/�@�0n@�2�@�4	@�4�@�4�@�5@�5�@�6>@�6>@�5@�5@�4�@�3G@�4@�3�@�4@�5�@�5@�5~@�4@�3�@�(f@�4@�2�@��@�5�@�1�@�2�@�27@�6#@�5@�3�@�6�@�7N@�5@�4V@�4Z@�6�@�5�@�5@�6>@�6�@�5�@�6�@�6�@�6>@�6�@�7O@�7�@�6�@�6�@�5�@�6>@�7J@�8I@�8J@�6�@�6�@�65@�5@�5�@�6{@�6(@�5�@�5�@�5�@�5�@�6{@�5�@�6�@�69@�2�@�6�@�6,@�6(@�6�@�7�@�6�@�6�@�6�@�78@�74@�7O@�7�@�6?@�6�@�6�@�6�@�77@�7�@�5�@�6�@�5�@�7�@�4]@�6�@�6�@�5@�5@�7L@�7M@�7�@�7:@�6>@�6>@�7N@�7�@�7�@�5�@�5@�6�@�7O@�6�@�6�@�7�@�8b@�7�@�7�@�77@�8�@�8^@�6�@�6�@�7�@�7�@�7�@�7�@�8@�8
@�6�@�6�@�6&@�8@�7�@�7�@�8
@�7�@�7�@�8^@�8b@�8
@�8b@�8_@�7�@�8I@�8	@�9@�8a@�7:@�7�@�9@�9@�9l@�9�@�;�@�;<@�;9@�:�@�;�@�;>@�;�@�<�@�;�@�;�@�<^@�<^@�=r@�=@�>@�=�@�=�@�=r@�>,@�=�@�=_@�=�@�=v@�>.@�>/@�=�@�=�@�=t@�=�@�=�@�=u@�=@�<�@�=
@�u�@�u@�u�@�w�@�y)@�v�@�y@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�}�@�~)@�~�@�~�@�~@�~)@�~�@�~R@�~l@�~�@�|�@�{�@�{�@�|	@�{�@�|@�|�@�{�@�{�@�|�@�}�@�}�@�}@�}m@���@���@���@���@��@���@��@��@��J@��@�#@�~�@��@�L@��@�Q@�Q@�~�@�w@��@��@��@���@��q@���@�� @��a@��Z@��@��^@��@�@�~�@�~�@�~�@�~�@�~�@�~�@�@��@��@��E@���@���@��s@��.@��6@���@���@���@���@���@���@���@���@��v@���@���@���@��@��/@���@���@���@�� @���@��=@��}@���@���@���@���@��}@��c@��?@��e@��@���@���@���@�x�@�xn@�xl@�t�@�s1@�uN@�z�@�u�@�vx@�w�@�t@@�y�@�t�@�r�@�q@�b;@�`Z@�`@�`�@�a�@�b@�a�@�a�@�a@�`�@�^�@�a�@�_F@�bL@�^v@�`�@�`�@�`�@�\�@�]t@�[E@�\@�Z�@�Z�@�Z�@�YJ@�Z@�\@�\�@�X�@�Q�@�P@�P"@�E{@�;�@�<8@�;�@�80@�4q@�3K@�1�@�33@�2h@�3�@�1�@�1�@�27@�2@�2&@�22@�2�@�2v@�2�@�2�@�3G@�3^@�3^@�3�@�3�@�3�@�3�@�4.@�4�@�4Z@�3�@�3�@�2�@Q[�@Q[�@Q\(@Q\ @Q[�@Q[[@QY�@QWk@QW=@QVn@QT�@QTs@QTM@QT�@QS�@QS&@QS&@QS*@QR�@QR�@QS&@QR�@QR�@QR�@QR�@QS�@QS�@QRU@QQ�@QQ5@QP�@QP`@QP3@QO�@QOb@QO@QN�@QN�@QN�@QN@QM�@QM�@QL�@QL�@QKr@QI~@QEf@QE�@QE;@QE�@QEc@QEc@QE@QE@QE@QE@QD�@QD�@QD>@QD>@QD�@QD=@Q>Z@Q;@Q:�@Q:�@Q:�@Q;:@Q;�@Q<3@Q;�@Q<^@Q>2@Q>V@Q>-@Q?P@Q?z@Q@{@Q@K@Q@P@Q>�@Q>0@Q<[@Q;�@Q;c@Q9�@Q:@Q;�@Q<@Q<@Q;�@Q;�@Q<@Q<8@Q<3@Q<�@Q<6@Q;�@Q:�@Q:@Q9�@Q8F@Q6�@Q6�@Q6�@Q6�@Q5�@Q5V@Q3�@Q3�@Q1�@Q0�@Q0�@Q0�@Q0�@Q1;@Q1f@Q1k@Q1�@Q1�@�u�@�u@�u�@�w�@�y)@�v�@�y@�|�@�|�@�|�@�|�@�|�@�|�@�|�@�}�@�~)@�~�@�~�@�~@�~)@�~�@�~R@�~l@�~�@�|�@�{�@�{�@�|	@�{�@�|@�|�@�{�@�{�@�|�@�}�@�}�@�}@�}m@���@���@���@���@��@���@��@��@��J@��@�#@�~�@��@�L@��@�Q@�Q@�~�@�w@��@��@��@���@��q@���@�� @��a@��Z@��@��^@��@�@�~�@�~�@�~�@�~�@�~�@�~�@�@��@��@��E@���@���@��s@��.@��6@���@���@���@���@���@���@���@���@��v@���@���@���@��@��/@���@���@���@�� @���@��=@��}@���@���@���@���@��}@��c@��?@��e@��@���@���@���@�x�@�xn@�xl@�t�@�s1@�uN@�z�@�u�@�vx@�w�@�t@@�y�@�t�@�r�@�q@�b;@�`Z@�`@�`�@�a�@�b@�a�@�a�@�a@�`�@�^�@�a�@�_F@�bL@�^v@�`�@�`�@�`�@�\�@�]t@�[E@�\@�Z�@�Z�@�Z�@�YJ@�Z@�\@�\�@�X�@�Q�@�P@�P"@�E{@�;�@�<8@�;�@�80@�4q@�3K@�1�@�33@�2h@�3�@�1�@�1�@�27@�2@�2&@�22@�2�@�2v@�2�@�2�@�3G@�3^@�3^@�3�@�3�@�3�@�3�@�4.@�4�@�4Z@�3�@�3�@�2�@Q[�@Q[�@Q\(@Q\ @Q[�@Q[[@QY�@QWk@QW=@QVn@QT�@QTs@QTM@QT�@QS�@QS&@QS&@QS*@QR�@QR�@QS&@QR�@QR�@QR�@QR�@QS�@QS�@QRU@QQ�@QQ5@QP�@QP`@QP3@QO�@QOb@QO@QN�@QN�@QN�@QN@QM�@QM�@QL�@QL�@QKr@QI~@QEf@QE�@QE;@QE�@QEc@QEc@QE@QE@QE@QE@QD�@QD�@QD>@QD>@QD�@QD=@Q>Z@Q;@Q:�@Q:�@Q:�@Q;:@Q;�@Q<3@Q;�@Q<^@Q>2@Q>V@Q>-@Q?P@Q?z@Q@{@Q@K@Q@P@Q>�@Q>0@Q<[@Q;�@Q;c@Q9�@Q:@Q;�@Q<@Q<@Q;�@Q;�@Q<@Q<8@Q<3@Q<�@Q<6@Q;�@Q:�@Q:@Q9�@Q8F@Q6�@Q6�@Q6�@Q6�@Q5�@Q5V@Q3�@Q3�@Q1�@Q0�@Q0�@Q0�@Q0�@Q1;@Q1f@Q1k@Q1�@Q1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         43344434444344443444444433444444444344433444443443444434444444433444444444343444444334443344334434444434334344434344344334444444344443443343344434434433444443443344433344433444443343433444334444333344443333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��9�m9�9��9��9��9��9�c9�c9�e9�s9��9��9��9��9��9�	9�	9��9��9�	9��9��9�	
9�s9��9��9�9��9�9�e9��9��9��9�/9�_9��9�9�
�9�
�9�
�9�
�9�
89�
�9�	�9�
9�
h9�
49�	|9�	I9�
&9�	�9�	�9�	�9�	�9�	;9�	�9�	�9�	�9�
9�
�9�
�9�
�9�9�H9�B9�9�E9�	�9�	l9�	9�	<9�	<9�	<9�	<9�	V9�	m9�
9�
89�19��9�
�9�
�9��9�%9�
�9�
�9�
�9�
�9�
�9�
�9��9�
�9�
�9�
�9�
�9�
�9�
�9� 9��9��9�P9��9�e9��9��9�H9�9�,9�9��9��9��9��9��9��9��9�/9��9�9�9�9���9��9��9��9��9��9� �9�+9�^9���9��79��[9���9��9��=9��9��99��9��)9��_9��]9��h9��9���9��h9��V9���9��.9��9��9��9���9��_9��9��9��R9��39���9��a9��*9��9��\9���9���9��Y9�Ӟ9���9��h9�ж9�Ͷ9���9�˽9�̸9��9��9�˽9�ˊ9���9���9���9���9��49��!9��19�̈9���9���9���9��9��9��-9��N9�́9���9�ͤ9��.9�� 9��U9'��9'�#9'�G9'�'9'�9'��9'�Q9'�|9'�W9'��9'�`9'�9'��9'�a9'��9'�9'�9'�9'��9'��9'�9'��9'��9'��9'��9'��9'�u9'�j9'�9'��9'�9'�9'�9'09'9'~�9'~�9'~�9'~�9'~9'}�9'}�9'}9'|�9'{�9'zW9'w9'w-9'v�9'w09'w9'w9'v�9'v�9'v�9'v�9've9've9'v$9'v$9'v�9'v#9'qm9'n�9'ng9'ng9'n�9'n�9'oo9'o�9'op9'o�9'qM9'qj9'qI9'r29'rS9's!9'r�9'r�9'q�9'qK9'o�9'oS9'o9'm�9'n9'o29'o�9'o�9'or9'op9'o�9'o�9'o�9'o�9'o�9'o39'n�9'n9'm�9'l�9'k�9'k�9'kc9'k�9'jy9'j69'h�9'h�9'g39'ff9'f�9'f�9'fj9'f�9'g9'g9'g/9'g5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	7B	7B	7B	7B	7B	7B1B1B1B%B%BBBBBBBBBBBBBBBF�Bn�BA�B1'B&�B.B8RB<jB>wB<jB?}BB�BH�BJ�BL�BN�BK�BL�BL�BL�BT�BZB`BBbNB`BB[#BW
BP�BT�BP�BK�BE�BC�BC�BC�BB�B?}B;dB7LB,B�BuB%B1BDBJBVBB�B�fB�B��B��B��B��B��B  B1BhBhBbBhBoB{B	7BB��B�B�B�ZB�/B�B��B�qB�9B�B��B�{Bw�BP�B�B�B��B�BB�BJB
�B
�5B
��B
�!B
��B
�B
q�B
^5B
<jB
(�B
�B
bB
B	�HB	�FB	��B	�B	r�B	^5B	D�B	,B	oB��B�B�yB�TB�;B�#B�B��BɺB�3B��B��B�\B�DB�Bw�Bn�BiyBffBgmBgmBp�Bn�Bn�Bs�Bp�Bt�B�B�DB�bB��B��B��B��B��B��B��B��B�uB�DB�7B�VB�bB�hB�bB�VB�%B|�Bp�Bm�Bn�Bn�Bm�Bl�BjBgmBffBffBe`BdZB`BBe`BgmBgmBaHBYBP�BJ�BF�BE�BD�BC�BA�B>wB<jB<jB:^B;dB:^B:^B<jB=qB>wBA�BE�BJ�BL�BK�BL�BK�BL�BN�BQ�BVBW
BW
BW
BT�BXB\)B]/B]/B]/B^5B_;B`BBaHBbNBe`BhsBhsBffBhsBe`BcTB_;BZBT�BP�BM�BN�BH�BD�BD�BC�BC�BG�BG�BG�BG�BJ�BK�BK�BK�BN�BO�BO�BQ�BS�BVBZB[#B]/B^5B_;B`BB`BBaHBe`Bp�By�Bx�By�By�B{�B� B�B�B�7B�7B�7B�=B�JB�VB�\B�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�'B�3B�XB�^B�qBBƨBɺB��B��B��B��B��B�#B�HB�TB�B��B��B	PB	2-B	>wB	?}B	@�B	B�B	H�B	J�B	K�B	L�B	N�B	Q�B	VB	T�B	R�B	O�B	O�B	O�B	O�B	P�B	W
B	[#B	]/B	^5B	_;B	aHB	dZB	bNB	YB	P�B	Q�B	ZB	]/B	cTB	cTB	^5B	VB	N�B	M�B	S�B	XB	_;B	[#B	ZB	\)B	bNB	ffB	gmB	gmB	iyB	k�B	l�B	k�B	jB	iyB	k�B	n�B	p�B	r�B	s�B	y�B	|�B	|�B	}�B	� B	�B	�+B	�+B	�7B	�=B	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�?B	�?B	�FB	�FB	�LB	�XB	�dB	�qB	�wB	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�#B	�B	�B	�#B	�#B	�)B	�/B	�;B	�;B	�)B
 B
�B
�B
�B
$�B
+�B
1�B
8�B
A B
E�B
KDB
O�B
V�B
Y�B
_;B
d�B
jB
n�B
s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?DG�Bu�B�?�@�K�?V��B\:?�?z�f?k<@?�pB�>�k�?�B?<yA���B:�>��:A_�l?1��A�{`>ܯ�?	?:�B��B��>���?�v�@�	�@��~>�7�?Zm�@0��?-#A?�B�?
�?<7rA<l�B�JA�D?4�?z�6@�3�?&�@�5B�k?*=@�=sA�>�? �A�#�?�|?��TA��>�D_>�R"@�.>��>�m"?#8A �?��FB�=B�?!��@�&�A0�r?(��?��k>�x�?�'?��?NP�B
�@ĺ�BE�?4�A��m?�@t@2n?(KAG �B�(B�G?&�(AFܣ?:��A�<�B�
?';2@���BЍB.�?4�A~��B�t@I�}>�b?�@�YAjB�u?:wBx5B�?@�}A���>�]?2|A14�B?T߽B%�>ݲ�?%2�A�-?3`@�N�BѱB�?]��? 6?��AV��@�Yw?d@&�/Bѹ@�0)?=��AW�}?y�>B�7A��?;sKAԅ�B��?+D�A�@�AŊa>��?�w�@MFB+@�]r?WyBB�A���?C�XB��A�T�>�٥@�>���@��Z?+��B
�?
}�?<�A�f�B�@U-�?+�_@��BӬB��B$�>�ѳ?	I�?���BϚBԗA��?T*A-�?(@lA��B��B�)?��Bҗ@;�A�QB�?b�?�c�?�lQB�|B�@F[>@?�!?Z?U_�A�o?A��BЗB �@��@��?���A��qB՛A���A�~A�`B�NB�M@�TcB�EB7tB�B�B�(B�	?�isB��B�B��B�9B�SB�[B��B�JB�^B�)BՓB��B؅B��B�OB�
B��BԍBօB�pB�SB׹B�uB�7B��B��B��B��B��B�LBלBՈB�&B֪B��B՚B��B�<BҖB֫B�IB��B�6B�DB��B�B�?BՏB֧B��BԞB��BՇB��B�B��B�AB�BԱB�B؞B؝BسB��B�jB�BֺBԻB��BٙB�oB�YB�B�^B��BֺB�B��B�B�BяB�\B֧B��B��B��B؜B�
BׂB��B�zB�B٬B�zB�fB��BӳB��B�BմB�6B՘BՙB�B�-B׵B�B��BׅB�hB�UB��B�/BԁB֟B�lB�<B�SBԲB�B�ZB��B�FB� BԁBԁB֟B�B�ZB֕B٤B،B�CB�UB�B�iB�oB،B�B�CB�\BƚB� B��B�uB�eB�IB�5B��BׅB�|B��B�&B�[B��B�zB�sB� B��BւB�B�mB� BֲB�GB��B��B�dB�1B� BՄB�lB��B��B��B�TB֧BտBՆB�BշB�UBտB��B�bBտBբB�B�HB�PB�nB�BٙB��B�BբBՙB�B�B�GB�	B�(B�dBֺB�,BԦB�BտBֺB�)B�3B�B�,B�B�FB�BԹB�B�KB�iB� B� B�OB�hB�BտB��B�B�GB�MB��B��BլB֝B��BԦB��B��BӅB��B��B֝B�B��B�oB�gB�dB��B�AB��B��B��B�^B��B��B�wBլB��BլBմB׵B��B؜B�bB�B��B�3B�5B՗B��B�B�KB��B�
BԽB�-B��B�{B�9B� B�YBֱBֱB�CB��B��BՓBՓB�_B��B�gB��B�rB�>B��B�B�zB�BջB�HB�BբB�UB��B��B�<B�/B��B�DB�B�.B�WB֟B�NB�{B�	B�B�TBԉBՆB�uB��B�sB�B�BӏB��B��B��BԉB�dB��B��B�%B�B�OB�9B֌BԡB�|B�/B��BբBԊB�!BՀB��B�,B�AB�BԵB�wBՌB�gBԱB�BԡB�=B�MB�zB��B��B�YB��B�"B��B��BԁBԽB��B��B�AB�dBԠBՋBՖB�#B�B�\B�(B�LB�xB�kB�$BՐB�1B��B��B�BԊBԂB��B�=BՌBՄB�|B��B�.B��B�B�3B�9BՒBՉB�?B�B�B��B�B�.BԆB��BԊBӷB�$BӼBӳBӫB��B�qBѻBѩBˑB�B�fBһBӹB�<B��B��B�*B��B�BЅB�6B�	B�B��B҅B��BӎB�9B�jB�ZB��BѫBЁBҍB�B�tB� B�B�#B�B��BϴB�%B�B�IBүBМB�
B�=B�B�,BͫBʕB�cB͙B��BŞB��B�iB�SB�<B�eB�IB�^B��B�EB��BЛBфB�|B��BѰBѕBѠB�ZB��B�-B��BтB�lB�|BҩBҘBҐBіB�;B�wBҌB�hB��BҒB	�1B	�$B	��B	��B	��B	��B	�.B	��B	�^B	�#B	�&B	��B	��B	�9B	��B	��B	��B	��B	��B	�eB	�*B	�B	�@B	�pB	�B	��B	�B	��B	��B	��B	�vB	�xB	��B	��B	�NB	�QB	�B	��B	�&B	�)B	��B	��B	�VB	�dB	�^B	��B	��B	�CB	��B	�B	�B	��B	�|B	��B	�$B	��B	�+B	�tB	�9B	�,B	�B	��B	��B	��B	��B	�(B	�:B	�{B	�PB	�QB	�B	��B	�1B	��B	��B	��B	�B	�$B	��B	��B	�3B	�QB	��B	��B	��B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�7B	��B	��B	�UB	��B	�HB	��B	�B	�LB	� B	�KB	�B	��B	�jB	��B	�/B	�PB	�bB	�B	�;B	�zB	�QB	��B	��B	�vB	EB	�B�B�B	4B�B	B	�B�B�B�B	B�B`BtB�B�B�BqB|B�B	UB�B�B	=B	BXB�BfB�B�B	B	B	B�B	B	=B�B	4B�B	B	 B�B	)B�B�B�B	B	B�B�BFB	jB6B	�BtB	B	B		B	2B	mB�B�B�B�BB{B�B�B�B	;B�B�B�B�B	jB�B	OB�B	�B�BcB�B�B�B	B�B�B,B�BB	�B�BxB�B�B�BB�B�B�B�B�B�B2BdB�BrB}BYB*BB�B�BXBTB�B�B�B�B	{B\B�BOB�B�BiB	tB�B�BFB�B
B�B�B8B%B9B.B�B6B�B	B B}B�BB�BSBHB,B�BB�B�BwB2B�BjBqBlB�B�BlB�B-B�BBfB�B�B@B�B{B�B�B$BQB�B{BCBBYB�BzB�B�BB�B�BOBQB\B{BxB�B�B�BB�B	�B	�dB	�vB	�JB	�B	٨B	ڈB	��B	ڴB	�B	��B	ڕB	�iB	ڸB	�B	�nB	�aB	�TB	�B	��B	�/B	��B	��B	ڞB	ٿB	�iB	�B	�B	��B	�1B	ڹB	ڍB	�aB	��B	ڰB	�fB	�JB	�/B	�B	�|B	�1B	�B	ڀB	�EB	�dB	�B	�B	�+B	��B	�B	��B	��B	�sB	�vB	�iB	�OB	��B	��B	ڎB	ۑB	��B	�iB	�[B	��B	ۑB	ۄB	۴B	��B	�EB	�eB	�B	�\B	ۑB	ܳB	�zB	�3B	�EB	��B	ܷB	ܪB	�[B	�B	��B	�<B	�B	��B	��B	��B	�;B	�!B	�B	��B	��B	�B	�B	�1B	��B	�aB	��B	�5B	��B	��B	��B	��B	ްB	��B	��B	߈B	�GB	�B	ޱB	��B	�B	��B	��B	�4B	�IB	�,B	�=B	�0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943344434444344443444444433444444444344433444443443444434444444433444444444343444444334443344334434444434334344434344344334444444344443443343344434434433444443443344433344433444443343433444334444333344443333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B	B	B	B	B	B	BBBB
B
BBBB B�B�B�B�B B�B�B�B�B�BF�Bn�BAoB1B&�B-�B88B<RB>[B<OB?dBBsBH�BJ�BL�BN�BK�BL�BL�BL�BT�BZB`&Bb2B`$B[BV�BP�BT�BP�BK�BE�BCxBC{BC{BBtB?^B;GB71B+�B�BZBBB'B0B8B�B�B�MB�mB��B��B��B��B��B��BBKBNBCBNBQB^B	B �B��B�B�B�=B�B��BˬB�TB�B�B��B�_Bw�BP�B�B��B��B��BBnB,B
�oB
�B
ʢB
�B
�~B
��B
q�B
^B
<HB
(�B
�B
CB
 �B	�)B	�%B	��B	��B	r�B	^B	D{B	+�B	NB��B��B�ZB�4B�B�B��B��BɘB�B��B�aB�9B�"B��Bw�BnuBiTBfDBgLBgKBp�BnuBnsBs�Bp�Bt�B��B�B�?B�}B��B��B��B��B��B��B�oB�RB�B�B�3B�>B�DB�?B�2B� B|�Bp�BmmBnuBnuBmmBljBj[BgIBf@BfBBe<Bd4B`Be<BgFBgGBa&BX�BP�BJ�BF�BE{BDwBCqBAcB>RB<FB<DB:8B;@B:9B:7B<DB=KB>PBAcBE|BJ�BL�BK�BL�BK�BL�BN�BQ�BU�BV�BV�BV�BT�BW�B\B]
B]
B]
B^B_B`Ba"Bb)Be9BhMBhMBf<BhKBe:Bc,B_BY�BT�BP�BM�BN�BH�BDuBDwBCnBCmBG�BG�BG�BG�BJ�BK�BK�BK�BN�BO�BO�BQ�BS�BU�BY�BZ�B]B^B_B`B`Ba!Be;Bp~By�Bx�By�By�B{�B�B��B��B�B�B�B�B�%B�-B�3B�9B�SB�sB�yB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�0B�7B�IB�hBƀBɑBʖB̥BͪBмB��B��B�!B�-B�B��B��B	%B	2B	>NB	?WB	@^B	BgB	H�B	J�B	K�B	L�B	N�B	Q�B	U�B	T�B	R�B	O�B	O�B	O�B	O�B	P�B	V�B	Z�B	]B	^B	_B	aB	d3B	b%B	X�B	P�B	Q�B	Y�B	]B	c-B	c,B	^B	U�B	N�B	M�B	S�B	W�B	_B	Z�B	Y�B	[�B	b$B	f=B	gCB	gBB	iSB	k[B	lbB	k\B	jUB	iNB	k^B	nqB	p{B	r�B	s�B	y�B	|�B	|�B	}�B	�B	��B	�B	�B	�B	�B	�"B	�9B	�NB	�ZB	�_B	�^B	�cB	�jB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�#B	�.B	�:B	�HB	�OB	�NB	�OB	�SB	�\B	�_B	�nB	�rB	�xB	�~B	ȋB	ɓB	ʖB	ʖB	̥B	̦B	ͩB	ΰB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�G�O�B	�B	��B
�B
�B
�B
$�B
+�B
1�B
8�B
@�B
E`B
KB
O�B
VxB
YqB
_B
d�B
i�B
n�B
s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bu�B�xG�O�G�O�G�O�B\ G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�B:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�B�+A�G�O�G�O�G�O�G�O�G�O�B�QG�O�G�O�A�>�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�%B�iG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��G�O�BE�G�O�G�O�G�O�G�O�G�O�G�O�B�B�0G�O�G�O�G�O�A�<�B��G�O�G�O�B�vB.sG�O�G�O�B�ZG�O�G�O�G�O�G�O�G�O�B�YG�O�BxB�zG�O�A���G�O�G�O�G�O�BG�O�B%�G�O�G�O�A�G�O�G�O�BљB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�BѠG�O�G�O�G�O�G�O�B� G�O�G�O�Aԅ�B�G�O�A�@qAŊ:G�O�G�O�G�O�BG�O�G�O�B�G�O�G�O�BӱA�TvG�O�G�O�G�O�G�O�G�O�B
��G�O�G�O�A�f{B��G�O�G�O�G�O�BӓBԼB$�G�O�G�O�G�O�B�~B�~G�O�G�O�G�O�G�O�G�O�B�tB�G�O�B�~G�O�A�!B��G�O�G�O�G�O�B�dB��G�O�G�O�G�O�G�O�A�oA���B�~B �G�O�G�O�G�O�G�O�BՄA�ƴA�}�A�_�B�5B�4G�O�B�+B7YB��B��B�B��G�O�B�B�BӫB�!B�;B�AB��B�2B�EB�B�zB��B�nB��B�6B��B��B�tB�gB�VB�;BמB�]B� B԰BөB��BճB��B�3BׄB�sB�B֒BԴBՂB��B�%B�B֔B�1B��B�B�)B��B�B�'B�zB֏B֬BԃB��B�kBպB��BԫB�)B� BԚB� B؃B؅B؜B��B�QB��B֠BԠBֽBـB�SB�?B��B�DB��B֠B��B��B�cB��B�vB�CB֌BԩB׸B��B؃B��B�hBتB�aB�oBٔB�aB�MB��BәB��B��B՚B�B�|BՂB�B�BלB��B��B�kB�LB�;BӱB�B�gBֈB�TB�%B�<BԚB��B�AB��B�-B��B�hB�hBֈB��B�AB�{BٌB�pB�.B�<B��B�MB�XB�pB�B�.B�DB�B��B��B�aB�LB�.B�B��B�kB�cBպB�B�AB֪B�bB�ZB�B��B�gB��B�QB�B֗B�.BկB��B�LB�B�B�iB�QBԴB��BԼB�8B֏BըB�kB��B՜B�<BըBխB�IBըBՉB��B�1B�5B�VB��BـBչB� BՉBՂB��B��B�.B��B�B�LB֠B�BԋB�BըB֠B�B�B�B�B� B�-B��BԞB��B�2B�PB�B�B�6B�OB�BըBٻB��B�.B�2B��BչBՒBօBըBԋB��B��B�lB��BչBօB��BչB�SB�MB�LB��B�)BױBչBչB�DBչBչB�]BՒB��BՒB՚BלBشB؃B�IB��B��B�B�B�|B��B��B�2B��B��BԦB�BնB�dB� B��B�?B֗B֗B�,B��BվB�zB�zB�GBմB�MBչB�XB�%B��B�B�bB�BգB�.B��BՉB�:B��B��B	,B	fB�B�B	B�B�B	�B�B�B�B�B�BFB_B�B�B�BUBaB�B	;BB�B	"B�B?B�BNBB�B�B�B	B�B�B	%B�B	BmB	B�BkB	B�B�BzB	B�B�B�B,B	QBB	�B]B�B�B�B	B	SB�BzB�B�B�BeB�B�B�B	 B�B�BpBiB	KB�B	5B�B	�B�BIB�B�B�B�B�B�BB�B B	�B�B_BkB�B�B�B�B�B�B�B�B}BBIB�B[BeB?BB�B�B�B?B9B�B�B�B�B	bBABwB6B�B�BQB	[B�B�B-B�B
BuB}BB
BBB�BB�B�BBaB�B�B�B:B.BB�B�B�B�B]BB�BPBWBUBiB�BQB�BB�B�BNB�B�B)B�BcB�B�BB8B�BaB,BB:B~BaBjB�BB�B�B7B7BDBaB_B�BuB�BhB�B	��B	�:B	�LB	�#B	��B	ـB	�_B	ڸB	ډB	��B	��B	�jB	�AB	ڒB	��B	�EB	�7B	�,B	��B	��B	�B	ڽB	ڢB	�wB	ٕB	�BB	��B	��B	ڞB	�
B	ڐB	�fB	�9B	ڴB	ڈB	�>B	� B	�B	��B	�TB	�	B	��B	�VB	�B	�:B	��B	��B	�B	ڪB	��B	ڮB	ڢB	�JB	�LB	�@B	�%B	ڼB	ڮB	�eB	�hB	ښB	�@B	�3B	��B	�jB	�[B	ۋB	ۮB	�B	�:B	��B	�3B	�jB	܊B	�QB	�
B	�B	��B	܌B	܁B	�1B	��B	ܙB	�B	��B	ܻB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�	B	��B	�9B	��B	�B	��B	��B	��B	ްB	ކB	ޘB	ޗB	�`B	�B	��B	މB	��B	��B	��B	ޡB	�B	�!B	�B	�B	�	B	,B	fB�B�B	B�B�B	�B�B�B�B�B�BFB_B�B�B�BUBaB�B	;BB�B	"B�B?B�BNBB�B�B�B	B�B�B	%B�B	BmB	B�BkB	B�B�BzB	B�B�B�B,B	QBB	�B]B�B�B�B	B	SB�BzB�B�B�BeB�B�B�B	 B�B�BpBiB	KB�B	5B�B	�B�BIB�B�B�B�B�B�BB�B B	�B�B_BkB�B�B�B�B�B�B�B�B}BBIB�B[BeB?BB�B�B�B?B9B�B�B�B�B	bBABwB6B�B�BQB	[B�B�B-B�B
BuB}BB
BBB�BB�B�BBaB�B�B�B:B.BB�B�B�B�B]BB�BPBWBUBiB�BQB�BB�B�BNB�B�B)B�BcB�B�BB8B�BaB,BB:B~BaBjB�BB�B�B7B7BDBaB_B�BuB�BhB�B	��B	�:B	�LB	�#B	��B	ـB	�_B	ڸB	ډB	��B	��B	�jB	�AB	ڒB	��B	�EB	�7B	�,B	��B	��B	�B	ڽB	ڢB	�wB	ٕB	�BB	��B	��B	ڞB	�
B	ڐB	�fB	�9B	ڴB	ڈB	�>B	� B	�B	��B	�TB	�	B	��B	�VB	�B	�:B	��B	��B	�B	ڪB	��B	ڮB	ڢB	�JB	�LB	�@B	�%B	ڼB	ڮB	�eB	�hB	ښB	�@B	�3B	��B	�jB	�[B	ۋB	ۮB	�B	�:B	��B	�3B	�jB	܊B	�QB	�
B	�B	��B	܌B	܁B	�1B	��B	ܙB	�B	��B	ܻB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�	B	��B	�9B	��B	�B	��B	��B	��B	ްB	ކB	ޘB	ޗB	�`B	�B	��B	މB	��B	��B	��B	ޡB	�B	�!B	�B	�B	�	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999943344434444344443444444433444444444344433444443443444434444444433444444444343444444334443344334434444434334344434344344334444444344443443343344434434433444443443344433344433444443343433444334444333344443333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.31 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649322020083116493220200831164932202008311649322020083116493220200831164932202008311649322020083116493220200831164932202008311649322020083116493220200831164932AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817182019021918171820190219181718    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817182019021918171820190219181718  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817182019021918171820190219181718  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649322020083116493220200831164932  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                