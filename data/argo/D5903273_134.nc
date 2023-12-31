CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  H   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:09Z creation      
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
_FillValue                  0 �4Argo profile    3.1 1.2 19500101000000  20190219181709  20200831164908  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�:m	��@�:m	��@�:m	��111 @�:�s�R@�:�s�R@�:�s�R@6E�Q�@6E�Q�@6E�Q��cN~��"��cN~��"��cN~��"�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�  @�  @���A   A@  A`  A�  A�  A�33A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dt� Dy{�D�
�D�4{D��)D���D���D�O\D��)D�˅D�RD�>D�s�Dǻ�D��\D�3�D�{�D��fD��D�ED�HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����;L�ͽ��;L�;L�;L�;����L�;L�;L��>L��>L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�;L�;L�;��������L�;L�ͽ��;L�;L�;L�;L�;L�;����L�;L��=���    �L�;L�;L�ͽ��;L�;L�;L��    ���;L�ͽ���    ���;L��    ���;L�;L�;L�ͽ��;L�;L�;������ͽ��;L�;L�;L�;L�;L�ͽ��;L�;����L�;L�;L�;L�;L��    �L�;L�;L��=���=��;L�;L�ͽ��;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��;����L��    ���;L�;����L�;L�ͽ��;L�;L�ͽ���    �L�;L�ͽ��ͽ��;L�;L�;L��    �L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�ͽ��;L�;L�;L�;L�;L�;L�;L�ͽ��;���    �L�;L��=��ͽ��;L�;L�;L��    �L�;L�;L�;����L��=���=���    >L�;L�;L�ͽ��ͽ��ͽ��ͽ��;L�;������ͽ���=��;L�;L�;L�;L�ͽ��ͽ��;L��=��;L�;L�;L�ͽ��;L�ͽ���    =��ͽ��;L�ͽ��ͽ��;L�ͽ��ͽ��ͽ��;L�ͽ��ͽ��;L��    ���ͽ��;L�ͽ��ͽ���    ���;L�ͽ���    ����    =���=���=���=���>L��>L��>���>L��=���    =���>���>L��    =���        =���=���    =���=���=���=���>L��>L��    =���=���=���=���>L�ͽ���>L�ͽ���=���                =���=���>L��=���    =���=���=���=���=���=���=���    ����=���>L��=���=���        ����=���=���=���    >L��        =���    =���=���=���>L��        =���=���=���=���=���=���    =���>���=���=���        =���=���=���>L��    =���>L��>L��=���    =���=���    ����    =���                =���>L��            =���=��ͽ��ͽ���    ����    ����        �L��=���    =���>L��=���>L��=���=���>L��=���=���>L��    ����=���    >L��            =���    =���=���>L��>L��=���=���=���>L��>L��>L��=���        =���=���    =���                    >L��=���=���=���    >L��    >���>���=���    ���ͽ���=���=��ͽ���>L�ͽ���    =���=���=���    ����                ����=��ͽ��ͽ���=���                    ����    =���=��ͽ���=���=���>L��>L��>���>���>���>���>���>���?   ?   ?��?333?L��?333?333?333?L��?fff?fff?�  ?�  ?�  ?�  ?���?�  ?���?���?���?�33?�ff?���?�ff?�33?�33?�ff?�33?�33?�33?�33?�  ?�  ?�  ?�  ?�  ?���?ٙ�?���?���?ٙ�?ٙ�?ٙ�?�ff?�ff?�33?�ff?�ff?�33?�33?�33?�33?�33@   @   @   @   @ff@��@��@��@33@33@33@��@��@��@   @   @   @&ff@,��@,��@333@9��@@  @Fff@L��@L��@S33@S33@Y��@`  @fff@fff@l��@s33@y��@�  @�  @�  @�ff@���@���@���@�  @�  @�33@���@���@���@�  @�33@�ff@���@���@�  @�33@���@���@���@�33@�ff@ə�@���@�  @�33@ٙ�@���@�  @�33@�ff@陚@�  @�33@�ff@���A   A��A33AffA  A	��A33A��A  A��A33AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A6ffA8  A;33A<��A>ffAA��AC33AD��AH  AI��AK33ANffAP  AS33AT��AVffAX  A[33A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��As33At��Ax  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A�ffA�33A���Aə�A�33A���A͙�A�33A�  Aљ�A�ffA�  A���A�ffA�33A�  Aٙ�A�ffA�  Dp�fDp��Dp��Dp� Dp��Dp�3Dp� Dp�fDp�3DpٚDp�fDp��Dp�3Dq  DqfDq3Dq�Dq&fDq,�Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��DrfDr�Dr�Dr  Dr&fDr33Dr9�DrFfDrL�DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Ds  DsfDs3Ds�Ds&fDs,�Ds33Ds@ DsFfDsS3DsY�DsffDsl�Dsy�Ds� Ds��Ds�3Ds��Ds�fDs�3Ds��Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt33Dt9�DtFfDtL�DtY�Dt` DtffDts3Dty�Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt�fDt��Dtٚ@,��@,��@333@9��@@  @Fff@L��@L��@S33@S33@Y��@`  @fff@fff@l��@s33@y��@�  @�  @�  @�ff@���@���@���@�  @�  @�33@���@���@���@�  @�33@�ff@���@���@�  @�33@���@���@���@�33@�ff@ə�@���@�  @�33@ٙ�@���@�  @�33@�ff@陚@�  @�33@�ff@���A   A��A33AffA  A	��A33A��A  A��A33AffA  A��A33A��A   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A6ffA8  A;33A<��A>ffAA��AC33AD��AH  AI��AK33ANffAP  AS33AT��AVffAX  A[33A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��As33At��Ax  Ay��A{33A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A�ffA�33A���Aə�A�33A���A͙�A�33A�  Aљ�A�ffA�  A���A�ffA�33A�  Aٙ�A�ffA�  Dp�fDp��Dp��Dp� Dp��Dp�3Dp� Dp�fDp�3DpٚDp�fDp��Dp�3Dq  DqfDq3Dq�Dq&fDq,�Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��DrfDr�Dr�Dr  Dr&fDr33Dr9�DrFfDrL�DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3DrٚDr� Dr��Dr�3Ds  DsfDs3Ds�Ds&fDs,�Ds33Ds@ DsFfDsS3DsY�DsffDsl�Dsy�Ds� Ds��Ds�3Ds��Ds�fDs�3Ds��Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt33Dt9�DtFfDtL�DtY�Dt` DtffDts3Dty�Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt�fDt��DtٚG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@:�G@��
@��
A Q�A!�AA�Aa�A���A���A�(�A���A���A���A�A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�HDq�Dq��Dr�Dr��Ds�Ds�HDt�Dt��Dy�3D�fD�8RD�� D�ȤD��\D�S3D�� D��\D�)D�A�D�w�Dǿ�D��3D�7�D�\D��=D��D�H�D�D�ǮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<��	���<��	����������8Q콣�������>��
>��
������������������������<��	<��	������<��	���������������<��	<��	����������������������8Q�8Q콣����<��	����������������8Q콣����>aG�=����������<��	���������=�<��	���<��	=�<��	���=�<��	���������<��	�������8Q�<��	<��	���������������<��	����8Q콣�������������=����������>aG�>aG�������<��	������������<��	������������<��	�8Q콣�=�<��	����8Q콣����<��	������<��	=�������<��	<��	���������=����������<��	������������������<��	���<��	���������������������<��	�8Q�=�������>aG�<��	���������=�����������8Q콣�>aG�>aG�=�>��
������<��	<��	<��	<��	����8Q�<��	<��	>aG�������������<��	<��	���>aG����������<��	���<��	=�>aG�<��	���<��	<��	���<��	<��	<��	���<��	<��	���=�<��	<��	���<��	<��	=�<��	���<��	=�<��	=�>aG�>aG�>aG�>aG�>��
>��
>�
>>��
>aG�=�>aG�>�
>>��
=�>aG�=�=�>aG�>aG�=�>aG�>aG�>aG�>aG�>��
>��
=�>aG�>aG�>aG�>aG�>��
<��	>��
<��	>aG�=�=�=�=�>aG�>aG�>��
>aG�=�>aG�>aG�>aG�>aG�>aG�>aG�>aG�=�<��	>aG�>��
>aG�>aG�=�=�<��	>aG�>aG�>aG�=�>��
=�=�>aG�=�>aG�>aG�>aG�>��
=�=�>aG�>aG�>aG�>aG�>aG�>aG�=�>aG�>�
>>aG�>aG�=�=�>aG�>aG�>aG�>��
=�>aG�>��
>��
>aG�=�>aG�>aG�=�<��	=�>aG�=�=�=�=�>aG�>��
=�=�=�>aG�>aG�<��	<��	=�<��	=�<��	=�=����>aG�=�>aG�>��
>aG�>��
>aG�>aG�>��
>aG�>aG�>��
=�<��	>aG�=�>��
=�=�=�>aG�=�>aG�>aG�>��
>��
>aG�>aG�>aG�>��
>��
>��
>aG�=�=�>aG�>aG�=�>aG�=�=�=�=�=�>��
>aG�>aG�>aG�=�>��
=�>�
>>�
>>aG�=�<��	<��	>aG�>aG�<��	>��
<��	=�>aG�>aG�>aG�=�<��	=�=�=�=�<��	>aG�<��	<��	>aG�=�=�=�=�=�<��	=�>aG�>aG�<��	>aG�>aG�>��
>��
>�
>>�
>?�?�?�?�?�R?�R?8Q�?Q�?k�?Q�?Q�?Q�?k�?��\?��\?�\)?�\)?�\)?�\)?�(�?�\)?�(�?�(�?���?\?�?���?�?\?\?�?\?\?\?\?�\)?�\)?�\)?�\)?�\)?�(�?���?�(�?�(�?���?���?���?�?�@G�?�?�@G�@G�@G�@G�@G�@�@�@�@�@z@z�@z�@z�@�G@�G@�G@!G�@!G�@!G�@'�@'�@'�@.z@4z�@4z�@:�G@AG�@G�@Nz@Tz�@Tz�@Z�G@Z�G@aG�@g�@nz@nz@tz�@z�G@���@��
@��
@��
@�=p@�p�@�p�@���@��
@��
@�
=@�p�@���@���@��
@�
=@�=p@�p�@���@��
@�
=@�p�@���@���@�
=@�=p@�p�@У�@��
@�
=@�p�@��@��
@�
=@�=p@�p�@��
@�
=@�=p@�p�A�A�A�AQ�A	�A�A�A�RA�A�A�AQ�A�A�A�A�RA!�A#�A%�A&�RA)�A+�A-�A.�RA1�A3�A5�A8Q�A9�A=�A>�RA@Q�AC�AE�AF�RAI�AK�AM�APQ�AQ�AU�AV�RAXQ�AY�A]�A^�RA`Q�Ac�Ae�AhQ�Ai�Am�An�RApQ�As�Au�Av�RAy�A{�A}�A�(�A���A�A�\)A�(�A���A��]A�\)A�(�A���A�A�\)A�(�A���A�A��]A�(�A���A�A��]A�\)A�(�A�A��]A�\)A�(�A���A��]A�\)A�(�A���A�A��]A�(�A���A�A��]A�(�A���A�A��]A�(�A���A�A�\)A�(�A���A��]A�\)A���A�A��]A�(�A���A��]A�\)A���A�A�\)A�(�A�A��]A�\)A���A]A�\)A�(�A�A�\)A�(�A�Aʏ]A�(�A�AΏ]A�(�A���Aҏ]A�\)A���A�A�\)A�(�A���Aڏ]A�\)A���Dp�Dp�{Dp�HDp��Dp�{Dp��DpǮDp�Dp��Dp�HDp�Dp�{Dp��Dq�DqDq�Dq!HDq.Dq4{DqAHDqG�DqT{DqZ�DqaHDqnDqt{Dq�HDq��Dq�{Dq��Dq��Dq�Dq��Dq�HDqǮDq�{Dq��Dq�Dq�Dq��DrHDrDr{Dr!HDr'�Dr.Dr:�DrAHDrNDrT{DraHDrg�Drt{Drz�Dr��Dr�Dr�{Dr�HDr��Dr�{Dr��DrǮDr�Dr��Dr�HDr�Dr�{Dr��Ds�DsDs�Ds!HDs.Ds4{Ds:�DsG�DsNDsZ�DsaHDsnDst{Ds�HDs��Ds�{Ds��Ds�HDs�Ds��Ds�HDsǮDs�{Ds��Ds�Ds�Ds��DtHDt�Dt{Dt�Dt'�Dt.Dt:�DtAHDtNDtT{DtaHDtg�DtnDtz�Dt�HDt�Dt�{Dt�HDt��Dt�Dt��Dt�HDt�Dt�{Dt�H@4z�@4z�@:�G@AG�@G�@Nz@Tz�@Tz�@Z�G@Z�G@aG�@g�@nz@nz@tz�@z�G@���@��
@��
@��
@�=p@�p�@�p�@���@��
@��
@�
=@�p�@���@���@��
@�
=@�=p@�p�@���@��
@�
=@�p�@���@���@�
=@�=p@�p�@У�@��
@�
=@�p�@��@��
@�
=@�=p@�p�@��
@�
=@�=p@�p�A�A�A�AQ�A	�A�A�A�RA�A�A�AQ�A�A�A�A�RA!�A#�A%�A&�RA)�A+�A-�A.�RA1�A3�A5�A8Q�A9�A=�A>�RA@Q�AC�AE�AF�RAI�AK�AM�APQ�AQ�AU�AV�RAXQ�AY�A]�A^�RA`Q�Ac�Ae�AhQ�Ai�Am�An�RApQ�As�Au�Av�RAy�A{�A}�A�(�A���A�A�\)A�(�A���A��]A�\)A�(�A���A�A�\)A�(�A���A�A��]A�(�A���A�A��]A�\)A�(�A�A��]A�\)A�(�A���A��]A�\)A�(�A���A�A��]A�(�A���A�A��]A�(�A���A�A��]A�(�A���A�A�\)A�(�A���A��]A�\)A���A�A��]A�(�A���A��]A�\)A���A�A�\)A�(�A�A��]A�\)A���A]A�\)A�(�A�A�\)A�(�A�Aʏ]A�(�A�AΏ]A�(�A���Aҏ]A�\)A���A�A�\)A�(�A���Aڏ]A�\)A���Dp�Dp�{Dp�HDp��Dp�{Dp��DpǮDp�Dp��Dp�HDp�Dp�{Dp��Dq�DqDq�Dq!HDq.Dq4{DqAHDqG�DqT{DqZ�DqaHDqnDqt{Dq�HDq��Dq�{Dq��Dq��Dq�Dq��Dq�HDqǮDq�{Dq��Dq�Dq�Dq��DrHDrDr{Dr!HDr'�Dr.Dr:�DrAHDrNDrT{DraHDrg�Drt{Drz�Dr��Dr�Dr�{Dr�HDr��Dr�{Dr��DrǮDr�Dr��Dr�HDr�Dr�{Dr��Ds�DsDs�Ds!HDs.Ds4{Ds:�DsG�DsNDsZ�DsaHDsnDst{Ds�HDs��Ds�{Ds��Ds�HDs�Ds��Ds�HDsǮDs�{Ds��Ds�Ds�Ds��DtHDt�Dt{Dt�Dt'�Dt.Dt:�DtAHDtNDtT{DtaHDtg�DtnDtz�Dt�HDt�Dt�{Dt�HDt��Dt�Dt��Dt�HDt�Dt�{Dt�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�1'A�5?A�33A�33A�=qA�G�A�G�A�E�A�E�A�G�A�I�A�G�A�G�A�M�A�K�A�K�A�O�A�M�A�O�A�M�A�Q�A�M�A�M�A�I�A�G�A�E�A�C�A�33A��wA���A�ffA�A��HA�A�~�A��yA��!A�S�A���A���A���A�~�A��A�9XA�
=A���A�&�A�x�A�=qA�p�A�1'A�"�A��!A��jA��+A�|�A�l�A���A��A�t�A�VA�9XA�A��mA�dZA��FA��FA��A���A�A�=qA�hsA�r�A��\A���A�%A��PA��PA�VA��yA���A���A���A�dZA�oA��A�p�A��A�"�A��A�A��A��HA���A��!A���A��PA�t�A�
=A�ZA�\)A���A�+A���A��-A�oA�S�A��PA��RA���A�bA���A�+A�{A��A�XA�-A��hA��+A}��AzVAwp�Au��Ap��AjZAh��AdȴAa�A`��A\��AY��AY�AV�AT~�AS;dAQ��AO�FAN�`AMAMVAL�AK�AIƨAH�AFȴADȴADn�AD(�AC�AC�wAC&�A@�A?��A<�/A9/A7|�A7`BA7?}A6jA5?}A41'A3�PA2�A0�jA.�9A,�A+�
A+hsA)�A(�A&ZA$VA#
=A!�hA ��A $�Ap�A�hA9XA�wAffA��A�AA�AS�A �AC�AM�A��A�!A��A|�A�/AVA�A�\A�A��A|�A
=A
��A
�uA
Q�A
1'A	��A��A��A�FA��A�7A��A�TA�HA-A��A+@�$�@��`@�b@��@�"�@���@�M�@�G�@�@�P@�M�@��@�G�@��@��^@�?}@�9X@�ƨ@�t�@�h@�@��@�ff@�M�@��/@� �@�+@���@��@�j@��@ߕ�@�
=@���@��
@�ȴ@ڰ!@�^5@���@�/@�Ĝ@�j@׮@Չ7@�r�@���@�j@��@�v�@ɲ-@��@��
@�M�@ēu@�"�@���@���@���@�1@�
=@�-@��9@�"�@���@�v�@�n�@�=q@��^@��`@���@�@�^5@���@��h@���@��@�1@��@��@��y@�v�@�5?@���@���@�j@�  @��P@�S�@�@��+@�J@��@��7@�Ĝ@�@��@�?}@��/@�9X@�r�@��@�C�@���@��\@��@�5?@�$�@�-@���@�?}@��@��`@���@���@���@�  @��P@�dZ@�\)@��@��+@��@��@�@���@���@�Q�@�Q�@��@���@��@� �@�@��H@��y@�v�@��+@�V@�`B@�V@��@���@�9X@��@�Z@�Ĝ@��u@�r�@�Z@�I�@�(�@�b@��@���@�dZ@�S�@�;d@�+@�@���@�n�@�V@�$�@���@���@��@�/@���@���@�Q�@�b@��;@��@�\)@��y@��!@��R@��!@��@�`B@���@��u@�z�@� �@�b@��@�bN@�1'@���@���@���@��P@�dZ@�+@��\@���@�`B@�?}@��/@��@�Q�@��@��@���@��R@���@�{@��@���@���@�p�@��@��9@�Z@�(�@�ƨ@�l�@��@�33@�;d@�o@��H@�v�@��T@�@���@��7@�G�@�7L@���@���@�j@�I�@�I�@�I�@�I�@�A�@�9X@�1'@�9X@�1'@�(�@�(�@�1@��;@��w@���@���@�t�@�S�@�S�@�S�@��@�o@��y@��R@�$�@��@��@��#@��@���@��`@��j@���@��@�z�@�z�@�r�@�j@�j@�A�@��!@}��@v^5@n��@e�=@[33@T]d@N!�@F��@?;d@8*�@2l�@-��@)q@$�/@W?@�T@{@	@2�@6zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A��9A�A���A�v�A�(�A�E�A�(�A�JA��7A���A��#A��FA��#A�z�A�XA�hsA���A��A��9A���A�7LA���A�JA�G�A�ffA��DA�$�A��A���A��A�1A��A�XA���A�M�A�n�A�~�A���A��/A��PA�"�A�x�A��A�9XA�1A��A�=qA�33A��A��yA��A��yA�ffA��FA���A��
A��-A�C�A��A��A���A��DA�XA���A���A��;A�ffA���A���A��A��;A���A�A�A�jA�ZA���A��A��A�I�A��A�E�A�+A���A��-A�I�A�VA�dZA��DA��A��\A�A�t�A���A�\)A��A�oA���A��A��A��A�O�A��A�oA��A�A��^A�ȴA�G�A�jA�oA�bA��A��HA���A���A���A���A��A�(�A��#A���A�  A�bA�I�A��mA�|�A�
=A�JA�1A���A�M�A��A��A�O�A�t�A�E�A�S�A���A�x�A�"�A�ƨA�VA��#A���A��PA�9XA�;dA�A�A���A��/A���A��7A�5?A�{A�|�A�XA�A���A� �A�E�A���A���A��A�1A��yA��/A��;A�  A���A�JA�ƨA�VA��/A�
=A�hsA�  A�bA�O�A��A���A�A�ȴA��!A��TA�XA���A���A���A�  A��`A�=qA���A�%A�x�A�5?A�dZA�JA�1A���A�5?A��uA�%A�$�A�|�A�1A�%A�A�%A��yA��A�
=A�JA�
=A�O�A��A�bA�JA���A��A�A�%A�bA�JA��A��A��A��A��A�oA�oA�VA��A��A��A�VA�A���A�%A�  A�A�  A�  A���A�A�oA�VA�oA��A��A��A�{A�{A��A��A�bA��A�bA���A�{A���A�A�VA�JA�1A�bA�JA�
=A�1A�bA�  A�VA�VA�VA�
=A�
=A�%A�
=A�oA�{A�oA�oA���A�{A�bA�bA��A�bA�oA�bA�{A�bA�VA��A�JA�bA�bA��A�oA�%A�JA�JA�
=A�VA�JA�  A�JA�
=A��A�A�  A�%A�%A�{A��A�bA�
=A�oA��A��A�oA�1A�{A�bA�VA�A�oA�{A�oA�bA�{A�A�
=A�{A��A�VA�{A�{A�{A�%A�
=A�bA�JA�bA�1A�A��A�oA���A��A��A�{A��A��A�{A�{A�bA�bA�bA�{A�bA�A�JA�%A�VA�
=A��
A�JA�JA�JA�VA�oA��A��A�VA�bA�JA�bA��A�{A��A�JA��A��A��A��A� �A��A��A��A��A��A�"�A��A��A��A�$�A�$�A�$�A�"�A�$�A�$�A�"�A���A�"�A��A�"�A�$�A�"�A�$�A�A�$�A�&�A�"�A�$�A�$�A� �A��A��A�"�A�&�A�$�A� �A�  A� �A�(�A�+A�+A�(�A�"�A�"�A�-A�+A�&�A�(�A�/A�-A�+A�(�A�+A�(�A�"�A�$�A�(�A�&�A�&�A�&�A�(�A�&�A�"�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A� �A� �A��A��A�"�A� �A�(�A�(�A�&�A�(�A�(�A�+A�&�A�+A�&�A�&�A�$�A�-A�-A�/A�+A�+A�+A�-A�+A�(�A�+A�+A�+A�(�A�-A�(�A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�&�A�$�A�$�A�$�A�+A�+A�+A�+A�-A�-A�(�A�(�A�+A�-A�+A�-A�/A�1'A�1'A�/A�/A�/A�/A�1'A�/A�/A�-A�/A�1'A�/A�/A�/A�/A�1'A�/A�/A�/A�/A�/A�33A�1'A�1'A�5?A�33A�33A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�5?A�7LA�5?A�33A�33A�5?A�5?A�5?A�5?A�33A�33A�33A�33A�33A�5?A�/A�1'A�1'A�33A�33A�5?A�33A�33A�33A�33A�5?A�7LA�7LA�5?A�1'A�33A�33A�1'A�1'A�33A�1'A�1'A�33A�33A�1'A�1'A�33A�33A�33A�1'A�5?A�33A�33A�5?A�5?A�5?A�33A�7LA�G�A�G�A�G�A�G�A�I�A�I�A�G�A�I�A�G�A�E�A�G�A�E�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�C�A�C�A�E�A�G�A�G�A�I�A�G�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�G�A�C�A�E�A�G�A�G�A�E�A�E�A�E�A�A�A�A�A�A�A�A�A�C�A�C�A�C�A�E�A�C�A�C�A�E�A�E�A�E�A�C�A�G�A�E�A�E�A�E�A�G�A�C�A�G�A�E�A�G�A�C�A�C�A�?}A�E�A�G�A�I�A�I�A�I�A�I�A�M�A�M�A�M�A�M�A�I�A�K�A�G�A�E�A�I�A�G�A�E�A�E�A�M�A�I�A�K�A�K�A�K�A�E�A�E�A�E�A�E�A�G�A�E�A�E�A�G�A�G�A�E�A�E�A�C�A�G�A�C�A�E�@��j@�Ĝ@��j@��j@�Ĝ@�Ĝ@��j@�Ĝ@�Ĝ@�Ĝ@���@���@���@���@���@���@���@���@���@���@���@���@���@�Ĝ@��j@���@���@���@���@��u@��D@��D@��D@��D@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�z�@�z�@�z�@�z�@�z�@�r�@�z�@�z�@�z�@�r�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�bN@�bN@�j@�j@�j@�j@�j@�bN@�j@�r�@�j@�r�@�j@�r�@�r�@�j@�bN@�j@�bN@�bN@�Z@�Q�@�I�@�A�@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�@�(�A�/A�/A�/A�/A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�33A�/A�1'A�1'A�/A�33A�33A�33A�1'A�33A�33A�33A�33A�33A�7LA�5?A�9XA�7LA�5?A�7LA�7LA�7LA�7LA�5?A�5?A�33A�33A�33A�33A�5?A�5?A�5?A�33A�33A�33A�1'A�1'A�1'A�33A�1'A�5?A�33A�5?A�33A�1'A�33A�1'A�5?A�33A�1'A�33A�33A�33A�33A�33A�1'A�33A�33A�33A�5?A�7LA�33A�1'A�33A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�;dA�I�A�I�A�I�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�I�A�G�A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�C�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�E�A�E�A�C�A�C�A�A�A�C�A�C�A�E�A�E�A�E�A�C�A�E�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�G�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�G�A�I�A�G�A�I�A�I�A�I�A�I�A�M�A�M�A�K�A�K�A�G�A�G�A�I�A�G�A�E�A�G�A�G�A�K�A�I�A�I�A�K�A�K�A�I�A�I�A�G�A�E�A�E�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�E�A�E�A�E�@��j@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@���@���@���@���@���@���@���@���@���@���@���@���@���@���@�Ĝ@��9@���@���@���@���@��u@��u@��u@��D@��D@��@��@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�j@�j@�j@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�r�@�j@�j@�bN@�bN@�j@�j@�j@�j@�j@�bN@�j@�r�@�r�@�j@�r�@�j@�r�@�j@�j@�j@�j@�bN@�bN@�Q�@�Q�@�I�@�A�@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999A�1'A�1'A�5?A�33A�33A�=qA�G�A�G�A�E�A�E�A�G�A�I�A�G�A�G�A�M�A�K�A�K�A�O�A�M�A�O�A�M�A�Q�A�M�A�M�A�I�A�G�A�E�A�C�A�33A��wA���A�ffA�A��HA�A�~�A��yA��!A�S�A���A���A���A�~�A��A�9XA�
=A���A�&�A�x�A�=qA�p�A�1'A�"�A��!A��jA��+A�|�A�l�A���A��A�t�A�VA�9XA�A��mA�dZA��FA��FA��A���A�A�=qA�hsA�r�A��\A���A�%A��PA��PA�VA��yA���A���A���A�dZA�oA��A�p�A��A�"�A��A�A��A��HA���A��!A���A��PA�t�A�
=A�ZA�\)A���A�+A���A��-A�oA�S�A��PA��RA���A�bA���A�+A�{A��A�XA�-A��hA��+A}��AzVAwp�Au��Ap��AjZAh��AdȴAa�A`��A\��AY��AY�AV�AT~�AS;dAQ��AO�FAN�`AMAMVAL�AK�AIƨAH�AFȴADȴADn�AD(�AC�AC�wAC&�A@�A?��A<�/A9/A7|�A7`BA7?}A6jA5?}A41'A3�PA2�A0�jA.�9A,�A+�
A+hsA)�A(�A&ZA$VA#
=A!�hA ��A $�Ap�A�hA9XA�wAffA��A�AA�AS�A �AC�AM�A��A�!A��A|�A�/AVA�A�\A�A��A|�A
=A
��A
�uA
Q�A
1'A	��A��A��A�FA��A�7A��A�TA�HA-A��A+@�$�@��`@�b@��@�"�@���@�M�@�G�@�@�P@�M�@��@�G�@��@��^@�?}@�9X@�ƨ@�t�@�h@�@��@�ff@�M�@��/@� �@�+@���@��@�j@��@ߕ�@�
=@���@��
@�ȴ@ڰ!@�^5@���@�/@�Ĝ@�j@׮@Չ7@�r�@���@�j@��@�v�@ɲ-@��@��
@�M�@ēu@�"�@���@���@���@�1@�
=@�-@��9@�"�@���@�v�@�n�@�=q@��^@��`@���@�@�^5@���@��h@���@��@�1@��@��@��y@�v�@�5?@���@���@�j@�  @��P@�S�@�@��+@�J@��@��7@�Ĝ@�@��@�?}@��/@�9X@�r�@��@�C�@���@��\@��@�5?@�$�@�-@���@�?}@��@��`@���@���@���@�  @��P@�dZ@�\)@��@��+@��@��@�@���@���@�Q�@�Q�@��@���@��@� �@�@��H@��y@�v�@��+@�V@�`B@�V@��@���@�9X@��@�Z@�Ĝ@��u@�r�@�Z@�I�@�(�@�b@��@���@�dZ@�S�@�;d@�+@�@���@�n�@�V@�$�@���@���@��@�/@���@���@�Q�@�b@��;@��@�\)@��y@��!@��R@��!@��@�`B@���@��u@�z�@� �@�b@��@�bN@�1'@���@���@���@��P@�dZ@�+@��\@���@�`B@�?}@��/@��@�Q�@��@��@���@��R@���@�{@��@���@���@�p�@��@��9@�Z@�(�@�ƨ@�l�@��@�33@�;d@�o@��H@�v�@��T@�@���@��7@�G�@�7L@���@���@�j@�I�@�I�@�I�@�I�@�A�@�9X@�1'@�9X@�1'@�(�@�(�@�1@��;@��w@���@���@�t�@�S�@�S�@�S�@��@�o@��y@��R@�$�@��@��@��#@��@���@��`@��j@���@��@�z�@�z�@�r�@�j@�jG�O�@��!@}��@v^5@n��@e�=@[33@T]d@N!�@F��@?;d@8*�@2l�@-��@)q@$�/@W?@�T@{@	@2�@6zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A��9A�A���A�v�A�(�A�E�A�(�A�JA��7A���A��#A��FA��#A�z�A�XA�hsA���A��A��9A���A�7LA���A�JA�G�A�ffA��DA�$�A��A���A��A�1A��A�XA���A�M�A�n�A�~�A���A��/A��PA�"�A�x�A��A�9XA�1A��A�=qA�33A��A��yA��A��yA�ffA��FA���A��
A��-A�C�A��A��A���A��DA�XA���A���A��;A�ffA���A���A��A��;A���A�A�A�jA�ZA���A��A��A�I�A��A�E�A�+A���A��-A�I�A�VA�dZA��DA��A��\A�A�t�A���A�\)A��A�oA���A��A��A��A�O�A��A�oA��A�A��^A�ȴA�G�A�jA�oA�bA��A��HA���A���A���A���A��A�(�A��#A���A�  A�bA�I�A��mA�|�A�
=A�JA�1A���A�M�A��A��A�O�A�t�A�E�A�S�A���A�x�A�"�A�ƨA�VA��#A���A��PA�9XA�;dA�A�A���A��/A���A��7A�5?A�{A�|�A�XA�A���A� �A�E�A���A���A��A�1A��yA��/A��;A�  A���A�JA�ƨA�VA��/A�
=A�hsA�  A�bA�O�A��A���A�A�ȴA��!A��TA�XA���A���A���A�  A��`A�=qA���A�%A�x�A�5?A�dZA�JA�1A���A�5?A��uA�%A�$�A�|�A�1A�%A�A�%A��yA��A�
=A�JA�
=A�O�A��A�bA�JA���A��A�A�%A�bA�JA��A��A��A��A��A�oA�oA�VA��A��A��A�VA�A���A�%A�  A�A�  A�  A���A�A�oA�VA�oA��A��A��A�{A�{A��A��A�bA��A�bA���A�{A���A�A�VA�JA�1A�bA�JA�
=A�1A�bA�  A�VA�VA�VA�
=A�
=A�%A�
=A�oA�{A�oA�oA���A�{A�bA�bA��A�bA�oA�bA�{A�bA�VA��A�JA�bA�bA��A�oA�%A�JA�JA�
=A�VA�JA�  A�JA�
=A��A�A�  A�%A�%A�{A��A�bA�
=A�oA��A��A�oA�1A�{A�bA�VA�A�oA�{A�oA�bA�{A�A�
=A�{A��A�VA�{A�{A�{A�%A�
=A�bA�JA�bA�1A�A��A�oA���A��A��A�{A��A��A�{A�{A�bA�bA�bA�{A�bA�A�JA�%A�VA�
=A��
A�JA�JA�JA�VA�oA��A��A�VA�bA�JA�bA��A�{A��A�JA��A��A��A��A� �A��A��A��A��A��A�"�A��A��A��A�$�A�$�A�$�A�"�A�$�A�$�A�"�A���A�"�A��A�"�A�$�A�"�A�$�A�A�$�A�&�A�"�A�$�A�$�A� �A��A��A�"�A�&�A�$�A� �A�  A� �A�(�A�+A�+A�(�A�"�A�"�A�-A�+A�&�A�(�A�/A�-A�+A�(�A�+A�(�A�"�A�$�A�(�A�&�A�&�A�&�A�(�A�&�A�"�A�$�A�$�A�$�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A� �A� �A��A��A�"�A� �A�(�A�(�A�&�A�(�A�(�A�+A�&�A�+A�&�A�&�A�$�A�-A�-A�/A�+A�+A�+A�-A�+A�(�A�+A�+A�+A�(�A�-A�(�A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�&�A�$�A�$�A�$�A�+A�+A�+A�+A�-A�-A�(�A�(�A�+A�-A�+A�-A�/A�1'A�/A�/A�/A�/A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�33A�/A�1'A�1'A�/A�33A�33A�33A�1'A�33A�33A�33A�33A�33A�7LA�5?A�9XA�7LA�5?A�7LA�7LA�7LA�7LA�5?A�5?A�33A�33A�33A�33A�5?A�5?A�5?A�33A�33A�33A�1'A�1'A�1'A�33A�1'A�5?A�33A�5?A�33A�1'A�33A�1'A�5?A�33A�1'A�33A�33A�33A�33A�33A�1'A�33A�33A�33A�5?A�7LA�33A�1'A�33A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�;dA�I�A�I�A�I�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�I�A�G�A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�C�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�E�A�E�A�C�A�C�A�A�A�C�A�C�A�E�A�E�A�E�A�C�A�E�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�G�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�G�A�I�A�G�A�I�A�I�A�I�A�I�A�M�A�M�A�K�A�K�A�G�A�G�A�I�A�G�A�E�A�G�A�G�A�K�A�I�A�I�A�K�A�K�A�I�A�I�A�G�A�E�A�E�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�E�A�E�A�E�@��j@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@���@���@���@���@���@���@���@���@���@���@���@���@���@���@�Ĝ@��9@���@���@���@���@��u@��u@��u@��D@��D@��@��@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�j@�j@�j@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�r�@�j@�j@�bN@�bN@�j@�j@�j@�j@�j@�bN@�j@�r�@�r�@�j@�r�@�j@�r�@�j@�j@�j@�j@�bN@�bN@�Q�@�Q�@�I�@�A�@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�A�/A�/A�/A�/A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�33A�/A�1'A�1'A�/A�33A�33A�33A�1'A�33A�33A�33A�33A�33A�7LA�5?A�9XA�7LA�5?A�7LA�7LA�7LA�7LA�5?A�5?A�33A�33A�33A�33A�5?A�5?A�5?A�33A�33A�33A�1'A�1'A�1'A�33A�1'A�5?A�33A�5?A�33A�1'A�33A�1'A�5?A�33A�1'A�33A�33A�33A�33A�33A�1'A�33A�33A�33A�5?A�7LA�33A�1'A�33A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�;dA�I�A�I�A�I�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�I�A�G�A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�C�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�E�A�E�A�C�A�C�A�A�A�C�A�C�A�E�A�E�A�E�A�C�A�E�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�G�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�G�A�I�A�G�A�I�A�I�A�I�A�I�A�M�A�M�A�K�A�K�A�G�A�G�A�I�A�G�A�E�A�G�A�G�A�K�A�I�A�I�A�K�A�K�A�I�A�I�A�G�A�E�A�E�A�E�A�G�A�E�A�G�A�G�A�E�A�E�A�E�A�E�A�E�@��j@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@�Ĝ@���@���@���@���@���@���@���@���@���@���@���@���@���@���@�Ĝ@��9@���@���@���@���@��u@��u@��u@��D@��D@��@��@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�j@�j@�j@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�r�@�j@�j@�bN@�bN@�j@�j@�j@�j@�j@�bN@�j@�r�@�r�@�j@�r�@�j@�r�@�j@�j@�j@�j@�bN@�bN@�Q�@�Q�@�I�@�A�@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�S�>�`@�F =u�=��=�&�=�Z=�Sz>~=@w0�@�I@�:�>0O�?և+@��=�:*=��?�4�=�>(K^@�M�@+k�=��^?��U@�E$>���=���=�:�>�=�@��[@�C=��>Yw�>5*�@���=��Z@b��>:э?���=�u�>Q1f=௣@��>�")=��u>$o@��w@,�c=٩T>�A�?�4@��@�F@ ?>>&k�@�>@�B�@)�>"��?f�@N��@�A�>�b�>�:�@�+@�>>�K�@ �@�EN@�>=� �=���@�]@��h=���>
�m>�@�K^@�9�=�Ks=�8=�M�=�B1?�($@��)=�;�>)a=?�@:>��=��>v�@��Q@�N=��N>:1�@�F@�O@�Jb>2:*@�Go?/�4>tcI>�l�>.�?���@�E9?傪>C��=�\�@,�%@�<6>b�?&�@�@d=��5=\>/�e>�-#@U��@H">�@�@@�Jb@�D�>��@�:@�0@�==�%�>)��@�J@�G�=�|>`p�?���@�3H@�8�=���=��{?�1{>��@���>(�@�8\?C> ��?l��=�i/> 7>��@�Ec?�?�?<?���@�Ks>��@	��@�N�@ ��=ۆn?��@�'g@�J=�iY=��=���=�6P>y�@�R�@�U@�X@�>&h@�^�@�M�?[#y@�Z@�]�>�3@��@�V�@�Ta@�V.=�=�=�|><�F@�@�Qn@�Q�@��S@�V�=�Jw>.��@�Wi@�X:>'��@�Y�@�Xd@�U�>��@h�l?tq�@�W>���?qv�@�X�@�Vm@�U�@�R~@�V.@�ɛ@�Yu@�\�@�Z�?���@�Z�@�Zq@�Z�@���?�"�@�[l@�Yu@�^�@�^�@�^t@�^t@�_@�^�@�\�@�\�@�\@�\�@�_F@�^�@�]y@�\h@�X�@�X@�Y�@�X@�U�@�X@�Xd@�W�@�_@�\�@�^t@�^�@�^�@�]�@�_p@�^�@�`@�^�@�_@�]@�`�@�\�@�U�@�Z�@�V�@�X�@�ZG@�[@�\h@�\@�\>@�Zq@�]y@�]@�ZG@�Z�@�[@�\@�[�@�[�@�\�@�]�@�]�@�]�@�^@�^�@�Y�@�]�@�]y@�_@�_p@�\�@�\�@�]@�\@�[B@�[l@�^@�Z�@�Z�@�[�@�Zq@�Z@�Zq@�Z�@�[@�Zq@�[�@�[�@�T�@�\h@�\�@�]�@�Z@�U@�^ @�^J@�_�@�_�@�]@�]y@�^J@�_@�^t@�^t@�[�@�\�@�\�@�]@�[�@�\�@�]y@�[l@�\>@�Y�@�X@�\@�^t@�^�@�^ @�]y@�^@�]�@�]y@�]@�\@�]O@�\>@�X?1a=@�_p@�`@���@�a@�aR@�^�@�]�@�^t@�^@�]O@�]@�]@�\�@�\�@�\@�[l@�Zq@�X�@�ZG@�Zq@�[l@�\�@�[�@�]@�^t@�`k@�b�@�b�@�]@�\h@�\�@�_@�_@�^�@�`@�]O@�`k@�a�@�bc@�bN@�bc@�bc@�bc@�c^@�bc@�_�@�cs@�aR@�`�@�c @�d@�do@�e@�do@�do@�d@�d@�m3@�c�@�d@�do@�d@�d@�d�@�c @�d�@�d�@�dZ@�d�@�d@�c�@�c@�c @�c @�do@�a�@�^�@�Z@�ek@�g#@�f�@�f�@�f'@�cs@�f�@�gb@�g#@�f�@�f�@�gb@�f{@�f�@�g#@�ff@�e@�d�@�do@�ek@�e�@�e�@�ek@�e�@�e@�d�@�e@�do@�d�@�d�@�do@�d�@�d�@�d�@�d@�c�@�cs@�d@�b�@�do@�d�@�f@�f�@�f{@�f�@�g#@�g@�f�@�f�@�f�@�g@�g�@�hs@�g�@�h@�hs@�g#@�g�@�g�@�h@�f�@�g#@�gw@�gb@�f�@�f{@�g#@�gw@�g@�g#@�f�@�f{@�f�@�f{@�f'@�f�@�g#@�f�@�h4@�h�@�h�@�h�@�h�@�h4@�h4@�g�@�h�@�i�@�i/@�i�@�in@�i�@�i�@�i�@�i�@�i�@�i�@�i�@�i�@�i�@�j+@�i�@�j+@�i�@�j+@�i�@�j+@�j+@�j@�j@�j@�j�@�j�@�k�@�k�@�k�@�k�@�k�@�k�@�l7@�m3@�l�@�m3@�m�@�m�@�m�@�m�@�m�@�m3@�l�@�l�@�l�@�l�@�l7@�l�@�l�@�l�@�l�@�l�@�l�@�l�@�l�@�l�@�l7@�l�@�l�@�m3@�m3@�m�@�m�@�m�@�m�@�m�@�m�@�nD@�nD@�m�@�mH@�m�@�m�@�mH@�nD@�n�@�m�@�m�@�nD@�m�@�n�@�nD@�n�@�o?@�oT@�n�@�o�@�o�@�p�@�p�@�p�@�q@�qL@�t�@�w\@�wp@�w�@�w�@�w\@�w\@�w\@�wp@�w@�v�@�w\@�w\@�w�@�wp@�w\@�w�@�w�@�x@�x@�x-@�x@�x@�w\@�w\@�w\@�w�@�xW@�xW@�xW@�x�@�x�@�w�@�w�@�x@�w�@�x-@�xW@�xW@�xW@�xW@�xW@�x�@�x�@�x�@�x�@�x�@�x�@�xl@�xl@�xl@�xl@�wp@�w�@�x-@�x-@�x�@�x�@�x@�x�@�x�@�z@�y�@�y�@�yh@�y�@�z@�y�@�y�@�zc@�z@�z:@�z�@�z�@�zc@�z@�yh@�z�@�z�@�{�@�|p@�|�@�|F@�|F@�}k@�}k@�}k@�}@�{�@�|p@�{�@�{�@�{�@�|@�|�@�},@�|�@�}�@�}�@�}�@�|1@�|1@�|�@�|1@�|�@�|�@�|�@�|�@�}@�}@�}@�},@�}�@�}�@�}V@�~@P��@P�E@P��@P�E@Pؙ@Pؙ@P�@@P��@P�<@P��@P�3@P܇@P��@P��@P��@P�/@P݃@P�/@P�/@P��@P��@Pی@P��@Pؙ@P�M@P�$@Pզ@P�R@PԀ@P�@P��@PӮ@Pӄ@P�@P҉@P�@P��@Pҳ@Pҳ@Pҳ@Pҳ@P�1@Pҳ@P҉@Pҳ@P�1@Pҳ@P҉@Pӄ@P�1@P�1@P�[@P��@PӮ@P�,@P�@P�,@P�@PӮ@PӮ@PӮ@Pӄ@P�[@P��@Pҳ@P�1@P�[@P�@PԪ@PԀ@P��@P�V@P�,@P�@P�,@P�@PԀ@PԀ@PԀ@PԪ@PԪ@P�(@PԪ@P��@P�V@PԀ@P�R@P��@P��@P��@P�|@P��@P��@P�w@P�$@P��@P�w@P�w@P�w@P�$@P�$@P��@P�|@P�,@P��@P҉@P�c@P�>@P��@P�p@P�@P��@P��@P͟@P��@P�!@P�!@P��@P�%@P�%@��|@��g@��g@��g@���@���@���@���@���@��|@���@��|@��|@��g@��g@���@��
@��c@���@���@���@��
@��x@��
@���@���@���@���@���@���@��@��Z@���@��k@��Z@��V@��A@��A@��@���@��E@��Z@��Z@��@��0@��Z@��@���@��o@��Z@��Z@���@��s@���@���@��@���@��@��,@���@��@��@��Z@��A@��o@��o@��o@���@���@��k@��@��A@��A@��k@���@��@���@��Q@��V@���@���@��Q@���@���@���@���@���@���@���@��D@���@�� @��*@�� @�� @���@���@��X@��m@��?@���@���@�� @��*@��@��?@��*@��*@��i@��i@���@�� @���@���@��*@��i@���@���@���@���@���@���@��i@��*@���@�� @��~@���@���@��~@��@��T@��@���@��;@���@���@��P@���@���@���@���@���@��@��@���@���@��~@���@��P@��&@��P@���@��e@���@��e@���@��u@���@���@��6@��6@��6@���@��e@��K@���@���@��2@���@��q@���@�� @��>@���@���@��\@��@���@��@���@��\@���@��)@�� @���@���@���@���@��S@��@��@���@���@���@���@��@��.@���@���@��@��q@���@Q�@Q)@QS@Q�@Q)@Q�@Q�@Q%@QJ@Q!@Q�@Q
g@Q
g@Q
�@Q
�@Qc@Q�@Q9@Q�@Q�@Q�@Q�@Q9@Q	�@Q!@Q�@Q.@Q.@Q�@Q`@Q@Q�@Q�@Q �@Q �@P��@P��@P��@Q @Q @Q @Q @Q i@Q @Q @P��@Q @Q ?@P��@Q @Q �@Q �@Q �@Q �@Q;@Q;@Q;@Qe@Q�@Q�@Q;@Q �@Q i@Q i@P��@P��@Q ?@Q;@Q�@Q�@Q@Q@Q�@Qe@Q�@Q;@Qe@Q;@Qe@Q�@Q;@Qe@Q�@Q@Q@Q�@Qe@Q�@Q\@Q\@Q�@Q\@Q�@Q�@Q.@QW@Q@Q�@Q@Q�@Q�@Q.@Q.@Q.@Q2@Q�@Qe@P��@P��@P��@P�z@P�@P�U@P�@P�@P�@P�U@P�@P�U@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        443444444333444444443444344443344434344444344434444434433444334433443344434443344444344444433443334344444344443443444433433343334433444334444343444444344434434443344444333343343344333444333334433433343434433333333343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�F"G�O�G�O�G�O�G�O�G�O�G�O�@w0�@�I@�:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�M�G�O�G�O�G�O�@�E&G�O�G�O�G�O�G�O�@��Z@�CG�O�G�O�G�O�@���G�O�@b��G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@��yG�O�G�O�G�O�G�O�G�O�@�FG�O�G�O�@�>@�B�G�O�G�O�G�O�@N��@�A�G�O�G�O�@�+@�>G�O�G�O�@�EL@�>G�O�G�O�G�O�@��gG�O�G�O�G�O�@�K[@�9�G�O�G�O�G�O�G�O�G�O�@��+G�O�G�O�G�O�G�O�G�O�G�O�@��T@�NG�O�G�O�@�F@�O@�JbG�O�@�GrG�O�G�O�G�O�G�O�G�O�@�E8G�O�G�O�G�O�G�O�@�<9G�O�G�O�@�@bG�O�G�O�G�O�G�O�@U��@H"G�O�@�@@�Jf@�D�G�O�@�:@�0~@�=G�O�G�O�@�J@�G�G�O�G�O�G�O�@�3I@�8�G�O�G�O�G�O�G�O�@���G�O�@�8[G�O�G�O�G�O�G�O�G�O�G�O�@�EbG�O�G�O�G�O�@�KrG�O�G�O�@�N�G�O�G�O�G�O�@�'j@�JG�O�G�O�G�O�G�O�G�O�@�R�@�U
@�X@�G�O�@�^�@�M�G�O�@�Z@�]�G�O�G�O�@�V�@�T^@�V.G�O�G�O�G�O�@�@�Qm@�Q�@��V@�V�G�O�G�O�@�Wj@�X;G�O�@�Y�@�Xe@�U�G�O�@h�jG�O�@�WG�O�G�O�@�X�@�Vo@�U�@�R�@�V1@�ɟ@�Ys@�\�@�Z�G�O�@�Z�@�Zq@�Z�@���G�O�@�[m@�Yy@�^�@�^�@�^u@�^r@�_@�^�@�\�@�\�@�\@�\�@�_B@�^�@�]y@�\g@�X�@�X@�Y�@�X@�U�@�X@�Xd@�W�@�_@�\�@�^x@�^�@�^�@�]�@�_o@�^�@�`@�^�@�_@�]@�`�@�\�@�U�@�Z�@�V�@�X�@�ZD@�[@�\n@�\@�\<@�Zn@�]v@�]@�ZJ@�Z�@�[@�\@�[�@�[�@�\�@�]�@�]�@�]�@�^@�^�@�Y�@�]�@�]z@�_@�_q@�\�@�\�@�]@�\@�[@@�[j@�^
@�Z�@�Z�@�[�@�Zs@�Z@�Zq@�Z�@�[@�Zn@�[�@�[�@�T�@�\k@�\�@�]�@�Z@�U
@�^#@�^L@�_�@�_�@�]@�]y@�^J@�_ @�^v@�^v@�[�@�\�@�\�@�]@�[�@�\�@�]}@�[m@�\>@�Y�@�X@�\@�^v@�^�@�^!@�]}@�^@�]�@�]~@�]@�\@�]N@�\>@�XG�O�@�_q@�`@���@�a@�aS@�^�@�]�@�^u@�^
@�]N@�]@�]@�\�@�\�@�\@�[n@�Zp@�X�@�ZD@�Zn@�[l@�\�@�[�@�]@�^u@�`l@�b�@�b�@�]@�\i@�\�@�_@�_@�^�@�`@�]R@�`n@�a�@�bc@�bQ@�bc@�bb@�bd@�c]@�bb@�_�@�cu@�aR@�`�@�c@�d@�dn@�e@�do@�dn@�d@�d@�m6@�c�@�d@�dn@�d@�d@�d�@�c @�d�@�d�@�d^@�d�@�d@�c�@�c@�c!@�c@�dq@�a�@�^�@�Z@�em@�g"@�f�@�f�@�f*@�cu@�f�@�ga@�g#@�f�@�f�@�g_@�f}@�f�@�g"@�ff@�e@�d�@�do@�ej@�e�@�e�@�ej@�e�@�e@�d�@�e@�dn@�d�@�d�@�dq@�d�@�d�@�d�@�d@�c�@�cs@�d
@�b�@�do@�d�@�f@�f�@�f{@�f�@�g"@�g@�f�@�f�@�f�@�g@�g�@�ht@�g�@�h!@�hr@�g#@�g�@�g�@�h@�f�@�g"@�gz@�gd@�f�@�f|@�g@�gw@�g@�g@�f�@�fv@�f�@�f{@�f&@�f�@�g&@�f�@�h8@�h�@�h�@�h�@�h�@�h2@�h2@�g�@�h�@�i�@�i.@�i�@�in@�i�@�i�@��~@��j@��f@��h@���@���@���@���@���@��~@���@���@��~@��j@��i@���@��
@��g@���@���@���@��@��w@��@���@���@���@���@���@���@��@��Z@���@��m@��Z@��Y@��A@��A@��@���@��G@��Z@��[@��@��1@��X@��@���@��n@��[@��Z@���@��v@���@���@��@���@��@��+@���@��@��@��[@��D@��p@��t@��p@���@���@��g@��@��C@��B@��o@���@��@���@��R@��T@���@���@��S@���@���@���@���@���@���@���@��H@���@���@��+@���@��@���@���@��[@��l@��@@���@���@�� @��/@��@��D@��.@��+@��j@��j@���@���@���@���@��)@��i@���@���@���@���@���@���@��m@��.@���@��@��~@���@���@��~@��@��S@��@���@��>@�� @���@��R@���@���@���@���@���@��@��@���@���@���@���@��R@��)@��U@���@��e@���@��f@���@��x@���@���@��9@��8@��6@���@��h@��N@���@���@��3@���@��v@���@��@��C@���@���@��\@��
@���@�� @���@��Z@���@��(@�� @���@���@���@���@��V@�� @��@���@���@���@���@��@��0@���@���@��@��o@���@Q�@Q%@QS@Q�@Q&@Q�@Q�@Q"@QM@Q#@Q�@Q
j@Q
j@Q
�@Q
�@Qc@Q�@Q;@Q�@Q�@Q�@Q�@Q=@Q	�@Q @Q�@Q*@Q+@Q�@Q`@Q@Q�@Q�@Q �@Q �@P��@P��@P��@Q @Q @Q @Q @Q h@Q @Q @P��@Q @Q >@P��@Q @Q �@Q �@Q �@Q �@Q;@Q:@Q8@Qf@Q�@Q�@Q=@Q �@Q h@Q k@P��@P��@Q B@Q:@Q�@Q�@Q@Q@Q�@Qc@Q�@Q;@Qf@Q>@Qh@Q�@Q8@Qe@Q�@Q@Q@Q�@Qc@Q�@Q]@Q[@Q�@Q]@Q�@Q�@Q.@QX@Q@Q�@Q@Q�@Q�@Q5@Q-@Q-@Q2@Q�@Qc@P��@P��@P��@P�{@P�~@P�R@P�~@P��@P��@P�X@P��@P�R@P��@��~@��j@��f@��h@���@���@���@���@���@��~@���@���@��~@��j@��i@���@��
@��g@���@���@���@��@��w@��@���@���@���@���@���@���@��@��Z@���@��m@��Z@��Y@��A@��A@��@���@��G@��Z@��[@��@��1@��X@��@���@��n@��[@��Z@���@��v@���@���@��@���@��@��+@���@��@��@��[@��D@��p@��t@��p@���@���@��g@��@��C@��B@��o@���@��@���@��R@��T@���@���@��S@���@���@���@���@���@���@���@��H@���@���@��+@���@��@���@���@��[@��l@��@@���@���@�� @��/@��@��D@��.@��+@��j@��j@���@���@���@���@��)@��i@���@���@���@���@���@���@��m@��.@���@��@��~@���@���@��~@��@��S@��@���@��>@�� @���@��R@���@���@���@���@���@��@��@���@���@���@���@��R@��)@��U@���@��e@���@��f@���@��x@���@���@��9@��8@��6@���@��h@��N@���@���@��3@���@��v@���@��@��C@���@���@��\@��
@���@�� @���@��Z@���@��(@�� @���@���@���@���@��V@�� @��@���@���@���@���@��@��0@���@���@��@��o@���@Q�@Q%@QS@Q�@Q&@Q�@Q�@Q"@QM@Q#@Q�@Q
j@Q
j@Q
�@Q
�@Qc@Q�@Q;@Q�@Q�@Q�@Q�@Q=@Q	�@Q @Q�@Q*@Q+@Q�@Q`@Q@Q�@Q�@Q �@Q �@P��@P��@P��@Q @Q @Q @Q @Q h@Q @Q @P��@Q @Q >@P��@Q @Q �@Q �@Q �@Q �@Q;@Q:@Q8@Qf@Q�@Q�@Q=@Q �@Q h@Q k@P��@P��@Q B@Q:@Q�@Q�@Q@Q@Q�@Qc@Q�@Q;@Qf@Q>@Qh@Q�@Q8@Qe@Q�@Q@Q@Q�@Qc@Q�@Q]@Q[@Q�@Q]@Q�@Q�@Q.@QX@Q@Q�@Q@Q�@Q�@Q5@Q-@Q-@Q2@Q�@Qc@P��@P��@P��@P�{@P�~@P�R@P�~@P��@P��@P�X@P��@P�R@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        443444444333444444443444344443344434344444344434444434433444334433443344434443344444344444433443334344444344443443444433433343334433444334444343444444344434434443344444333343343344333444333334433433343434433333333343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9���9���9���9���9���9���9��9���9���9���9���9���9���9���9���9���9���9��k9���9���9��9���9��w9���9���9���9��p9���9��t9��t9��}9���9���9���9���9���9���9���9��}9��i9���9���9���9���9���9���9��}9��)9��9���9���9��s9��B9��Q9���9���9��+9���9���9��9���9���9���9���9��9��9��9��79��I9���9���9���9���9���9���9��R9���9���9���9��]9���9���9���9���9���9���9���9��9���9���9��W9��g9���9��g9��k9��9��9���9���9���9��&9��K9��h9���9��z9���9���9���9���9���9���9��g9��9��Y9���9���9���9���9��9���9���9���9���9���9��79��j9���9��9���9���9��z9���9��}9��9��d9��39��29��t9���9��9��9��9��$9���9��{9�� 9��9���9��X9��t9��T9��w9��"9���9���9���9��9��]9���9���9��+9��+9��)9���9���9��<9���9���9���9��V9��'9���9��d9���9��@9��>9��9���9��F9���9���9��9��T9��9��_9��/9���9��)9��9���9���9���9���9��y9��w9��F9���9���9��F9��w9���9��!9��C9%��9%�9%�C9%��9%�9%��9%©9%��9%��9%ó9%�:9%�L9%�L9%Ʈ9%Ư9%�9%�T9%��9%�W9%�T9%�Q9%�w9%��9%Š9%ñ9%��9%�X9%�X9%�9%��9%��9%��9%�G9%��9%�|9%��9%��9%��9%�9%�9%�9%�9%�]9%�9%�9%��9%�!9%�;9%��9%�9%��9%��9%�~9%��9%�9%�9%�9%�&9%�G9%�G9%�9%��9%�]9%�_9%��9%��9%�?9%�9%�F9%��9%��9%��9%��9%�$9%�E9%�9%�&9%�9%�(9%�j9%�9%�%9%�19%��9%��9%�E9%�$9%��9%��9%��9%�9%��9%�.9%�9%�[9%�|9%�:9%��9%�:9%��9%��9%�`9%�Z9%�Z9%��9%�09%�$9%��9%�9%�j9%�@9%�w9%�T9%�w9%�y9%�y9%�Y9%�y9%�T9%��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B$�B'�B.BVB��B�XB�wB�qB�wB��B�}B�}B�qB�XB�^B�wBĜBŢBƨB��B�
BÖB�^BɺB�jB�1Bt�Bx�B�B~�B�BhsB]/B]/B]/B_;B\)B]/BdZBbNBW
B@�B.B.B;dBK�BC�BYBiyB|�B�9B�-B�oB�JB��B��B��B��B��B�oB�VB��B�{Bx�B]/BQ�BJ�BI�BI�BF�BD�BB�B?}B,B\B��BǮB�B�bBo�BbNBQ�B49B%�B�B+B
�B
�B
�RB
��B
�=B
k�B
\)B
/B
B	�TB	ȴB	�?B	�oB	u�B	s�B	\)B	H�B	>wB	%�B	�B	oB	B��B��B�B�yB�fB�TB�BB�/B�
B��B��BȴBÖBB��B��B�}B�dB�FB�!B��B��B��B��B��B��B��B�uB�oB��B��B�hB�1B�+B�DB�Bz�Bt�Bo�Bm�Bk�BhsBm�Bs�Bl�BgmBe`BaHB^5BZBW
BS�BP�BN�BL�BJ�BI�BH�BG�BE�BC�BA�B@�B@�B?}B>wB=qB<jB<jB;dB:^B9XB7LB7LB5?B33B2-B1'B/B.B-B,B(�B'�B'�B&�B%�B'�B'�B&�B)�B,B)�B,B.B0!B0!B.B/B.B1'B1'B2-B/B.B49B9XB<jB<jB:^B8RB6FB6FB5?B49B49B5?B49B49B33B33B2-B1'B0!B/B-B+B0!B33B49B6FB8RB<jB<jB;dB<jB?}BE�BI�BM�BP�BQ�BR�BS�BQ�BT�BXBZB[#B]/B`BBcTBffBhsBjBl�Bp�Bs�Bz�Bz�Bz�Bz�B{�B~�B� B� B�B�B�B�B�1B�7B�VB�hB�hB�{B��B��B��B��B��B��B�B�B�-B�-B�'B�?B�XB�jB�wB��BǮB��B��B��B��B��B�B�5B�;B�;B�5B�5B�NB�mB�B�B�B�B�B��B��B��B��B��B	  B	B		7B	JB	PB	bB	\B	\B	oB	�B	�B	�B	�B	%�B	'�B	)�B	+B	-B	.B	0!B	49B	6FB	7LB	8RB	9XB	:^B	?}B	A�B	B�B	D�B	G�B	I�B	L�B	N�B	Q�B	S�B	W
B	ZB	^5B	`BB	aHB	dZB	gmB	gmB	gmB	l�B	o�B	r�B	u�B	v�B	x�B	y�B	{�B	}�B	�%B	�DB	�PB	�VB	�VB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�FB	�LB	�LB	�LB	�FB	�^B	�dB	�jB	�dB	�jB	�jB	�}B	��B	��B	��B	��B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	��B	��B
  B

=B
�B
"4B
)*B
.�B
6B
<6B
BuB
H�B
N�B
UgB
Z�B
`B
ffB
h�B
m�B
sMB
yXG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?3BAp>�<�>�)�>�+$?(5?�?>ccA�ݮB��B�[?ju�A)9A[&w?�?,@҂??�?a+�B�A�c�?%{�@��BX??�"d>��?�*?�[)A���B��?m?��?rӄB�Z?��A���?��<A	�	?�?���?�A�6�?���>��?\,�BڜA�??�t:@_��AzF0B�`AL
�?_�GB��B��AR.�?[,b@A�ZA�9YB�?��v?���B�}B��?�A~$LB��B�>>�5�?#?Ay%7A�s*>�	�?8�b@ !BB�B��>�A�>��>���?'K@�VB�t?�?hE�AK�h@%J�?}$?J ;B��Bl?r?x�
B��B��B�?o��B�`@�'e?�+@}=?<1@λ%B��A;�1?���?'�A�e�B'?��s@k��B�>>�K?c�?mZ?��=A���A��o?S�B�sB�B�?M5B��B��B5�?�:?a%�B�4B$�?`�?��1A��B�B��>�K@?�Au�?I7;A���?a�:B�@Z��?,�g@��?
�;?\�5?>	�B�A&VT@Z�|A'q[B?.?;+$AV�B�A�9�?o�@L��B#B?>�Ec>��>�R�?�?�u}B�B�B��B��?]�lA�h=B��@��B�B�?2w-Ag�B�SB�mB�>�_�?+�?{QYB�ZB�B��B��B��?�?h�RB�B��?]�B-[B�-B�B@#�A�El@�E�B�d@0�T@�wSB�&B��B�B� B��A��wB�B�JBQ�@˞�B�
B�B�sA��A �B�\B�B��B�"B��B�CB�B�B��B��B�+B�`B�B�lB��B�B�SB�+B��B��B�}B��B�B�;B�B�B�"B��B�B�BB�=B�B�DB�B�B��B�&B�B�B�+B�B��B�+B��B�B�B��B��B�B��B��B�B��B��B�B�+B��B�2B�B��B�B��B�B��B�sB��B��B�B��B��B�uB�LB�;B�B�kB��B��B�B�cB�B�B�B��B�B�3B�B�B�0B��B�	B��B�B�BB��B�$B��B��B�cB��B��B�zB�#B�B��B��B��B�*B��B�B�=B�B�B�B�B�SB��B��B�WB�:B�pB�~B�B��B�FB�|@~��B�B�A��DB�&B�
B��B�&B��B�FB�B��B��B��B�IB�B�SB�+B�B�+B��B1B�AB�<B�B�+B�dB��B�2B��B�\B�B��B��B��B�XB��B��B�RB�7B�SB�B�gB��B�0B�pB�B��B�)B�aB�B�B��B�B��B��B�B�YB��B�B�B�B�B�jB�5B��B�FB�nB�B�>B�B��B��B��B�B�&B�UB�MB�B�B��B�B��B��B��B��B�B�B�dB�B��B�B��B��B�WB��B��B��B�%B�EB�EB��B�mB�B��B�QB�B�
B��B�B��B��B��B�B��B�NB��B�~B�B�B��B�B��B�B�|B�`B�TB��B�TB�/B��B�TB�lB�B�=B�B�<B�3B�B�BB�bB��B��B�&B�B�B�B�=B�QB��B�B��B�wB�'B�B��B�B��B�B�B��B�B�B�B��B�B�B�sB�B�B�;B�cB�
B�*B��B�B�B�8B��B�OB��B�FB�B�5B��B�,B�$B�B�cB�ZB�B�B�B�B�uB�lB�B��B�,B�<B��B�3B�B�B��B��B�B��B��B�B�\B�B��B�"B�B�B��B��B�B�B�B�$B��B�bB�B�B�B�B�GB�>B�5B�8B�B�(B�B�B�B�LB��B�aB�IB��B��B�B�NB��B�]B�B��B�B�B�)B�B�B�B�7B�BB�B�B�'B�.B�:B�B�pB�3B�+B��B�%B�B�2B�B�B��B�B�B�B��B�B��B�B�B��B�+B��B�B�PB��B��B�B��B��B�;B�3B�RB��B�0B�OB�wB�>B�]B�UB�DB�B�B�-B�UB�|B�#B��B��B��B�vB�B�B�B�B�B�B�B��B��B�B�B�?B�.B��B�mB�yB��B��B�B�nB�-B��B�7B�B�B��B�B��B��B�B�}B��B��B��B�_B��B�B�~B�EB�B�B��B�fB��B��B�B�B��B�B�^B�B�lB�B�zB�rB�B�B�oB�B�
B��B�dB�7B	��B	��B	��B	��B	��B	��B	�9B	��B	��B	�(B	��B	�B	�TB	�*B	�B	�@B	�pB	�B	�B	��B	��B	��B	�eB	�rB	��B	��B	�GB	��B	�XB	��B	��B	��B	�qB	�B	��B	�HB	��B	��B	��B	�sB	�fB	��B	�?B	�B	�'B	�fB	��B	��B	�bB	�B	�B	�B	�PB	�$B	�wB	�:B	�LB	�B	��B	��B	��B	�xB	�LB	��B	��B	�B	�B	�{B	��B	��B	��B	�YB	�0B	�B	�B	��B	�,B	�B	�B	�	B	��B	�=B	��B	��B	�oB	��B	�B	�BB	�5B	�(B	��B	�fB	��B	�TB	��B	�jB	�B	�B	��B	��B	��B	�KB	��B	��B	��B	��B	��B	��B	��B	��B	�[B	�/B	��B	��B	��B	�FB	�9B	�B	�iB	�OB�B�B�B�B�B�B�B�B�BvB�BeB\BHB@B�B BB�BnB�B�BpB�B�B�B�B�BiBiBB(B�BLBB'B
B�B�BpB�B�B�BLBWB�B6B�BhBKBBBiBB'B�B�BiB�B�BBfB~B�B�B�B�B�B�B�BwBBB-BLBWBB�B�B�B�B[BB6BMBDBGB*B]B�BB�B�B�B�B�B�B�B�B�B�B�B�B?B&B9BPB3B�B%BB3B�B1BpB�B�B2B�BIB�B�B�BQBB�B�BrB�B�BOB�B�B�BtB�B�B�B�B�BB�B�BuBB�B�B�BjB�BB�B7B�B1B�B�BoBB�BYB�B�BkB�B�B�B#B�BmB�B�B�BSB�B�B�B�B�BYB�B!B�BB�BWBB�BUB�BbBB�B�BeB�BB�B�B�B B6B�B�B	آB	��B	��B	�4B	׀B	�B	��B	�B	��B	ׅB	��B	شB	اB	��B	��B	�B	�MB	��B	�&B	�B	��B	�B	ًB	�ZB	ٓB	��B	�cB	�VB	��B	��B	٥B	�yB	�1B	٩B	�oB	��B	٭B	ڣB	��B	ڹB	ڬB	ڒB	��B	�jB	�]B	�1B	�6B	�GB	��B	�B	�CB	�6B	�B	�-B	�oB	�aB	�TB	�YB	�jB	�PB	�B	ڟB	�TB	�:B	��B	��B	��B	ڒB	ڵB	��B	��B	��B	ڣB	�:B	�KB	�B	�B	��B	��B	�B	ڨB	ڬB	ڄB	��B	��B	ۙB	�`B	ڒB	ۙB	یB	��B	�eB	��B	ڣB	ڬB	ھB	ۄB	ڵB	�\B	ڎB	ۑB	�9B	�,B	�B	�\B	��B	�B	��B	��B	�iB	�JB	ەB	�[B	�mB	�`B	�FB	�B	�B	�B	�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999443444444333444444443444344443344434344444344434444434433444334433443344434443344444344444433443334344444344443443444433433343334433444334444343444444344434434443344444333343343344333444333334433433343434433333333343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B$�B'�B.BU�B��B�OB�kB�fB�kB�zB�qB�rB�gB�NB�QB�nBđBŗBƝB��B��BÉB�TBɯB�aB�(Bt�Bx�B�B~�B�BhgB]$B]&B]%B_-B\!B]&BdMBbBBW B@xB.B.B;ZBK�BC�BYBioB|�B�.B�$B�fB�?B��B��B��B��B��B�gB�MB�xB�oBx�B]#BQ�BJ�BI�BI�BF�BD�BB�B?tB+�BPB��BǠB��B�UBo�BbCBQ�B4-B%�B�B#B
�B
�B
�DB
��B
�2B
kyB
\B
/B
 B	�IB	ȨB	�3B	�cB	u�B	s�B	\B	H�B	>lB	%�B	�B	bB	B��B��B�B�kB�YB�GB�5B�#B��B��B��BȤBÇBB�zB�wB�pB�WB�:B�B��B��B��B��B��B��B��B�gB�cB��B�sB�ZB�"B�B�6B�
Bz�Bt�Bo�Bm�BkuBhdBm�Bs�Bl{Bg`BeRBa;B^(BZBV�BS�BP�BN�BL�BJ�BI�BH�BG�BE�BC�BAxB@uB@vB?nB>iB=dB<[B<\B;WB:OB9IB7>B7>B50B3$B2B1B/B.B- B+�B(�B'�B'�B&�B%�B'�B'�B&�B)�B+�B)�B+�B.B0B0B.B/B.B1B1B2B/B.B4)B9IB<]B<]B:PB8CB67B66B5/B4*B4+B52B4*B4(B3%B3#B2B1B0B/B- B*�B0B3#B4)B68B8BB<[B<ZB;UB<^B?lBE�BI�BM�BP�BQ�BR�BS�BQ�BT�BW�BZB[B]B`2BcDBfWBhbBjoBl~Bp�Bs�Bz�Bz�Bz�Bz�B{�B~�B�B�B��B��B�B�B�B�&B�FB�XB�XB�iB�wB��B��B��B��B��B��B�B�B�B�B�0B�EB�YB�gB�zBǞB˶B��B��B��B��B�B�$B�,B�-B�%B�$B�>B�_B�wB�B�B�B�B��B��B��B��B��B��B	B		(B	:B	?B	QB	LB	MB	^B	vB	|B	�B	�B	%�B	'�B	)�B	*�B	,�B	.B	0B	4(B	66B	7<B	8CB	9IB	:OB	?oB	AvB	BB	D�B	G�B	I�B	L�B	N�B	Q�B	S�B	V�B	ZB	^&B	`3B	a9B	dJB	g]B	g_B	g]B	l{B	o�B	r�B	u�B	v�B	x�B	y�B	{�B	}�B	�B	�4B	�@B	�EB	�EB	�MB	�RB	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�*B	�5B	�<B	�=B	�<B	�5B	�NB	�SB	�YB	�RB	�\B	�[B	�lB	�uB	�wB	�zB	�xB	ÉB	ÇB	ċB	ĊB	œB	őB	őB	ŒB	ŒB	őB	őB	ƗB	ƗB	ƕB	ǞB	ɫB	ʲB	̽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�G�O�B	�B	��B	��B

+B
�B
"%B
)B
.�B
6 B
<'B
BfB
H�B
N{B
UXB
ZxB
` B
fXB
h�B
m�B
s=B
yHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BAhG�O�G�O�G�O�G�O�G�O�G�O�A�ݟB��B�PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�BX5G�O�G�O�G�O�G�O�A��pB��G�O�G�O�G�O�B�RG�O�A��{G�O�G�O�G�O�G�O�G�O�A�6�G�O�G�O�G�O�BړG�O�G�O�G�O�G�O�G�O�B�WG�O�G�O�B��B��G�O�G�O�G�O�A�9JB��G�O�G�O�B�vB��G�O�G�O�B�B�5G�O�G�O�G�O�A�sG�O�G�O�G�O�B�B��G�O�G�O�G�O�G�O�G�O�B�lG�O�G�O�G�O�G�O�G�O�G�O�B��BbG�O�G�O�B��BݻB�G�O�B�WG�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�BG�O�G�O�B�5G�O�G�O�G�O�G�O�A���A��eG�O�B�iB�B�G�O�B��B��B5{G�O�G�O�B�*B$�G�O�G�O�G�O�B�B��G�O�G�O�G�O�G�O�A���G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�B?%G�O�G�O�B�G�O�G�O�G�O�B#B3G�O�G�O�G�O�G�O�G�O�B�B�B��B��G�O�A�h*B��G�O�B�B�G�O�G�O�B�IB�bB�G�O�G�O�G�O�B�QB�B��B��B��G�O�G�O�B�B��G�O�B-UB�#B�5G�O�A�E[G�O�B�XG�O�G�O�B�B�B�B��B��A��hB��B�BBQ�G�O�B� B�B�gA��G�O�B�PB�B��B�B��B�7B�B�B��B��B�B�TB�B�aB��B�B�GB�#B��B��B�rB��B��B�/B�B�B�B��B�B�6B�0B��B�9B� B�B��B�B�B�B�B�B��B� B�B�B� B�B��B�B��B��B�B��B��B��B�"B��B�,B�B��B�B��B�B��B�hB��B��B�B��B��B�jB�AB�.B�B�_B��B��B��B�XB�B�B�B��B�B�(B�B��B�%B�B��B��B�B�7B��B�B��B�B�WB��B��B�oB�B�B�B��B�B� B��B�B�3B��B�B�{B�B�HB��B��B�MB�.B�gB�tB�B��B�:B�qG�O�B�B��A��1B�B��B��B�B��B�8B�B��B��B�B�=B��B�JB�B�B� B��B$B�9B�0B�B�!B�YB��B�(B�B�QB�~B��B��B��B�MB��B��B�HB�,B�JB�B�ZB��B�%B�eB�B��B�B�VB�B�B��B�wB�B��B�B�MB��B�B�B�B�B�^B�(B��B�9B�bB�B�3B�B��B��B��B�yB�B�KB�DB�B�B��B�B��B��B��B��B�B�B�YB�B�B�B�B��B�KB��B��B��B�B�:B�:B��B�`B�B��B�FB�B� B��B�wB�B�B�B�B��B�DB��B�pB��B�B��B�B��B��B�pB�XB�KB��B�KB�&B��B�JB�aB�B�0B�B�/B�(B�B�7B�XB��B�B�B�B�B�B�3B�FB��B�B�B�mB�B�B��B�B��B�B��B��B��B�B�B�B�B�B�gB�B�B�/B�XB�B�B�B{B�B�B�B�B�BlB�B]BQB@B6B�BBByBeB�B�BdB�B�B�BvB�B_B_B�BB�BBBBB�B�B�BaB�B�B�BDBNB�B,B�B]BBB8BaBBB�B�B`B�B�B�B]BrB�B�B�B{B�B�B�BjBBB#BDBNB�B�B�B�B{BQB�B,BDB:B>BBQB�BB�B�B�B�B�B�B�B�B�B�B�B�B3BB/BIB*B�BBB)B�B(BfB�B�B&B�B@B�B�B�BGBB�B�BfB�B�BEByB�B�BjB�B�B{B�B�B�B�B�BjB�B�B�B�B`B�BB�B/B�B(B�B�BdBBBNB�B�B]B�B}B{BB�BbB�B�B�BKB}B�B�B�B�BNB�BB�B�B�BNBB�BLB�BYBB�B}B]B�BB�B�B�BB,B�B�B	ؓB	ׯB	׷B	�#B	�oB	��B	��B	��B	زB	�wB	��B	ؤB	ؗB	��B	��B	�B	�=B	��B	�B	��B	��B	��B	�~B	�GB	قB	��B	�QB	�FB	��B	��B	ٕB	�iB	�#B	ٙB	�]B	��B	٠B	ړB	��B	کB	ڛB	ځB	ڲB	�[B	�NB	�!B	�*B	�5B	��B	��B	�5B	�)B	�B	�B	�_B	�PB	�BB	�IB	�[B	�@B	�B	ڎB	�BB	�*B	ڳB	��B	��B	ځB	ڦB	��B	��B	��B	ړB	�*B	�<B	��B	�B	��B	��B	�B	ږB	ڞB	�vB	��B	��B	ۊB	�OB	ڂB	ۊB	�{B	��B	�TB	��B	ڒB	ڝB	گB	�tB	ڦB	�MB	�}B	ۀB	�.B	�B	� B	�MB	��B	��B	ڱB	��B	�YB	�<B	ۂB	�KB	�]B	�PB	�6B	�B	�B	��B	�AB�B�B�B{B�B�B�B�B�BlB�B]BQB@B6B�BBByBeB�B�BdB�B�B�BvB�B_B_B�BB�BBBBB�B�B�BaB�B�B�BDBNB�B,B�B]BBB8BaBBB�B�B`B�B�B�B]BrB�B�B�B{B�B�B�BjBBB#BDBNB�B�B�B�B{BQB�B,BDB:B>BBQB�BB�B�B�B�B�B�B�B�B�B�B�B�B3BB/BIB*B�BBB)B�B(BfB�B�B&B�B@B�B�B�BGBB�B�BfB�B�BEByB�B�BjB�B�B{B�B�B�B�B�BjB�B�B�B�B`B�BB�B/B�B(B�B�BdBBBNB�B�B]B�B}B{BB�BbB�B�B�BKB}B�B�B�B�BNB�BB�B�B�BNBB�BLB�BYBB�B}B]B�BB�B�B�BB,B�B�B	ؓB	ׯB	׷B	�#B	�oB	��B	��B	��B	زB	�wB	��B	ؤB	ؗB	��B	��B	�B	�=B	��B	�B	��B	��B	��B	�~B	�GB	قB	��B	�QB	�FB	��B	��B	ٕB	�iB	�#B	ٙB	�]B	��B	٠B	ړB	��B	کB	ڛB	ځB	ڲB	�[B	�NB	�!B	�*B	�5B	��B	��B	�5B	�)B	�B	�B	�_B	�PB	�BB	�IB	�[B	�@B	�B	ڎB	�BB	�*B	ڳB	��B	��B	ځB	ڦB	��B	��B	��B	ړB	�*B	�<B	��B	�B	��B	��B	�B	ږB	ڞB	�vB	��B	��B	ۊB	�OB	ڂB	ۊB	�{B	��B	�TB	��B	ڒB	ڝB	گB	�tB	ڦB	�MB	�}B	ۀB	�.B	�B	� B	�MB	��B	��B	ڱB	��B	�YB	�<B	ۂB	�KB	�]B	�PB	�6B	�B	�B	��B	�AG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999443444444333444444443444344443344434344444344434444434433444334433443344434443344444344444433443334344444344443443444433433343334433444334444343444444344434434443344444333343343344333444333334433433343434433333333343333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649082020083116490820200831164908202008311649082020083116490820200831164908202008311649082020083116490820200831164908202008311649082020083116490820200831164908AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817092019021918170920190219181709    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817092019021918170920190219181709  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817092019021918170920190219181709  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649082020083116490820200831164908  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                