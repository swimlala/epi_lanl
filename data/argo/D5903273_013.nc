CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  F   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:33Z creation      
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
resolution        =���   axis      Z        'H  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  lX   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'H  v,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     'H  �H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'H  ΐ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'H  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� &�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'H 0�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'H X   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� X   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'H �,   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �t   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     'H �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'H �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'H �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 9�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     'H C�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � k   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   k�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   w�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181633  20200831164710  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @ՙ�@���@ՙ�@���@ՙ�@���111 @ՙͲ@�,@ՙͲ@�,@ՙͲ@�,@5�?|�h@5�?|�h@5�?|�h�c}�hr�!�c}�hr�!�c}�hr�!111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @9��@y��@���A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  B   B  B  B  B   B(  B0ffB7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�)D���D�/
D�z=D���D��D�K3D�z=D��)D�  D�D�D�t{D���D��\D�M�D�|{D�fD��D�&D��D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����    �L�;L�;L��        �L�;L�;L�ͽ��;L�;L�ͽ���    �L�;L�;L�ͽ��ͽ��;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��ͽ��;L�;L��    ���;L�;L�ͽ���    �L�;L�;L�;L�;L�;L�;L�ͽ���    ���;L��    ���;L�;L�ͽ��ͽ��ͽ��;L�ͽ���    �L�;L�ͽ��ͽ��;L�;L��    =���    �L��    ���ͽ��;L�ͽ��;L�;L�;L��=��ͽ��;L�ͽ���        �L�;L�;L�;L�;L�;L��    ���;L�;L�;L�ͽ��ͽ��;L�;L�ͽ���    �L�ͽ��;L�;L�;L�;L�ͽ��ͽ��;L��=���    �L�;L�ͽ��ͽ��;L�;L�;L��    >L��>������;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�;L�ͽ��;L�ͽ���    �L�;L�;L�;L��=��ͽ��;L�;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ��;L�;L�ͽ���    �L�ͽ��;L�ͽ��ͽ��;L�;L�;L��    ���;L�ͽ��ͽ��;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ���    �L�;L�;L��    �L�ͽ��ͽ��ͽ��;L�ͽ��ͽ��;L�;L�ͽ��ͽ��;L�;L�ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��;L��    �L��        �L�;L�ͽ��ͽ��ͽ��ͽ���    ���ͽ���=��ͽ��ͽ��ͽ���    ���ͽ��ͽ���            =���    =���>L��>L��            =���=���                ����        =��ͽ���            ����        =���=���    =���            ����        =���                                =���=���=���>���>L��=���>L��>L��=���            =���            =���    >L��=���=���                =���    =���    =���    =���=���=���=���    =���                        =���=���=���    =���                =���=���=���    =���        =���=���=���            =���=���=���            =���        =���=���=���=���=���    =���=���=���=���    >L��=���=���=���            =���=���=���=���        >L��>L��=���=���=���        =���    =���    =���>L��>L��    =���        =���    =���    =��ͽ���                    =���=���=���    =���=���=���=���    =���        �L��    =���=���    =���=���=��ͽ���        =��ͽ���>L��=���    >L��    =���=���                                        =���        =���=���=���=���=���>L��>L��        ����    =���=���=���=���>L��>���>���>���>���>���?   ?   ?��?��?333?333?L��?L��?fff?fff?�  ?���?���?�  ?���?���?���?���?�ff?�33?�  ?�33?�  ?�  ?���?���?ٙ�?�ff?�ff?�33?�33?�33@   @ff@ff@��@��@��@33@33@��@��@   @   @&ff@&ff@&ff@,��@,��@333@333@9��@9��@9��@Fff@Fff@L��@L��@S33@S33@Y��@Y��@`  @fff@l��@l��@s33@y��@�  @�  @�ff@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@�ff@���@�  @�33@�ff@���@�  @�33@ٙ�@���@�  @�ff@陚@���@�33@�ff@���@���A��A33A��AffA	��A33A��AffA��A33A��AffA��A33A��AffA!��A#33A$��A&ffA)��A+33A,��A.ffA1��A333A4��A6ffA9��A;33A<��A>ffA@  AA��AD��AFffAH  AK33AL��ANffAP  AS33AT��AVffAX  A[33A\��A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAp  As33At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  Ař�A�ffA�  A���A�ffA�33A�  A͙�A�ffA�33A���Aљ�A�ffA�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A���Aݙ�Dq�Dq  Dq&fDq,�Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrS3Dr` DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Ds� Ds�fDs��Ds��Ds� Ds�fDs��Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt�3Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Du  Duf@9��@9��@9��@Fff@Fff@L��@L��@S33@S33@Y��@Y��@`  @fff@l��@l��@s33@y��@�  @�  @�ff@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@�ff@���@�  @�33@�ff@���@�  @�33@ٙ�@���@�  @�ff@陚@���@�33@�ff@���@���A��A33A��AffA	��A33A��AffA��A33A��AffA��A33A��AffA!��A#33A$��A&ffA)��A+33A,��A.ffA1��A333A4��A6ffA9��A;33A<��A>ffA@  AA��AD��AFffAH  AK33AL��ANffAP  AS33AT��AVffAX  A[33A\��A^ffA`  Ac33Ad��AfffAh  Ak33Al��AnffAp  As33At��AvffAy��A{33A|��A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  Ař�A�ffA�  A���A�ffA�33A�  A͙�A�ffA�33A���Aљ�A�ffA�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A���Aݙ�Dq�Dq  Dq&fDq,�Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�DrFfDrL�DrS3Dr` DrffDrl�Dry�Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs33Ds9�Ds@ DsL�DsS3DsY�Ds` Dsl�Dss3Ds� Ds�fDs��Ds��Ds� Ds�fDs��Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt�3Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt��Dt�3Du  DufG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @Dz�@�=q@�=qA�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�(�A�(�A�\)B �B�B�B�B �B(�B1zB8G�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>��D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg��Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq{Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��Dt�GDy�
D�4D�4{D��D���D��D�P�D��D�ٚD�qD�J>D�y�D��4D��D�S4Dځ�D���D�RD�+�D�>D��)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�\)>.{������>.{>.{������=�\)����=�\)>.{������=�\)=�\)������=�\)��������=�\)=�\)����>.{=�\)����=�\)>.{��������������=�\)>.{=�\)��>.{=�\)����=�\)=�\)=�\)��=�\)>.{����=�\)=�\)����>.{>�=q>.{��>.{=�\)=�\)��=�\)������>�=q=�\)��=�\)>.{>.{������������>.{=�\)������=�\)=�\)����=�\)>.{��=�\)��������=�\)=�\)��>�=q>.{����=�\)=�\)������>.{>�p�>��=�\)��=�\)=�\)����=�\)������=�\)��=�\)>.{��������>�=q=�\)����=�\)������=�\)=�\)��=�\)����=�\)>.{��=�\)��=�\)=�\)������>.{=�\)��=�\)=�\)��������=�\)��������=�\)>.{������>.{��=�\)=�\)=�\)��=�\)=�\)����=�\)=�\)����=�\)=�\)=�\)��=�\)=�\)=�\)=�\)��>.{��>.{>.{����=�\)=�\)=�\)=�\)>.{=�\)=�\)>�=q=�\)=�\)=�\)>.{=�\)=�\)=�\)>.{>.{>.{>�=q>.{>�=q>�p�>�p�>.{>.{>.{>�=q>�=q>.{>.{>.{>.{=�\)>.{>.{>�=q=�\)>.{>.{>.{=�\)>.{>.{>�=q>�=q>.{>�=q>.{>.{>.{=�\)>.{>.{>�=q>.{>.{>.{>.{>.{>.{>.{>.{>�=q>�=q>�=q>��>�p�>�=q>�p�>�p�>�=q>.{>.{>.{>�=q>.{>.{>.{>�=q>.{>�p�>�=q>�=q>.{>.{>.{>.{>�=q>.{>�=q>.{>�=q>.{>�=q>�=q>�=q>�=q>.{>�=q>.{>.{>.{>.{>.{>.{>�=q>�=q>�=q>.{>�=q>.{>.{>.{>.{>�=q>�=q>�=q>.{>�=q>.{>.{>�=q>�=q>�=q>.{>.{>.{>�=q>�=q>�=q>.{>.{>.{>�=q>.{>.{>�=q>�=q>�=q>�=q>�=q>.{>�=q>�=q>�=q>�=q>.{>�p�>�=q>�=q>�=q>.{>.{>.{>�=q>�=q>�=q>�=q>.{>.{>�p�>�p�>�=q>�=q>�=q>.{>.{>�=q>.{>�=q>.{>�=q>�p�>�p�>.{>�=q>.{>.{>�=q>.{>�=q>.{>�=q=�\)>.{>.{>.{>.{>.{>�=q>�=q>�=q>.{>�=q>�=q>�=q>�=q>.{>�=q>.{>.{��>.{>�=q>�=q>.{>�=q>�=q>�=q=�\)>.{>.{>�=q=�\)>�p�>�=q>.{>�p�>.{>�=q>�=q>.{>.{>.{>.{>.{>.{>.{>.{>.{>.{>�=q>.{>.{>�=q>�=q>�=q>�=q>�=q>�p�>�p�>.{>.{=�\)>.{>�=q>�=q>�=q>�=q>�p�>��?�?�?�?�?+�?+�?E�?E�?^�R?^�R?xQ�?xQ�?���?���?�?��\?��\?�?��\?�\)?�\)?�\)?�(�?���?�?���?�?�?�\?�\?�\)?�(�?�(�@z�@z�@z�@
�H@G�@G�@�@�@�@{@{@$z�@$z�@*�H@*�H@1G�@1G�@1G�@7�@7�@>{@>{@Dz�@Dz�@Dz�@QG�@QG�@W�@W�@^{@^{@dz�@dz�@j�H@qG�@w�@w�@~{@�=q@�p�@�p�@��
@��
@�
>@�=q@�p�@���@��
@�
>@�=q@�p�@���@��
@�
>@�=q@���@��
@��
@�=q@�p�@ȣ�@��
@�=q@�p�@أ�@�
>@�=q@�p�@��
@�
>@�=q@���@��
@�
>A�AQ�A�A�A	�AQ�A�A�A�AQ�A�A�A�AQ�A�A�A!�A$Q�A%�A'�A)�A,Q�A-�A/�A1�A4Q�A5�A7�A9�A<Q�A=�A?�AA�AB�RADQ�AG�AI�AJ�RAM�AO�AQ�AR�RAU�AW�AY�AZ�RA]�A_�Aa�Ab�RAe�Ag�Ai�Aj�RAm�Ao�Aq�Ar�RAu�Aw�Ay�A|Q�A}�A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A��\A�(�A���A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A��\A�(�A���Aď\A�\)A���A�A�\)A�(�A�Ȁ\A�\)A���A�AЏ\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�A܏\A�(�A���Dq${Dq*�Dq1GDq7�DqD{DqJ�DqQGDq^Dqd{Dqj�Dqw�Dq~Dq�{Dq�GDq��Dq�Dq��Dq�GDq��Dq�Dq��Dq�GDq׮Dq�{Dq��Dq�GDq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrQGDrW�Dr^Drj�DrqGDrw�Dr�{Dr��Dr�GDr�Dr�{Dr��Dr��Dr�Dr�{Dr�GDr׮Dr�Dr�{Dr�GDr��Dr�Ds
�DsGDs�Ds${Ds*�Ds1GDs>DsD{DsJ�DsW�Ds^Dsd{Dsj�Dsw�Ds~Ds��Ds�GDs��Ds�{Ds��Ds�GDs��Ds�{Ds��Ds׮Ds�Ds�{Ds�GDs��Ds�Dt
�DtGDt�Dt${Dt*�Dt1GDt7�DtD{DtJ�DtW�Dt^Dtd{DtqGDtw�Dt~Dt��Dt�GDt��Dt�Dt��Dt�GDt�Dt�{Dt��Dt�GDt�Dt�{Dt��Dt��Dt�Du
�DuG@Dz�@Dz�@Dz�@QG�@QG�@W�@W�@^{@^{@dz�@dz�@j�H@qG�@w�@w�@~{@�=q@�p�@�p�@��
@��
@�
>@�=q@�p�@���@��
@�
>@�=q@�p�@���@��
@�
>@�=q@���@��
@��
@�=q@�p�@ȣ�@��
@�=q@�p�@أ�@�
>@�=q@�p�@��
@�
>@�=q@���@��
@�
>A�AQ�A�A�A	�AQ�A�A�A�AQ�A�A�A�AQ�A�A�A!�A$Q�A%�A'�A)�A,Q�A-�A/�A1�A4Q�A5�A7�A9�A<Q�A=�A?�AA�AB�RADQ�AG�AI�AJ�RAM�AO�AQ�AR�RAU�AW�AY�AZ�RA]�A_�Aa�Ab�RAe�Ag�Ai�Aj�RAm�Ao�Aq�Ar�RAu�Aw�Ay�A|Q�A}�A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A��\A�(�A���A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A��\A�(�A���Aď\A�\)A���A�A�\)A�(�A�Ȁ\A�\)A���A�AЏ\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�A܏\A�(�A���Dq${Dq*�Dq1GDq7�DqD{DqJ�DqQGDq^Dqd{Dqj�Dqw�Dq~Dq�{Dq�GDq��Dq�Dq��Dq�GDq��Dq�Dq��Dq�GDq׮Dq�{Dq��Dq�GDq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrQGDrW�Dr^Drj�DrqGDrw�Dr�{Dr��Dr�GDr�Dr�{Dr��Dr��Dr�Dr�{Dr�GDr׮Dr�Dr�{Dr�GDr��Dr�Ds
�DsGDs�Ds${Ds*�Ds1GDs>DsD{DsJ�DsW�Ds^Dsd{Dsj�Dsw�Ds~Ds��Ds�GDs��Ds�{Ds��Ds�GDs��Ds�{Ds��Ds׮Ds�Ds�{Ds�GDs��Ds�Dt
�DtGDt�Dt${Dt*�Dt1GDt7�DtD{DtJ�DtW�Dt^Dtd{DtqGDtw�Dt~Dt��Dt�GDt��Dt�Dt��Dt�GDt�Dt�{Dt��Dt�GDt�Dt�{Dt��Dt��Dt�Du
�DuGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�x�A�t�A�~�A�VA�33A�-A�"�A���A��;AɶFAɓuA�M�A�9XA�=qA�5?A�-A�$�A�{A��A��AŴ9A�-A�7LA�Q�A��A���A��wA��`A���A�+A��!A�33A���A��\A���A�?}A�bA��
A��hA��HA��DA�-A�oA��A��A�(�A��A�ȴA��A��A�A�(�A�
=A���A���A�=qA�?}A��\A�=qA��A�^5A�A��A�VA�C�A��A�hsA��A��DA�VA�K�A�;dA�9XA�;dA�9XA�/A���A��A��A��A���A���A�XA�\)A��TA�&�A���A�  A��jA�ffA���A�oA��HA�r�A��mA��A�ffA�O�A�/A��A��A��FA��A�=qA��A���A�^5A��;A��A��A�oA}XAz�Aw"�AvVAt~�As�TArAox�Al��Aj��Ah��Ag�Ae�7AdI�AchsAbbA_/A]G�A[�;AX5?AU�AS`BAOl�AM�ALv�AL$�AJ-AIS�AHJAG%AE��AD��AD�ACx�AB��AA��AA�hAA&�A@�A@E�A=��A9�-A8��A8A7oA5�A4v�A3�A3oA3�A2�uA2�A1;dA0z�A/��A/t�A/�A.�A-ƨA-C�A-A,��A,ZA+/A*5?A)��A)�A(�9A'��A'G�A&M�A%
=A$��A#��A"��A!�FA!S�A $�A�A�A�AƨAE�A�HA/AbAȴAAZA��AbNAXA	��A�/A �A{A|�A?}A�^A�A�hA�jA|�A��A��A�A�TA �\@��R@���@�?}@�t�@���@��/@��y@��@��9@�K�@�~�@�@�9@�
=@�{@�D@�~�@�E�@�v�@��@ꗍ@�%@�o@�n�@�$�@���@��@�@�?}@䛦@�7L@��@�+@��T@��@�/@�/@��@Ӿw@�b@·+@�|�@��@�G�@Ǖ�@Ł@��@�
=@�z�@��@�  @�@�7L@��@��D@���@��;@�1@�\)@�n�@�n�@�-@�O�@��@�1'@�^5@��-@��@�r�@�Q�@�1@�t�@�@�V@���@�r�@�9X@�I�@�1'@���@�ȴ@���@�p�@�x�@�`B@�O�@��@��/@���@�z�@�z�@�j@���@�K�@�C�@��@�{@�hs@��`@��D@�(�@�  @��F@���@���@�7L@���@�Q�@��@�@��^@��@�hs@�?}@���@�bN@�b@��w@��@�n�@�E�@�5?@��@��h@�X@��@��`@�Ĝ@���@�r�@�9X@�ƨ@�\)@�;d@�o@��y@�~�@�E�@�5?@�-@�@��^@��h@���@�z�@��m@��F@���@���@��@�33@�o@���@��@�ȴ@���@�v�@��T@�`B@��@�Ĝ@��@�I�@�  @��
@��F@��P@�K�@�+@�"�@�o@���@�v�@�5?@�@�hs@��@���@�Z@�A�@��@��
@���@�|�@�@��R@�=q@�=q@���@�hs@�&�@�V@��`@��@��@��
@���@��@�l�@���@�^5@�{@��^@��7@�x�@�`B@�`B@�/@�V@���@���@��9@�Q�@� �@��w@�+@�o@��y@�~�@�M�@��@���@��^@��@��@��@��T@���@�I�@�&�@�?}@� �@��F@�+@�+@��;@�b@�1@���@�l�@�^5@�{@��@�J@���@��@��@���@��@��@��#@���@��-@��7@�`B@�O�@�?}@���@�Ĝ@��@��D@�Q�@�1'@� �@�b@��m@��F@��P@�33@��@��\@�F�@y�@qp�@hy>@bkQ@[!-@SK�@NZ�@G�@>�@7
=@1��@-&�@(�@$`�@�\@T�@�Q@U�@	�>@~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A���A�dZA�\)A��;A�XA�p�A��A�jA��A�1'A�$�A���A�O�A�5?A���A�M�A�1A�A�A�=qA��#A�x�A���A�x�A���A�G�A���A�dZA� �A���A��RA�M�A�C�A��mA���A�z�A�VA�dZA��DA�G�A�A�A��RA��/A��hA��HA�S�A�I�A�7LAș�A��TA��A�?}Aţ�AǙ�A�9XA�jA�Q�A�XAɸRA�z�A���A��A�bNA�Q�Aɇ+A�XA�S�A�x�A���A�XA�JA�JA��A�r�A��/A���A��A�XAÑhAș�A�I�A�O�A���A�1'A�A�A���A��;A�%A�A�1'A�A�O�AìA�`BA�I�A�\)A�t�A�l�A�E�A��A��hA�7LA��
A���A� �A� �A�E�A�p�A�A�bNA�t�A���A�jA�^5A���A�-AĲ-A�K�A�bNA�dZA�`BA�ƨA�A�AɼjA�A��AȁA�S�A���A�
=AÃAģ�A�/A�dZA�G�A��Aȏ\A�t�A�~�A�G�A�9XA�`BA��A���A�ȴA��jAƅA�C�A��Aƙ�A�/A�x�AǋDA�n�A�I�A�`BA�-A��jA�~�A��-A�A�  A���A�^5Aė�A��
A�ffA�(�A��mA���A�1A���A�=qA��A�A�JA�?}A�jA��mAÃA��TA��
A�$�A��
A��mA���A��yAɋDA�dZA�\)A�A�K�AɁA���AƟ�A��A�JA�hsA�E�A�ffA�O�A�XAǼjAȰ!A���A�ffA�ĜA�l�A�33A�VA�Q�A�jA�?}A�hsA�hsA�n�A�
=A�hsA�bNA�A�hsA�n�A�l�A�ffA�dZA�ffA�bNA�bNA�l�A�jA�ffA�p�A�p�A�hsA�dZA�jA�n�A�l�A�n�A�l�A�jA�l�A�ZA�ĜA�n�A�l�A�l�A�hsA�dZA�l�A�jA�dZA�n�A�l�A�l�A�l�A�jA�n�A�hsA�l�A�jA�l�A�hsA�l�A�l�A�jA�n�A�jA�l�A�jA�hsA�ffA�ffA�l�A�jA�n�A�p�A�hsA�jA�p�A�l�A�jA�hsA�\)A�hsA�dZA�\)A�jA�ffA�\)A�n�A�p�A�ffA�hsA�ffA�hsA�hsA�hsA�jA�bNA�hsA�n�A�hsA�l�A�n�A�l�A�p�A�p�A�l�A�l�A�hsA�jA�l�A�jA�jA�l�A�n�A�n�A�jA�jA�l�A�l�A�l�A�n�A�n�A�r�A�jA�n�A�n�A�n�A�ffA�n�A�jA�dZA�dZA�l�A�hsA�l�A�l�A�p�A�dZA�dZA�l�A�l�A�jA�hsA�r�A�n�A�n�A�l�A�r�A�p�A�jA�t�A�v�A�bNA�n�A�x�A�|�A�z�A�z�A�jA�p�A�v�A�t�A�v�A�|�A�x�A�v�A�|�A�t�A�|�A�~�A�z�A�v�A�x�A�p�A�|�A�z�A�x�A�~�A�~�A�~�A�z�A�~�A�~�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A�r�Aɛ�A�~�A�~�A�~�A�|�AʁAʃAʁAʁAʁAʁAʁA�~�A�bNA�|�A�|�A�z�A�1A�A�A�|�A�|�A�~�A�t�AʁA�~�A�x�A�/AʁA�|�A�|�A�z�AʁA�~�A�~�AʁA�~�AʁA�|�A�~�AʁA�~�A�z�A�~�A�~�A�~�A�n�AʁA�|�A�|�AʁAʃA�~�AʃAʁAʁAʁA�~�A�~�A�z�A�|�A�S�A�|�A�x�AʁAʁAʃAʁAʃAʃAʁAʁAʁAʁA�~�AʅAʃAʃAʅAʃAʅAʅAʅAʅAʃAʅAʅAʅAʃAʅAʅAʅAʅAʇ+AʅAʃAʅAʃAʃAʁAʃAʃAʁAʁAʁAʃAʁAʁAʁA�~�AʁA�~�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A�|�A�|�A�z�A�z�A�x�A�z�A�z�A�z�A�z�A�v�A�t�A�t�A�t�A�p�A�r�A�r�A�t�A�t�A�p�A�r�A�p�A�r�A�t�A�t�A�v�A�x�A�x�A�v�A�v�A�v�A�z�A�v�A�v�A�z�A�z�A�z�A�v�A�x�A�n�AʃAʃA�~�AʁAʅAʉ7Aʉ7Aʇ+Aʇ+Aʇ+A�x�A�v�A�ffA�Q�A�O�A�XA�Q�A�S�A�M�A�I�A�I�A�I�A�I�A�I�A�C�A�?}A�;dA�=qA�=qA�7LA�;dA�5?A�33A�/A�1'A�1'A�/A�-A�-A�1'A�/A�/A�+A�(�A�(�A�(�A�&�A�$�A�&�A�-A�-A�/A�5?A�9XA�7LA�1'A�1'A�5?A�5?A�-A�+A�"�A�5?A�/A�&�A�/A�33A�oA�bA��A�{A�VA�VA�VA�oA�
=A�A�A�  A�A�A���A�A���A��A��A��A��A���A��`A��;A��`A��;A��`A��#A��HA��HA��TA��HA��TA��TA��#A��A���A��A��
Aɰ!Aɰ!Aɰ!Aɲ-Aɴ9Aɰ!AɮAɰ!Aɲ-Aɰ!Aɩ�AɮAɬAɬAɡ�AɬAɬAɩ�Aə�Aɕ�AɑhAɃA�|�A�r�A�n�A�l�A�bNA�ZA�^5A�^5A�S�A�I�A�A�A�C�A�A�A�?}A�?}A�=qA�=qA�=qA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�9XA�9XA�9XA�9XA�9XA�=qA�9XA�;dA�=qA�;dA�;dA�;dA�=qA�=q@�(�@�(�@�(�@�(�@�(�@�(�@�(�@�(�@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@��@��@�b@�b@�b@�b@�b@�b@�b@�1@�1@�1@�1@�1@�1@�1@�1@�  @���@��@��@��@��@��@��m@��m@��;@��
@��
@���@�ƨ@�ƨ@��w@��w@��F@��F@��F@��@��@��F@��@��@��@��@��@���@���@���@���@���@���@���@��P@��P@��@��@�|�@�|�@�|�@�t�@�dZ@�dZ@�S�@�K�@�C�@�C�@�C�@�C�@�;d@�33@�;d@�33@�"�@�"�@��@��@�o@��@�o@�o@�
=@��@��H@�ȴ@���@��R@��R@��R@��R@��R@��!@��!@��!@��!@���@���@��\@��\@��+@�v�@�n�A�x�A�x�A�x�A�x�A�|�A�z�A�v�A�t�A�t�A�t�A�r�A�t�A�p�A�p�A�p�A�t�A�n�A�t�A�v�A�v�A�v�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�v�A�x�A�x�A�x�A�t�A�n�A�|�A�~�A�~�AʁAʅAʉ7Aʇ+Aʇ+Aʇ+Aʇ+Aʉ7A�x�A�l�A�O�A�K�A�XA�^5A�VA�S�A�I�A�G�A�K�A�I�A�M�A�E�A�9XA�;dA�A�A�?}A�;dA�7LA�33A�7LA�/A�1'A�/A�-A�-A�-A�-A�+A�-A�(�A�$�A�&�A�&�A�$�A�$�A�$�A�&�A�&�A�/A�9XA�7LA�1'A�1'A�5?A�5?A�33A�+A�+A�/A�;dA�5?A�=qA�1'A��A�oA�VA�oA�bA�bA�bA�VA�bA�A�A�A�  A�A���A���A���A���A��A��A��A��A��;A��mA��HA��/A��`A��/A��#A��#A��;A��;A��HA��TA��HA��#A��#A��
A��#A���Aɺ^Aɴ9AɮAɲ-Aɴ9Aɰ!AɬAɮAɰ!Aɩ�Aɰ!Aɰ!AɮAɩ�Aɧ�AɮAɛ�Aə�Aɕ�Aɏ\Aɉ7A�~�A�v�A�p�A�l�A�l�A�ffA�bNA�^5A�^5A�VA�G�A�A�A�A�A�?}A�?}A�?}A�=qA�=qA�;dA�;dA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�;dA�9XA�;dA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�=qA�;d@�(�@�(�@�(�@�(�@�(�@�(�@�(�@�(�@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@��@�b@�b@�b@�b@�b@�b@�b@�b@�1@�1@�1@�1@�1@�1@�1@�1@�1@�  @��@��@��@��@��@��m@��m@��;@��
@���@���@���@�ƨ@�ƨ@��w@��F@��F@��F@��F@��F@��@��@��@��@��@��@���@���@���@���@���@���@���@���@��P@��@��@��@�|�@�|�@�t�@�l�@�dZ@�\)@�K�@�C�@�C�@�C�@�C�@�;d@�;d@�33@�33@�+@�"�@��@��@��@��@�o@�o@�
=@�@��y@���@���@��R@��R@��R@��R@��R@��!@��!@��!@��!@���@���@���@��\@��+@�~�@�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�x�A�t�A�~�A�VA�33A�-A�"�A���A��;AɶFAɓuA�M�A�9XA�=qA�5?A�-A�$�A�{A��A��AŴ9A�-A�7LA�Q�A��A���A��wA��`A���A�+A��!A�33A���A��\A���A�?}A�bA��
A��hA��HA��DA�-A�oA��A��A�(�A��A�ȴA��A��A�A�(�A�
=A���A���A�=qA�?}A��\A�=qA��A�^5A�A��A�VA�C�A��A�hsA��A��DA�VA�K�A�;dA�9XA�;dA�9XA�/A���A��A��A��A���A���A�XA�\)A��TA�&�A���A�  A��jA�ffA���A�oA��HA�r�A��mA��A�ffA�O�A�/A��A��A��FA��A�=qA��A���A�^5A��;A��A��A�oA}XAz�Aw"�AvVAt~�As�TArAox�Al��Aj��Ah��Ag�Ae�7AdI�AchsAbbA_/A]G�A[�;AX5?AU�AS`BAOl�AM�ALv�AL$�AJ-AIS�AHJAG%AE��AD��AD�ACx�AB��AA��AA�hAA&�A@�A@E�A=��A9�-A8��A8A7oA5�A4v�A3�A3oA3�A2�uA2�A1;dA0z�A/��A/t�A/�A.�A-ƨA-C�A-A,��A,ZA+/A*5?A)��A)�A(�9A'��A'G�A&M�A%
=A$��A#��A"��A!�FA!S�A $�A�A�A�AƨAE�A�HA/AbAȴAAZA��AbNAXA	��A�/A �A{A|�A?}A�^A�A�hA�jA|�A��A��A�A�TA �\@��R@���@�?}@�t�@���@��/@��y@��@��9@�K�@�~�@�@�9@�
=@�{@�D@�~�@�E�@�v�@��@ꗍ@�%@�o@�n�@�$�@���@��@�@�?}@䛦@�7L@��@�+@��T@��@�/@�/@��@Ӿw@�b@·+@�|�@��@�G�@Ǖ�@Ł@��@�
=@�z�@��@�  @�@�7L@��@��D@���@��;@�1@�\)@�n�@�n�@�-@�O�@��@�1'@�^5@��-@��@�r�@�Q�@�1@�t�@�@�V@���@�r�@�9X@�I�@�1'@���@�ȴ@���@�p�@�x�@�`B@�O�@��@��/@���@�z�@�z�@�j@���@�K�@�C�@��@�{@�hs@��`@��D@�(�@�  @��F@���@���@�7L@���@�Q�@��@�@��^@��@�hs@�?}@���@�bN@�b@��w@��@�n�@�E�@�5?@��@��h@�X@��@��`@�Ĝ@���@�r�@�9X@�ƨ@�\)@�;d@�o@��y@�~�@�E�@�5?@�-@�@��^@��h@���@�z�@��m@��F@���@���@��@�33@�o@���@��@�ȴ@���@�v�@��T@�`B@��@�Ĝ@��@�I�@�  @��
@��F@��P@�K�@�+@�"�@�o@���@�v�@�5?@�@�hs@��@���@�Z@�A�@��@��
@���@�|�@�@��R@�=q@�=q@���@�hs@�&�@�V@��`@��@��@��
@���@��@�l�@���@�^5@�{@��^@��7@�x�@�`B@�`B@�/@�V@���@���@��9@�Q�@� �@��w@�+@�o@��y@�~�@�M�@��@���@��^@��@��@��@��T@���@�I�@�&�@�?}@� �@��F@�+@�+@��;@�b@�1@���@�l�@�^5@�{@��@�J@���@��@��@���@��@��@��#@���@��-@��7@�`B@�O�@�?}@���@�Ĝ@��@��D@�Q�@�1'@� �@�b@��m@��F@��P@�33@��G�O�@�F�@y�@qp�@hy>@bkQ@[!-@SK�@NZ�@G�@>�@7
=@1��@-&�@(�@$`�@�\@T�@�Q@U�@	�>@~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A���A�dZA�\)A��;A�XA�p�A��A�jA��A�1'A�$�A���A�O�A�5?A���A�M�A�1A�A�A�=qA��#A�x�A���A�x�A���A�G�A���A�dZA� �A���A��RA�M�A�C�A��mA���A�z�A�VA�dZA��DA�G�A�A�A��RA��/A��hA��HA�S�A�I�A�7LAș�A��TA��A�?}Aţ�AǙ�A�9XA�jA�Q�A�XAɸRA�z�A���A��A�bNA�Q�Aɇ+A�XA�S�A�x�A���A�XA�JA�JA��A�r�A��/A���A��A�XAÑhAș�A�I�A�O�A���A�1'A�A�A���A��;A�%A�A�1'A�A�O�AìA�`BA�I�A�\)A�t�A�l�A�E�A��A��hA�7LA��
A���A� �A� �A�E�A�p�A�A�bNA�t�A���A�jA�^5A���A�-AĲ-A�K�A�bNA�dZA�`BA�ƨA�A�AɼjA�A��AȁA�S�A���A�
=AÃAģ�A�/A�dZA�G�A��Aȏ\A�t�A�~�A�G�A�9XA�`BA��A���A�ȴA��jAƅA�C�A��Aƙ�A�/A�x�AǋDA�n�A�I�A�`BA�-A��jA�~�A��-A�A�  A���A�^5Aė�A��
A�ffA�(�A��mA���A�1A���A�=qA��A�A�JA�?}A�jA��mAÃA��TA��
A�$�A��
A��mA���A��yAɋDA�dZA�\)A�A�K�AɁA���AƟ�A��A�JA�hsA�E�A�ffA�O�A�XAǼjAȰ!A���A�ffA�ĜA�l�A�33A�VA�Q�A�jA�?}A�hsA�hsA�n�A�
=A�hsA�bNA�A�hsA�n�A�l�A�ffA�dZA�ffA�bNA�bNA�l�A�jA�ffA�p�A�p�A�hsA�dZA�jA�n�A�l�A�n�A�l�A�jA�l�A�ZA�ĜA�n�A�l�A�l�A�hsA�dZA�l�A�jA�dZA�n�A�l�A�l�A�l�A�jA�n�A�hsA�l�A�jA�l�A�hsA�l�A�l�A�jA�n�A�jA�l�A�jA�hsA�ffA�ffA�l�A�jA�n�A�p�A�hsA�jA�p�A�l�A�jA�hsA�\)A�hsA�dZA�\)A�jA�ffA�\)A�n�A�p�A�ffA�hsA�ffA�hsA�hsA�hsA�jA�bNA�hsA�n�A�hsA�l�A�n�A�l�A�p�A�p�A�l�A�l�A�hsA�jA�l�A�jA�jA�l�A�n�A�n�A�jA�jA�l�A�l�A�l�A�n�A�n�A�r�A�jA�n�A�n�A�n�A�ffA�n�A�jA�dZA�dZA�l�A�hsA�l�A�l�A�p�A�dZA�dZA�l�A�l�A�jA�hsA�r�A�n�A�n�A�l�A�r�A�p�A�jA�t�A�v�A�bNA�n�A�x�A�|�A�z�A�z�A�jA�p�A�v�A�t�A�v�A�|�A�x�A�v�A�|�A�t�A�|�A�~�A�z�A�v�A�x�A�p�A�|�A�z�A�x�A�~�A�~�A�~�A�z�A�~�A�~�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A�r�Aɛ�A�~�A�~�A�~�A�|�AʁAʃAʁAʁAʁAʁAʁA�~�A�bNA�|�A�|�A�z�A�1A�A�A�|�A�|�A�~�A�t�AʁA�~�A�x�A�/AʁA�|�A�|�A�z�AʁA�~�A�~�AʁA�~�AʁA�|�A�~�AʁA�~�A�z�A�~�A�~�A�~�A�n�AʁA�|�A�|�AʁAʃA�~�AʃAʁAʁAʁA�~�A�~�A�z�A�|�A�S�A�|�A�x�AʁAʁAʃAʁAʃAʃAʁAʁAʁAʁA�~�AʅAʃAʃAʅAʃAʅAʅAʅAʅAʃAʅAʅAʅAʃAʅAʅAʅAʅAʇ+AʅAʃAʅAʃAʃAʁAʃAʃAʁAʁAʁAʃAʁAʁAʁA�~�AʁA�~�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A�|�A�|�A�z�A�z�A�x�A�x�A�x�A�x�A�x�A�|�A�z�A�v�A�t�A�t�A�t�A�r�A�t�A�p�A�p�A�p�A�t�A�n�A�t�A�v�A�v�A�v�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�v�A�x�A�x�A�x�A�t�A�n�A�|�A�~�A�~�AʁAʅAʉ7Aʇ+Aʇ+Aʇ+Aʇ+Aʉ7A�x�A�l�A�O�A�K�A�XA�^5A�VA�S�A�I�A�G�A�K�A�I�A�M�A�E�A�9XA�;dA�A�A�?}A�;dA�7LA�33A�7LA�/A�1'A�/A�-A�-A�-A�-A�+A�-A�(�A�$�A�&�A�&�A�$�A�$�A�$�A�&�A�&�A�/A�9XA�7LA�1'A�1'A�5?A�5?A�33A�+A�+A�/A�;dA�5?A�=qA�1'A��A�oA�VA�oA�bA�bA�bA�VA�bA�A�A�A�  A�A���A���A���A���A��A��A��A��A��;A��mA��HA��/A��`A��/A��#A��#A��;A��;A��HA��TA��HA��#A��#A��
A��#A���Aɺ^Aɴ9AɮAɲ-Aɴ9Aɰ!AɬAɮAɰ!Aɩ�Aɰ!Aɰ!AɮAɩ�Aɧ�AɮAɛ�Aə�Aɕ�Aɏ\Aɉ7A�~�A�v�A�p�A�l�A�l�A�ffA�bNA�^5A�^5A�VA�G�A�A�A�A�A�?}A�?}A�?}A�=qA�=qA�;dA�;dA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�;dA�9XA�;dA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�=qA�;d@�(�@�(�@�(�@�(�@�(�@�(�@�(�@�(�@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@��@�b@�b@�b@�b@�b@�b@�b@�b@�1@�1@�1@�1@�1@�1@�1@�1@�1@�  @��@��@��@��@��@��m@��m@��;@��
@���@���@���@�ƨ@�ƨ@��w@��F@��F@��F@��F@��F@��@��@��@��@��@��@���@���@���@���@���@���@���@���@��P@��@��@��@�|�@�|�@�t�@�l�@�dZ@�\)@�K�@�C�@�C�@�C�@�C�@�;d@�;d@�33@�33@�+@�"�@��@��@��@��@�o@�o@�
=@�@��y@���@���@��R@��R@��R@��R@��R@��!@��!@��!@��!@���@���@���@��\@��+@�~�@�n�A�x�A�x�A�x�A�x�A�|�A�z�A�v�A�t�A�t�A�t�A�r�A�t�A�p�A�p�A�p�A�t�A�n�A�t�A�v�A�v�A�v�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�v�A�x�A�x�A�x�A�t�A�n�A�|�A�~�A�~�AʁAʅAʉ7Aʇ+Aʇ+Aʇ+Aʇ+Aʉ7A�x�A�l�A�O�A�K�A�XA�^5A�VA�S�A�I�A�G�A�K�A�I�A�M�A�E�A�9XA�;dA�A�A�?}A�;dA�7LA�33A�7LA�/A�1'A�/A�-A�-A�-A�-A�+A�-A�(�A�$�A�&�A�&�A�$�A�$�A�$�A�&�A�&�A�/A�9XA�7LA�1'A�1'A�5?A�5?A�33A�+A�+A�/A�;dA�5?A�=qA�1'A��A�oA�VA�oA�bA�bA�bA�VA�bA�A�A�A�  A�A���A���A���A���A��A��A��A��A��;A��mA��HA��/A��`A��/A��#A��#A��;A��;A��HA��TA��HA��#A��#A��
A��#A���Aɺ^Aɴ9AɮAɲ-Aɴ9Aɰ!AɬAɮAɰ!Aɩ�Aɰ!Aɰ!AɮAɩ�Aɧ�AɮAɛ�Aə�Aɕ�Aɏ\Aɉ7A�~�A�v�A�p�A�l�A�l�A�ffA�bNA�^5A�^5A�VA�G�A�A�A�A�A�?}A�?}A�?}A�=qA�=qA�;dA�;dA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�;dA�9XA�;dA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�=qA�;d@�(�@�(�@�(�@�(�@�(�@�(�@�(�@�(�@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@� �@��@�b@�b@�b@�b@�b@�b@�b@�b@�1@�1@�1@�1@�1@�1@�1@�1@�1@�  @��@��@��@��@��@��m@��m@��;@��
@���@���@���@�ƨ@�ƨ@��w@��F@��F@��F@��F@��F@��@��@��@��@��@��@���@���@���@���@���@���@���@���@��P@��@��@��@�|�@�|�@�t�@�l�@�dZ@�\)@�K�@�C�@�C�@�C�@�C�@�;d@�;d@�33@�33@�+@�"�@��@��@��@��@�o@�o@�
=@�@��y@���@���@��R@��R@��R@��R@��R@��!@��!@��!@��!@���@���@���@��\@��+@�~�@�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@W�	>!��?���?Pk@���?�U�>kf@�z=��Y?CS�>$t?�@��Z@���=��=���?&[�@��/@_d0=��r=ݼ�>1��>M�g=ײ>)��?�C�>�4@���?ˏ�>8�>~ �@���>�R�>�P>a?�@���@(/Z=}��=���=kF5=�=�v=�>>.�w@���@��> �,?vyh@��?�ti>�>O��?�@���?Ŀ�>�t@��`@y��=�L�?���=���>�8�>?�G�@���@���?	��?�>@��6@��>��?ЮS@�J=�U�>-	@���@���=�>Z`�@��a@���=⌽=��A=��j>��@�K>
%�?�,@��r=��=Ω�>�z�?���@��l?�@�=�+�>��E>r�@��T>D@�H>��=�n/=ݗ�?"XO@���>
q�>�C�@���@��z> n?;:@��>�+�=��=��>j��@��l@��>@��=�D�>+@�#O?րI=�i>��=�=>-��>��>/Y�>>R>T�@���@���>΅?���?!��?v��@���=�ؙ>a>��j@���=Ġ�>7F�>$�?LcI@���>:�=�e=��>E��@��1@���>�@�� @(�E>!> ��?<�>��@b�r@���>��>zf@��p?8�k?$"h>�c@��>xGo?!Z>�1�?�|=��>lG@���?��=���?	b�@S&@�õ@I��>N=@��><��?�/�>pk@���=�:?/��?qo @��b>
d�@]�4?���@���>4�@��$?���@���@f�P@��$?]�1@���?��@��s@��@���>G��@��p@��q@���@��V@��@hNQ@���@��@+�@���@��s@��s@���@���@��c@��c@��@���@���@���@��A@��E@��@��c@���@��E@���@���@��c@��x@���@��@v��@���@��@��c@��@���@��c@���@���@���@��s@��@���@���@��s@���@���@��x@��@���@���@���@��@��s@��c@���@��@��@��c@���@���@��s@��0@���@���@��0@��0@��E@���@���@��@���@���@��,@���@��$@��@���@��s@��$@���@���@���@��x@��@���@���@���@��@���@���@���@��@���@��0@���@��@���@���@��0@��E@��@���@���@��V@��s@���@��0@��E@���@��A@���@���@���@���@���@��V@���@��0@���@��R@��@��c@���@���@���@��f@���@��@���@��s@��s@��c@��f@���@���@��A@���@���@���@���@��4@���@���@��Y@��@��j@���@��4@���@��Y@���@���@���@���@��@��@���@��@��@��@���@��@��@��j@��j@��j@��j@��j@��j@��j@��j@��@��@���@��j@��j@���@���@���@��z@��H@��z@��z@��@��@��@��z@��@��@��z@��z@��@��z@��z@��j@��@��j@���@���@��@��@��j@��@��z@��@��@���@���@��z@��z@���@��z@��z@���@���@��@��z@��@��z@��z@���@��@���@���@��@��@��@��z@��z@��7@��7@���@���@���@���@���@���@���@���@��j@���@��j@��@�Ë@�Ë@�Ë@�Ë@���@���@���@�Ë@��7@��7@���@���@��]@��H@��]@�ı@�ı@��H@�ı@�ı@��@�ı@��@��]@�ı@��@�ı@��]@�ı@�ı@�ı@���@��]@���@�à@��]@�à@�Ë@��7@���@��L@�à@��z@��z@��L@��7@��z@��z@��z@��z@��L@��L@���@��7@���@���@���@���@���@��@��@��Y@���@��@���@���@��H@���@��H@��H@��4@��H@��4@��4@��H@���@���@��Y@��Y@��@��j@��j@��@��j@���@���@��@���@���@��'@�@���@��'@��@��	@��]@��@��@��*@��?@��*@���@�ł@���@���@��@��@���@��x@��V@��R@��|@���@���@���@���@���@��5@��c@���@���@���@���@��$@��c@��p@��-@��-@��p@���@���@��F@��@��p@���@��`@���@���@��@��@��y@��@���@���@��1@��l@��S@���@��l@��B@���@��l@��B@��@���@��u@���@��u@���@��>@��~@���@��i@��~@���@���@��H@��m@���@��z@��e@���@��j@��j@���@��Y@���@���@��V@��V@��f@��@��c@���@��R@���@���@���@���@���@���@��A@���@���@��1@��S@��p@��1@��@��z@���@��/@��Y@���@���@���@��H@���@���@���@���@���@���@��7@���@���@���@��
@���@��
@�|p@�y)@�w@�u�@�t�@�s@�o~@�o�@�p�@�oT@�l7@�i�@�hs@�g�@�g�@�hs@�h@�g�@�g8@�f�@�fQ@�f�@�f{@�f�@�g@�g@�g@�g@�g�@�g�@�h@�g�@�h�@�h�@�i�@�j@�i�@�jU@�j@�j@�j�@P�5@P�_@P��@P�@P�@P�@P�5@P�@P�@P�@P�@P��@P�@P�@P�c@P��@P�@P�@P�g@P�@P�g@P�g@P�@P�>@P��@P��@P�l@P�B@P�B@P��@P��@P��@P��@P��@P�F@P��@P�@P��@P�S@P�}@P��@P�@P�@P��@P�@P��@P�@P��@P�@P�C@P�H@P��@P��@P��@Pߤ@P�P@P�P@P�P@P�P@Pީ@Pީ@P�@P�@P�@P�Y@P�@Pܱ@Pܱ@P۶@P�@P�@P�@P�o@P��@P�@P�$@P��@P��@P��@P҉@P��@Pϖ@P��@P��@P�@P��@P�@P�u@P��@P�%@P�)@Pʂ@PɆ@P��@PȊ@P�6@P�;@PƓ@Pŗ@Pà@P�@P�@P�o@P��@P�s@P�@P��@P�x@P�$@P�|@P�|@P�R@P��@P��@P��@P�>@P�@P�F@P��@P��@��@�ݘ@�ݭ@�ݭ@��@�ݘ@��r@�ܱ@��@�۶@��w@��@��w@���@���@���@��<@��]@�ݭ@��/@��@��n@�ݘ@��Y@��/@��/@�ݭ@���@��U@���@��j@�ޔ@���@��/@��8@��@���@��a@��H@��*@��?@��*@��?@��T@��*@��z@��"@�ܱ@�ջ@��k@��@�پ@�֡@��^@��,@��F@�Ԫ@���@��,@���@�ϫ@��)@��$@��9@��)@��-@���@��F@��6@���@���@���@��6@��:@�̸@�̸@��:@���@���@���@���@��)@��)@���@��>@�˧@�͟@��}@���@��F@���@���@���@�ρ@�͟@�̣@�͟@��N@�Ц@��_@��W@�ɰ@��@��@�� @���@��.@�ŗ@���@�Ɠ@���@��U@��@@��@��j@���@���@���@���@���@��A@��@��@���@���@���@��@��|@��@��1@��_@���@���@���@��@���@��F@��p@���@��@���@���@��T@���@��m@���@���@���@���@���@���@��X@���@���@���@��<@��@��@���@��'@���@��g@��J@��h@���@��y@��%@���@���@���@��P@��r@���@��{@��@���@��<@��<@���@��@��Z@��@���@���@���@���@���@��@��@��E@���@���@���@��A@��A@���@��@���@��#@��8@��M@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q.@Q.@QW@QW@QW@QW@QW@QW@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q`@Q6@Q�@Q�@Q�@Q�@Q6@Q6@Q@Q�@Q �@P��@P��@P��@P�H@P��@P�v@P��@P��@P�+@P�@P�3@P��@P�8@P��@P��@P�@P��@P�o@P�E@P�E@P�@P��@P��@P��@P��@P�s@P�I@P�@P��@P�M@P��@P�@P�(@P�,@P�@P��@P�@P��@P�>@P��@P�B@P��@P�O@P��@P��@P�@P��@P�6@P�@P�@P�e@P��@P�?@P�@P�H@P�L@Pߤ@Pߤ@P�P@P�P@P��@P�@Pܱ@P�@PԀ@P��@P�h@P�@P��@Pϖ@Pϖ@P�B@P�B@P��@P�p@P��@P��@P�S@P�2@P�@P��@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      33444344344443344433444444443444344434444444433443444434433444444334433443443344334444444344443444434344443443344344443334434444444443344443444344443444433434444433443444344444434444344344434443434343433343433343333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@W�G�O�G�O�G�O�@���G�O�G�O�@�zG�O�G�O�G�O�G�O�@��Z@���G�O�G�O�G�O�@��-@_d0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�@���G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@��G�O�G�O�@��G�O�G�O�G�O�G�O�@���G�O�G�O�@��e@y��G�O�G�O�G�O�G�O�G�O�G�O�@���@���G�O�G�O�@��6@��G�O�G�O�@�FG�O�G�O�@���@���G�O�G�O�@��b@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��vG�O�G�O�G�O�G�O�@��iG�O�G�O�G�O�G�O�@��TG�O�@�HG�O�G�O�G�O�G�O�@���G�O�G�O�@���@��xG�O�G�O�@��G�O�G�O�G�O�G�O�@��m@��>@��G�O�G�O�@�#NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@���G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@��/@���G�O�@�� G�O�G�O�G�O�G�O�G�O�@b�r@���G�O�G�O�@��rG�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@�úG�O�G�O�@��G�O�G�O�G�O�@���G�O�G�O�G�O�@��bG�O�@]�5G�O�@���G�O�@��&G�O�@���@f�R@��$G�O�@���G�O�@��|@��@���G�O�@��p@��s@���@��V@��"@hNU@���@��G�O�@���@��v@��|@�� @���@��d@��c@��$@���@���@���@��C@��E@�� @��a@���@��B@���@���@��d@��v@���@��@v��@���@�� @��a@��@���@��d@��~@���@���@��|@��@���@���@��v@���@���@��v@��@���@���@���@��@��x@��f@���@��@��"@��a@���@���@��r@��2@���@���@��2@��2@��E@���@���@��@���@���@��*@���@��)@��@���@��v@��"@���@���@���@��y@��"@���@���@���@��@���@���@���@��@���@��2@���@��@���@���@��,@��B@��@���@���@��Y@��r@���@��1@��E@���@��A@���@���@���@���@���@��Y@���@��1@���@��U@��!@��d@���@���@���@��j@���@�� @���@��|@��r@��`@��h@���@���@��>@���@���@���@���@��3@���@���@��[@��@��j@���@��3@���@��[@���@���@���@���@��@��@���@��@���@��@���@��@��@��p@��j@��f@��l@��i@��l@��j@��l@��@���@���@��p@��l@���@���@���@��z@��F@��|@��|@��@��@��@��z@��@��@��{@��~@��@��~@��{@��p@��@��m@���@���@��@��@��l@��@��~@��@��@���@���@��{@��@���@��|@��~@���@���@��@��~@��@��|@��{@���@��@���@���@��@��@��@��{@��@��>@��:@���@���@���@���@���@���@���@���@��p@���@��p@��@�Î@�Î@�Ê@�Î@���@���@���@�Î@��:@��:@���@���@��\@��L@��a@�Ĵ@�ı@��J@�Ĳ@�ı@��@�Ĵ@��@��\@�Ĵ@��@�Ĳ@��a@�Ĳ@�Ĳ@�Ĳ@���@��_@���@�â@��Z@�â@�Î@��8@���@��R@�ß@��y@��y@��O@��;@��~@��y@��{@��z@��N@��N@���@��8@���@���@���@���@���@��@���@�ݚ@�ݱ@�ݯ@��~@�ݚ@��p@�ܰ@��@�۵@��v@��@��v@���@���@���@��9@��]@�ݬ@��/@��@��k@�ݗ@��V@��/@��1@�ݮ@���@��V@���@��l@�ޘ@���@��.@��;@��@���@��e@��I@��'@��>@��-@��>@��S@��.@��w@��$@�ܳ@�ս@��j@��@���@�֥@��[@��*@��G@�ԫ@���@��.@���@�ϭ@��,@��&@��@@��(@��.@���@��G@��5@���@���@���@��:@��:@�̺@�̷@��:@��@���@���@���@��,@��)@���@��A@�˦@�͘@�Ё@���@��J@���@���@���@��~@�͞@�̤@�͞@��O@�Р@��^@��\@�ɵ@��@��@��@���@��0@�Ś@���@�Ɛ@���@��V@��>@��@��m@���@���@���@���@���@��@@��@��@���@���@���@��@��}@��@��2@��^@���@���@���@��@���@��F@��r@���@��
@���@���@��S@���@��n@���@��~@���@���@���@���@��X@���@���@���@��<@�� @��!@���@��*@���@��g@��M@��i@���@��~@��'@���@���@���@��T@��t@���@��|@��@���@��<@��<@���@��@��Z@��@���@���@���@���@���@��@��@��J@���@���@���@��E@��?@���@��@���@��&@��6@��N@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q+@Q(@QV@QV@QS@QV@QU@QZ@Q�@Q}@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Qb@Q6@Q�@Q�@Q�@Q�@Q5@Q6@Q@Q�@Q �@P��@P��@P��@P�E@P��@P�x@P��@P��@P�+@P�@P�8@P��@P�6@P��@P��@P�@P��@P�r@P�F@P�J@P�@P��@P��@P��@P��@P�z@P�F@P�"@P��@P�N@P��@P�@P�*@P�(@P�@P��@P�@P��@P�=@P��@P�@@P��@P�R@P��@P��@P�@P��@P�8@P�@P�@P�e@P��@P�@@P�@P�H@P�N@Pߣ@Pߥ@P�R@P�V@P��@P� @Pܶ@P�@P�~@P��@P�j@P�@P��@Pϛ@Pϖ@P�>@P�E@P��@P�s@P��@P��@P�V@P�3@P�@P��@P�@���@�ݚ@�ݱ@�ݯ@��~@�ݚ@��p@�ܰ@��@�۵@��v@��@��v@���@���@���@��9@��]@�ݬ@��/@��@��k@�ݗ@��V@��/@��1@�ݮ@���@��V@���@��l@�ޘ@���@��.@��;@��@���@��e@��I@��'@��>@��-@��>@��S@��.@��w@��$@�ܳ@�ս@��j@��@���@�֥@��[@��*@��G@�ԫ@���@��.@���@�ϭ@��,@��&@��@@��(@��.@���@��G@��5@���@���@���@��:@��:@�̺@�̷@��:@��@���@���@���@��,@��)@���@��A@�˦@�͘@�Ё@���@��J@���@���@���@��~@�͞@�̤@�͞@��O@�Р@��^@��\@�ɵ@��@��@��@���@��0@�Ś@���@�Ɛ@���@��V@��>@��@��m@���@���@���@���@���@��@@��@��@���@���@���@��@��}@��@��2@��^@���@���@���@��@���@��F@��r@���@��
@���@���@��S@���@��n@���@��~@���@���@���@���@��X@���@���@���@��<@�� @��!@���@��*@���@��g@��M@��i@���@��~@��'@���@���@���@��T@��t@���@��|@��@���@��<@��<@���@��@��Z@��@���@���@���@���@���@��@��@��J@���@���@���@��E@��?@���@��@���@��&@��6@��N@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q+@Q(@QV@QV@QS@QV@QU@QZ@Q�@Q}@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Qb@Q6@Q�@Q�@Q�@Q�@Q5@Q6@Q@Q�@Q �@P��@P��@P��@P�E@P��@P�x@P��@P��@P�+@P�@P�8@P��@P�6@P��@P��@P�@P��@P�r@P�F@P�J@P�@P��@P��@P��@P��@P�z@P�F@P�"@P��@P�N@P��@P�@P�*@P�(@P�@P��@P�@P��@P�=@P��@P�@@P��@P�R@P��@P��@P�@P��@P�8@P�@P�@P�e@P��@P�@@P�@P�H@P�N@Pߣ@Pߥ@P�R@P�V@P��@P� @Pܶ@P�@P�~@P��@P�j@P�@P��@Pϛ@Pϖ@P�>@P�E@P��@P�s@P��@P��@P�V@P�3@P�@P��@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      33444344344443344433444444443444344434444444433443444434433444444334433443443344334444444344443444434344443443344344443334434444444443344443444344443444433434444433443444344444434444344344434443434343433343433343333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9/]9/�9/9/9/�9/�9/�9/
9/u9/9/�9/w9/�9/69/N9/!9/�9/�9/	9/�9/v9/�9/�9/�9/�9/�9/9/49/�9/ 9/�9/�9/I9/�9/�9/t9/�9/�9/�9/�9/�9/�9/�9/�9/�9/�9/�9/9/
 9/�9/g9/9/
�9/�9/h9/�9/�9/)9/l9/19/�9/]9/Z9/t9/Y9/\9/9/r9/\9/9/ �9/ �9/a9/ ^9/ �9/ �9/ ^9/ %9.�9.�9.�9.�M9.�J9.�
9.�b9.��9/�9/�9/9/u9/�9//9/9/�9/�9/ �9/�9/�9/�9/�9/�9.��9.�&9.�)9.�9.��9.�=9.��9.��9.��9.��9.�W9.�>9.�9.�n9.�9.��9.�9.�9.�9.�09.�p9.�9.�f9.��9.��9.��9.�a9.��9.�9.�79.�9.�s9.��9.��9.�9.�"9.�O9.�i9.��9.�9.߫9.�9.�E9.�9.݇9.�-9.څ9.�I9.�W9.�N9.�9.ۜ9.�K9.�`9.��9.ۮ9.ռ9.�P9.Ӿ9.�h9.��9.��9.��9.��9.��9.ō9.�'9.�(9.�9.��9.��9.�9.��9.�G9.�	9.�r9.�r9.��9.�79.��9.�69.�9.��9.��9.��9.�9.�29.�E9.�y9.��9.�
9.��9.�x9.�r9.�9.�I9.�49.�_9.�o9.��8ӯ=8ӯ8ӯ88ӯc8ӯ_8ӯ=8ӯ:8ӯ8Ӯ�8Ӯ�8Ӯ�8Ӯ�8Ӯ�8Ӯ�8Ӯ�8Ӯ�8Ӯ�8Ӯ�8Ӯ�8ӭC8ӭ>8ӭ>8ӭ8ӭ98ӭ8ӭ8Ӭ�8Ӭ�8Ӭ�8Ӭ>8Ӭ8Ӭ8Ӭ8Ӭ�8Ӭ�8Ӭi8Ӭ8Ӫ�8ө8Ө8ө8Ө�8Ө@8ӧ�8ӧ8Ӧ8ӥp8ӤD8ӣw8Ӣ�8Ӣr8ӡ�8Ӡ�8ӠJ8ӟ�8ӟ�8ӟx8ӟ|8ӟN8ӟ8ӟ8Ӟ�8Ӟ�8Ӟ�8Ӟu8ӞP8ӝ�8ӝz8ӝ"8Ӝ�8ӜR8ӛM8ә�8Ә�8Ә�8Ә 8ӗU8Ӗ�8ӖU8ӕ8ӓ]8Ӓ8ӏ�8ӎ�8ӎ�8ӏ68ӏ�8ӎ�8ӎ`8Ӎ�8Ӎ78ӌ8Ӌ98ӊ<8Ӊ�8Ӊ�8Ӊ<8Ӊ@8ӈ�8Ӈ�8ӆ�8Ӂ�8�~E8�{�8�z$8�y�8�yw8�yR8�yM8�x�8�x�8�x~8�x&8�w~8�v�8�t�8�r�8�q�8�p�8�m�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�5B�TB�ZB�`B�yB�B�B�B��B��B��B��B��B��B��B  BbB\BB�ZB��B��B��B��B��B�wB�qB�RB��B��B�oB�hB�=B�=B�bB�Bz�Bx�Bm�BaHB^5B`BBt�By�Bt�Br�Bt�Bm�BW
BW
BZBH�B6FBDB  B��B�B�B�TB�B��BŢB�}B�FB�B��B��B��B��B��B��B��B��B��B�hB�B{�Bo�BZB<jB&�B�B�B�BÖB�jB�3B��B�uBz�B`BBS�BJ�BG�BE�BA�B:^B(�BPB
�B
�#B
�}B
��B
��B
��B
�1B
z�B
ffB
L�B
8RB
�B
\B
B	��B	�B	�#B	ɺB	�^B	�B	��B	�{B	�DB	�B	w�B	hsB	aHB	XB	G�B	<jB	.B	�B	�B	�B	�B	PB		7B	B��B��B��B��B�B�B�B�B�B�B�`B��B�dB�RB�?B�9B�'B�B�B�B�B�?B�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�=B�%B�B{�Bt�Bn�BjBdZB_;BZBW
BS�BN�BJ�BF�BD�BA�B@�B=qB:^B;dB@�BM�BffBw�Bu�Bq�Bo�Bo�Bn�Bm�Bl�BjBiyBhsBffBdZBjBiyBffBe`BhsBgmBo�Bm�BiyBffBe`BdZBq�By�B�%B�B� B|�By�Bw�Bu�Bp�Bu�B�B�1B}�B~�B~�B~�By�Bw�Bw�Bu�Bm�BgmBbNB[#B[#B[#B^5B^5B]/B^5Bn�Bw�Bx�Bw�Bv�Bw�Bx�B}�B�{B��B��B��B��B��B��B��B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�3B�9B�?B�FB�FB�FB�LB�LB�RB�XB��BÖBÖBĜBŢBȴB��B��B��B��B�B�HB�ZB�yB�B�B��B��B��B��B��B	B	+B	+B		7B	PB	hB	�B	�B	�B	�B	�B	%�B	)�B	+B	,B	-B	.B	/B	1'B	5?B	7LB	8RB	:^B	<jB	>wB	?}B	@�B	@�B	B�B	C�B	E�B	G�B	L�B	O�B	P�B	P�B	Q�B	XB	ZB	[#B	]/B	]/B	]/B	_;B	bNB	dZB	ffB	iyB	jB	jB	l�B	m�B	m�B	n�B	p�B	r�B	s�B	t�B	x�B	{�B	}�B	~�B	�B	�%B	�1B	�=B	�=B	�=B	�DB	�PB	�PB	�PB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�FB	�FB	�LB	�LB	�XB	�dB	�}B	��B	ÖB	ƨB	ɺB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�NB	�NB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�}B	��B
�B
pB
�B
!bB
,"B
1�B
7LB
@�B
F%B
M�B
O�B
WYB
[�B
^OB
`�B
h$B
l=B
rB
wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�A���?L�B@�G�@���B��@��?I��A���>�� @���?@Yd@�K+B�KB�h>�,�?@b0�B��A���>���?��?e��?�V�?y>?Y�LA"J�?4KoB��A
Z?GW?���B�&?�9?&f�?�EB�SA���>�>�&�>�x%>���>��>��3?^_BB� B��?#]�@��2B�DA��?'z�?�!�@X@�B��Aҍ?�HZB�!A�z?�A�?�.?�f�?0�=AK�B��B�@9` A�9B��A��|?;��A��A�X�?H�?]�6A��{B��?��?�hB�B�s?c�>�? �m?,bAI:�?/��@��Bì>Ғ�?��?��jA��B�>A#N�>�L+@
҅?��B�?}��B;%@%��>ϞZ?k@_��B��?/�@N�B��Bt�?"�$@O��B��?��;>�?Mf?��B��B��B�h?/�?*4eBn�A��?��@$�>�zF?^�'@'B?_�z?s7�?�t�B��Bb-?1��@�l@_�@��B�>��?'�/@,��B�0>���?k�4?O@�E�B�?m�?8�?�6?{O�B�QB�I?E�XB��A��0?Ev%?#A}@/�8?7�A���B�^?��?�qzB��@��?@b��?J0#A�|�?���@^d�?�JA@�?�?�+�B�H@3u�>괖@9�@AP�B��A���?�pB�I?o�a@��$?��B��?��@o��@�^�A���?-X A�K�AbpB��?c��B�G@�ԻB�xA�K[Bk8@�#B��@�(�B�B�CA���?�ŰB��BB��B�eB��A���B�B�nA��B�B�AB�B�/B�UB�xB�
B��B�B�B��B�B�B�AB�=B�$B��B�JB�}B�	B��B�WB��A��B��B��B�B�ZB��B�	B�B��B��B�B��B�B��B�0B��B�B��B��B��B�fB�B��B�9B��B�jB��B�RB�pB��B�OB��B��B�bB��B�xB�	B��B�mB��B�jB�B��B�fB�(B�6B�B��B�[B�-B� B��B�BB��B�RB�'B�B��B��B��B�B��B�mB��B�B�B�mB��B��B��B��B��B�bB��B��B��B��B��B��B�B��B�4B��B�+B�;B�4B��B�B��B��B�9B��B�	B�BB�WB�B�B��B��B�B�B��B��B�XB�;B�,B��B��B�_B��B��B�`B�SB�7B��B��B��B�#B�.B��B�xB��B��B�VB��B�
B��B��B��B��B�qB��B�>B�zB�B��B��B�8B�@B�8B��B�0B��B��B�[B�B�8B��B��B��B�BX�B�<B�<B��B��B�B��B�B�B�oB�gB�B�4B�iB�B��B��B�ZA��AB��B��B�8B��B�gB��B�:A�6�B��B�	B�B�0B�_B�4B��B��B��B�gB��B�<B�oB��B�uB��B��B��B�CB�B�	B�B�B�QB��B��B��B��B��B�yB�yB�'B�B�fB�B�:B�cB�cB��B�ZB��B��B��B�JB��B��B�oB� B�.B�B�YB�jB��B�4B��B��B��B��B��B�7B�IB��B�|B�&B�kB��B�kB��B�B�yB�,B��B�B�B�B�1B��B�
B��B��B�yB�2B��B�|B�HB�@B�B�-B��B�B��B��B��B�YB�QB�oB��B��B�HB��B�B��B�=B��B��B��B��B�$B�B��B��B��B�B��B��B�'B�GB�?B�B��B�TB��B��B�:B��B��B�6B�B��B��B�5B��B��B��B�?B��B��B�B��B��B��B��B��B�sB��B��B��B�BB��BŹB�wBĭB�SB��BŕB�xB�1B�zB�qB��BȎB�+BɞB�0BɬB��B��B��B��B�qB��B�1B�B�B�)B�!B�^B̢B�AB�B�dBʲB�B�B�\BɔB��B�B��B��B�gB�AB� B�bB��B�>B�7B�QBͨB�LB�uB̱B�SB�ZB��B�)B�mB��B�B�/B�yBϊB�cBΓB��BϣBЖB�sB�-B�DB��B��BҊB�
B��B�>B�)B�B�}BϟB��B�mB�lBАB�	BȦB�vBըBվBԇBҶB�?B�eB�"B�MB�B�YB�sB�8B��B֡B�lB�KBџB��B��B�BժB�BإBٔBؚB�IBܰB�BڛB۫B�%B�VB��B��B��BߣB��B߂B�7B�.B�B�nB�ZB�B��B��B��B��B��B�dB�5B�-B��B��B�UB�B�`B�
B�B�B	ڽB	��B	�B	��B	��B	��B	�aB	�(B	�+B	�B	�B	��B	��B	�rB	�FB	ڕB	�\B	�0B	�LB	�nB	�5B	�8B	��B	��B	ڧB	ڙB	�#B	��B	��B	ڢB	�vB	�iB	�\B	�AB	��B	�oB	ٍB	��B	ڝB	ڢB	�8B	��B	�XB	لB	�9B	ٴB	��B	�B	��B	ڷB	��B	ڪB	��B	��B	ڝB	�EB	�HB	�;B	�B	ژB	ڋB	��B	��B	��B	�VB	�B	��B	٦B	��B	�iB	أB	��B	�hB	��B	�[B	٥B	�NB	؈B	��B	��B	آB	ٿB	�B	�B	�OB	��B	�oB	��B	�rB	��B	�$B	ۯB	��B	�qB	�&B	��B	�	B	ڄB	پB	�PB	��B	٫B	ۛB	�B	��B	ܒB	�fB	��B	ۤB	�B	�B	��B	�?B	�|B	�-B	�+B	�9B	ۣB	��B	��B�BؽB��B��B��B��B�bB�aB��B�pB�B��BٿB�/B�CB�bB�PB��B�BBؽBةBٻB�B��B؛BؓB��B�BؼB��BؾB��B�:B�B٢BוB�B��B��B� B�iB�B�%B�(B��B�YBٮB�FB�B�YB�B��B�IBߴB�B�B�PB�^B��B�B�B�TB��B�B�;B��B�7B��B�&B��B�B�qB��B��B�<B�B�B�B�B��B�B�B�B�jB��B�:B��B�bB�uB�{B��B�^B�CB�B�B�B�aB��B�B�B�B��B�_B�	B�;B��B�-B�B�B�^B�^B�B��B�B��B�B��B�[B�+B�B�[B�B�B�#B��B�B�CB�cB�*B�-B�ZB�+B�B�*B�{B�B��B�B��B�B�B��B��B�B��B�7B�B�B�rB�B�"B�)B��B�/B��B�SB�nB�)B�B�B��B��B�B�wB�'B��B�_B��B�-B��B�QB��B�B�RB��B�kB��B��B��B�DB�mB�B��B��B��B�FB��B��B��B��B� B�dB�B��B��B�#B�UB�UB�sB�vB�NB	�B	�nB	�B	�B	�wB	�KB	�=B	�B	��B	�B	�B	�B	�B	�nB	�`B	�SB	�9B	�KB	�=B	�B	�B	��B	��B	��B	�B	�B	�\B	�0B	�B	�B	�B	�B	�vB	�B	�B	�B	�'B	�SB	�B	�BB	��B	�B	�,B	��B	�JB	�B	�B	�<B	�B	�B	��B	�0B	�B	�"B	��B	�B	�SB	�FB	�B	��B	��B	�B	�B	�SB	�*B	��B	�B	�=B	�B	�B	�NB	�B	�zB	��B	�B	�B	�B	�4B	�B	��B	�B	�B	�B	�B	�MB	�pB	�B	�
B	�B	�YB	��B	��B	�YB	�B	�B	�B	�B	�B	�bB	�B	�B	�sB	��B	�B	��B	�B	�eB	�,B	�B	��B	��B	�aB	�B	�B	��B	��B	�0B	�\B	�zB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933444344344443344433444444443444344434444444433443444434433444444334433443443344334444444344443444434344443443344344443334434444444443344443444344443444433434444433443444344444434444344344434443434343433343433343333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B�B�B�B�%B�FB�LB�RB�kB�{B�B�B��B��B��B��B��B��B��B��BWBPB�B�JB��B��B��B̽BʳB�iB�bB�CB��B�yB�_B�YB�-B�-B�QB��Bz�Bx�Bm�Ba9B^%B`5Bt�By�Bt�Br�Bt�Bm�BV�BV�BZBH�B65B2B��B��B�B�qB�CB�B��BŔB�kB�8B�
B��B��B��B��B��B��B��B��B��B�VB�B{�Bo�BZB<]B&�B�B�B��BÄB�^B�#B��B�gBz�B`0BS�BJ�BG�BE�BAyB:MB(�BAB
�B
�B
�nB
��B
��B
��B
� B
z�B
fRB
L�B
8BB
�B
LB
 B	��B	�zB	�B	ɨB	�KB	��B	��B	�jB	�/B	�B	w�B	haB	a5B	W�B	G�B	<VB	.B	�B	�B	�B	tB	<B		$B	�B��B��B��B��B�B�B�B�B�xB�qB�MB��B�PB�=B�-B�&B�B��B��B��B�B�+B� B��B��B��B��B��B��B��B��B��B��B��B��B�xB�zB�yB�mB��B��B��B��B��B�sB�VB�<B�(B�B��B{�Bt�Bn�BjmBdEB_%BZ	BV�BS�BN�BJ�BF�BD�BAtB@mB=\B:HB;OB@nBM�BfSBw�Bu�Bq�Bo�Bo�Bn�Bm}BluBjjBidBh`BfPBdEBjjBibBfQBeLBh]BgVBo�Bm}BifBfRBeJBdEBq�By�B�B�B�B|�By�Bw�Bu�Bp�Bu�B��B�B}�B~�B~�B~�By�Bw�Bw�Bu�BmzBgYBb8B[B[B[B^B^B]B^Bn�Bw�Bx�Bw�Bv�Bw�Bx�B}�B�cB��B��B��B��B��B��B��B�_B�^B�kB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�#B�*B�2B�.B�0B�4B�7B�<B�AB�uB�BÁBĆBŎBȝBͼB��B��B��B��B�4B�EB�dB�hB��B��B��B��B��B��B	�B	B	B		!B	8B	RB	rB	~B	�B	�B	�B	%�B	)�B	*�B	+�B	,�B	-�B	/B	1B	5+B	75B	8;B	:GB	<RB	>bB	?eB	@lB	@mB	BwB	C�B	E�B	G�B	L�B	O�B	P�B	P�B	Q�B	W�B	ZB	[B	]B	]B	]B	_$B	b9B	d@B	fSB	ibB	jjB	jjB	luB	m{B	mzB	n�B	p�B	r�B	s�B	t�B	x�B	{�B	}�B	~�B	��B	�B	�B	�*B	�'B	�(B	�,B	�;B	�:B	�;B	�@B	�LB	�XB	�dB	�sB	�qB	�rB	�pB	�qB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�0B	�1B	�3B	�4B	�?B	�NB	�eB	�kB	ÀB	ƏB	ɣB	ȟB	ǗB	;B	��B	ͼB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�$B	�9B	�5B	�?B	�EB	�IB	�OB	�TB	�UB	�TB	�]B	�^B	�`B	�aB	�iB	�jB	�hB	�hB	�iB	�iB	�jB	�oB	�tG�O�B	�fB	��B
�B
YB
�B
!LB
,B
1�B
76B
@�B
FB
M�B
OxB
WDB
[wB
^8B
`�B
hB
l&B
q�B
w G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�A���G�O�G�O�G�O�B��G�O�G�O�A���G�O�G�O�G�O�G�O�B�:B�\G�O�G�O�G�O�B��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B�G�O�G�O�G�O�B�GG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B��G�O�G�O�B�8G�O�G�O�G�O�G�O�B��G�O�G�O�B�A�y�G�O�G�O�G�O�G�O�G�O�G�O�B��B�G�O�G�O�B��A��`G�O�G�O�A�X�G�O�G�O�A��bB�xG�O�G�O�B�B�iG�O�G�O�G�O�G�O�G�O�G�O�G�O�BàG�O�G�O�G�O�G�O�B�,G�O�G�O�G�O�G�O�B��G�O�B;G�O�G�O�G�O�G�O�B��G�O�G�O�B��Bt�G�O�G�O�B��G�O�G�O�G�O�G�O�B��B��B�XG�O�G�O�Bn�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��BbG�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B�"G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�B�BB�=G�O�B��G�O�G�O�G�O�G�O�G�O�A���B�LG�O�G�O�B��G�O�G�O�G�O�A�|�G�O�G�O�G�O�G�O�G�O�G�O�B�9G�O�G�O�G�O�G�O�B��G�O�G�O�B�9G�O�G�O�G�O�BøG�O�G�O�G�O�A���G�O�A�K�G�O�B��G�O�B�9G�O�B�lA�KGBk+G�O�B��G�O�B��B�3A��G�O�B�wB�B��B�VB��A���B��B�_G�O�B� B�4B��B� B�FB�kB��B��B�B�
B��B�B�B�1B�.B�B��B�<B�pB��B��B�IB��A���B�xB��B��B�JB�xB��B�	B��B��B��B��B�B��B�$B��B�B��B��B��B�[B�B�vB�+B��B�ZB�vB�EB�`B��B�?B��B��B�UB��B�kB��B��B�bB��B�[B��B��B�VB�B�+B�B�xB�MB�B��B��B�2B��B�EB�B��B��B��B��B��B�B�`B��B�B��B�`B��B��B��B��B�vB�RB�B��B��B��B��B��B�B��B�&B��B�B�/B�&B��B�oB��B��B�-B��B��B�2B�IB�B�B�uB��B�B��B��B��B�LB�/B�B��B��B�SB��B��B�RB�EB�(B��B��B��B�B�B�rB�lB��B��B�JB��B��B��B��B��B��B�dB��B�/B�lB�B��B��B�+B�1B�+B��B�"B��B��B�LB�B�+B��B�{B��B��BX�B�1B�1B��B��B��B��B��B��B�`B�[B��B�)B�[B�B��B��B�MA��*B��B��B�+B��B�[B��B�-A�6�B��B��B��B�!B�RB�)B��B��B��B�[B��B�1B�`B��B�gB��B��B��B�5B��B��B��B�B�EB�tB��B��B��B��B�kB�kB�B�B�XB�B�-B�YB�YB��B�NB��B��B��B�<B��B��B�_B��B�B�B�MB�^B��B�%B��B�~B��B�wB��B�'B�<B��B�oB�B�^B��B�^B�qB�	B�mB�B��B�B��B�oB�$B��B��B��B��B�mB�'B��B�mB�:B�1B��B�!B��B�B��B��B��B�LB�?B�`B�BخB��BسB��B��B�RB�RB��B�_B��BغBٱB�B�3B�RB�AB��B�3BخB؛B٬B��BغB؋B؄B��B�BخB��BرB��B�,B�BٔBׇB�B��B��B��B�XB�B�B�B��B�JBٟB�9B��B�IB��B��B�;BߣB�B��B�AB�NB��B�B�B�HB�B�B�-B��B�)B��B�B��B�B�cB�B��B�-B��B�B��B�B�B�B�B�B�ZB��B�+B�B�VB�fB�nB��B�QB�9B�B��B�B�TB��B�B��B��B��B�QB��B�.B��B� B�yB�}B�NB�QB��B��B�pB��B�B�B�JB�B�{B�MB�B�B�B��B�sB�5B�TB�B� B�KB�B�B�B�lB��B��B��B��B�B�B��B��B�B�B�(B��B��B�cB�~B�B�B�B�B��B�FB�\B�B�B�B��B��B��B�jB�B��B�SB�B�B��B�CB��B�B�BB��B�]B��B��B��B�6B�_B��B��B��B�qB�7B�sB��B��B��B��B�VB�B�vB��B�B�IB�IB�eB�gB�?B	�B	�XB	�hB	�yB	�]B	�6B	�&B	��B	��B	�B	�jB	�~B	�qB	�UB	�JB	�:B	�$B	�6B	�$B	��B	��B	��B	�B	�B	�B	�~B	�DB	�B	��B	�B	�zB	�jB	�_B	�B	�B	�hB	�B	�=B	��B	�*B	��B	�kB	�B	�B	�4B	�pB	��B	�%B	�rB	�B	�B	�B	�B	�
B	��B	�iB	�>B	�1B	�B	��B	��B	�B	�jB	�@B	�B	��B	�B	�&B	��B	�B	�9B	�sB	�aB	��B	�B	��B	�jB	�B	�B	�B	�vB	�B	��B	�B	�8B	�ZB	�B	��B	�B	�EB	�B	��B	�BB	�B	�B	��B	�B	�B	�MB	�B	�B	�^B	��B	��B	��B	�B	�NB	�B	�B	�B	�B	�OB	�B	�}B	��B	�B	�B	�DB	�cB	�jB�BخB��BسB��B��B�RB�RB��B�_B��BغBٱB�B�3B�RB�AB��B�3BخB؛B٬B��BغB؋B؄B��B�BخB��BرB��B�,B�BٔBׇB�B��B��B��B�XB�B�B�B��B�JBٟB�9B��B�IB��B��B�;BߣB�B��B�AB�NB��B�B�B�HB�B�B�-B��B�)B��B�B��B�B�cB�B��B�-B��B�B��B�B�B�B�B�B�ZB��B�+B�B�VB�fB�nB��B�QB�9B�B��B�B�TB��B�B��B��B��B�QB��B�.B��B� B�yB�}B�NB�QB��B��B�pB��B�B�B�JB�B�{B�MB�B�B�B��B�sB�5B�TB�B� B�KB�B�B�B�lB��B��B��B��B�B�B��B��B�B�B�(B��B��B�cB�~B�B�B�B�B��B�FB�\B�B�B�B��B��B��B�jB�B��B�SB�B�B��B�CB��B�B�BB��B�]B��B��B��B�6B�_B��B��B��B�qB�7B�sB��B��B��B��B�VB�B�vB��B�B�IB�IB�eB�gB�?B	�B	�XB	�hB	�yB	�]B	�6B	�&B	��B	��B	�B	�jB	�~B	�qB	�UB	�JB	�:B	�$B	�6B	�$B	��B	��B	��B	�B	�B	�B	�~B	�DB	�B	��B	�B	�zB	�jB	�_B	�B	�B	�hB	�B	�=B	��B	�*B	��B	�kB	�B	�B	�4B	�pB	��B	�%B	�rB	�B	�B	�B	�B	�
B	��B	�iB	�>B	�1B	�B	��B	��B	�B	�jB	�@B	�B	��B	�B	�&B	��B	�B	�9B	�sB	�aB	��B	�B	��B	�jB	�B	�B	�B	�vB	�B	��B	�B	�8B	�ZB	�B	��B	�B	�EB	�B	��B	�BB	�B	�B	��B	�B	�B	�MB	�B	�B	�^B	��B	��B	��B	�B	�NB	�B	�B	�B	�B	�OB	�B	�}B	��B	�B	�B	�DB	�cB	�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111199999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933444344344443344433444444443444344434444444433443444434433444444334433443443344334444444344443444434344443443344344443334434444444443344443444344443444433434444433443444344444434444344344434443434343433343433343333333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647102020083116471020200831164710202008311647102020083116471020200831164710202008311647102020083116471020200831164710202008311647102020083116471020200831164710AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816332019021918163320190219181633    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816332019021918163320190219181633  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816332019021918163320190219181633  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647102020083116471020200831164710  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                