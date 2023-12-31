CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  F   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:31Z creation      
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181631  20200831164706  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @Փ0v���@Փ0v���@Փ0v���111 @Փ0���6@Փ0���6@Փ0���6@6�\(�@6�\(�@6�\(��c��E����c��E����c��E���111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�D�G�D���D�ָD���D�L�D�}D��{D��D�F�D�r=Dǿ
D��D�'\D�q�D�=D���D�+3D�s�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    ���;L�ͽ��;L��        �L�ͽ��;L�;L�ͽ���    �L�;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ��ͽ��;L�;L�;L�ͽ��ͽ��;L�ͽ��;L�;L��        �L��    �L�ͽ��;L�;L�ͽ���    �L�;L�ͽ��ͽ��ͽ��;L�;L��        �L�;L�ͽ��ͽ��;L�ͽ���    �L�;L�ͽ���        �L�;L�ͽ��ͽ��;L��    =��ͽ��;L�ͽ���    ���;L�;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ���    ���;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�ͽ���    ���;L�;L�;L�;L�;L�;L�;L��    ���;L�;L�;L�ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ��;L�ͽ���    �L�;L�;L�;L��    ���;L�ͽ��;L�;L�;L�ͽ��ͽ���    �L�;L�;L�ͽ��;L�;L�ͽ���    ���;L��    =��ͽ��;L�ͽ��;L�ͽ��;L��        �L�ͽ��;L�ͽ��ͽ��;L��    ���;L�ͽ��ͽ��ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ��ͽ��;L�ͽ��;L�ͽ���    ���ͽ��ͽ��ͽ��;L�;L��        >L��=��ͽ��ͽ��ͽ��ͽ��ͽ���        �L�;L�ͽ���        ���ͽ��ͽ���    ����                ���ͽ���                =���=���=���    >L��=���>L��=���>L��>L��            =���            =���=���>L��=���=���            =���=���=���>L��    =���=���    =���    =���    ����=���=���=���=���=���    =���=���    =���=���>L��=���    =��ͽ���        =���=���>L��=���=���        =���=���        =���=���>L��>L��    =���=���    =���    =���=���=���=���=���    >L��=���=���        =���=���    =���            =���=���=���=���    =���=���    >L��=���=���=���    =���    =���    =���=���=���=���    >L��=���=���=���=���    =���>L��>L��=���=���            =���=���    >L��=���    =���>L��=���>L��>L��=���            =���=���=���=���=���    =���=���=���=���=���>L��=���=���    =���            =���>L��    =���=���=���=���=���=���=���=��ͽ���    =���    =���    =���=���        =���=���=���=���=���    =���=���=���>L��            =���=���>L��=���>���=���        =���>L��>L��=���    =���=���=���=���>L��    =���=���        =���=���=���=���=���=���=���=���=���=���    =���    =���=���>L��=���=���        =���=���    =���=���    =���=���=���>L��>L��=���        =���        =���=���=���=���    >L��>L��>L��=��ͽ���    =���            =���>L��>���>���>���>���?��?��?��?333?L��?L��?fff?�  ?���?���?���?�ff?�ff?�33?�33?�  ?���?ٙ�?�ff?�ff?�33@   @   @ff@��@33@��@��@   @&ff@,��@333@9��@@  @L��@S33@Y��@`  @l��@s33@y��@�33@�33@���@���@�  @�33@���@���@�  @�33@���@���@�  @�ff@���@���@�33@�ff@ə�@���@�33@�ff@ٙ�@���@�33@�ff@陚@�  @�33@�ff@���A   A33A��AffA	��A33A��A  A��A��AffA  A33A��AffA!��A#33A&ffA(  A)��A+33A.ffA0  A333A4��A6ffA9��A;33A<��A@  AA��AC33AD��AH  AI��AL��ANffAP  AS33AT��AVffAY��A[33A\��A`  Aa��Ac33AfffAh  Ai��Al��Ap  Aq��As33At��Ax  Ay��A|��A~ffA�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  Ař�A�ffA�33A���Aə�A�ffA�  A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A�  A���Aٙ�A�33A�  DqfDq�Dq3Dq  Dq&fDq,�Dq33Dq@ DqFfDqL�DqS3Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3Dtٚ@&ff@,��@333@9��@@  @L��@S33@Y��@`  @l��@s33@y��@�33@�33@���@���@�  @�33@���@���@�  @�33@���@���@�  @�ff@���@���@�33@�ff@ə�@���@�33@�ff@ٙ�@���@�33@�ff@陚@�  @�33@�ff@���A   A33A��AffA	��A33A��A  A��A��AffA  A33A��AffA!��A#33A&ffA(  A)��A+33A.ffA0  A333A4��A6ffA9��A;33A<��A@  AA��AC33AD��AH  AI��AL��ANffAP  AS33AT��AVffAY��A[33A\��A`  Aa��Ac33AfffAh  Ai��Al��Ap  Aq��As33At��Ax  Ay��A|��A~ffA�  A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  Ař�A�ffA�33A���Aə�A�ffA�  A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A�  A���Aٙ�A�33A�  DqfDq�Dq3Dq  Dq&fDq,�Dq33Dq@ DqFfDqL�DqS3Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��DqٚDq� Dq�fDq��Dq��Dr  DrfDr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrS3DrY�Dr` DrffDrs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs,�Ds9�Ds@ DsFfDsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��Ds��Dt  DtfDt�Dt�Dt  Dt&fDt33Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3DtٚG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @>{@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�(�A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
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
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�CdECf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�Dt��Dy��D��D�MD���D��)D��gD�R>D���D���D��D�L)D�w�D��{D��D�,�D�w]D෮D��>D�0�D�yHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>.{=�\)��=�\)��>.{>.{��=�\)����=�\)>.{����=�\)=�\)����=�\)������=�\)=�\)��=�\)=�\)������=�\)=�\)��=�\)����>.{>.{��>.{��=�\)����=�\)>.{����=�\)=�\)=�\)����>.{>.{����=�\)=�\)��=�\)>.{����=�\)>.{>.{����=�\)=�\)��>.{>�=q=�\)��=�\)>.{=�\)������=�\)����������=�\)>.{=�\)����=�\)=�\)����������=�\)>.{=�\)��������������>.{=�\)������=�\)��=�\)��=�\)=�\)=�\)��=�\)>.{��������>.{=�\)��=�\)������=�\)=�\)>.{������=�\)����=�\)>.{=�\)��>.{>�=q=�\)��=�\)��=�\)��>.{>.{��=�\)��=�\)=�\)��>.{=�\)��=�\)=�\)=�\)��=�\)��=�\)=�\)=�\)=�\)��=�\)��=�\)>.{=�\)=�\)=�\)=�\)����>.{>.{>�p�>�=q=�\)=�\)=�\)=�\)=�\)>.{>.{����=�\)>.{>.{=�\)=�\)=�\)>.{=�\)>.{>.{>.{>.{=�\)=�\)>.{>.{>.{>.{>�=q>�=q>�=q>.{>�p�>�=q>�p�>�=q>�p�>�p�>.{>.{>.{>�=q>.{>.{>.{>�=q>�=q>�p�>�=q>�=q>.{>.{>.{>�=q>�=q>�=q>�p�>.{>�=q>�=q>.{>�=q>.{>�=q>.{=�\)>�=q>�=q>�=q>�=q>�=q>.{>�=q>�=q>.{>�=q>�=q>�p�>�=q>.{>�=q=�\)>.{>.{>�=q>�=q>�p�>�=q>�=q>.{>.{>�=q>�=q>.{>.{>�=q>�=q>�p�>�p�>.{>�=q>�=q>.{>�=q>.{>�=q>�=q>�=q>�=q>�=q>.{>�p�>�=q>�=q>.{>.{>�=q>�=q>.{>�=q>.{>.{>.{>�=q>�=q>�=q>�=q>.{>�=q>�=q>.{>�p�>�=q>�=q>�=q>.{>�=q>.{>�=q>.{>�=q>�=q>�=q>�=q>.{>�p�>�=q>�=q>�=q>�=q>.{>�=q>�p�>�p�>�=q>�=q>.{>.{>.{>�=q>�=q>.{>�p�>�=q>.{>�=q>�p�>�=q>�p�>�p�>�=q>.{>.{>.{>�=q>�=q>�=q>�=q>�=q>.{>�=q>�=q>�=q>�=q>�=q>�p�>�=q>�=q>.{>�=q>.{>.{>.{>�=q>�p�>.{>�=q>�=q>�=q>�=q>�=q>�=q>�=q>�=q=�\)>.{>�=q>.{>�=q>.{>�=q>�=q>.{>.{>�=q>�=q>�=q>�=q>�=q>.{>�=q>�=q>�=q>�p�>.{>.{>.{>�=q>�=q>�p�>�=q>��>�=q>.{>.{>�=q>�p�>�p�>�=q>.{>�=q>�=q>�=q>�=q>�p�>.{>�=q>�=q>.{>.{>�=q>�=q>�=q>�=q>�=q>�=q>�=q>�=q>�=q>�=q>.{>�=q>.{>�=q>�=q>�p�>�=q>�=q>.{>.{>�=q>�=q>.{>�=q>�=q>.{>�=q>�=q>�=q>�p�>�p�>�=q>.{>.{>�=q>.{>.{>�=q>�=q>�=q>�=q>.{>�p�>�p�>�p�>�=q=�\)>.{>�=q>.{>.{>.{>�=q>�p�>��>��?�?�?E�?E�?E�?^�R?xQ�?xQ�?���?�?��\?��\?�\)?�(�?�(�?���?���?�?�\?�\)?�(�?�(�@z�@
�H@
�H@G�@�@{@$z�@$z�@*�H@1G�@7�@>{@Dz�@J�H@W�@^{@dz�@j�H@w�@~{@�=q@���@���@�
>@�=q@�p�@���@�
>@�=q@�p�@���@�
>@�=q@�p�@��
@�
>@�=q@ȣ�@��
@�
>@�=q@أ�@��
@�
>@�=q@��@��
@�
>@�p�@���@��
A�A�RA�A�A	�AQ�A�A�A�RAQ�A�A�A�RA�A�A!�A$Q�A%�A)�A*�RA,Q�A-�A1�A2�RA5�A7�A9�A<Q�A=�A?�AB�RADQ�AE�AG�AJ�RALQ�AO�AQ�AR�RAU�AW�AY�A\Q�A]�A_�Ab�RAdQ�Ae�Ai�Aj�RAlQ�Ao�Ar�RAtQ�Au�Aw�Az�RA|Q�A�A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A�A��\A�\)A�(�A�Aď\A�\)A���A�Aȏ\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���Aԏ\A�\)A�(�A�A؏\A�\)A�(�A���A܏\A�\)DqGDq�DqDq*�Dq1GDq7�Dq>DqJ�DqQGDqW�Dq^Dqj�DqqGDqw�Dq~Dq��Dq�GDq��Dq�{Dq��Dq�GDq��Dq�{Dq��Dq�GDq׮Dq�{Dq��Dq�GDq��Dr{Dr
�DrGDrDr${Dr*�Dr1GDr>DrD{DrJ�DrQGDr^Drd{Drj�DrqGDr~Dr�{Dr��Dr��Dr�Dr�{Dr��Dr��Dr�Dr�{Dr�GDr׮Dr�Dr��Dr�GDr��Dr�Ds
�DsGDs�Ds${Ds*�Ds1GDs7�DsD{DsJ�DsQGDs^Dsd{Dsj�Dsw�Ds~Ds�{Ds��Ds��Ds�Ds�{Ds�GDs��Ds�Ds�{Ds�GDs׮Ds�Ds��Ds�GDs��Dt{Dt
�DtGDt�Dt${Dt*�Dt1GDt>DtD{DtJ�DtW�Dt^Dtd{DtqGDtw�Dt~Dt��Dt�GDt��Dt�{Dt��Dt�GDt�Dt�{Dt��Dt�GDt�Dt�{@1G�@7�@>{@Dz�@J�H@W�@^{@dz�@j�H@w�@~{@�=q@���@���@�
>@�=q@�p�@���@�
>@�=q@�p�@���@�
>@�=q@�p�@��
@�
>@�=q@ȣ�@��
@�
>@�=q@أ�@��
@�
>@�=q@��@��
@�
>@�p�@���@��
A�A�RA�A�A	�AQ�A�A�A�RAQ�A�A�A�RA�A�A!�A$Q�A%�A)�A*�RA,Q�A-�A1�A2�RA5�A7�A9�A<Q�A=�A?�AB�RADQ�AE�AG�AJ�RALQ�AO�AQ�AR�RAU�AW�AY�A\Q�A]�A_�Ab�RAdQ�Ae�Ai�Aj�RAlQ�Ao�Ar�RAtQ�Au�Aw�Az�RA|Q�A�A��\A�\)A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A���A�A��\A�(�A���A�A�\)A�(�A�A��\A�\)A�(�A�Aď\A�\)A���A�Aȏ\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���Aԏ\A�\)A�(�A�A؏\A�\)A�(�A���A܏\A�\)DqGDq�DqDq*�Dq1GDq7�Dq>DqJ�DqQGDqW�Dq^Dqj�DqqGDqw�Dq~Dq��Dq�GDq��Dq�{Dq��Dq�GDq��Dq�{Dq��Dq�GDq׮Dq�{Dq��Dq�GDq��Dr{Dr
�DrGDrDr${Dr*�Dr1GDr>DrD{DrJ�DrQGDr^Drd{Drj�DrqGDr~Dr�{Dr��Dr��Dr�Dr�{Dr��Dr��Dr�Dr�{Dr�GDr׮Dr�Dr��Dr�GDr��Dr�Ds
�DsGDs�Ds${Ds*�Ds1GDs7�DsD{DsJ�DsQGDs^Dsd{Dsj�Dsw�Ds~Ds�{Ds��Ds��Ds�Ds�{Ds�GDs��Ds�Ds�{Ds�GDs׮Ds�Ds��Ds�GDs��Dt{Dt
�DtGDt�Dt${Dt*�Dt1GDt>DtD{DtJ�DtW�Dt^Dtd{DtqGDtw�Dt~Dt��Dt�GDt��Dt�{Dt��Dt�GDt�Dt�{Dt��Dt�GDt�Dt�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�M�A�M�A�O�A�K�A�M�A�O�A�Q�A�Q�A�O�A�O�A�Q�A�S�A�t�Aǉ7AǋDAǣ�Aǣ�AǏ\AǏ\Aǉ7AǅAǁA�p�A�bNA�^5A�?}A��A�JAƴ9A��`A�bNAĶFA��mA¶FA�l�A���A�A�A���A�XA��;A��TA��FA��\A�dZA�r�A��A���A���A�x�A��yA��A�  A��9A��A��A���A�VA�/A��A�XA�-A�(�A�
=A�ĜA�oA�1A��A�C�A�oA�t�A�(�A��yA��#A��TA�/A�-A�jA��TA��9A���A�I�A�\)A�ZA�1A��-A��A�^5A��A��-A��A���A��A��A��A�7LA��TA�|�A�ȴA�ƨA��A�v�A�ĜA�A�A�ĜA�9XA�ZA���A�-A��A��TA�l�A��jA��TA��yA�VA�ĜA��A� �A���A�+A��A�dZA��RA���A��DA��A�bNA��^A�ȴA~ȴA}A|=qAz�Az=qAwdZAt�Ar�!Ap�RAoXAl�!Aj�+Agx�Ad5?A`M�AYl�AT~�AShsAQ�7AO�mAN�ANA�AK\)AI�wAIdZAIK�AH�AH-AFA�ABffA@jA>�A<$�A;
=A8��A6v�A4�DA3A1�#A01A/dZA/A.r�A-p�A+7LA*Q�A)�FA(~�A'�A&��A&I�A%C�A$1A#�PA"�A!��A!
=A �uA z�A�A�A�A�7AdZA�A�A�A�!AoAJAK�A�A�`A��A
=A��AA��A��Ap�A�A��A��A{A�TA%A��A�A
��A	G�A�A�FA��AJA\)A~�A{AXAȴA=qA�AG�A ȴA v�@��@�X@��@�  @��F@���@�l�@���@���@�9X@�%@�l�@�^@��@�C�@�ȴ@�J@�dZ@�@�hs@�!@�z�@��@�\)@��@�ȴ@�=q@�|�@�b@ڰ!@�ƨ@���@�o@�p�@�r�@·+@��
@��@с@ёh@���@҇+@��@�S�@���@�G�@��
@�33@��y@�33@ũ�@�^5@�b@��/@őh@�bN@���@��@��!@���@��@���@��m@��@��-@�p�@���@��@��w@�
=@�V@�n�@�5?@���@�&�@���@�z�@�\)@��\@��h@�bN@��@���@���@��\@���@���@�^5@���@��@�ƨ@��@��!@��@��@��^@�&�@��@�A�@��;@���@���@�ff@�$�@���@�p�@��@�Z@�9X@� �@�ƨ@�^5@�J@���@�Ĝ@��9@��D@�Q�@��@��w@��@�C�@�+@�o@�@�ff@�V@�-@���@�$�@���@�%@���@�r�@�I�@��;@��
@�;d@���@�^5@��@���@�{@�@�hs@��@���@�X@�Ĝ@��D@�1'@�1'@�/@�7L@�`B@��h@���@��-@��@�/@��j@��u@��D@�z�@�A�@��;@�
=@��@�n�@�=q@���@��@���@��@�K�@�~�@��@��^@�p�@���@��@��/@���@��j@�j@�Q�@�b@��@���@��;@��P@��y@��@���@��+@�n�@�$�@�@�p�@�O�@�7L@�7L@�Ĝ@�A�@��@��@��F@��@��@�ƨ@��
@���@�@�-@��-@�$�@�ȴ@���@���@��R@���@���@�n�@�5?@���@���@�hs@���@���@���@�A�@���@�C�@�o@���@�M�@��@���@�hs@�X@�G�@�&�@��@���@�z�@�(�@��m@���@��F@���@���@�\)@�+@��@��y@��@xI�@p�@fTa@^8�@Us�@O��@H2�@C�a@>��@:s�@6O@.��@)hs@$$@ �5@�@��@iD@\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AŴ9A���Aŕ�A��A��TA�9XA��A�|�AƸRA�&�A��A� �A�~�A�?}A���AÑhA��jA�hsA�=qA��!A�VA�O�A�(�A���A�r�Aé�A�XA�%A��;A�Q�A��
A�|�A�Q�A��
A��/A���A�ĜA�(�A��!AƼjA��uA�z�A�hsA��HA�z�A�ĜA�{A�7LAŶFA���A�$�A���A�E�AƧ�A�9XA���A��9A���AËDA��#A��7A��A�ĜA�jA��A�;dA�?}A�E�A�oA���A���A�S�A���A�?}A�-A��A�bNA�C�A�z�A�jA���A�?}A�ffA�\)A�XA�E�A��/A�;dA�ȴA��TA��A���A��PA���A�5?A�%A�"�A�M�A�A���A��uA��AōPA��/A��wA��A�hsA��#A��`A��7A�x�A��;A�%A��PA���A���A��yA���A��/A�
=A�/AœuA�JAŝ�A��A��HA�A��
A�dZA�{A��A��AƧ�A���A���A���A�VA�A�A�9XA��A��A���A�l�A��TA��A��TA�A�A��`A�C�A�z�A�/A���A���A�hsA�dZAöFA��7A�C�A�9XA���Aŧ�A��+A²-A�;dA�JA�&�A��A�p�A�S�AƇ+A� �A�"�A�33A��hA�
=A�ƨA�33A�JA���A�I�A�~�A�-A�=qA�7LA�33A���A��A��A��HA®A�7LA�?}A�A�A��A���A�&�A�A�A�33A��A�1'A�-A��jA�G�A��A�33A�33AƉ7A�/A�hsA�VA�33A�/A�5?A�9XA�$�Aå�A�(�A�;dA�$�A�9XA�?}A�=qA�A�A�;dA�?}A�?}A�7LA�=qA�A�A�=qA�9XA�;dA�;dA�9XA�5?A�1'A�+A�?}A�9XA�?}A�C�A�A�A�A�A�?}A��mA�A�A�A�A�E�A�A�A�A�A�A�A�C�A�=qA�A�A�;dA�=qA�C�A��A�A�A�=qA�=qA�C�A�C�A�;dA�C�A�C�A�C�A�?}A�A�A�C�A�G�A�C�A�E�A�C�A�?}A�A�A�E�A�A�A�G�A�G�A�E�A�E�A�A�A��A�E�A�E�A�E�A�C�A�E�A�G�A�E�A�E�A�A�A�C�A�G�A�G�A�C�A�G�A�I�A�E�A�E�A�G�A�E�A�E�A�G�A�E�A�G�A�C�A�I�A�G�A�E�A�G�A�G�A�C�A��A�C�A�E�A�C�A�C�A�?}A�A�A�E�A�C�A�C�A�G�A�E�A�C�A�A�A�A�A�C�A�C�A�A�A�C�A�G�A�G�A�E�A�E�A�G�A�G�A�E�A�?}A�C�A�?}A�A�A�C�A�I�A�E�A�=qA�=qA�9XA�?}A�E�A�A�A�?}A�C�A�G�A�E�A�G�A�E�A�I�A�I�A�I�A�G�A�G�A�?}A�C�A�C�A�;dA�E�A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�G�A�E�A�G�A�I�A�G�A�?}A�I�A�G�A�I�A�;dA�M�A�I�A�G�A�G�A�K�A�G�A�I�A�K�A�I�A�I�A�I�A�G�A�G�A�K�A�K�A�I�A�I�A�I�A�I�A�G�A�I�A�M�A�K�A�I�A��A�K�A�I�A�M�A�I�A�I�A�K�A�K�A�I�A�M�A�G�A�M�A�K�A�I�A�K�A�I�A�E�A�I�A�K�A�K�A�K�A�G�A�I�A�K�A�I�A�E�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�O�A�K�A�M�A�K�A�K�A�M�A�O�A�M�A�K�A�K�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�K�A�M�A�K�A�K�A�M�A�O�A�Q�A�K�A�K�A�K�A�I�A�K�A�I�A�G�A�M�A�O�A�O�A�I�A�C�A�K�A�K�A�I�A�G�A�G�A�K�A�I�A�K�A�=qA�I�A�I�A�K�A�K�A�O�A�K�A�O�A�M�A�M�A�O�A�M�A�O�A�M�A�O�A�O�A�I�A�M�A�M�A�M�A�K�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A�K�A�M�A�O�A�O�A�M�A�Q�A�O�A�O�A�M�A�M�A�O�A�O�A�M�A�Q�A�O�A�M�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�S�A�O�A�Q�A�O�A�O�A�O�A�Q�A�O�A�Q�A�O�A�S�A�S�A�S�A�S�A�Q�A�VA�S�A�Q�A�S�A�S�A�S�A�O�A�M�A�O�A�M�A�M�A�K�A�M�A�M�A�O�A�K�A�K�A�M�A�K�A�M�A�K�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�Q�A�O�A�M�A�Q�A�O�A�O�A�M�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�S�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�S�A�S�A�Q�A�S�A�S�A�VA�S�A�Q�A�O�A�O�A�M�A�O�A�O�A�Q�A�S�A�O�A�Q�A�Q�A�Q�A�S�A�O�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�Q�A�S�A�S�A�S�A�Q�A�S�A�O�A�Q�A�S�A�S�A�VA�Q�A�S�A�S�A�Q�A�VA�S�A�Q�A�S�A�Q�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�VA�VA�XA�VA�S�A�XA�VA�S�A�S�A�S�A�S�A�S�A�S�A�XA�ZA�X@�ƨ@��w@�ƨ@��w@��w@��w@��w@��w@��w@��w@��w@��F@��w@��w@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��@��@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@���@��P@��P@��P@��@��@�|�@�t�@�t�@�dZ@�\)@�S�@�S�@�C�@�C�@�C�@�;d@�;d@�;d@�33@�33@�+@�+@�+@�+@�+@�+@�+@�+@�33@�+@�+@�+@�+@�+@�+@�"�@�"�@�"�@��@��@��@��@��@�o@�o@�o@�
=@�
=@�
=@�
=@�@�@�@�@���@���@��@��y@��H@��@���@���@�ȴ@���@���@��R@���@��RA�K�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�M�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�Q�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�Q�A�S�A�Q�A�O�A�M�A�O�A�O�A�M�A�M�A�M�A�Q�A�O�A�Q�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�Q�A�O�A�O�A�M�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�VA�XA�VA�S�A�S�A�S�A�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�S�A�VA�V@�ƨ@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��@��@��@�t�@�l�@�l�@�\)@�S�@�S�@�K�@�C�@�C�@�C�@�;d@�33@�33@�33@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�"�@�"�@�"�@��@��@��@��@��@��@�o@�o@�
=@�
=@�
=@�
=@�
=@�@�@�@�@���@���@��y@��y@��@���@�ȴ@�ȴ@�ȴ@���@��R@��R@��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�K�A�M�A�M�A�O�A�K�A�M�A�O�A�Q�A�Q�A�O�A�O�A�Q�A�S�A�t�Aǉ7AǋDAǣ�Aǣ�AǏ\AǏ\Aǉ7AǅAǁA�p�A�bNA�^5A�?}A��A�JAƴ9A��`A�bNAĶFA��mA¶FA�l�A���A�A�A���A�XA��;A��TA��FA��\A�dZA�r�A��A���A���A�x�A��yA��A�  A��9A��A��A���A�VA�/A��A�XA�-A�(�A�
=A�ĜA�oA�1A��A�C�A�oA�t�A�(�A��yA��#A��TA�/A�-A�jA��TA��9A���A�I�A�\)A�ZA�1A��-A��A�^5A��A��-A��A���A��A��A��A�7LA��TA�|�A�ȴA�ƨA��A�v�A�ĜA�A�A�ĜA�9XA�ZA���A�-A��A��TA�l�A��jA��TA��yA�VA�ĜA��A� �A���A�+A��A�dZA��RA���A��DA��A�bNA��^A�ȴA~ȴA}A|=qAz�Az=qAwdZAt�Ar�!Ap�RAoXAl�!Aj�+Agx�Ad5?A`M�AYl�AT~�AShsAQ�7AO�mAN�ANA�AK\)AI�wAIdZAIK�AH�AH-AFA�ABffA@jA>�A<$�A;
=A8��A6v�A4�DA3A1�#A01A/dZA/A.r�A-p�A+7LA*Q�A)�FA(~�A'�A&��A&I�A%C�A$1A#�PA"�A!��A!
=A �uA z�A�A�A�A�7AdZA�A�A�A�!AoAJAK�A�A�`A��A
=A��AA��A��Ap�A�A��A��A{A�TA%A��A�A
��A	G�A�A�FA��AJA\)A~�A{AXAȴA=qA�AG�A ȴA v�@��@�X@��@�  @��F@���@�l�@���@���@�9X@�%@�l�@�^@��@�C�@�ȴ@�J@�dZ@�@�hs@�!@�z�@��@�\)@��@�ȴ@�=q@�|�@�b@ڰ!@�ƨ@���@�o@�p�@�r�@·+@��
@��@с@ёh@���@҇+@��@�S�@���@�G�@��
@�33@��y@�33@ũ�@�^5@�b@��/@őh@�bN@���@��@��!@���@��@���@��m@��@��-@�p�@���@��@��w@�
=@�V@�n�@�5?@���@�&�@���@�z�@�\)@��\@��h@�bN@��@���@���@��\@���@���@�^5@���@��@�ƨ@��@��!@��@��@��^@�&�@��@�A�@��;@���@���@�ff@�$�@���@�p�@��@�Z@�9X@� �@�ƨ@�^5@�J@���@�Ĝ@��9@��D@�Q�@��@��w@��@�C�@�+@�o@�@�ff@�V@�-@���@�$�@���@�%@���@�r�@�I�@��;@��
@�;d@���@�^5@��@���@�{@�@�hs@��@���@�X@�Ĝ@��D@�1'@�1'@�/@�7L@�`B@��h@���@��-@��@�/@��j@��u@��D@�z�@�A�@��;@�
=@��@�n�@�=q@���@��@���@��@�K�@�~�@��@��^@�p�@���@��@��/@���@��j@�j@�Q�@�b@��@���@��;@��P@��y@��@���@��+@�n�@�$�@�@�p�@�O�@�7L@�7L@�Ĝ@�A�@��@��@��F@��@��@�ƨ@��
@���@�@�-@��-@�$�@�ȴ@���@���@��R@���@���@�n�@�5?@���@���@�hs@���@���@���@�A�@���@�C�@�o@���@�M�@��@���@�hs@�X@�G�@�&�@��@���@�z�@�(�@��m@���@��F@���@���@�\)@�+@��G�O�@��@xI�@p�@fTa@^8�@Us�@O��@H2�@C�a@>��@:s�@6O@.��@)hs@$$@ �5@�@��@iD@\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AŴ9A���Aŕ�A��A��TA�9XA��A�|�AƸRA�&�A��A� �A�~�A�?}A���AÑhA��jA�hsA�=qA��!A�VA�O�A�(�A���A�r�Aé�A�XA�%A��;A�Q�A��
A�|�A�Q�A��
A��/A���A�ĜA�(�A��!AƼjA��uA�z�A�hsA��HA�z�A�ĜA�{A�7LAŶFA���A�$�A���A�E�AƧ�A�9XA���A��9A���AËDA��#A��7A��A�ĜA�jA��A�;dA�?}A�E�A�oA���A���A�S�A���A�?}A�-A��A�bNA�C�A�z�A�jA���A�?}A�ffA�\)A�XA�E�A��/A�;dA�ȴA��TA��A���A��PA���A�5?A�%A�"�A�M�A�A���A��uA��AōPA��/A��wA��A�hsA��#A��`A��7A�x�A��;A�%A��PA���A���A��yA���A��/A�
=A�/AœuA�JAŝ�A��A��HA�A��
A�dZA�{A��A��AƧ�A���A���A���A�VA�A�A�9XA��A��A���A�l�A��TA��A��TA�A�A��`A�C�A�z�A�/A���A���A�hsA�dZAöFA��7A�C�A�9XA���Aŧ�A��+A²-A�;dA�JA�&�A��A�p�A�S�AƇ+A� �A�"�A�33A��hA�
=A�ƨA�33A�JA���A�I�A�~�A�-A�=qA�7LA�33A���A��A��A��HA®A�7LA�?}A�A�A��A���A�&�A�A�A�33A��A�1'A�-A��jA�G�A��A�33A�33AƉ7A�/A�hsA�VA�33A�/A�5?A�9XA�$�Aå�A�(�A�;dA�$�A�9XA�?}A�=qA�A�A�;dA�?}A�?}A�7LA�=qA�A�A�=qA�9XA�;dA�;dA�9XA�5?A�1'A�+A�?}A�9XA�?}A�C�A�A�A�A�A�?}A��mA�A�A�A�A�E�A�A�A�A�A�A�A�C�A�=qA�A�A�;dA�=qA�C�A��A�A�A�=qA�=qA�C�A�C�A�;dA�C�A�C�A�C�A�?}A�A�A�C�A�G�A�C�A�E�A�C�A�?}A�A�A�E�A�A�A�G�A�G�A�E�A�E�A�A�A��A�E�A�E�A�E�A�C�A�E�A�G�A�E�A�E�A�A�A�C�A�G�A�G�A�C�A�G�A�I�A�E�A�E�A�G�A�E�A�E�A�G�A�E�A�G�A�C�A�I�A�G�A�E�A�G�A�G�A�C�A��A�C�A�E�A�C�A�C�A�?}A�A�A�E�A�C�A�C�A�G�A�E�A�C�A�A�A�A�A�C�A�C�A�A�A�C�A�G�A�G�A�E�A�E�A�G�A�G�A�E�A�?}A�C�A�?}A�A�A�C�A�I�A�E�A�=qA�=qA�9XA�?}A�E�A�A�A�?}A�C�A�G�A�E�A�G�A�E�A�I�A�I�A�I�A�G�A�G�A�?}A�C�A�C�A�;dA�E�A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�G�A�E�A�G�A�I�A�G�A�?}A�I�A�G�A�I�A�;dA�M�A�I�A�G�A�G�A�K�A�G�A�I�A�K�A�I�A�I�A�I�A�G�A�G�A�K�A�K�A�I�A�I�A�I�A�I�A�G�A�I�A�M�A�K�A�I�A��A�K�A�I�A�M�A�I�A�I�A�K�A�K�A�I�A�M�A�G�A�M�A�K�A�I�A�K�A�I�A�E�A�I�A�K�A�K�A�K�A�G�A�I�A�K�A�I�A�E�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�O�A�K�A�M�A�K�A�K�A�M�A�O�A�M�A�K�A�K�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�K�A�M�A�K�A�K�A�M�A�O�A�Q�A�K�A�K�A�K�A�I�A�K�A�I�A�G�A�M�A�O�A�O�A�I�A�C�A�K�A�K�A�I�A�G�A�G�A�K�A�I�A�K�A�=qA�I�A�I�A�K�A�K�A�O�A�K�A�O�A�M�A�M�A�O�A�M�A�O�A�M�A�O�A�O�A�I�A�M�A�M�A�M�A�K�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A�K�A�M�A�O�A�O�A�M�A�Q�A�O�A�O�A�M�A�M�A�K�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�M�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�Q�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�Q�A�S�A�Q�A�O�A�M�A�O�A�O�A�M�A�M�A�M�A�Q�A�O�A�Q�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�Q�A�O�A�O�A�M�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�VA�XA�VA�S�A�S�A�S�A�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�S�A�VA�V@�ƨ@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��@��@��@�t�@�l�@�l�@�\)@�S�@�S�@�K�@�C�@�C�@�C�@�;d@�33@�33@�33@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�"�@�"�@�"�@��@��@��@��@��@��@�o@�o@�
=@�
=@�
=@�
=@�
=@�@�@�@�@���@���@��y@��y@��@���@�ȴ@�ȴ@�ȴ@���@��R@��R@��RA�K�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�M�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�Q�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�Q�A�S�A�Q�A�O�A�M�A�O�A�O�A�M�A�M�A�M�A�Q�A�O�A�Q�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�Q�A�O�A�O�A�M�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�VA�XA�VA�S�A�S�A�S�A�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�S�A�VA�V@�ƨ@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��w@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��F@��@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��@��@��@�t�@�l�@�l�@�\)@�S�@�S�@�K�@�C�@�C�@�C�@�;d@�33@�33@�33@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�+@�"�@�"�@�"�@��@��@��@��@��@��@�o@�o@�
=@�
=@�
=@�
=@�
=@�@�@�@�@���@���@��y@��y@��@���@�ȴ@�ȴ@�ȴ@���@��R@��R@��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�3=�K�?l�>�?T�v@�(@��>-Z\@��=�R�> �@��?�o?=�^�=�ջ?MM�@��=�x=��>�@:=���>
=?��;>�@�=�پ>3;@�0@R�P=��S=Lx�=v+k=�F_? 8\>>-@���=���>TK@�*0> @�o?.�=���=� q=���=�7?��@�&�=���?晅?:�?��b>�H�>'�S@VW�@�&B@�پ=��=� �>��@���>�b@]�
@�#d=Ë>-Ց@�6@�"@��>�e@c<�?��>5T>�'�@��@�(�=���>_��@�
|@�K�= ��=��=%P�=J��=�$�=��^>�,=���>��O>�H@�J@�=���=��>�m=do=bw�=l7a=���=�� =ȩ�>_�@��@�$�=\�N=]$�=ix�=��=���=®�>C�?ߗx@�&B=�I>&��@�M�=x��=���=�?n�>(�A@�'R?���>?eIR>�w2@�+�=�A�=�bc>��>��@�"�>�V?�>�?�/=�9�=��=�e�@	��@�&B@�.
>�@��>y�|=���=ί%>ݭ@�,R@�,R=��#>�G@�) @�+A=���?�e?��>!�)?��>m0j@�+�@�$5?�n�=��;>U @�!?��@}�@���=��>��@s��@���@�*=���?˷�>�>*\�>&ff@�'�>\��=Ы�>�G�>���@�.^@�)t@���@�'?�1=���>%U�>bl@�,�@�/o@�/o@�-�>G�?@�,�?��@�)J@(U@�$5@�ȴ>
�E?�;:@��M@�*�@�,�@��<@�*�?�E@�+�@�+@�*�@�,R@�,R@�'�>�T@�,R@�-#@�,R@�.^@�/@�/@�.�@�.�@�.^@�.�@�.^@�/@�/o@�.^@�.�@�.
@�.�@�-#@�,R@�*�@�-M@�.�@�.�@�0@�0+@�0@@�0+@�/�@{b9@�/�@�0�@�0+@�0@@�0+@�0@�0@�/�@�0+@�.^@�/�@�/�@�.
@�/�@�/@�/o@�0�@�1<@�1<@�0@@�0�@�0�@�0�@�0�@�1�@�1<@�0+@�1Q@�1<@�0@�1<@�0�@�1�@�2M@�2M@�1�@�1�@�0@��4@�1�@�1�@�1�@�1�@�1�@�2�@�2M@�1�@�0@@�1�@�1�@�1�@�1<@�2�@�2M@�2�@�2M@�1�@�2M@�1�@�2M@�2�@�2�@�1�@�2M@�2�@�1�@�1�@�1�@�1�@e��@�1�@�2M@�1�@�1�@�1<@�0+@�1�@�1Q@�1�@�2�@�1�@�1�@�1�@�1�@�1�@�0�@�0@�2M@�2a@�2�@�2�@�1�@�2M@�1�@�2M@�1<@�0�@�1�@�1�@�1�@�1�@�/�@�.�@�.^@�0�@�1�@�1�@�1�@�2�@�2M@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�1�@�1<@�2M@�1<@�0@�2M@�2M@�2M@�1�@�2M@�2�@�3	@�2�@�2�@�2�@�2�@�2�@�2�@�1�@�3]@�3H@�4@�3]@�3�@�3�@�3H@�3�@�3�@�3H@�4@�4@�3�@�3�@�3�@�3�@�3�@�4Y@�4�@�4n@�4Y@�4Y@�4@�3�@�4@�4�@�4Y@�4Y@�4@�2�@�4n@�4Y@�4Y@�4n@�4Y@�4@�3�@�4@�3�@�3�@�4Y@�4@�4@�3�@�3H@�4Y@�4Y@�4@�3H@�1�@�2�@�4Y@�4n@�3H@�4Y@�5@�4�@�4Y@�4�@�5@�5~@�5@�5@�5@�5@�5@�4�@�5@�5@�5@�4�@�4�@�5@�5i@�5@�4Y@�4�@�5@�4�@�5@�5i@�5@�5@�5i@�5i@�4Y@�5@�5i@�5i@�5@�4�@�4�@�4�@�4n@�5@�4�@�5@�5i@�4�@�4�@�4Y@�4n@�4�@�4�@�3�@�3�@�4�@�4Y@�4Y@�3�@�4@�4@�4�@�5@�5~@�6&@�5�@�5�@�5�@�5�@�5@�5i@�5+@�5i@�5i@�5�@�5@�5@�5i@�5i@�5~@�5�@�5i@�5�@�6&@�5�@�5i@�6&@�6z@�6z@�6z@�6�@�6&@�6�@�6�@�6&@�6�@�6�@�6�@�6�@�77@�7�@�77@�77@�7�@�7�@�8G@�8�@�8G@�9@�8G@�8G@�8�@�8�@�9@�9@�9@�9@�9X@�9X@�9X@�9X@�9�@�9X@�9X@�9@�9�@�9�@�9X@�9�@�9X@�:@�:@�:~@�:�@�;%@�:�@�:�@�:�@�:~@�:~@�;%@�:�@�:�@�:�@�9m@�9@�9@�8�@�8\@�7�@�8�@�8�@�8�@�9@�8\@�8�@�8\@�8�@�8�@�8�@�8�@�8�@�9m@�9�@�:*@�:*@�:*@�:~@�:�@�:�@�;:@�:�@�:�@�;:@�;�@�;�@�;�@�;:@�;�@�<K@�<K@�<K@�<�@�<�@�<�@�<�@�<�@�<�@�=@�=@�=@�=\@�=�@�=�@�=�@�=\@�>@�>@�=�@�>@�>@�>@�>�@�>l@�>l@�>�@�?)@�?)@�?)@�?)@�?�@�?}@�@:@�?�@�?}@�>�@�>@�=�@�>@�=�@�=�@�>@�@:@�?)@�>�@�?�@�>�@�>�@�?}@�>�@�>�@�>�@�?)@�@O@�@O@�?�@�@O@�@:@�@O@�@O@�@O@�@�@�@�@�@�@�@�@�@�@�?�@�@�@�B@�B@�A�@�A_@�A�@�B@�A_@�A�@�A�@�Bp@�A�@�A�@�Bp@�B�@�Bp@�B�@�B�@�C@�D(@�D�@�E�@�F5@�G@�F�@�FJ@�E�@�E�@�FJ@�GZ@�GZ@�Hk@�I(@�J8@�K�@�N�@�P�@�W�@P��@P��@P�+@P�+@P��@P�+@P�+@P�+@P��@P�+@P�+@P��@P��@P��@P�+@P�+@P��@P��@P��@P�+@P�+@P��@P݃@P�Y@P�Y@P�Y@P��@Pܱ@P�3@Pܱ@P܇@P�3@P�3@Pܱ@P�3@P܇@P�3@P�
@P�]@P�]@P�]@P�]@P�
@P۶@P۶@P۶@P�@Pں@Pں@Pں@P�<@P�@P�@@P�j@P��@P�@P��@P�w@P�R@PԪ@P�[@P�_@P��@P�@P�>@P�@Pϖ@P�@P�F@P�p@P�@P��@P��@P͟@P��@P�F@P�@P��@P�p@P�@P��@P��@P��@P��@P��@P�u@P�u@P�y@P�y@P�%@P�%@P˧@P�%@P˧@P�}@P��@P��@P�.@P��@P�.@PɆ@P�2@P��@P��@P�6@P��@P�@PƓ@P��@PĜ@P�L@P�P@P��@P�U@P��@P�Y@P�]@P��@P��@P�
@�Q@�Q@�Q/@�QD@�QD@�Q�@�Q�@�Q�@�R�@�S@�R�@�R�@�R�@�S@�R�@�R�@�R�@�R�@�R�@�Sz@�S;@�S;@�S;@�SP@�SP@�SP@�S�@�S�@�S�@�Sz@�S�@�S�@�S�@�S�@�T"@�S�@�T@�S�@�U@�U@�U@�T�@�UG@�U\@�U\@�UG@�U�@�U�@�T@�S�@�SP@�Ri@�Ri@�Ri@�R*@�R�@�R�@�R�@�R?@�R�@�R @�R?@�R?@�R�@�R�@�R~@�R�@�S�@�S�@�S&@�S�@�T�@�T7@�T@�T�@�T�@�T�@�U@�U2@�U�@�U@�UG@�Uq@�VC@�V.@�V@�V@�VC@�VC@�V�@�V�@�V�@�V�@�V�@�V�@�W@�W?@�W*@�WT@�W~@�W@�W�@�W~@�X@�W�@�W�@�X�@�X�@�X@�W�@�X�@�X�@�Y!@�Y@�Y`@�Y�@�ZG@�Z@�Z�@�Y6@�X�@�W @�X@�X�@�W @�W?@�W�@�ZG@�Xy@�Y6@�YK@�Y6@�W~@�W�@�W�@�W�@�X�@�Y�@�YK@�Y�@�Yu@�YK@�Yu@�Y6@�Y�@�Z\@�Y�@�Z�@�Z\@�Z2@�Y6@�Z\@�[@�[W@�Z�@�ZG@�[@�[@�[@�Z�@�\@�[B@�Z�@�Z�@�[@�[B@�[@�Z�@�Z�@�[@�[B@�]@�^�@�_[@�^�@�^ @�]@�]%@�]%@�]O@�]@�]:@�]O@�]�@�^@�^J@�`�@�`�@P�(@P�@P�@P�,@P�@P�,@P�,@P�,@P�V@P�V@P�@P�@P�@P��@P�@P�@P�@P�@P�V@P�@P�,@P�,@P��@P�@P�0@P�Z@P�0@P�@P��@P�@P�@P�@P�@P�_@P�@P�_@P�_@P�_@P�_@P�_@P�_@P�_@P�5@P�_@P�@P�@P�@P�@P�9@P�c@P�9@P�@P��@P�@P�@P��@P�l@P��@P��@P�@P��@P�.@P��@P�`@P�;@P�@P�@P��@P�@P�@P�H@P�@P�@P�L@P�v@P�@P�@P��@P��@P��@P�@P��@P�@P��@P��@P��@P�@P�L@P��@P�z@P�z@P��@P��@P�&@P��@P��@P�U@P�@Pݭ@P�Y@P�/@P��@Pܱ@P܇@P܇@P�
@P�3@P۶@P�@Pپ@P�@P�s@P��@P��@PԀ@P�V@P�@P��@P�1@PӄG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      34444334344344444444444444334444443443444444443444444333444343344334434443344334444444444334444444444334444444434434444434444344443444444433434444334433444444334443433443334444434444333344443333434343344333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�5G�O�G�O�G�O�G�O�@�(@��G�O�@��G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�0~@R�PG�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@�*1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&�G�O�G�O�G�O�G�O�G�O�G�O�@VW�@�&A@�ٿG�O�G�O�G�O�@���G�O�@]�@�#cG�O�G�O�@�9@�"G�O�G�O�@c<�G�O�G�O�G�O�@��@�(�G�O�G�O�@�
~@�K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�J@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@�$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&?G�O�G�O�@�M�G�O�G�O�G�O�G�O�G�O�@�'LG�O�G�O�G�O�G�O�@�+�G�O�G�O�G�O�G�O�@�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&A@�.G�O�@��G�O�G�O�G�O�G�O�@�,T@�,RG�O�G�O�@�)#@�+DG�O�G�O�G�O�G�O�G�O�G�O�@�+�@�$6G�O�G�O�G�O�@�!G�O�@}�~@���G�O�G�O�@s��@���@�*G�O�G�O�G�O�G�O�G�O�@�'�G�O�G�O�G�O�G�O�@�.`@�)u@���@�'G�O�G�O�G�O�G�O�@�,�@�/p@�/m@�-�G�O�@�,�G�O�@�)IG�O�@�$6@�ȯG�O�G�O�@��J@�*�@�,�@��:@�*�G�O�@�+�@�+@�*�@�,O@�,Q@�'�G�O�@�,R@�-%@�,R@�.\@�/@�/@�.�@�.�@�.]@�.�@�.`@�/@�/n@�.c@�.�@�.@�.�@�-"@�,O@�*�@�-N@�.�@�.�@�0|@�0(@�0B@�0*@�/�@{bE@�/�@�0�@�0-@�0B@�0-@�0}@�0�@�/�@�0*@�.b@�/�@�/�@�.@�/�@�/@�/p@�0�@�1=@�1<@�0>@�0�@�0�@�0�@�0�@�1�@�1;@�0+@�1R@�1=@�0}@�1:@�0�@�1�@�2P@�2N@�1�@�1�@�0}@��6@�1�@�1�@�1�@�1�@�1�@�2�@�2N@�1�@�0B@�1�@�1�@�1�@�1=@�2�@�2P@�2�@�2P@�1�@�2P@�1�@�2N@�2�@�2�@�1�@�2J@�2�@�1�@�1�@�1�@�1�@e��@�1�@�2P@�1�@�1�@�1<@�0*@�1�@�1P@�1�@�2�@�1�@�1�@�1�@�1�@�1�@�0�@�0@�2K@�2d@�2�@�2�@�1�@�2J@�1�@�2P@�1<@�0�@�1�@�1�@�1�@�1�@�/�@�.�@�.b@�0�@�1�@�1�@�1�@�2�@�2K@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�2�@�1�@�19@�2K@�19@�0�@�2P@�2P@�2P@�1�@�2P@�2�@�3
@�2�@�2�@�2�@�2�@�2�@�2�@�1�@�3]@�3F@�4@�3\@�3�@�3�@�3F@�3�@�3�@�3J@�4@�4@�3�@�3�@�3�@�3�@�3�@�4[@�4�@�4n@�4W@�4Z@�4@�3�@�4@�4�@�4[@�4Z@�4@�2�@�4n@�4X@�4Z@�4n@�4Y@�4@�3�@�4 @�3�@�3�@�4Y@�4@�4 @�3�@�3I@�4W@�4[@�4@�3J@�1�@�2�@�4[@�4n@�3I@�4[@�5@�4�@�4[@�4�@�5@�5~@�5@�5@�5@�5@�5@�4�@�5@�5@�5@�4�@�4�@�5@�5j@�5@�4X@�4�@�5@�4�@�5@�5j@�5@�5@�5j@�5j@�4[@�5@�5j@�5n@�5@�4�@�4�@�4�@�4n@�5@�4�@�5@�5m@�4�@�4�@�4Z@�4n@�4�@�4�@�3�@�3�@�4�@�4[@�4W@�3�@�4@�4@�4�@�5@�5~@�6&@�5�@�5�@�5�@�5�@�5@�5l@�5.@�5l@�5l@�5�@�5@�5@�5l@�5j@�5~@�5�@�5j@�5�@�6)@�5�@�5g@�6&@�6~@�6y@�6{@�6�@�6&@�6�@�6�@�6$@�6�@�6�@�Q@�Q@�Q/@�QD@�QH@�Q�@�Q�@�Q�@�R�@�S@�R�@�R�@�R�@�S@�R�@�R�@�R�@�R�@�R�@�S{@�S@@�S;@�S:@�ST@�SR@�SR@�S�@�S�@�S�@�Sy@�S�@�S�@�S�@�S�@�T@�S�@�T@�S�@�U
@�U
@�U@�T�@�UG@�UY@�U_@�UD@�U�@�U�@�T@�S�@�SP@�Rj@�Rg@�Rj@�R*@�S@�R�@�R�@�R?@�R�@�R@�RA@�RB@�R�@�R�@�R~@�R�@�S�@�S�@�S&@�S�@�T�@�T6@�T@�T�@�T�@�T�@�U@�U3@�U�@�U@�UG@�Ur@�VG@�V.@�V@�V@�VD@�VB@�V�@�V�@�V�@�V�@�V�@�V�@�W@�WA@�W*@�WS@�W|@�W@�W�@�W|@�X@�W�@�W�@�X�@�X�@�X@�W�@�X�@�X�@�Y!@�Y@�Ya@�Y�@�ZI@�Z@�Z�@�Y5@�X�@�V�@�X@�X�@�W@�W@@�W�@�ZF@�Xz@�Y7@�YN@�Y;@�W�@�W�@�W�@�W�@�X�@�Y�@�YJ@�Y�@�Yt@�YM@�Yv@�Y:@�Y�@�Z_@�Y�@�Z�@�Z\@�Z2@�Y6@�Z\@�[@�[Z@�Z�@�ZI@�[@�[@�[@�Z�@�\@�[B@�Z�@�Z�@�[@�[F@�[@�Z�@�Z�@�[@�[F@�]@�^�@�_]@�^�@�^@�]@�])@�]&@�]N@�]@�]=@�]L@�]�@�^@�^L@�`�@�`�@P�(@P�@P�@P�0@P�@P�*@P�+@P�+@P�S@P�U@P�@P�@P�@P��@P� @P� @P�@P�@P�U@P�@P�-@P�+@P��@P�@P�0@P�^@P�0@P�@P��@P�@P�@P�@P�@P�]@P�@P�c@P�^@P�b@P�b@P�^@P�c@P�`@P�5@P�b@P�@P�@P�@P�@P�3@P�e@P�:@P�@P��@P�@P�@P�@P�f@P��@P��@P�@P��@P�.@P��@P�b@P�8@P�@P�@P�@P�@P�@P�E@P�@P�@P�K@P�u@P�@P�@P��@P��@P��@P�@P��@P�@P��@P��@P��@P��@P�M@P��@P�}@P�}@P��@P��@P�(@P��@P��@P�V@P��@Pݫ@P�Z@P�+@P��@Pܳ@P܆@P܆@P�@P�5@P۳@P�@P��@P�@P�r@P��@P��@P�~@P�X@P�@P��@P�.@Pӈ@�Q@�Q@�Q/@�QD@�QH@�Q�@�Q�@�Q�@�R�@�S@�R�@�R�@�R�@�S@�R�@�R�@�R�@�R�@�R�@�S{@�S@@�S;@�S:@�ST@�SR@�SR@�S�@�S�@�S�@�Sy@�S�@�S�@�S�@�S�@�T@�S�@�T@�S�@�U
@�U
@�U@�T�@�UG@�UY@�U_@�UD@�U�@�U�@�T@�S�@�SP@�Rj@�Rg@�Rj@�R*@�S@�R�@�R�@�R?@�R�@�R@�RA@�RB@�R�@�R�@�R~@�R�@�S�@�S�@�S&@�S�@�T�@�T6@�T@�T�@�T�@�T�@�U@�U3@�U�@�U@�UG@�Ur@�VG@�V.@�V@�V@�VD@�VB@�V�@�V�@�V�@�V�@�V�@�V�@�W@�WA@�W*@�WS@�W|@�W@�W�@�W|@�X@�W�@�W�@�X�@�X�@�X@�W�@�X�@�X�@�Y!@�Y@�Ya@�Y�@�ZI@�Z@�Z�@�Y5@�X�@�V�@�X@�X�@�W@�W@@�W�@�ZF@�Xz@�Y7@�YN@�Y;@�W�@�W�@�W�@�W�@�X�@�Y�@�YJ@�Y�@�Yt@�YM@�Yv@�Y:@�Y�@�Z_@�Y�@�Z�@�Z\@�Z2@�Y6@�Z\@�[@�[Z@�Z�@�ZI@�[@�[@�[@�Z�@�\@�[B@�Z�@�Z�@�[@�[F@�[@�Z�@�Z�@�[@�[F@�]@�^�@�_]@�^�@�^@�]@�])@�]&@�]N@�]@�]=@�]L@�]�@�^@�^L@�`�@�`�@P�(@P�@P�@P�0@P�@P�*@P�+@P�+@P�S@P�U@P�@P�@P�@P��@P� @P� @P�@P�@P�U@P�@P�-@P�+@P��@P�@P�0@P�^@P�0@P�@P��@P�@P�@P�@P�@P�]@P�@P�c@P�^@P�b@P�b@P�^@P�c@P�`@P�5@P�b@P�@P�@P�@P�@P�3@P�e@P�:@P�@P��@P�@P�@P�@P�f@P��@P��@P�@P��@P�.@P��@P�b@P�8@P�@P�@P�@P�@P�@P�E@P�@P�@P�K@P�u@P�@P�@P��@P��@P��@P�@P��@P�@P��@P��@P��@P��@P�M@P��@P�}@P�}@P��@P��@P�(@P��@P��@P�V@P��@Pݫ@P�Z@P�+@P��@Pܳ@P܆@P܆@P�@P�5@P۳@P�@P��@P�@P�r@P��@P��@P�~@P�X@P�@P��@P�.@PӈG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      34444334344344444444444444334444443443444444443444444333444343344334434443344334444444444334444444444334444444434434444434444344443444444433434444334433444444334443433443334444434444333344443333434343344333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�99��99��99��99��99��99�n99��99�99�V99��99�m99��99��99��99�m99��99��99��99��99�<99��99��99��99�99�99�99�Q99�T99��99�:99��99��99��99��99��99��99��99��99��99��99��99��99�599�I99�P99�299�|99�v99��99�V99�99�99�99�99��99��99��99�i99��99�j99��99��99��99�S99�A99�&99�Y99��99��99��99�}99��99�
99��99�h99��99�h99�99� 99�99��99�599�e99�O99�399�99�99�K99�I99��99��99��99��99��99�99�199�a99�H99�u99��99�499��99��99�F99�99��99�+99��99�H99��99�+99��99�p99�]99��9: &9: �9: l9:99��99��99�99�F99��99�99�`99��9: �99��99��99��99��99��99��99��99�.99�99��99��9: @99��99��99��99��9: 9: �9: (9:?9: �9: �99��9: �9:�9:�9:9: �9:�9:�9:�9:9:�9:�9: �9:m9:�9:�9:�9:V9:N9:�9:�9:�9:�9:I9:d9:�9:�9:�9:�9:9:�9:�9:9:�9:�9:9:�9:�8�}8�{�8�{�8�|	8�{�8�|8�|8�|8�|/8�|28�{�8�{�8�{�8�{�8�{�8�{�8�{�8�{�8�|28�{�8�|8�|8�{�8�{w8�z�8�{"8�z�8�z�8�z�8�zi8�z:8�z48�z;8�z8�z;8�z8�z	8�z8�z8�z	8�z8�z8�y�8�z8�y�8�y�8�yP8�y%8�x�8�x�8�x�8�x�8�xq8�x8�w�8�w'8�v�8�v8�u 8�s�8�r�8�q8�o�8�o8�m�8�m8�l�8�l+8�kx8�j�8�j�8�jc8�i�8�i{8�i�8�i�8�i�8�j78�j8�j8�ja8�j38�jc8�j8�j8�j8�i�8�i}8�i 8�h�8�h�8�h8�h8�h;8�g�8�g�8�gU8�f�8�f�8�f@8�f8�e�8�e�8�eW8�eW8�d�8�d�8�do8�c�8�bK8�a�8�_�8�]�8�]8�\�8�\[8�Z�8�Z�8�[8�[wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B�/B�B��B{B-B:^B=qB?}BC�BF�BI�BP�BW
B]/B^5B]/B[#B[#BYBXBZB^5BcTBk�Bz�B�+B�hB��B�LB�^B�jB�qB�B�/B�`B��B��BB�sBŢB�wB�^B��B�B�HB�B��B��BhB!�B.BC�B\)Bp�Bz�B� B�B�B�Bv�B|�B�1B}�Bn�BcTBaHBe`BgmBk�BZBK�BK�BJ�BB�BD�BB�B<jB6FB,B�BPBJBB��B�B�TB��B�!B��B��B�1B~�Br�BaHBR�BI�BC�B@�B5?B#�BoB  B�B�`B��B��B�PB�BdZBE�B'�B{B
��B
�5B
�?B
��B
r�B
YB
M�B
A�B
6FB
/B
�B
B	��B	�`B	�#B	��B	�jB	��B	~�B	T�B	$�B		7B	B��B�B�fB�BB��BɺBȴBƨBÖB�qB�3B��B��B�{B�\B�PB�+B� B}�B|�Bx�Bx�Bw�Bv�Bs�Bs�Bt�Bs�Bq�Bq�Bo�Bn�Bm�Bl�Bm�Bl�Bl�Bm�Bl�Bn�Bu�Bt�Bl�Bn�Bn�Bn�Bn�Bm�Bm�Bn�Bp�Br�Bt�Bt�Bt�Bu�Bv�Bv�Bw�Bw�Bw�Bw�Bx�B}�B� B�B� B�B�%B�1B�1B�=B�VB�VB�\B�oB��B�uB�{B�oB�\B�\B�bB�oB��B��B�oB�PB�PB�\B�bB�bB�\B�VB�PB�VB�DB�%B� B�B�B�%B�B�B�B�B|�Bx�By�B~�B�B�B�%B�B~�Bz�Br�Bq�Br�Bn�BcTBbNBq�B� B�B�%B�bB��B��B��B��B�hB� B� B�Bv�Bv�B{�B�+B�PB�B}�By�Br�Br�Br�Bu�Bu�Bu�Bx�By�Bz�B~�B�B�7B�=B�{B��B��B��B��B��B��B��B��B��B�B�!B�-B�FB�qB�wB�wB�wB�}B�wB�}BŢB��B��B��B��B��B�#B�BB�BB�fB�sB�yB�B�B�B��B��B��B	  B	B	PB	\B	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	%�B	&�B	)�B	,B	.B	0!B	7LB	8RB	6FB	5?B	5?B	9XB	?}B	C�B	G�B	H�B	J�B	L�B	N�B	N�B	N�B	M�B	M�B	N�B	O�B	O�B	O�B	Q�B	T�B	]/B	hsB	m�B	p�B	q�B	q�B	s�B	u�B	v�B	v�B	x�B	z�B	z�B	{�B	|�B	~�B	~�B	�B	�B	�B	�B	�+B	�+B	�%B	�B	�B	�B	�%B	�=B	�DB	�\B	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�?B	�FB	�FB	�LB	�LB	�jB	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�5B	�BB	�BB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B
�B
}B
kB
#�B
*KB
0;B
5tB
;�B
BuB
FYB
MB
UB
Z�B
_VB
b�B
f�B
j�B
l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B8�?	2/@� ?�_�@���B��B*�?]8�B�?�0?K�B��A��>嶢?t�@��CAa�S>�q\>�.j?�#)?%}?B�YA4�@/�?DA?d�{BU=A�b!>�a>�t>�L8>�@b�'?+�jA��>��i?��<B��?>i�AW�;@Z�?t�>��z>�uy?��A5�B��?��A+7G@�i-A�&@�r?X!A�VB��B	�>��?�z?���B�?04�A��;B�q>���?^�B��B��Aad<?G�A�dH@K��?&��?��B��B��?��?��CB�&A�l�>I�j>>>O�>~��>�|�>��?8i@>؂�@��?<�aA���B�>��>��?�T�>�g#>�1j>��>�D>�=>��h??��A�BP�>�К>�Ԫ>�e>��>�Or>��7?9W�A%9jB��?
��?UG�B Һ>���>��>��;@��3?Z7B�B@�V=?$�e@���?��VB��>�o�>���?/P�@RYB��?=6�A#B'@��>���>�b�?
�AP�B��B�g?/*�A�5^?�?5�?F$??��B�]B�	?8�?��3B��B�H?�@��@K��?M��A>l�?�@�B��B@�?��?���B�=@�HA��ZB�'?��?<w%A��B��Bx?�]A�8@�?\%?T&B��?��?��?���@+cB��B��B	̜B8�AjW>��)?U��?�sqB��B�B�(B�h?���B�HA*��B�A���B�$A�S?1dA6�B	�B��B�/A�c�B�!An�B�yB��B�B�B��B�?�b�B��B�xB�oB�gB��B�xB��B��B�B�B�*B�pB�0B��B��B�NB��B�CB�B�NB��B�hB��B��B�B��B��B�QA�:
B��B�{B�JB��B��B�6B�bB�B��B��B�B��B��B��B�xB��B��B�B�>B�0B��B��B�OB�{B�sB�sB�B�cB�B�	B��B��B�+B�yB�qB��B��B�6BĎB��B��B��B�hB��B��B�<B��B��B�`B�B�3B�B��B��B��B�DB�B�DB��B�qB��B��B��B��B��B��B�3B�B�hA���B�hB�DB��B��B��B��B��B�%B��B��B��B��B�+B��B�`B��B�-B�B��B�B��B��B��B�B�DB��B��B�DB�3B�sB��B��B�*B��B��B��B��B��B��B�B��B��B�B��B�AB�JB�AB�B�+B��B�B�B��B�DB�yB�yB��B�LB��B��B��B��B��B��B��B�B�LB��B�kB�OB�AB�cB��B�kB��B�.B�cB�GB��B��B��B��B��B��B��B� B��B��B��B�[B��B�OB�MB��B��BĆB�B��B��B��B��B��B��B�B��B��B�cB��B�[B�kB��B�6B��B��B�tB��B�+B�RB��B��B�.B��B�qB� B��B�B��B�B��B��B�yB��B�yB�B��B��B��B�B�4B��B�:B��B��B�EB��B�MB��B��B��B��B��B��B��B��B��B�1B�B�#B�B�B��B��B��B�B�B��B��B��B�	B� B�B��B��B��B��B��B�.B�B�cB��B�yB��B��B�B�zB�EB�4B��B��B��B��B��B�PB��B�[B��B��B�vB��B�[B��B�7B��B�wB�B�7B�cB��B��B�B�B��B�!B��B��B��B��B��B�wB��B��B�iB�,B��B�EB��B��B��B��B��B��B�B�B��B�/B�?B�lB�[B�SB�cB�:B�2B�B�EB�rB��B�,B��B��B�XB��B��B��B�HB�8B�/B��B��B�\B��B��B��B��B��B�B�VB��B�XB��B��B��B�uB�SB��B�oB�wB��B�zB��B��B�)B�gB��B��B��B�VB�zB�5B��B�YB�=B�YB�B��B��B�dB��B�}B�uB�dB��B��B��B��B��B�pB��B��B��B��B�hB�_B�"B��B�jB��B�%B�~B�%B�0B��B�eB�TB�_B��B�B��B��B��B��B��B�hB�FB�ZB�xB��B�*B��B��B�	B�0B�WB��B�sB��B��B�sB�B�B��B��B��B��B�LB��B��B��B��B�|B�<B��B�+B��B��B��B��B��B��B��B��B�cB��B��B��B�B��B�4B��B��B��B�|B��B��B��B��B��B��B�B��B��B�B��B��B��B��B��B��B�7B�+B��B��B��B�3B	��B	��B	�B	�B	ۻB	��B	��B	��B	�yB	۩B	ۜB	�TB	�7B	�)B	�jB	�PB	�B	��B	��B	�B	�B	۶B	�^B	�2B	�5B	�(B	۱B	ۅB	�B	�kB	�BB	��B	��B	�,B	��B	�B	ܹB	܀B	ܰB	ےB	ܖB	�|B	�1B	��B	��B	۾B	�GB	��B	��B	��B	�{B	�?B	ۛB	۬B	�CB	۟B	�TB	�aB	ۀB	��B	�B	�`B	��B	�SB	��B	ݓB	�*B	��B	�*B	�.B	��B	��B	ޟB	�sB	ޅB	��B	ޛB	�oB	޾B	�fB	�B	�B	�B	��B	��B	ݏB	�tB	ݿB	ݲB	�ZB	�]B	��B	�5B	��B	ݠB	�(B	�B	݅B	�KB	�nB	��B	ݛB	�TB	�GB	ݾB	�fB	ވB	�fB	��B	��B	��B	�9B	��B	�vB	��B	߷B	��B	�2B	�B	ߏB˛B��B˱B��B��B�`B�lB�B�?BːB�:B�B�[B�nB��B��B�B��B��BˍB�JB�BB��B�B��B��B�!B�MB�BB��B�1B�<B�?B�7BˊB�&B�^B�BB�7B�&B�SB�7B�tB�B�oB�SB˅B�tBˣB��B˦B��B˂BʮB�lB�B�B��B�MBʺB��B��B�#B�|B�XB�<B�B�oB�fBʦB�[B�PBʷBʈB��B�&B��BʐB�^BˤB�&B�PBʤB�^B�:B�
B��B�aB�$B�VB�EB�=B�oBʧB�tB˓B˲B��BˬB��B�YB��B˩B�VB��B��B��B˙B��B��B��B˂B��B��B�B˖B��B̃B�7B˧B��B�B�@BˤB��B�'BˋB�HB�_B�2B�B̸B�B�-B�8BʖB�QB�-BˬB�,B̎B��B˪BʜB��B˔B�B��B�rB�CB�B�QB��B�"B�jB�B˲B˽B˙B�0B̆B˻B��B�TB�_BˉB�%B��B�B�%B�WB�'B� B̢B̢B��B��B�B��B�B̗B̭B��B�ZB̌B̾B�B�6B	��B	� B	��B	��B	��B	��B	��B	�B	��B	�B	�oB	�TB	�XB	�,B	�=B	�#B	�B	�B	�,B	��B	��B	��B	�B	�aB	�B	�B	��B	�B	�B	�mB	�4B	�'B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�mB	�AB	�SB	�B	��B	�B	�B	�0B	�AB	�B	��B	��B	�vB	�B	��B	�zB	��B	�@B	�lB	��B	�B	�B	�;B	�gB	��B	�yB	�.B	�B	�PB	�B	��B	�sB	�)B	�:B	�?B	�2B	�bB	�6B	�B	�LB	� B	�$B	��B	��B	�B	�B	�LB	�B	�B	�B	�$B	�
B	�B	��B	��B	�]B	�B	��B	�B	�TB	�
B	��B	�B	�B	�1B	�CB	��B	�TB	�rB	��B	��B	�B	��B	�B	�SB	�`B	�EB	�hB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999934444334344344444444444444334444443443444444443444444333444343344334434443344334444444444334444444444334444444434434444434444344443444444433434444334433444444334443433443334444434444333344443333434343344333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  BʰB˸B˹B˶BʱBʳBʱB˸B˶B˸B˸B˸B˹B�B�B��BlB- B:MB=`B?nBC�BF�BI�BP�BV�B]!B^'B]B[B[BY
BXBZB^&BcEBkyBz�B�B�WB��B�:B�MB�]B�]B��B�%B�QB��B��B �B�bBŐB�kB�QB��B�B�8B�B��B��B[B!�B.	BC�B\Bp�Bz�B�B��B�B�	Bv�B|�B�!B}�Bn�BcDBa9BePBg_BkvBZBK�BK�BJ�BB}BD�BBB<XB64B+�B�BBB<BB��B�B�BBʯB�B��B�rB�"B~�Br�Ba5BR�BI�BC�B@rB5/B#�B_B��B�B�QB̿B��B�@B��BdIBE�B'�BkB
��B
� B
�.B
�vB
r�B
YB
M�B
AxB
64B
/B
�B
B	��B	�NB	�B	˵B	�XB	��B	~�B	T�B	$�B		%B	 �B��B�zB�TB�/B��BɦBȢBƕBÃB�aB� B��B��B�gB�KB�>B�B�B}�B|�Bx�Bx�Bw�Bv�Bs�Bs�Bt�Bs�Bq�Bq�Bo�Bn�Bm}BlvBm~BlwBlyBmBlxBn�Bu�Bt�BlxBn�Bn�Bn�Bn�Bm}Bm~Bn�Bp�Br�Bt�Bt�Bt�Bu�Bv�Bv�Bw�Bw�Bw�Bw�Bx�B}�B�B��B�B��B�B�B�B�)B�BB�CB�GB�[B�yB�aB�fB�[B�GB�IB�MB�[B�qB�sB�XB�<B�;B�GB�OB�MB�FB�AB�<B�@B�.B�B�B��B�
B�B�B�B��B��B|�Bx�By�B~�B��B�B�B�B~�Bz�Br�Bq�Br�Bn�BcBBb8Bq�B�B��B�B�LB�lB��B��B�{B�QB�B�B��Bv�Bv�B{�B�B�9B��B}�By�Br�Br�Br�Bu�Bu�Bu�Bx�By�Bz�B~�B�B� B�(B�eB�xB�B��B��B��B��B��B��B��B��B�
B�B�0B�]B�`B�aB�`B�gB�_B�fBŎB̷B��BͻB��B��B�B�-B�.B�MB�]B�cB�oB�B�B��B��B��B��B	�B	:B	IB	wB	vB	xB	�B	�B	�B	"�B	#�B	$�B	%�B	&�B	)�B	+�B	-�B	0B	77B	8<B	61B	5'B	5&B	9BB	?dB	C~B	G�B	H�B	J�B	L�B	N�B	N�B	N�B	M�B	M�B	N�B	O�B	O�B	O�B	Q�B	T�B	]B	h]B	m{B	p�B	q�B	q�B	s�B	u�B	v�B	v�B	x�B	z�B	z�B	{�B	|�B	~�B	~�B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�'B	�/B	�GB	�[B	�YB	�]B	�qB	�rB	�tB	�yB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�
B	�B	�(B	�/B	�2B	�7B	�5B	�TB	�rB	ȞB	̶B	̶B	̸B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�.B	�0B	�2B	�5B	�=B	�>B	�=B	�DB	�JB	�JB	�LB	�UB	�^B	�cB	�dB	�eB	�hB	�jB	�oB	�rB	�uG�O�B	�B	��B
�B
fB
UB
#�B
*3B
0%B
5^B
;�B
B_B
FCB
L�B
UB
Z�B
_@B
blB
f�B
j�B
lwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B8�G�O�G�O�G�O�G�O�B�|B*�G�O�B��G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BU-A�bG�O�G�O�G�O�G�O�G�O�G�O�A�lG�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�A�V B��B	��G�O�G�O�G�O�B�G�O�A��&B�bG�O�G�O�B��B��G�O�G�O�A�d2G�O�G�O�G�O�B�~B��G�O�G�O�B�A�l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�BP�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BηG�O�G�O�B ҮG�O�G�O�G�O�G�O�G�O�B�0G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�YG�O�A�5IG�O�G�O�G�O�G�O�B�NB��G�O�G�O�B�qB�=G�O�G�O�G�O�G�O�G�O�G�O�B��BG�O�G�O�G�O�B�2G�O�A��BB�G�O�G�O�A��B��BjG�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�B��B��B	̎B8�G�O�G�O�G�O�G�O�B��B��B�B�[G�O�B�8G�O�B�	G�O�B�A�R�G�O�G�O�B	�oB�uB� A�c�B�G�O�B�jB��B�B�B�rB�G�O�B��B�iB�`B�XB��B�kB��B��B��B�B�B�bB�!B��B��B�@B��B�2B��B�?B��B�XB��B��B��B��B��B�@A�9�B��B�mB�;B��B��B�'B�WB�B��B��B�B�~B��B��B�kB��B��B�B�0B�!B��B��B�?B�mB�fB�eB�B�UB�B��B��B��B�B�nB�cB��B��B�'BĀB��B��B��B�WB��B��B�.B��B��B�SB�B�#B�B��B��B��B�9B�B�9B��B�cB��B��B��B��B��B��B�#B�B�WA���B�WB�9B��B��B��B��B��B�B��B��B��B��B�B�qB�SB��B�B�	B��B�B��B��B�pB��B�9B��B��B�8B�&B�fB��B��B�B��B��B��B��B�B��B�	B��B��B�B��B�4B�>B�4B��B�B��B�	B�	B��B�9B�nB�nB��B�=B��B��B��B��B��B��B��B�B�=B��B�[B�@B�2B�TB��B�[B��B�B�VB�9B��B��B��B��B��B��B��B�B��B��B��B�MB��B�@B�>B��B��B�xB�sB��B��B��B��B��B�uB��B��B��B�TB��B�MB�ZB��B�&B��B��B�eB��B�B�DB��B��B�!B��B�bB�B��B�	B��B�
B��B��B�lB��B�lB�	B��B��B��B�	B�#B��B�+B��B��B�6B��B�>B��B��B��B��B��B��B��B�sB��B�#B�B�B��B�	B��B�sB��B�B��B�uB�uB��B��B�B��B��B��B��B��B��B�B�B�TB��B�lB��B��B�B�lB�9B�'B��B��B��B��B��B�AB��B�PB��B��B�hB��B�MB��B�*B��B�gB�B�+B�TB��B��B��B�	B�{B�B��B��BˍBʷBˣB��B��B�QB�\B��B�/BˁB�+B�B�NB�^B��B��B��B��B��B�~B�?B�4B��B��B��B��B�B�?B�3B��B�"B�/B�1B�)B�zB�B�QB�4B�)B�B�CB�&B�eB�lB�`B�CB�xB�dB˕B��B˘BʸB�qBʟB�^B�B�B��B�=BʩBʶB��B�B�nB�JB�0B�B�^B�WBʘB�LB�?BʩB�zB��B�B��BʃB�QB˗B�B�ABʕB�SB�+B��B˿B�SB�B�IB�8B�/B�^BʙB�gB˅BˣBʾB˝B��B�LB��B˙B�IB��B��B��BˌB��BʿB˵B�tB��B˾B��B˅B̹B�rB�+B˗B˾B�B�1B˕B��B�B�zB�:B�QB�$B��B̬B� B�B�)BʉB�CB�B˝B�B�BʱB˛BʐB˾B˅B��B��B�dB�4B�B�CB��B�B�^B�BˣB˭BˋB�"B�zB˫B˸B�FB�QB�zB�B��B��B�B�LB�B��B̕B̕B��B��B��B��B��B̈B̠B��B�LB́B̰B�B�(B	�B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�YB	�>B	�DB	�B	�&B	�B	� B	��B	�B	��B	��B	��B	�wB	�IB	��B	�B	��B	�B	�uB	�WB	�B	�B	�B	��B	��B	�B	�B	�B	�B	�~B	�sB	�WB	�+B	�>B	��B	��B	�B	�sB	�B	�,B	��B	��B	�B	�_B	��B	�B	�aB	��B	�*B	�WB	�B	�B	�B	�&B	�OB	��B	�cB	�B	�B	�:B	��B	��B	�\B	�B	�#B	�(B	�B	�MB	� B	�B	�5B	�	B	�B	��B	�B	�B	�pB	�5B	��B	�tB	�xB	�B	��B	�B	�B	�B	�FB	��B	��B	�jB	�<B	��B	�B	�B	�B	�B	�,B	��B	�<B	�\B	��B	��B	�wB	��B	�jB	�=B	�KB	�/B	�QB	�BˍBʷBˣB��B��B�QB�\B��B�/BˁB�+B�B�NB�^B��B��B��B��B��B�~B�?B�4B��B��B��B��B�B�?B�3B��B�"B�/B�1B�)B�zB�B�QB�4B�)B�B�CB�&B�eB�lB�`B�CB�xB�dB˕B��B˘BʸB�qBʟB�^B�B�B��B�=BʩBʶB��B�B�nB�JB�0B�B�^B�WBʘB�LB�?BʩB�zB��B�B��BʃB�QB˗B�B�ABʕB�SB�+B��B˿B�SB�B�IB�8B�/B�^BʙB�gB˅BˣBʾB˝B��B�LB��B˙B�IB��B��B��BˌB��BʿB˵B�tB��B˾B��B˅B̹B�rB�+B˗B˾B�B�1B˕B��B�B�zB�:B�QB�$B��B̬B� B�B�)BʉB�CB�B˝B�B�BʱB˛BʐB˾B˅B��B��B�dB�4B�B�CB��B�B�^B�BˣB˭BˋB�"B�zB˫B˸B�FB�QB�zB�B��B��B�B�LB�B��B̕B̕B��B��B��B��B��B̈B̠B��B�LB́B̰B�B�(B	�B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�YB	�>B	�DB	�B	�&B	�B	� B	��B	�B	��B	��B	��B	�wB	�IB	��B	�B	��B	�B	�uB	�WB	�B	�B	�B	��B	��B	�B	�B	�B	�B	�~B	�sB	�WB	�+B	�>B	��B	��B	�B	�sB	�B	�,B	��B	��B	�B	�_B	��B	�B	�aB	��B	�*B	�WB	�B	�B	�B	�&B	�OB	��B	�cB	�B	�B	�:B	��B	��B	�\B	�B	�#B	�(B	�B	�MB	� B	�B	�5B	�	B	�B	��B	�B	�B	�pB	�5B	��B	�tB	�xB	�B	��B	�B	�B	�B	�FB	��B	��B	�jB	�<B	��B	�B	�B	�B	�B	�,B	��B	�<B	�\B	��B	��B	�wB	��B	�jB	�=B	�KB	�/B	�QB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999934444334344344444444444444334444443443444444443444444333444343344334434443344334444444444334444444444334444444434434444434444344443444444433434444334433444444334443433443334444434444333344443333434343344333334333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647072020083116470720200831164707202008311647072020083116470720200831164707202008311647072020083116470720200831164707202008311647072020083116470720200831164707AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816312019021918163120190219181631    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816312019021918163120190219181631  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816312019021918163120190219181631  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647072020083116470720200831164707  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                