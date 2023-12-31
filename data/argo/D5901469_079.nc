CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:21:12Z creation      
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
resolution        =���   axis      Z        *H  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  oX   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     *H  y�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     *H  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *H  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *H �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� 84   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *H B�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *H m   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �X   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *H ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �4   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *H ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *H    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� +X   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *H 5�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� `4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *H j�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20181120212112  20200901153536  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               O   O   OAAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @��P?V�@��P?V�@��P?V�111 @��P�I��@��P�I��@��P�I��@4��E���@4��E���@4��E����c@�1&��c@�1&��c@�1&�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @�  @�  A   AffA@  Aa��A�  A�33A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�{D��D�Q�D�x�D��3D� D�C�D���D���D���D�R=D���D��{D���D�+3D�|)D�)D��D�<)D�uqD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�        >L��                    =���>L��    =���=���                                    >L��=���    =���>L��    =���=���                                                        >���                =���                                    =���=���=���    =���    =���>L��>L��        >L��            ���ͽ���                >L��                            =���        =���                                    =���>L��        =���                >L��>L��=���            =���        =���=���        =���>���>L��            =���                                >L��                >���>L��        >L��>L��>L��            =���=���                    >���>���                >L��>L��>L��=���    =���=���>���=���            >L��>���>���=���    >L��>L��>L��                =���=���=���=���                =���=���>L��>L��=���        =���=���>L��>���>L��        =���=���>���>L��=���=���>L��>L��    =���=���>L��=���>L��>L��>L��>L��>���>L��>L��=���>L��>L��>L��>���=���>���>���>���=���>���>L��    >L��>L��>L��>���>���>L��>L��>L��>L��>���>���>L��>L��>���?   >L��>���>L��>���>���>L��>���>���>L��=���>L��>L��>L��>L��>���>���>���>L��>���>���>���>���=���>���>L��>���>���>L��>L��>���>L��>L��>L��>L��>���>���>L��>L��>L��=���>���>L��>���>L��>L��>L��=���>L��>���>���>���>���>���>���>���>���>L��>���>���>���>L��>L��>���>L��>L��>���>���>���>���>L��>���>L��>L��>L��>���>���>���>L��>L��>L��>L��>L��>L��>L��>���>���=���>L��>���>L��=���>L��>L��=���>���>���>L��=���>���>���>���>���>���=���>L��>L��=���    >L��>L��>L��>���>���>���>���>���?   ?   >���>���>���>L��=���>L��>L��>L��=���>L��>L��>���>���>���>���>���>���?   >���=���>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>L��>L��=���>L��>L��>L��>L��>L��>���>L��>L��>���>���>���>���>L��>L��>L��>���>���>L��>L��>���>L��>���>L��>L��>L��>���>���>L��>���>���>L��>L��>���>���>���>���>L��>���>���>���>���>���>���>L��=���=���>L��>L��>L��>���>���>���>���>���>���?   ?��?��?��?333?333?333?333?L��?L��?fff?�  ?�  ?�  ?�  ?���?���?���?���?�ff?�ff?�33?�  ?�  ?�  ?���?���?���?ٙ�?�ff?�ff?�33?�33@   @   @ff@33@33@��@33@��@��@   @   @&ff@&ff@,��@,��@333@9��@@  @@  @@  @Fff@L��@S33@Y��@Y��@`  @fff@fff@l��@s33@y��@�  @�  @�33@�33@�ff@���@���@���@�  @�  @�33@���@���@���@���@���@�  @�33@�33@�ff@���@���@���@�  @�  @�33@�ff@�ff@���@���@���@�  @�  @�33@�ff@�ff@ə�@���@���@�  @�33@�33@�ff@�ff@ٙ�@���@���@�  @�  @�33@�ff@�ff@陚@���@���@�  @�33@�33@�ff@���@���@���A   A   A��A33A33A��AffA  A  A	��A33A33A��A��AffA  A  A��A��A33A��A��AffA  A  A��A33A33A��AffA   A   A!��A#33A$��A$��A&ffA(  A)��A+33A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A;33A<��A>ffA@  AA��AC33AD��AFffAH  AK33AL��ANffAP  AQ��AS33AT��AVffAX  A[33A[33A\��A^ffAa��Ac33Ad��AfffAh  Ai��Ak33Al��AnffAp  Aq��As33At��AvffAvffAy��A{33A|��A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���Aə�A�33A�  A���A�ffA�33A�  A���Aљ�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33Dp�3Dp��Dq  DqfDq3Dq�Dq  Dq&fDq33Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrL�DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds�Ds&fDs,�Ds33Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�f@@  @Fff@L��@S33@Y��@Y��@`  @fff@fff@l��@s33@y��@�  @�  @�33@�33@�ff@���@���@���@�  @�  @�33@���@���@���@���@���@�  @�33@�33@�ff@���@���@���@�  @�  @�33@�ff@�ff@���@���@���@�  @�  @�33@�ff@�ff@ə�@���@���@�  @�33@�33@�ff@�ff@ٙ�@���@���@�  @�  @�33@�ff@�ff@陚@���@���@�  @�33@�33@�ff@���@���@���A   A   A��A33A33A��AffA  A  A	��A33A33A��A��AffA  A  A��A��A33A��A��AffA  A  A��A33A33A��AffA   A   A!��A#33A$��A$��A&ffA(  A)��A+33A+33A,��A.ffA0  A1��A333A4��A6ffA8  A9��A;33A<��A>ffA@  AA��AC33AD��AFffAH  AK33AL��ANffAP  AQ��AS33AT��AVffAX  A[33A[33A\��A^ffAa��Ac33Ad��AfffAh  Ai��Ak33Al��AnffAp  Aq��As33At��AvffAvffAy��A{33A|��A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���Aə�A�33A�  A���A�ffA�33A�  A���Aљ�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33Dp�3Dp��Dq  DqfDq3Dq�Dq  Dq&fDq33Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3DqٚDq�fDq��Dq�3Dq��DrfDr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrL�DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds�Ds&fDs,�Ds33Ds@ DsFfDsL�DsY�Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs�fDs��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @q�@���@���A�GA<z�A^{A|z�A�p�A�=qA�=qA�=qA�=qA�
>A�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+k�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTk�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp�Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dy�gD��D�J�D�q�D��)D��D�<{D���D���D��D�K3D���DǽqD���D�$)D�uD�D��D�5D�ngD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��aG��aG����	�aG��aG��aG��aG��aG������	�aG������aG��aG��aG��aG��aG��aG��aG��aG��aG����	���aG������	�aG������aG��aG��aG��aG��aG��aG��aG��aG��aG��aG��aG��aG��aG��aG�=���aG��aG��aG��aG����aG��aG��aG��aG��aG��aG��aG��aG��aG��������aG����aG������	���	�aG��aG����	�aG��aG��aG����
���
�aG��aG��aG��aG����	�aG��aG��aG��aG��aG��aG��aG����aG��aG����aG��aG��aG��aG��aG��aG��aG��aG��aG������	�aG��aG����aG��aG��aG��aG����	���	���aG��aG��aG����aG��aG������aG��aG���=�����	�aG��aG��aG����aG��aG��aG��aG��aG��aG��aG��aG����	�aG��aG��aG��aG�=�����	�aG��aG����	���	���	�aG��aG��aG������aG��aG��aG��aG��aG�=��=���aG��aG��aG��aG����	���	���	���aG�����=�����aG��aG��aG����	>8Q�>8Q���aG����	���	���	�aG��aG��aG��aG����������aG��aG��aG��aG��������	���	���aG��aG��������	=�����	�aG��aG�����=�����	�������	���	�aG��������	�����	���	���	���	=�����	���	�����	���	���	=����=��=��=����=�����	�aG����	���	���	=��=�����	���	���	���	=��=�����	���	=��>�\)���	=�����	>8Q�=�����	=��=�����	�����	���	���	���	=��=��=�����	=��=��=��=����=�����	=��=�����	���	=�����	���	���	���	=��=�����	���	���	��=�����	=�����	���	���	�����	=��=��=��=��=��=��>8Q�=�����	=��>8Q�=�����	���	=�����	���	=��=��=��>8Q켣�	=�����	���	���	=��=��>8Q켣�	���	���	���	���	���	���	=��=�������	=�����	�����	���	��=��=�����	��=��=��>8Q�>8Q�=�������	���	���aG����	���	���	=��=��=��=��>8Q�>�\)>�\)>8Q�=��=�����	�����	���	���	�����	���	=��=��=��=��=��=��>�\)=�������	���	���	���	�����	���	���	���	���	���	���	�����	���	���	���	���	=�����	���	=��=��=��=�����	���	���	=��=�����	���	=�����	=�����	���	���	=��=�����	>8Q�=�����	���	=��=��>8Q�>8Q켣�	=��=��=��=��>8Q�>8Q켣�	�������	���	���	=��>8Q�=��>8Q�>8Q�>8Q�>�\)>]>]>]>�>�>�>�?z�?z�?.z?G�?G�?G�?G�?aG�?z�H?z�H?z�H?�=p?�=p?�
=?��
?��
?��
?���?���?���?�p�?�=p?�=p?�
=?�
=?��
?��
?��@�@�?�p�@�@�@�@�@�@Q�@Q�@�R@�R@%�@+�@1�@1�@1�@8Q�@>�R@E�@K�@K�@Q�@XQ�@XQ�@^�R@e�@k�@q�@q�@xQ�@xQ�@~�Q@��]@�@�@���@���@�(�@��]@��]@��]@�@�@���@�(�@�(�@�\)@��]@��]@�@���@���@�(�@�\)@�\)@��]@�@�@���@���@�(�@�\)@�\)@]@�@�@���@�(�@�(�@�\)@�\)@ҏ]@�@�@���@���@�(�@�\)@�\)@�]@�@�@���@�(�@�(�@�\)@�]@�]@�@���@���@�(�@�\)@�\)AG�A�GAz�Az�A{A�A�A	G�A	G�A
�GAz�Az�A{A{A�AG�AG�A�GAz�Az�A{A�A�AG�A�GAz�Az�A{A�A!G�A!G�A"�GA$z�A&{A'�A'�A)G�A*�GA,z�A.{A/�A1G�A2�GA4z�A6{A7�A9G�A:�GA<z�A>{A?�AAG�AB�GADz�AG�AIG�AJ�GALz�AN{AO�AQG�AR�GATz�AW�AW�AYG�AZ�GA^{A_�AaG�Ab�GAdz�Af{Ag�AiG�Aj�GAlz�An{Ao�AqG�Ar�GAr�GAv{Aw�AyG�AyG�Az�GA|z�A~{A�A���A�p�A�=qA�
>A��A���A�p�A�p�A�=qA�
>A��A���A�p�A�=qA�
>A��A���A�=qA�
>A��A���A�p�A�=qA��A���A�p�A�=qA��A���A�p�A�
>A��A���A�=qA�
>A���A�p�A�=qA�
>A���A�p�A�
>A��A���A�=qA�
>A��A���A�=qA�
>A��A�p�A�=qA�
>A���A�p�A�=qA�
>A���A�p�A�=qA��A���A�p�A�
>A��A���A�=qA�
>A��A�p�A�=qA�
>A��A�p�A�=qA�
>Ạ�A�p�A�=qA�
>A��A�p�A�=qA�
>Aԣ�A�p�A�=qA�
>Aأ�A�p�A�=qA�
>Aܣ�A�p�Dp�Dp�Dp��Dp�RDqDq�Dq�DqRDq%Dq+�Dq1�Dq8RDqEDqK�DqQ�Dq^�DqeDqk�Dqq�Dq~�Dq�Dq��Dq��Dq��Dq�Dq��Dq�RDq��Dq�DqˆDq�RDq޹Dq�Dq�Dq�RDq��DrDr�DrRDr�Dr+�Dr1�Dr8RDr>�DrK�DrQ�DrXRDr^�Drk�Drq�DrxRDr~�Dr��Dr��Dr�RDr�Dr��Dr��Dr��Dr�DrˆDr��Dr޹Dr�Dr�Dr�RDr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsK�DsQ�DsXRDs^�Dsk�Dsq�DsxRDs�Ds��Ds��Ds��Ds�Ds��Ds��Ds��Ds�DsˆDs�RDs޹Ds�Ds�Ds�RDs��DtDt�DtRDt�Dt+�Dt1�Dt8RDtEDtK�DtQ�Dt^�DteDtk�DtxRDt~�Dt�Dt��Dt�RDt��Dt�Dt��Dt�R@1�@8Q�@>�R@E�@K�@K�@Q�@XQ�@XQ�@^�R@e�@k�@q�@q�@xQ�@xQ�@~�Q@��]@�@�@���@���@�(�@��]@��]@��]@�@�@���@�(�@�(�@�\)@��]@��]@�@���@���@�(�@�\)@�\)@��]@�@�@���@���@�(�@�\)@�\)@]@�@�@���@�(�@�(�@�\)@�\)@ҏ]@�@�@���@���@�(�@�\)@�\)@�]@�@�@���@�(�@�(�@�\)@�]@�]@�@���@���@�(�@�\)@�\)AG�A�GAz�Az�A{A�A�A	G�A	G�A
�GAz�Az�A{A{A�AG�AG�A�GAz�Az�A{A�A�AG�A�GAz�Az�A{A�A!G�A!G�A"�GA$z�A&{A'�A'�A)G�A*�GA,z�A.{A/�A1G�A2�GA4z�A6{A7�A9G�A:�GA<z�A>{A?�AAG�AB�GADz�AG�AIG�AJ�GALz�AN{AO�AQG�AR�GATz�AW�AW�AYG�AZ�GA^{A_�AaG�Ab�GAdz�Af{Ag�AiG�Aj�GAlz�An{Ao�AqG�Ar�GAr�GAv{Aw�AyG�AyG�Az�GA|z�A~{A�A���A�p�A�=qA�
>A��A���A�p�A�p�A�=qA�
>A��A���A�p�A�=qA�
>A��A���A�=qA�
>A��A���A�p�A�=qA��A���A�p�A�=qA��A���A�p�A�
>A��A���A�=qA�
>A���A�p�A�=qA�
>A���A�p�A�
>A��A���A�=qA�
>A��A���A�=qA�
>A��A�p�A�=qA�
>A���A�p�A�=qA�
>A���A�p�A�=qA��A���A�p�A�
>A��A���A�=qA�
>A��A�p�A�=qA�
>A��A�p�A�=qA�
>Ạ�A�p�A�=qA�
>A��A�p�A�=qA�
>Aԣ�A�p�A�=qA�
>Aأ�A�p�A�=qA�
>Aܣ�A�p�Dp�Dp�Dp��Dp�RDqDq�Dq�DqRDq%Dq+�Dq1�Dq8RDqEDqK�DqQ�Dq^�DqeDqk�Dqq�Dq~�Dq�Dq��Dq��Dq��Dq�Dq��Dq�RDq��Dq�DqˆDq�RDq޹Dq�Dq�Dq�RDq��DrDr�DrRDr�Dr+�Dr1�Dr8RDr>�DrK�DrQ�DrXRDr^�Drk�Drq�DrxRDr~�Dr��Dr��Dr�RDr�Dr��Dr��Dr��Dr�DrˆDr��Dr޹Dr�Dr�Dr�RDr��DsDs�DsRDs�Ds%Ds1�Ds8RDs>�DsK�DsQ�DsXRDs^�Dsk�Dsq�DsxRDs�Ds��Ds��Ds��Ds�Ds��Ds��Ds��Ds�DsˆDs�RDs޹Ds�Ds�Ds�RDs��DtDt�DtRDt�Dt+�Dt1�Dt8RDtEDtK�DtQ�Dt^�DteDtk�DtxRDt~�Dt�Dt��Dt�RDt��Dt�Dt��Dt�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�C�A�=qA�;dA�=qA�?}A�A�A�;dA�bNAĝ�A�;dAò-AÃA�XA�/A�
=A�;dA�A�I�A���A��TA��wA���A�G�A���A��DA�jA�r�A�Q�A�oA��A��A��A���A�|�A��yA���A�l�A��TA���A��wA��+A�K�A�"�A���A�~�A�M�A���A���A��A�n�A���A��A��mA�5?A��wA�hsA��A��RA���A��DA�I�A��wA�JA�1'A���A��^A�  A���A�p�A�33A�A��A���A��A��+A���A�x�A�%A��
A��A�1A�+A�bNA���A�ĜA��A���A��+A�v�A���A�{A�ƨA�v�A���A�r�A��yA�=qA�bNA���A�%A���A���A�-A�l�A�A��uA�A�A���A�&�A�1'A���A���A�ffA�A��A��A�"�A�7A"�A|M�Ayt�Av�RAsp�Ao��Am�-Ag�PAd�HAc��Aa�^A]�TA[oAW��AUhsAT�uAQ�AN9XAI�AG�PAF�+AFjAFv�AF�AF-AE�^ADr�AC�AB�A@��A?G�A<�9A;"�A:=qA7��A5�A3��A2��A1�A.�HA-�A-�A,-A+A)�A);dA(�/A(��A(1'A'�FA&��A&�A&n�A%A$�A#|�A"-A �A n�A 1A�mA�#A�A�9AAn�AG�AbNA�A��A�A��A\)A^5A��A�A�mAdZA7LAA{AE�At�A�A��A
�!A
9XA	��AffA��AXA��A�DA�TA�A7LA1'AA+A��A ��A �uA A�@��F@��@�{@�7L@�;d@��@�j@��@�-@���@�1'@�|�@�&�@��@�@�@�K�@�@��#@�w@�7@���@���@��`@�I�@���@㕁@�o@���@��@��u@ܼj@�(�@��@�;d@Η�@�~�@��@��@�"�@θR@�dZ@�j@�33@��T@�?}@�I�@�1@���@ēu@Ɨ�@�ȴ@�o@��H@�`B@��`@��@�  @�S�@¸R@�E�@��@�Q�@�I�@��@�=q@��@���@��u@�1'@��@��@�@��-@�O�@�%@�bN@�C�@���@���@�t�@�dZ@��R@�V@�%@��
@�~�@���@�dZ@��;@���@��H@�p�@�Q�@��h@���@�M�@���@�V@��`@��h@���@�"�@�j@�5?@��y@��@�  @��F@���@�(�@��@��@��`@��@�7L@�7L@�7L@�V@��j@��@���@�S�@��!@��@��;@��
@�l�@�
=@���@�n�@�{@�@��7@�7L@�Ĝ@�A�@�K�@��R@�v�@�n�@�5?@���@�x�@��/@�dZ@�@�ȴ@���@�~�@�@��@���@�O�@��7@�bN@���@��/@���@���@��@�?}@��7@�x�@��!@��@�@��j@�Z@���@�dZ@�ȴ@�+@�z�@���@��@�I�@���@�\)@�33@���@��D@�%@�@��m@��#@�ff@��@�C�@�ƨ@�l�@�5?@�z�@�9X@�l�@�b@�Z@��D@���@��w@�"�@���@��@��H@�ȴ@���@�E�@��@�hs@���@�/@���@���@�%@��@��@�j@�Z@�A�@��@��
@���@���@�C�@�ȴ@�5?@��@��7@�?}@�&�@��@��9@�z�@��P@�ȴ@�~�@�$�@��-@�O�@��@���@�1@�@�n�@�=q@�{@�J@��#@�p�@�hs@�p�@�p�@���@�{@��@��@�J@��@��#@���@���@�x�@�O�@�7L@��@���@�W�@z8�@s|�@lg8@c�	@Yԕ@T>B@Mx�@E�D@>@�@7@23�@,%�@(g8@$N�@ �@o@@^5@}�@
�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aĺ^Aţ�A�?}A�VAžwA���A�/A�oA�1A��A���A���A�{A�1A���A��`A�E�A�$�A���A���A� �A���A���A�`BA��7A�%A�33A�JA�^5A�K�A�z�A�VA�(�A�jA�bAÁA�ffA���A�A��wA��TA���A�1AĶFA�  A���A��A�/A�1A�(�A�v�A�VA��A�C�A�A�A�M�A���A�Q�A�-A�  A�VA���A��
A�%A��wA��A�(�A��TA�9XA��A�
=A��TA��PAîA�v�A��A��A���A��`A��;A��A�-A�hsA�I�A���A�JA�;dA�K�A��\A�;dA+A�  A��\A���A��A��A��/A�C�A�O�A���A�p�A��A�1'A��`A²-A�"�A� �A��`A���A�v�A��A��A�G�A�E�A�dZAŲ-A���A�XA�+A��mA�VA�E�A��A�1A�%A�dZA�S�A�;dA��Aš�A��PAŏ\A��uAļjA�A�ĜA�33A�t�A���A�5?A���A���A�VA�bA�C�A��hA�&�A�"�A�{A�l�A�9XA�ffA�ȴA�VA�1A�l�A�{A�7LA�{AŸRA��A� �A���A���A���A�33A�&�A��A��A�?}A���A�$�A�$�A�VA���A���A��jA�S�A�&�A�"�A��A���A��;A��A�VA��A���A�K�A�oA���A�bA��A�VA�-Aé�A���A��-Aú^A� �A��A��A�{A�1'A�oA�1A��A�?}A�-A�oA���A�O�A�A�A��A� �A�&�A��A�oA��A�&�A��;A�Q�A��A� �A� �A�+A�(�A�"�A� �A� �A� �A�{A�VA��A��A��A�$�A�{A��A��A� �A�{A��A��A�p�A��A��A��A��A�{A�oA�{A�oA�oA�oA�{A��A�%A�{A��A��A�1A�VA�
=A�{A�AŲ-A��A��A�JA�{A�&�A��A�{A�bA��A��A�
=A��A�{A��A��A�oA��A�{A�{A��A��A�bA��A��A�oA�bA�oA�bA�bA�oA�VA�
=A�VA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA��A�{A�
=A��A��A��A��A��A��A��A��A��A� �A�"�A�$�A�$�A�{A��A��A��A��A� �A� �A�&�A�$�A��A�(�A�$�A��A��A� �A��A��A� �A�(�A�(�A�(�A�(�A�"�A�"�A��A�$�A��A�"�A�&�A���Aŏ\A�(�A�+A�/A�$�A��A� �A�"�A�$�A��A��A��A�oA��AœuA��A��A�VA��A�bA��Aġ�A�oA� �A��A��A�"�A��A�"�A�"�A�"�A�&�A�"�A� �A� �A�$�A�+A�"�A� �A� �A��A��A��A��A��A��A��A��A�"�A� �A�"�A�$�A� �A��A� �A��A��A��A��A��A��A��A� �A� �A� �A��A�$�A�$�A� �A� �A� �A��A�"�A� �A�$�A�$�A�"�A�$�A� �A� �A��A� �A� �A��A��A�{A��A��A��A�%A��A�(�A�"�A��A��A��A��A��A��A�"�A�&�A�(�A�&�A�&�A�&�A�-A�$�A�1'A�/A�1'A�-A�(�A�+A�&�A�&�A�+A�(�A�+A�(�A�+A�-A�/A�(�A�-A�/A�/A�1'A�1'A�/A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�(�A�-A�/A�/A�-A�5?A�9XA�7LA�(�A�1'A�7LA�5?A�/A�7LA�7LA�7LA�?}A�?}A�?}A�=qA�?}A�?}A�?}A�?}A�?}A�?}A�=qA�?}A�?}A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�?}A�?}A�A�A�?}A�A�A�?}A�?}A�A�A�?}A�A�A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�?}A�A�A�?}A�A�A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�?}A�?}A�A�A�A�A�?}A�?}A�=qA�;dA�;dA�=qA�9XA�;dA�=qA�5?A�7LA�7LA�9XA�7LA�;dA�5?A�=qA�A�A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�;dA�;dA�9XA�9XA�9XA�7LA�9XA�;dA�;dA�=qA�=qA�;dA�9XA�;dA�;dA�;dA�=qA�?}A�=qA�;dA�7LA�7LA�5?A�7LA�5?A�1'A�33A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�9XA�=qA�=qA�;dA�9XA�?}A�;dA�A�A�A�A�;dA�;dA�;dA�9XA�9XA�;dA�9XA�;dA�=qA�=qA�;dA�7LA�5?A�;dA�?}A�?}A�?}A�;dA�=qA�;dA�=qA�;dA�;dA�9XA�9XA�9XA�=qA�=qA�;dA�=qA�=qA�=qA�?}A�=qA�=qA�A�A�=qA�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�C�A�A�A�?}A�=qA�?}A�?}A�?}A�?}A�?}A�;dA�33A�33A�1'A�-A�-A�-A�+A�+A�$�A�JA�%A���AŶFAŬAŇ+A�ffA�Q�A�E�A��`A��TA��;A���A���A�Aĺ^AĸRAĲ-Aĥ�Aĝ�Aě�Aė�Aĕ�Aď\Aď\AċDAĉ7A�|�A�|�A�~�A�r�A�bNA�^5A�\)A�^5A�^5A�Q�A�I�A�/A�{A�{A�JA���A���A��A��TA��#A���Að!Að!AîAìAìAé�Aã�Aã�Aá�Aß�AÝ�AÛ�AÛ�A×�AÓuAÏ\AÍPAÇ+AÅAÁAÁ@��#@���@��#@��#@��#@��#@���@���@��#@��#@���@���@���@���@���@���@���@���@���@���@��^@��^@��-@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@��7@��7@��7@��7@��7@��7@��@�p�@�`B@�X@�X@�X@�G�@�?}@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�G�@�?}@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�&�@�&�@�&�@��@��@��@��@��@�%@�%@���@���@��`@��/@���@���@���@���@���@���@�Ĝ@�Ĝ@��j@��9@��@��@���@���@��u@��u@��u@��u@��u@��u@��D@��D@��D@��@�jA�1'A�/A�/A�7LA�?}A�?}A�A�A�A�A�C�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�?}A�A�A�C�A�A�A�A�A�A�A�A�A�C�A�A�A�C�A�C�A�A�A�C�A�A�A�A�A�A�A�C�A�A�A�C�A�A�A�C�A�C�A�C�A�A�A�A�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�A�A�=qA�?}A�A�A�=qA�;dA�7LA�7LA�7LA�7LA�9XA�9XA�;dA�=qA�=qA�A�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�?}A�?}A�=qA�=qA�9XA�=qA�;dA�=qA�;dA�?}A�?}A�=qA�=qA�=qA�?}A�?}A�=qA�?}A�?}A�A�A�?}A�;dA�;dA�9XA�9XA�7LA�7LA�9XA�5?A�7LA�9XA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�?}A�=qA�?}A�A�A�A�A�C�A�C�A�9XA�;dA�=qA�;dA�;dA�=qA�=qA�;dA�?}A�A�A�=qA�=qA�=qA�?}A�A�A�A�A�?}A�A�A�?}A�?}A�?}A�=qA�?}A�?}A�;dA�9XA�;dA�?}A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�A�A�?}A�C�A�C�A�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�;dA�?}A�A�A�C�A�C�A�A�A�A�A�7LA�1'A�1'A�-A�/A�+A�+A�+A�A��#AŶFAŶFAŏ\A�p�A�l�A�K�A�$�A��yA��mA��mA��;A���A�ƨAľwAĸRAĮAģ�Aĝ�Aĝ�Aė�Aę�Aę�Aď\AčPAĉ7AāA�~�A�|�A�r�A�ffA�`BA�`BA�^5A�ZA�S�A�E�A�$�A��A�{A�
=A���A���A��A��HA��AþwAô9Að!Að!Að!Að!AîAå�Aç�Aã�Aá�Aá�Aß�AÛ�AÙ�AÕ�AÏ\AËDAÇ+AÇ+AÃ@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@���@���@���@�@��^@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@��h@��h@��h@��7@��7@��7@��7@�x�@�p�@�`B@�`B@�X@�X@�O�@�O�@�G�@�G�@�O�@�O�@�O�@�G�@�G�@�G�@�G�@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�/@�&�@�&�@��@��@��@��@�V@�V@�%@���@��@��/@��/@��/@��/@��/@���@���@���@�Ĝ@�Ĝ@��9@��@��@���@���@���@���@��u@��u@��u@��u@��u@��u@��D@�dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  A�?}A�C�A�=qA�;dA�=qA�?}A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/G�O�G�O�A�A�I�A���A��TA��wA���G�O�A���A��DG�O�G�O�A�Q�A�oA��A��A��A���A�|�A��yA���A�l�A��TA���A��wA��+A�K�A�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A���A�p�A�33A�A��A���A��A��+A���A�x�A�%A��
A��A�1A�+A�bNA���A�ĜA��A���A��+A�v�A���A�{A�ƨA�v�A���A�r�A��yA�=qA�bNA���A�%A���A���A�-A�l�A�A��uA�A�A���A�&�A�1'A���A���A�ffA�A��A��A�"�A�7A"�A|M�Ayt�Av�RAsp�Ao��Am�-Ag�PAd�HAc��Aa�^A]�TA[oAW��AUhsAT�uAQ�AN9XAI�AG�PAF�+AFjAFv�AF�AF-AE�^ADr�AC�AB�A@��A?G�A<�9A;"�A:=qA7��A5�A3��A2��A1�A.�HA-�A-�A,-A+A)�A);dA(�/A(��A(1'A'�FA&��A&�A&n�A%A$�A#|�A"-A �A n�A 1A�mA�#A�A�9AAn�AG�AbNA�A��A�A��A\)A^5A��A�A�mAdZA7LAA{AE�At�A�A��A
�!A
9XA	��AffA��AXA��A�DA�TA�A7LA1'AA+A��A ��A �uA A�@��F@��@�{@�7L@�;d@��@�j@��@�-@���@�1'@�|�@�&�@��@�@�@�K�@�@��#@�w@�7@���@���@��`@�I�@���@㕁@�o@���@��@��u@ܼj@�(�@��@�;d@Η�@�~�@��@��@�"�@θR@�dZ@�j@�33@��T@�?}@�I�@�1@���@ēu@Ɨ�@�ȴ@�o@��H@�`B@��`@��@�  @�S�@¸R@�E�@��@�Q�@�I�@��@�=q@��@���@��u@�1'@��@��@�@��-@�O�@�%@�bN@�C�@���@���@�t�@�dZ@��R@�V@�%@��
@�~�@���@�dZ@��;@���@��H@�p�@�Q�@��h@���@�M�@���@�V@��`@��h@���@�"�@�j@�5?@��y@��@�  @��F@���@�(�@��@��@��`@��@�7L@�7L@�7L@�V@��j@��@���@�S�@��!@��@��;@��
@�l�@�
=@���@�n�@�{@�@��7@�7L@�Ĝ@�A�@�K�@��R@�v�@�n�@�5?@���@�x�@��/@�dZ@�@�ȴ@���@�~�@�@��@���@�O�@��7@�bN@���@��/@���@���@��@�?}@��7@�x�@��!@��@�@��j@�Z@���@�dZ@�ȴ@�+@�z�@���@��@�I�@���@�\)@�33@���@��D@�%@�@��m@��#@�ff@��@�C�@�ƨ@�l�@�5?@�z�G�O�@�l�@�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�E�@��@�hs@���@�/@���@���@�%@��@��@�j@�Z@�A�@��@��
@���@���@�C�@�ȴ@�5?@��@��7@�?}@�&�@��@��9@�z�@��P@�ȴ@�~�@�$�@��-@�O�@��@���@�1@�@�n�@�=q@�{@�J@��#@�p�@�hs@�p�@�p�@���@�{@��@��@�J@��@��#@���@���@�x�@�O�@�7L@��G�O�@�W�@z8�@s|�@lg8@c�	@Yԕ@T>B@Mx�@E�D@>@�@7@23�@,%�@(g8@$N�@ �@o@@^5@}�@
�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aĺ^Aţ�A�?}A�VAžwA���A�/A�oA�1A��A���A���A�{A�1A���A��`A�E�A�$�A���A���A� �A���A���A�`BA��7A�%A�33A�JA�^5A�K�A�z�A�VA�(�A�jA�bAÁA�ffA���A�A��wA��TA���A�1AĶFA�  A���A��A�/A�1A�(�A�v�A�VA��A�C�A�A�A�M�A���A�Q�A�-A�  A�VA���A��
A�%A��wA��A�(�A��TA�9XA��A�
=A��TA��PAîA�v�A��A��A���A��`A��;A��A�-A�hsA�I�A���A�JA�;dA�K�A��\A�;dA+A�  A��\A���A��A��A��/A�C�A�O�A���A�p�A��A�1'A��`A²-A�"�A� �A��`A���A�v�A��A��A�G�A�E�A�dZAŲ-A���A�XA�+A��mA�VA�E�A��A�1A�%A�dZA�S�A�;dA��Aš�A��PAŏ\A��uAļjA�A�ĜA�33A�t�A���A�5?A���A���A�VA�bA�C�A��hA�&�A�"�A�{A�l�A�9XA�ffA�ȴA�VA�1A�l�A�{A�7LA�{AŸRA��A� �A���A���A���A�33A�&�A��A��A�?}A���A�$�A�$�A�VA���A���A��jA�S�A�&�A�"�A��A���A��;A��A�VA��A���A�K�A�oA���A�bA��A�VA�-Aé�A���A��-Aú^A� �A��A��A�{A�1'A�oA�1A��A�?}A�-A�oA���A�O�A�A�A��A� �A�&�A��A�oA��A�&�A��;A�Q�A��A� �A� �A�+A�(�A�"�A� �A� �A� �A�{A�VA��A��A��A�$�A�{A��A��A� �A�{A��A��A�p�A��A��A��A��A�{A�oA�{A�oA�oA�oA�{A��A�%A�{A��A��A�1A�VA�
=A�{A�AŲ-A��A��A�JA�{A�&�A��A�{A�bA��A��A�
=A��A�{A��A��A�oA��A�{A�{A��A��A�bA��A��A�oA�bA�oA�bA�bA�oA�VA�
=A�VA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA��A�{A�
=A��A��A��A��A��A��A��A��A��A� �A�"�A�$�A�$�A�{A��A��A��A��A� �A� �A�&�A�$�A��A�(�A�$�A��A��A� �A��A��A� �A�(�A�(�A�(�A�(�A�"�A�"�A��A�$�A��A�"�A�&�A���Aŏ\A�(�A�+A�/A�$�A��A� �A�"�A�$�A��A��A��A�oA��AœuA��A��A�VA��A�bA��Aġ�A�oA� �A��A��A�"�A��A�"�A�"�A�"�A�&�A�"�A� �A� �A�$�A�+A�"�A� �A� �A��A��A��A��A��A��A��A��A�"�A� �A�"�A�$�A� �A��A� �A��A��A��A��A��A��A��A� �A� �A� �A��A�$�A�$�A� �A� �A� �A��A�"�A� �A�$�A�$�A�"�A�$�A� �A� �A��A� �A� �A��A��A�{A��A��A��A�%A��A�(�A�"�A��A��A��A��A��A��A�"�A�&�A�(�A�&�A�&�A�&�A�-A�$�A�1'A�/A�1'A�-A�(�A�+A�&�A�&�A�+A�(�A�+A�(�A�+A�-A�/A�(�A�-A�/A�/A�1'A�1'A�/A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�(�A�-A�/A�/A�-A�5?A�9XA�7LA�(�A�1'A�7LA�5?A�1'A�/A�/A�7LA�?}A�?}A�A�A�A�A�C�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�?}A�A�A�C�A�A�A�A�A�A�A�A�A�C�A�A�A�C�A�C�A�A�A�C�A�A�A�A�A�A�A�C�A�A�A�C�A�A�A�C�A�C�A�C�A�A�A�A�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�A�A�=qA�?}A�A�A�=qA�;dA�7LA�7LA�7LA�7LA�9XA�9XA�;dA�=qA�=qA�A�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�?}A�?}A�=qA�=qA�9XA�=qA�;dA�=qA�;dA�?}A�?}A�=qA�=qA�=qA�?}A�?}A�=qA�?}A�?}A�A�A�?}A�;dA�;dA�9XA�9XA�7LA�7LA�9XA�5?A�7LA�9XA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�?}A�=qA�?}A�A�A�A�A�C�A�C�A�9XA�;dA�=qA�;dA�;dA�=qA�=qA�;dA�?}A�A�A�=qA�=qA�=qA�?}A�A�A�A�A�?}A�A�A�?}A�?}A�?}A�=qA�?}A�?}A�;dA�9XA�;dA�?}A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�A�A�?}A�C�A�C�A�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�;dA�?}A�A�A�C�A�C�A�A�A�A�A�7LA�1'A�1'A�-A�/A�+A�+A�+A�A��#AŶFAŶFAŏ\A�p�A�l�A�K�A�$�A��yA��mA��mA��;A���A�ƨAľwAĸRAĮAģ�Aĝ�Aĝ�Aė�Aę�Aę�Aď\AčPAĉ7AāA�~�A�|�A�r�A�ffA�`BA�`BA�^5A�ZA�S�A�E�A�$�A��A�{A�
=A���A���A��A��HA��AþwAô9Að!Að!Að!Að!AîAå�Aç�Aã�Aá�Aá�Aß�AÛ�AÙ�AÕ�AÏ\AËDAÇ+AÇ+AÃ@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@���@���@���@�@��^@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@��h@��h@��h@��7@��7@��7@��7@�x�@�p�@�`B@�`B@�X@�X@�O�@�O�@�G�@�G�@�O�@�O�@�O�@�G�@�G�@�G�@�G�@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�/@�&�@�&�@��@��@��@��@�V@�V@�%@���@��@��/@��/@��/@��/@��/@���@���@���@�Ĝ@�Ĝ@��9@��@��@���@���@���@���@��u@��u@��u@��u@��u@��u@��D@�dZA�1'A�/A�/A�7LA�?}A�?}A�A�A�A�A�C�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�?}A�A�A�C�A�A�A�A�A�A�A�A�A�C�A�A�A�C�A�C�A�A�A�C�A�A�A�A�A�A�A�C�A�A�A�C�A�A�A�C�A�C�A�C�A�A�A�A�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�A�A�=qA�?}A�A�A�=qA�;dA�7LA�7LA�7LA�7LA�9XA�9XA�;dA�=qA�=qA�A�A�A�A�C�A�C�A�C�A�C�A�C�A�A�A�?}A�?}A�=qA�=qA�9XA�=qA�;dA�=qA�;dA�?}A�?}A�=qA�=qA�=qA�?}A�?}A�=qA�?}A�?}A�A�A�?}A�;dA�;dA�9XA�9XA�7LA�7LA�9XA�5?A�7LA�9XA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�?}A�=qA�?}A�A�A�A�A�C�A�C�A�9XA�;dA�=qA�;dA�;dA�=qA�=qA�;dA�?}A�A�A�=qA�=qA�=qA�?}A�A�A�A�A�?}A�A�A�?}A�?}A�?}A�=qA�?}A�?}A�;dA�9XA�;dA�?}A�A�A�?}A�A�A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�A�A�?}A�C�A�C�A�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�;dA�?}A�A�A�C�A�C�A�A�A�A�A�7LA�1'A�1'A�-A�/A�+A�+A�+A�A��#AŶFAŶFAŏ\A�p�A�l�A�K�A�$�A��yA��mA��mA��;A���A�ƨAľwAĸRAĮAģ�Aĝ�Aĝ�Aė�Aę�Aę�Aď\AčPAĉ7AāA�~�A�|�A�r�A�ffA�`BA�`BA�^5A�ZA�S�A�E�A�$�A��A�{A�
=A���A���A��A��HA��AþwAô9Að!Að!Að!Að!AîAå�Aç�Aã�Aá�Aá�Aß�AÛ�AÙ�AÕ�AÏ\AËDAÇ+AÇ+AÃ@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@��#@���@���@���@�@��^@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��h@��h@��h@��h@��7@��7@��7@��7@�x�@�p�@�`B@�`B@�X@�X@�O�@�O�@�G�@�G�@�O�@�O�@�O�@�G�@�G�@�G�@�G�@�O�@�O�@�O�@�O�@�O�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�/@�&�@�&�@��@��@��@��@�V@�V@�%@���@��@��/@��/@��/@��/@��/@���@���@���@�Ĝ@�Ĝ@��9@��@��@���@���@���@���@��u@��u@��u@��u@��u@��u@��D@�dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�;oG�O�G�O�;o;o;o;o;o;oG�O�;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@{�@-0@���>@�'@���=���?��d?+��@��?{t>W�4@���@�'(=�^t>�->��=v�?=��}=�*�=�28>5@��Q@���>���>��]@��0>�d�><;O?��@��q>o~>���?�M=��>�.>�
�@���=� �=�]�>Y�@W�=�&W=�#O?U�@�õ=���>s>#��@���@=Q��=�Ri=��>#L=��Q>c5@?/�>�9?�(@��@��V>܀�>2�D@���>D0+@���?�	-@��>�Q?9��@���=�S�>��@��=���?i�=�C�>.�c>D@p�@�3�=M+,=O�4=�W*=���=�R�=��>��>�8�>!>�P	?v�=��=���?Z�8?�C=t��=�D�?
1�=�H�>�g@��@?I��>ә>�b@�vu>��@���>�@�R@���@���=�`=�;%>�u@�e@_�=���?Yla@��h@`�=�P]>�@��N@���@��=�)�>���@��L@���>i/@���> ٔ@���?�D=�]�>'�?}K�@���=��>N�@��Q@�S�@���?�(�>:��?�Z�@���@��@=�q7=�D�>��@d�@Kn?��b=��=�s�>1��?sm?�$�@��;@eP�>*y�>�?+/E>�V�@�Ǥ@���@��@	��>��'@��K@��@��;@,�>s?���>��~@�Ɠ@��r@��a>��">�V�@�¤@��Q@��r=�n�=�؄>�҉?�9@��	@��@��>C�>�G�@��L>	l?D�@��@��@�Ɠ@���?��>�@�_[@��)>�2�@��@��@ә>��@^:*?g��@��\@��@��;@��?�9�@���@f��?Ҹ�@��m@��m@��}@��d@��u@��.@�˧@��:@��:@��K@��;@�ł@�ɰ@���@���@�ȟ@��K@���@��)@���@��;@�ȟ@�ƽ@���@��@��m@��@�ł@��*@���@��@��.@���@��.@��a@��@�Ɠ@�Ɠ@�Ǥ@���@��@�Ǥ@��@���@���@�ȟ@���@���@���@��m@�ȟ@���@��;@���@���@��P@��;@��;@��*@���@��?@���@���@���@�Ɠ@�Ɠ@���@��?@��@���@���@���@��a@��@���@��r@��@���@�Ǥ@��K@�Ǥ@�Ǥ@�Ɠ@���@�Ǥ@��@��@���@��u@�Ǥ@��u@���@��u@��@��@��e@��;@��;@���@�Ɠ@��*@��;@��@��@�ɰ@�Ǥ@�ɰ@��\@�ɰ@��m@��)@��)@��}@��:@�͟@���@��.@�Ɇ@���@�ʗ@��.@���@���@��@�ʗ@�ʗ@��}@��)@��m@���@��}@��@���@��@���@���@�͟@��@���@���@��}@��K@���@���@��:@��.@/��@���@��[@��@��m@��\@��@��@��@��\@��@��K@�Ɠ@�Ǥ@���@�Ɠ@��*@���@��;@���@��K?�g�@�Ɇ@���@�Ɇ@���@���@��d@��d@��@�̸@�̸@�˧@�ɰ@���@�͟@�͟@���@���@��}@��m@��\@�ɰ@��@���@��@��@��)@��:@���@��:@�̎@��)@�ʗ@���@�ʗ@�ʗ@���@�Ɇ@��>@�˧@�˧@��>@�̎@��)@��)@�̎@���@�̎@��)@��@�ɰ@��@�̎@���@��:@�̎@�̎@���@���@��)@�̎@�ɰ@��@��@��u@�Ɇ@��@���@��@��>@��>@���@��}@��:@��@��\@��}@��K@��K@��[@�ί@��l@��@��}@��)@���@���@��}@��}@��)@��)@��)@��l@��B@��B@�ϖ@��S@��S@��S@��c@��S@��S@��c@��c@���@��J@��9@���@���@��)@���@��)@��)@���@���@���@��9@�Б@��9@��J@���@���@�Ҟ@��@��J@���@��J@���@��g@��@��w@��w@�׈@���@��0@�ؙ@���@�خ@�خ@���@�خ@�خ@��E@�خ@�خ@�خ@��@�خ@�خ@��@��U@��U@��U@��U@��U@���@��U@��U@��U@���@��U@�٩@���@�٩@�پ@�پ@��j@��j@�پ@�پ@�پ@��'@��'@��'@��'@��{@��'@��{@��'@��f@��f@��{@���@��@��f@��f@��@�٩@��@��@��U@�ؙ@�ؙ@��Z@��@�خ@��j@�خ@��I@��@���@��Z@���@��@���@�ی@���@���@��3@��H@��#@���@�ی@���@��@��@�پ@��@��j@��@��@��{@��{@�ی@��8@���@���@�ڐ@���@���@�ڐ@���@���@���@��@��@��@���@��@��o@���@��o@���@��j@��j@��j@��'@��'@��{@���@�ۡ@�ۡ@�ۡ@��8@���@��H@�ܱ@��n@�ۡ@���@��M@��M@���@��M@��M@���@�ܱ@��]@��
@�ۡ@�ۡ@��M@��@��n@��n@��@��H@�ܱ@�ܱ@��]@��]@��]@��]@��]@��M@��n@�ܱ@��@��@��n@��n@��@��n@���@��n@��@��@��@��n@��n@���@��@��n@��n@��@���@��
@��M@�۶@��
@�۶@��
@�۶@�ڥ@���@��M@�ڐ@�ڐ@��@��@��@�ײ@��
@��M@�Ѣ@��:@�˼@���@���@���@�� @��y@���@���@��@��N@��,@��N@���@��@��y@���@��P@��~@��n@��]@��L@��Q@���@���@���@���@���@��Q@���@���@���@��/@��@@���@���@���@��L@��@���@���@���@�{�@�y@�u�@�o?@�kQ@�g#@�\}@�\@�[�@�[�@�Z�@�Y�@�W~@�W@�V�@�V�@�V�@�W@�V.@�U@�S�@�R @�O�@�N�@�OL@�N<@�L�@S!�@S!W@S!�@S!�@S!�@S!�@S!�@S!�@S!�@S!�@S!�@S!�@S!�@S!W@S!�@S!�@S!W@S!@S 2@S6@S:@S?@SC@S�@SC@SC@S�@S�@S�@S�@SC@S�@S�@S�@SH@S�@S�@SH@S�@S�@Sz@S�@S�@S+@S+@S+@S@S�@S�@Sb@S�@So@S�@S�@S�@Sw@SM@S�@S�@S�@S�@S�@S�@S�@S�@S�@Ss@Ss@S�@SI@S@S�@SM@Sw@S#@SR@S�@SV@SZ@S�@S�@S@S�@S@S�@S@S
g@S
@S	@S�@S�@S�@SW@S�@S�@Se@S �@S i@R��@R�@R��@R��@R��@R��@R��@R�]@R��@R�@R��@R�j@R��@R��@R�@R��@R�s@R��@R�@R�Z@R�@R�@�Tv@�T7@�T�@�VC@�Z�@�Z2@�[l@�[B@�[l@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�\@�[�@�[�@�[�@�\@�\)@�\)@�\)@�\@�\h@�\h@�\S@�\)@�\)@�\h@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�]@�\�@�]@�]@�]@�]@�]@�]O@�]�@�\�@�[�@�[B@�\�@�[W@�Z�@�Y@�Xy@�Y@�X�@�Y@�Y�@�Zq@�\}@�[�@�^@�]�@�^_@�^�@�^_@�^�@�^�@�^_@�]�@�\�@�\}@�\S@�[�@�[�@�[�@�\@�\S@�\)@�]y@�]d@�\�@�\�@�]O@�]%@�\�@�]O@�]�@�^J@�^@�\�@�\)@�[@�[B@�ZG@�Z�@�[�@�Zq@�Z�@�Z�@�[�@�[W@�[�@�\S@�\>@�\S@�]%@�]�@�]%@�]y@�]�@�^�@�^�@�_F@�[�@�[�@�\�@�\}@�[�@�\�@�]:@�\�@�]d@�^�@�]y@�]�@�\�@�^�@�_@�_@�_[@�_[@�^J@�^ @�^�@�^@�^5@�]�@�]�@�]%@�\}@�^�@�_F@�^�@�_[@�_F@�_F@�_@�_F@�_�@�_�@�_�@�_@�_1@�_@�`-@�`k@�`@�_�@�_�@�`W@�`k@�`B@�`@�_�@�`@�a@�a(@�a@�`�@�a=@�a(@�`�@�`k@�_�@�_�@�_�@�_F@�^�@�^�@�^@�Xd@�R�@�LY@�Jb@�F5@�?)@�=�@�8G@�0�@�#�@�#�@�"�@�!@�@��@��@�]@�'@��@��@��@�@��@��@�9@�
�@�	�@��@��@��@�@��@� @�
@�x@��@�b@��@�'@�V@�o@�
�@�\@��@��r@��{@��@���@�ߏ@��@�޾@�ޔ@��j@�ݭ@���@��@��@���@���@���@��@�؄@�֌@���@���@�л@�Б@��@QP3@QP]@QP�@QP]@QP�@QP�@QP�@QP�@QP�@QP�@QP�@QP�@QQ@QP�@QQ@QQ/@QQ@QQ/@QP�@QP3@QO�@QM�@QM@QK@QJ�@QJ�@QJ�@QJ�@QK@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJM@QI�@QH�@QH�@QHV@QH@QG�@QG�@QG�@QG�@QF5@QC�@QA�@Q@�@Q@O@Q?�@Q>�@Q>�@Q?)@Q?)@Q?)@Q?S@Q>�@Q?S@Q?S@Q?}@Q?�@Q?�@Q@�@Q@y@Q@�@Q@O@Q@O@Q?�@Q?}@Q?S@Q>�@Q>@Q=�@Q=2@Q=@Q<�@Q<�@Q<�@Q<�@Q;�@Q;@Q:�@Q:?@Q9�@Q8@Q7v@Q6�@Q5@Q3�@Q1�@Q1@Q0�@Q0@@Q0j@Q/E@Q.s@Q-�@Q-w@Q,�@Q+,@Q*Z@Q)�@Q(�@Q(@Q'=@Q&�@Q&�@Q&�@Q&�@Q&l@Q%�@Q%p@Q$�@Q"}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      3434344434433444444443344344434444443444444434443444444444433443434344344344444334444444444444444444434443434433444444433443334433434344443443334443344433444444334444333443334444333443334444333443443333443343344343333433433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@{�G�O�@���G�O�@���G�O�G�O�G�O�@��G�O�G�O�@���@�'+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��T@���G�O�G�O�@��1G�O�G�O�G�O�@��sG�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�÷G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��XG�O�G�O�@���G�O�@���G�O�@���G�O�G�O�@���G�O�G�O�@��
G�O�G�O�G�O�G�O�G�O�@p�@�3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��DG�O�G�O�G�O�@�vxG�O�@���G�O�G�O�@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��i@`�G�O�G�O�@��R@���@��G�O�G�O�@��N@���G�O�@���G�O�@���G�O�G�O�G�O�G�O�@���G�O�G�O�@��O@�S�@���G�O�G�O�G�O�@���@��BG�O�G�O�G�O�@d�@KnG�O�G�O�G�O�G�O�G�O�G�O�@��;@eP�G�O�G�O�G�O�G�O�@�ǥ@��@��G�O�G�O�@��O@��@��=G�O�G�O�G�O�G�O�@�Ɩ@��v@��bG�O�G�O�@�¦@��Q@��qG�O�G�O�G�O�G�O�@��@��@��G�O�G�O�@��JG�O�G�O�@��@��@�ƕ@���G�O�G�O�@�_Z@��,G�O�@��
@��G�O�G�O�@^:+G�O�@��Z@��@��:@��!G�O�@���@f��G�O�@��o@��r@��|@��g@��v@��-@�˧@��8@��;@��L@��:@�ņ@�ɯ@���@���@�Ȥ@��L@���@��-@���@��>@�ȡ@�ƾ@���@��@��n@��@�ņ@��(@���@��@��6@���@��4@��b@��
@�ƒ@�Ɣ@�Ǧ@���@��@�ǩ@��@���@���@�ȣ@���@���@���@��n@�ȡ@���@��8@���@���@��R@��:@��>@��*@���@��?@���@��@���@�Ƙ@�Ɣ@���@��>@��@���@���@���@��d@��@���@��t@��@���@�ǩ@��K@�ǣ@�Ǣ@�Ƒ@���@�Ǧ@��@��@���@��t@�Ǣ@��x@���@��w@��@��@��d@��:@��@@���@�Ɠ@��,@��>@��@��@�ɬ@�ǥ@�ɶ@��b@�ɲ@��m@��+@��+@��|@��:@�͞@���@��2@�Ɇ@���@�ʖ@��.@���@���@��@�ʘ@�ʕ@��~@��)@��l@���@��|@��@���@��@���@���@�͟@��@���@���@��|@��M@���@���@��9@��.G�O�@���@��^@��	@��p@��[@��@��@��@��[@��
@��K@�Ƙ@�ǥ@���@�Ɩ@��,@���@��7@���@��JG�O�@�Ɋ@���@�Ɍ@���@���@��f@��b@��@�̼@�̹@�˪@�ɯ@���@�͜@�͞@���@���@��|@��n@��^@�ɲ@��@���@��@��@��+@��7@���@��7@�̏@��-@�ʘ@���@�ʘ@�ʖ@���@�Ɇ@��@@�˥@�˪@��?@�̑@��-@��)@�̏@���@�̎@��-@��@�ɲ@��@�̑@���@��>@�̌@�̎@���@���@��*@�̑@�ɮ@��@��@��{@�ɉ@��@���@��"@��;@��@@���@��~@��>@��	@��c@��|@��J@��M@��Y@�ΰ@��l@��@��@��*@�Ͽ@���@��@��~@��)@��'@��*@��q@��>@��C@�ϓ@��R@��V@��R@��c@��R@��R@��c@��g@���@��J@��;@���@�Ͽ@��,@���@��*@��*@���@���@���@��;@�Б@��;@��F@���@���@�Ң@��@��J@���@��L@���@��j@��@�Tr@�T6@�T�@�VB@�Z�@�Z0@�[o@�[B@�[l@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�\@�\ @�[�@�[�@�\@�\'@�\+@�\,@�\@�\j@�\e@�\U@�\+@�\+@�\g@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�]@�\�@�]@�]@�]@�]@�]@�]S@�]�@�\�@�[�@�[C@�\�@�[\@�Z�@�Y@�Xz@�Y@�X�@�Y
@�Y�@�Zs@�\z@�[�@�^
@�]�@�^b@�^�@�^^@�^�@�^�@�^_@�]�@�\�@�\�@�\U@�[�@�[�@�[�@�\@�\R@�\)@�]z@�]e@�\�@�\�@�]P@�]&@�\�@�]N@�]�@�^J@�^
@�\�@�\)@�[@�[E@�ZG@�Z�@�[~@�Zr@�Z�@�Z�@�[�@�[Y@�[�@�\Q@�\>@�\P@�]#@�]�@�]&@�]v@�]�@�^�@�^�@�_G@�[�@�[�@�\�@�\@�\@�\�@�]<@�\�@�]e@�^�@�]v@�]�@�\�@�^�@�_
@�_!@�_V@�_^@�^J@�^ @�^�@�^
@�^5@�]�@�]�@�]$@�\~@�^�@�_J@�^�@�_Z@�_G@�_G@�_@�_F@�_�@�_�@�_�@�_@�_5@�_@�`-@�`k@�`@�_�@�_�@�`X@�`l@�`B@�`@�_�@�`@�a@�a*@�a@�`�@�aB@�a)@�`�@�`k@�_�@�_�@�_�@�_K@�^�@�^�@�^@�Xh@�R�@�LX@�Jd@�F5@�?)@�=�@�8I@�0�@�#�@�#�@�"�@�!@�@��@��@�^@�*@��@��@��@�@��@��@�9@�
�@�	�@��@��@��@�@��@� @�
@�y@��@�e@��@�'@�X@�n@�
�@�^@��@��q@��|@��@���@�ߏ@��~@�޽@�ސ@��l@�ݮ@���@��@��@���@���@���@��@�؆@�֐@���@���@�з@�В@��@QP6@QP`@QP�@QP]@QP�@QP�@QP�@QP�@QP�@QP�@QP�@QP�@QQ@QP�@QQ@QQ*@QQ@QQ.@QP�@QP3@QO�@QM�@QM@QK@QJ�@QJ�@QJ�@QJ�@QK@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJM@QI�@QH�@QH�@QHV@QH@QG�@QG�@QG�@QG�@QF3@QC�@QA�@Q@�@Q@M@Q?�@Q>�@Q>�@Q?(@Q?(@Q?+@Q?R@Q>�@Q?R@Q?R@Q?{@Q?�@Q?�@Q@�@Q@v@Q@�@Q@N@Q@R@Q?�@Q?z@Q?S@Q>�@Q>@Q=�@Q=5@Q=
@Q<�@Q<�@Q<�@Q<�@Q;�@Q;@Q:�@Q:@@Q9�@Q8@Q7x@Q6�@Q4�@Q3�@Q1�@Q1@Q0�@Q0B@Q0k@Q/H@Q.s@Q-�@Q-z@Q,�@Q++@Q*Z@Q)�@Q(�@Q(@Q'=@Q&�@Q&�@Q&�@Q&�@Q&n@Q%�@Q%p@Q$�@Q"z@�Tr@�T6@�T�@�VB@�Z�@�Z0@�[o@�[B@�[l@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�[�@�\@�\ @�[�@�[�@�\@�\'@�\+@�\,@�\@�\j@�\e@�\U@�\+@�\+@�\g@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�\�@�]@�\�@�]@�]@�]@�]@�]@�]S@�]�@�\�@�[�@�[C@�\�@�[\@�Z�@�Y@�Xz@�Y@�X�@�Y
@�Y�@�Zs@�\z@�[�@�^
@�]�@�^b@�^�@�^^@�^�@�^�@�^_@�]�@�\�@�\�@�\U@�[�@�[�@�[�@�\@�\R@�\)@�]z@�]e@�\�@�\�@�]P@�]&@�\�@�]N@�]�@�^J@�^
@�\�@�\)@�[@�[E@�ZG@�Z�@�[~@�Zr@�Z�@�Z�@�[�@�[Y@�[�@�\Q@�\>@�\P@�]#@�]�@�]&@�]v@�]�@�^�@�^�@�_G@�[�@�[�@�\�@�\@�\@�\�@�]<@�\�@�]e@�^�@�]v@�]�@�\�@�^�@�_
@�_!@�_V@�_^@�^J@�^ @�^�@�^
@�^5@�]�@�]�@�]$@�\~@�^�@�_J@�^�@�_Z@�_G@�_G@�_@�_F@�_�@�_�@�_�@�_@�_5@�_@�`-@�`k@�`@�_�@�_�@�`X@�`l@�`B@�`@�_�@�`@�a@�a*@�a@�`�@�aB@�a)@�`�@�`k@�_�@�_�@�_�@�_K@�^�@�^�@�^@�Xh@�R�@�LX@�Jd@�F5@�?)@�=�@�8I@�0�@�#�@�#�@�"�@�!@�@��@��@�^@�*@��@��@��@�@��@��@�9@�
�@�	�@��@��@��@�@��@� @�
@�y@��@�e@��@�'@�X@�n@�
�@�^@��@��q@��|@��@���@�ߏ@��~@�޽@�ސ@��l@�ݮ@���@��@��@���@���@���@��@�؆@�֐@���@���@�з@�В@��@QP6@QP`@QP�@QP]@QP�@QP�@QP�@QP�@QP�@QP�@QP�@QP�@QQ@QP�@QQ@QQ*@QQ@QQ.@QP�@QP3@QO�@QM�@QM@QK@QJ�@QJ�@QJ�@QJ�@QK@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJ�@QJM@QI�@QH�@QH�@QHV@QH@QG�@QG�@QG�@QG�@QF3@QC�@QA�@Q@�@Q@M@Q?�@Q>�@Q>�@Q?(@Q?(@Q?+@Q?R@Q>�@Q?R@Q?R@Q?{@Q?�@Q?�@Q@�@Q@v@Q@�@Q@N@Q@R@Q?�@Q?z@Q?S@Q>�@Q>@Q=�@Q=5@Q=
@Q<�@Q<�@Q<�@Q<�@Q;�@Q;@Q:�@Q:@@Q9�@Q8@Q7x@Q6�@Q4�@Q3�@Q1�@Q1@Q0�@Q0B@Q0k@Q/H@Q.s@Q-�@Q-z@Q,�@Q++@Q*Z@Q)�@Q(�@Q(@Q'=@Q&�@Q&�@Q&�@Q&�@Q&n@Q%�@Q%p@Q$�@Q"zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      3434344434433444444443344344434444443444444434443444444444433443434344344344444334444444444444444444434443434433444444433443334433434344443443334443344433444444334444333443334444333443334444333443443333443343344343333433433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��9�e9��9�49�09��9��9��9��9�9�9��9��9��9�9�!9�%9�9�9��9�49��9�59�\9�F9��9�D9�X9�i9�l9�m9�Y9��9��9��9�l9�l9��9��9��9��9��9��9��9� 9��9��9��9��9��9�9�9�79�9�79�79�59�79�79�r9��9��9��9��9��9��9�C9��9�*9��9��9��9�Q9��9��9��9�9�9�a9��9�^9��9��9�^9��9��9��9��9��9�9��9�X9��9�j9��9��9�9�9�o9�J9��9�m9��9�L9�9��9�j9�g9��9��9��9��9��9��9�T9��9��9��9��9�}9��9�G9��9�J9��9��9��9��9�+9�9�D9�9��9�H9��9�]9��9��9��9��9��9�'9��9��9�
9�99�@9�L9�'9��9�9�99��9��9�H9��9��9�.9��9�<9�+9�+9��9�*9��9��9�q9�9�9�9��9�-9��9�b9�v9�9�.9�	9��9��9��9��9��9��9�h9��9��9�z9�-9��9�e9�e9�/9��9��9�9�9�A9�r9��9�9��9��9��9��9��9��9��9�C9|�9z�9x�9w�9u�9r�9q9q9pe9p.9pP9m�9m�9l�9j�9j9j9i]9j�9n�9l�9n%9p9r~9u�9u�9p�9o�9mZ9f�9f�9b�9\~9X#9N�9G\9Fk9F�9Fz9F[9E�9BN9B�9A�9Al9Ar9C;9B�9A%9?i9=9;49:?9:98�8��8���8��8���8��-8��/8��
8��)8��)8��N8��X8��Q8��s8��R8��w8��8��v8��8��P8��8��*8�߸8���8��?8���8���8��8��8��>8���8���8���8���8���8���8��8���8�� 8���8���8�܆8��8��^8���8���8�ڃ8��Y8��68��[8��8���8�ֽ8���8��F8�ӱ8��k8��B8��e8�Ү8�Ү8�ұ8���8�҉8���8���8���8��8��k8���8���8���8�Ӳ8�Ӷ8��A8���8���8�҈8�ѭ8��`8���8���8�Ы8�Ь8�Ы8��`8�ϥ8��8���8��Y8�Ϳ8��t8���8��N8�ɲ8��h8�Ʈ8��<8���8�ņ8�Ū8�ĩ8���8��}8��8��W8��8��N8���8���8��G8���8��F8��"8���8��"8���8��f8���8��d8��YG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BM�BM�BN�BN�BM�BM�BM�BP�B�B��BĜB��B��B�B�BB�B�B�RB��B�#B�NB�yB�B  B{B�B#�B33B?}BQ�B_;BiyBl�Bu�B{�B� B�{B��B�B�'B�?B�3B�FB�^B�qBBƨBÖB��B�wB�jB�XB��B��B��B��BɺBɺBȴBĜB�qB�XB�dB��B��B�?BÖB�wB��B��B��B��B��B��B��B�`B�5B�B��B��B��B�B��B�BhsBR�B/B�B��B�B�#B��B��BȴB�XB��B�oBy�Bl�BaHBW
BE�B49B)�B�B�BB
��B
�B
�HB
��B
�=B
v�B
p�B
hsB
VB
B�B
(�B
 �B
�B
B	�sB	��B	�?B	��B	�B	^5B	J�B	B�B	6FB	"�B	hB	B�B�B�
B��B��B��B��B��B��B�'B�9B�9B��B�jBÖB�wB�dB�!B�B��B��B��B��B��B�oB�\B�JB�=B�7B�=B�7B�7B�7B�7B�=B�DB�\B�bB�hB�hB�bB�\B�PB�7B�1B�+B�%B�%B�B�B}�B{�Bx�Bt�Br�Bp�Bo�Bn�Bn�Br�Bw�Bw�Bx�By�Bx�Bx�Bx�Bw�Bt�Bn�BiyBhsBgmBgmBhsBhsBhsBgmBffBffBhsBn�Bq�Bp�Bp�Bp�Bp�Bp�Bq�Bs�Bv�By�Bw�Bv�Bt�Bs�Bs�Bs�Bq�Bp�Bn�Bm�Bl�BiyBgmBgmBgmBdZB_;BZBZB\)B`BBaHBbNBdZBl�Bp�Bo�Bk�BaHBW
BQ�BL�B>wBA�BC�BG�BM�BP�BH�BD�BH�BF�BJ�BL�BQ�BW
B_;Bz�B�B�PB�bB�uB��B��B��B��B��B��B��B��B�B�B�!B�9B�?B�RB�XB�dB�}B�}B�}B��B��B��B��BBĜBȴBȴB��B��B��B�
B�B�5B�fB�B�B�B�B��B	B	PB	�B	�B	$�B	!�B	�B	�B	&�B	2-B	?}B	E�B	@�B	=qB	>wB	A�B	C�B	F�B	J�B	K�B	R�B	VB	W
B	[#B	bNB	cTB	ffB	hsB	k�B	jB	cTB	l�B	q�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	u�B	s�B	r�B	q�B	p�B	q�B	q�B	q�B	o�B	n�B	n�B	m�B	l�B	l�B	m�B	n�B	m�B	m�B	m�B	q�B	}�B	{�B	}�B	~�B	� B	�B	�B	�B	�%B	�+B	�bB	�{B	�uB	�hB	�{B	��B	�{B	�{B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�!B	�LB	�'B	�B	�'B	�XB	�^B	�}B	��B	�wB	�dB	�dB	�dB	�}B	B	ĜB	ĜB	ǮB	ǮB;dB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�B	�)B	�/B	�/B	�/B	�/B	�)B	�)B	�)B	�/B	�)B	�)B	�#B	�#B	�B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�;B	�TB	�TB	�ZB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�MB	��B
�B
�B
'B
)�B
49B
9	B
A B
JrB
NpB
TFB
YKB
]�B
d&B
ffB
k�B
oOB
t�B
y$B
|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S"A���B�?y�B�#>�A�^@p�	B��@K�0?���B�nA�-�>�bc?ǫ�?Ns�>��}>�Jf>�?�?y??S�A���B��@K+?�پB��@-�?r­@2�B��?Ax�@,%@ˈY?��?B7�?�h{B�_>�`�?iQ?I��AZ�$>�I�?�/@��{B�<>�5�?C�V?RA�B�8Ar~>�Y>��">ЖW?X�
>�>W?<(�A��?&ץA+��B��B�)@\�?eVB�I?}�B��A��B��?&��@���B� >��O?6�qB�?�@�R�?�?ew?I��A�CwB��>�L@>���>�in>�T�>�=�?�&?�P�?��n?G�"?�@�6�>݂�>��@�@�l}>��f>�n3@A0�? ��?F�zB��@��?,��?�B1B��?4�B��?8a�AG��B��B��?\>�U�?=�RAodAv�?#��@���B��A�	�?��?AC(B�eB�/B��?ZS?�p�B��B�W?5�A�{�?N(�B+@6��?�r?Z��@�:TB�s>��M?K	�B�,B	�B�)@�ޖ?v'(A �B�B��>��>�TP?+�pA�� A�@�u�>�
>�1?iھ@5�3A;�B�PA��,?`�M?M9�@r��?��B��B�QB�sAU�}?�7�B�+B�1B��A���?��@瀊@�QB��B�HB�?�]1?�vB�DB��BÂ?	"�?�P@��AeB�3B�dB��?�*�@ͣB�?E:�@0�B��B��B��B�<@�?,��A�/B�@I�B�]B��As��?E��A�� @��7B��B��B�%B�eAM�B��A�o�Ai�B��B��B��B��B�YB��B��B�gB�_B��B��B�-B��B��B��B��B�,B��B�aB�5B��B��B�8B��B�lB� B��B��B�RB�?B�\B�pB�"B��B�RB�AB��B�MB�:B�B��B��B�)B��B�%B�B�gB�qB��B�RB�B�<B�&B�gB��B��B��B��B��B�~B�fB��B�IB�SB�B�MB��B��B� B�B��B�B��B�!B�B�ZB��B��B�\B��B��B��B�#B�qB�!B�B��B��B��B��B�B�]B�BB��B�zB�B��B��B�hB��B��B��B��B��B��B�~B��B�wB��B�~B�B�B��B��B�B�-B�LB�|B��B�zB�KB�.B�-B��B�>B�B��B��B�SB��B��B�8B��B�bB��B��B��B��B�FB�;B�sB��B��B�LB�B�+A��(B��B�cB�{B�B�B�bB��B��B�;B��B��B��B�KB��B�+B��B��B��B��B��@�GzB�tB�(B��B��B�WB�SB��B�VB�B�vB�B�	B�B�"B��B�LB�B��B� B��B��B��B��B�B�8B�B��B��B��B�B�aB��B�&B��B��B��B�|B�B��B��B�}B��B�iB�.B�B�B��B�aB�bB��B��B��B�B��B��B�B�
B�
B�6B��B�B��B�zB��B�B�OB�)B�GB��B�IB�bB�IB�#B��B��B��B��B�B��B�]B��B��B��B�B��B�B��B��B�B��B��B��B��B��B�B��B��B��B�B�1B��B��B�'B��B�1B�+B��B��B�OB�B�GB�zB�B��B��B�gB��B�#B��B�OB�PB��B��B��B�
B� B�IB�<B��B�mB�1B�'B�mB�B��B�<B��B��B�+B��B��B�`B��B��B��B��B��B��B��B�4B�4B�,B�B�B��B�B�B�=B��B�5B�HB��B�rB�JB�uB��B�B�dB�dB�)B��B��B�yB��B��B��B��B�`B��B��B��B�B�gB�yB�yB�#B��B�MB�B�bB�vB�;B� B��B�
B��B�hB�GB�*B��B��B��B�uB��B��B��B��B��B��B��B�uB��B�nB�B�B��B��B�<B��B��B�\B�\B��B�.B��B�rB�NB��B��B�pB��B��B�~B�^B�^B�"B��B�B�B��B�dB��B�>B�>B�iB�B�B��B��B��B��B��B�:B��B��B��B��B�B��B�B�
B�mB��B��B�KB�$B� B��B�B��B�B�7B��B��B��B�fB��B��B��B�WB�OB�B�B�B�^B��B��B��B�=B�=B�B�$B�iB��B��B��B��B��B�$B�jB��B�B�B��B�VB�B� B�GB�YB�6B�{B�%B�B�dB�CB��B��B�tB��B��B�FB�fB��B��B�9B�B�	B�B�NBӂB�BݴBށB�KB�LB��B4B�B UB�BBDBBBrB8B�B�B	bB�B	�B	�B�BTB�B�B�B�B�B<BB%�B*_B0�B9B7�B6�B6�B6uB6PB53B4�B4�B7�B77B7sB81B7cB7=B7�B7"B7�B8kB90B:)B9HB9�B:#B:B8�B:cB;wB<B:�B>�B?�B>�B>�B>�B>B?�B?�B>iB>�B?aB?rB?9B>�B?B?B>�B>nB=�B>B?gB>�B>�B@�B@�B@�B@hB@zB@mB@@B@�B?�B@B?�B?�B?�B@�B?RB@B?�B>�B>tB?zB?�B?�B?�B?�B?GB>�B>�B?IB@�BAB@�B@BBBB�BBBA�BA�BA�BA�BA�BA�BA�BA�BBBA�BBBA�BA�BA6BA�BA�BA�BA�BAoBA$BAqBA�BA�BBBA�BA1BA�BAdBA�BA�B@�BA�BABA�BA]BBaBB�BCXBB�BBwBBBAYBA�BBOBA�BA�BBBBBAqBBBA�BB�BB]BBBBA�BAPBA$BAB@B? B=�B>6BNBN�BO&BM?BN=BM�BNBM�BM3BNCBN;BNBNBNBN"BN5BN-BNBN	BM�BM]BN�BNBMkBN"BM�BNBN-BMmBN0BMeBMIBNZBM�BN>BNBNBMvBN_BM�BNjBM�BM�BM|BN�BNRBM~BNIBNTBM�BM�BM�BM�BM�BN�BM�BM�BM�BM�BM�BNBM�BNzBMuBM�BNBBN�BNxBM�BNoBN@BM�BNCBNBO!BN7BN�BN�BNqBN�BNaBNBN�BOBOBN0BN�BN�BO�BN2BN�BNdBOiBM�BN�BO�BOBOBN�BNmBN�BN�BOBN�BO"BOYBN�BN�BN�BN�BN�BN�BO�BN�BNcBN�BN�BNBN�BN�BM�BN�BN0BN�BNBM�BNBBM�BNBN�BNBN BNuBM�BM�BN8BN^BM�BM�BNQBN�BM�BNcBNBNBOBN:BM�BM�BN)BNvBM�BM`BN�BOBM�BN<BM�BM�BM�BN�BNtBN1BM�BNBNBM�BN#BMdBNBMsBM�BNBM�BM�BMpBM�BMTBM%BM�BM�BQ!BO�BN�BM{BM�BN�BN8BQ�BS�BSQBT�BS�BT�BT�BT B]�Bh�Bp�Bn�By�BEB�B�B��B��B��B��B�RB��B�B��B�@B�B�B�mB�dB��B��B�B�gB��B�lB��B��B�<B�B��B�=B�JB�SB��B��BɦB֝B��B֜B��B��B�
B��B��B�_BԢB��B�hBѓB�cB�3B�FB��B�2B��B�`B�NB��B��B�B��BӱB�BB��BӛBӷB	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�jB	�|B	�B	�UB	�fB	�,B	�B	�B	��B	�XB	��B	�B	�B	�B	�B	�B	�TB	�JB	�=B	�/B	�"B	�B	�B	�B	��B	��B	�B	�BB	��B	�SB	��B	�B	�fB	�JB	�B	�#B	��B	��B	�,B	�B	�B	�B	�KB	�XB	�jB	�B	�B	�sB	�B	�,B	�mB	�`B	�dB	�vB	�B	�B	��B	��B	�B	�B	�B	��B	�B	�`B	�B	�`B	��B	��B	�B	�B	�zB	�@B	�B	�B	��B	�`B	��B	��B	�BB	�B	�~B	�mB	�B	�B	�/B	��B	��B	�B	�B	�B	��B	�B	��B	�cB	��B	�B	�B	��B	��B	�B	�[B	�mB	�B	��B	�BB	��B
XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444444441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993434344434433444444443344344434444443444444434443444444444433443434344344344444334444444444444444444434443434433444444433443334433434344443443334443344433444444334444333443334444333443334444333443443333443343344343333433433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  BM�BM�BN�BN�BM�BM�BM�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�dB��B�4B�bB�B�G�O�B�B�G�O�G�O�B?�BRB_NBi�Bl�Bu�B{�B�B��B��B�)B�7B�RB�GB�\B�rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�B��B��B��B��B��B�B�vB�JB�1B��B��B��B�!B��B�%Bh�BS	B/2B�B�B�B�7B�B��B��B�jB��B��By�Bl�Ba_BWBE�B4MB*B�B�B$B
��B
��B
�[B
��B
�SB
v�B
p�B
h�B
VB
B�B
)B
 �B
�B
$B	�B	��B	�VB	��B	�"B	^KB	J�B	B�B	6]B	"�B	B	B��B�B�"B��B�B��B��B��B�B�=B�OB�PB��B��BìB��B�{B�;B�B� B��B��B��B��B��B�sB�eB�VB�NB�VB�RB�PB�RB�RB�WB�]B�tB�{B��B��B�{B�vB�kB�SB�LB�CB�@B�>B�7B�&B~B|Bx�Bt�Br�Bp�Bo�Bn�Bn�Br�Bw�Bw�Bx�By�Bx�Bx�Bx�Bw�Bt�Bn�Bi�Bh�Bg�Bg�Bh�Bh�Bh�Bg�Bf�Bf|Bh�Bn�Bq�Bp�Bp�Bp�Bp�Bp�Bq�Bs�Bv�By�Bw�Bv�Bt�Bs�Bs�Bs�Bq�Bp�Bn�Bm�Bl�Bi�Bg�Bg�Bg�BdtB_WBZ6BZ8B\DB`]BacBbjBduBl�Bp�Bo�Bk�BadBW&BRBL�B>�BA�BC�BG�BM�BP�BH�BD�BH�BF�BJ�BL�BRBW$B_VBz�B�-B�lB�B��B��B��B��B��B��B��B�B�B�B�*B�?B�TB�ZB�nB�sB��B��B��B��B��B��B��B��BªBĺB��B��B��B��B��B�'B�9B�QB�B��B�B��B��B��B	;B	kB	�B	�B	$�B	!�B	�B	�B	'B	2KB	?�B	E�B	@�B	=�B	>�B	A�B	C�B	F�B	J�B	K�B	SB	VB	W'B	[BB	bkB	cpB	f�B	h�B	k�B	j�B	coB	l�B	q�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	u�B	s�B	r�B	q�B	p�B	q�B	q�B	q�B	o�B	n�B	n�B	m�B	l�B	l�B	m�B	n�B	m�B	m�B	m�B	q�B	~B	|B	~B	B	�B	�"B	�/B	�5B	�CB	�HB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�1B	�8B	�+B	�B	�=B	�jB	�DB	�&B	�EB	�tB	�}B	��B	��B	��B	�G�O�B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�4B	�:B	�8B	�3B	�6B	�=B	�:B	�GB	�MB	�NB	�JB	�KB	�EB	�CB	�GB	�OB	�HB	�EB	�AB	�@B	�3B	�-B	�(B	�'B	�(B	�(B	�(B	�4B	�5B	�:B	�?B	�ZB	�pB	�pB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	��G�O�B	�jB	�
B
�B
	B
'"B
*B
4XB
9%B
A:B
J�B
N�B
TeB
YjB
^B
dBB
f�B
k�B
omB
t�B
yAB
|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S>G�O�B�$G�O�B�5G�O�G�O�G�O�B�G�O�G�O�B��A�-�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���B��G�O�G�O�B�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�B�qG�O�G�O�G�O�G�O�G�O�G�O�G�O�B�NG�O�G�O�G�O�B�JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�=G�O�G�O�B�[G�O�B��G�O�B��G�O�G�O�B�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�A�C�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B�G�O�B��G�O�G�O�B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��A�	�G�O�G�O�B�|B�FB��G�O�G�O�B��B�hG�O�A�|G�O�B+G�O�G�O�G�O�G�O�BʆG�O�G�O�B�?B	�B�;G�O�G�O�G�O�B�B�G�O�G�O�G�O�A��8A�1G�O�G�O�G�O�G�O�G�O�G�O�B�aA��EG�O�G�O�G�O�G�O�B��B�fB��G�O�G�O�B�@B�FB��G�O�G�O�G�O�G�O�B��B�[B�+G�O�G�O�B�WB��BÔG�O�G�O�G�O�G�O�B�KB�sB�G�O�G�O�B�G�O�G�O�B��B��B�B�NG�O�G�O�A�LB�G�O�B�mB��G�O�G�O�A��G�O�B��B�B�6B�|G�O�B��A�o�G�O�B�
B��B��B��B�mB��B��B�yB�sB��B�B�BB��B��B��B��B�>B��B�uB�HB�B��B�LB��B�~B�3B��B��B�cB�PB�oB��B�7B��B�fB�QB��B�_B�PB�'B�B�B�8B��B�8B�B�|B��B�B�fB�B�NB�6B�|B��B�B�B��B��B��B�wB��B�_B�fB�2B�_B� B��B�4B� B��B� B��B�4B�B�nB��B��B�qB��B��B��B�5B��B�4B�%B��B��B��B��B�%B�nB�TB��B��B�.B��B��B�|B��B�B��B��B��B��B��B��B��B��B��B�B�B��B��B�%B�=B�`B��B��B��B�_B�>B�=B��B�QB�-B��B��B�eB��B��B�MB��B�tB��B��B��B��B�XB�PB��B��B�B�_B�B�<G�O�B��B�vB��B�,B��B�tB��B��B�MB��B��B��B�_B��B�>B�B��B��B��B��G�O�B��B�;B��B�B�jB�hB��B�hB�.B��B�%B�B�+B�2B��B�_B�+B��B�3B�B��B��B��B�B�MB�B��B�B��B�0B�uB��B�9B��B��B��B��B�"B��B��B��B��B�~B�?B�0B��B��B�uB�tB��B��B��B��B��B��B�%B�B�B�HB��B�B��B��B��B�B�_B�>B�^B��B�]B�tB�[B�8B��B��B��B� B��B��B�oB��B��B��B�.B��B�#B��B��B�B��B��B��B��B��B�B��B��B��B�B�BB��B�B�;B��B�DB�?B��B�B�cB�B�ZB��B�B�B�B�zB��B�7B��B�aB�cB��B��B��B�B�B�[B�PB��BNBN�BO8BMOBNPBM�BNBM�BMDBNWBNPBNBNBNBN6BNFBNBBN"BNBNBMoBN�BN2BMBN6BM�BN)BN>BM}BNDBMzBM\BNmBM�BNRBN BN BM�BNoBM�BN{BM�BM�BM�BN�BNbBM�BN\BNfBM�BM�BM�BM�BM�BN�BM�BM�BM�BM�BM�BN'BNBN�BM�BM�BNYBN�BN�BM�BN�BNTBM�BNUBN BO4BNKBO	BN�BN�BN�BNsBN�BN�BO.BO-BNEBN�BN�BO�BNCBN�BNvBO{BM�BN�BO�BO!BOBN�BN�BN�BN�BOBN�BO4BOlBOBN�BN�BN�BN�BOBO�BN�BNuBN�BN�BN+BN�BN�BM�BN�BNDBN�BNBM�BNWBM�BNBN�BN"BNBN�BNBM�BNKBNoBM�BM�BNeBN�BM�BNuBN BN-BO#BNOBNBM�BN;BN�BM�BMuBN�BO&BM�BNPBM�BNBM�BN�BN�BNDBM�BNBN+BM�BN3BMyBN"BM�BM�BN+BM�BM�BM�BM�BMeBM8BM�BM�BQ4BO�BN�BM�BNBN�BNMBQ�BS�BScBT�BS�BT�BUBT3B]�Bh�Bp�Bn�BzBWB�B� B��B��B��B��B�fB��B�0B�B�RB�-B� B�~B�wB�B��B�B�yB��B�B��B��B�OB��B��B�NB�\B�eB�	B��BɺB֮B� B֮B��B��B�B�B��B�qBԶB��B�zBѤB�tB�HB�ZB��B�CB��B�qB�dB�B��B�-B��B��B�SB��BӭB��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�rB	�B	�HB	�B	�<B	�B	�uB	�	B	��B	�B	�B	�B	�B	�qB	�gB	�[B	�KB	�AB	�&B	�7B	��B	�B	��B	�B	�_B	�B	�qB	�B	��B	�B	�fB	�;B	�?B	��B	�B	�HB	�B	�5B	�B	�kB	�vB	�B	�B	�B	�B	�B	�IB	�B	�|B	�B	�B	�B	�!B	��B	��B	�B	�B	�9B	��B	��B	�|B	��B	�|B	�B	��B	�B	�B	�B	�^B	�B	�0B	�B	�~B	��B	��B	�`B	��B	�B	�B	�-B	��B	�JB	�B	�B	�3B	�B	�%B	��B	�&B	�B	�B	��B	�4B	��B	�B	��B	�B	�wB	�B	�3B	��B	�`B	��B
tBNBN�BO8BMOBNPBM�BNBM�BMDBNWBNPBNBNBNBN6BNFBNBBN"BNBNBMoBN�BN2BMBN6BM�BN)BN>BM}BNDBMzBM\BNmBM�BNRBN BN BM�BNoBM�BN{BM�BM�BM�BN�BNbBM�BN\BNfBM�BM�BM�BM�BM�BN�BM�BM�BM�BM�BM�BN'BNBN�BM�BM�BNYBN�BN�BM�BN�BNTBM�BNUBN BO4BNKBO	BN�BN�BN�BNsBN�BN�BO.BO-BNEBN�BN�BO�BNCBN�BNvBO{BM�BN�BO�BO!BOBN�BN�BN�BN�BOBN�BO4BOlBOBN�BN�BN�BN�BOBO�BN�BNuBN�BN�BN+BN�BN�BM�BN�BNDBN�BNBM�BNWBM�BNBN�BN"BNBN�BNBM�BNKBNoBM�BM�BNeBN�BM�BNuBN BN-BO#BNOBNBM�BN;BN�BM�BMuBN�BO&BM�BNPBM�BNBM�BN�BN�BNDBM�BNBN+BM�BN3BMyBN"BM�BM�BN+BM�BM�BM�BM�BMeBM8BM�BM�BQ4BO�BN�BM�BNBN�BNMBQ�BS�BScBT�BS�BT�BUBT3B]�Bh�Bp�Bn�BzBWB�B� B��B��B��B��B�fB��B�0B�B�RB�-B� B�~B�wB�B��B�B�yB��B�B��B��B�OB��B��B�NB�\B�eB�	B��BɺB֮B� B֮B��B��B�B�B��B�qBԶB��B�zBѤB�tB�HB�ZB��B�CB��B�qB�dB�B��B�-B��B��B�SB��BӭB��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�rB	�B	�HB	�B	�<B	�B	�uB	�	B	��B	�B	�B	�B	�B	�qB	�gB	�[B	�KB	�AB	�&B	�7B	��B	�B	��B	�B	�_B	�B	�qB	�B	��B	�B	�fB	�;B	�?B	��B	�B	�HB	�B	�5B	�B	�kB	�vB	�B	�B	�B	�B	�B	�IB	�B	�|B	�B	�B	�B	�!B	��B	��B	�B	�B	�9B	��B	��B	�|B	��B	�|B	�B	��B	�B	�B	�B	�^B	�B	�0B	�B	�~B	��B	��B	�`B	��B	�B	�B	�-B	��B	�JB	�B	�B	�3B	�B	�%B	��B	�&B	�B	�B	��B	�4B	��B	�B	��B	�B	�wB	�B	�3B	��B	�`B	��B
tG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444444441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993434344434433444444443344344434444443444444434443444444444433443434344344344444334444444444444444444434443434433444444433443334433434344443443334443344433444444334444333443334444333443334444333443443333443343344343333433433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333433333333333333333333433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.22 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.22 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.22 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011535362020090115353620200901153536202009011535362020090115353620200901153536202009011535362020090115353620200901153536202009011535362020090115353620200901153536AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202121122018112021211220181120212112    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121122018112021211220181120212112  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202121122018112021211220181120212112  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011535362020090115353620200901153536  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                