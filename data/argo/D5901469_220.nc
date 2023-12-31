CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T21:22:03Z creation      
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
resolution        =���   axis      Z        +�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  p�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     +�  {�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     +�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     +�  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� 
�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     +� �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� A�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     +� L�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     +� x�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �t   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     +� �p   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �\   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     +� �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     +� D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� >0   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     +� I,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� u   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     +� �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   р   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ь   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ј   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ҄   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 Ґ   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20181120212203  20200901153815  5901469 5901469 5901469 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  2688                            2688                            2688                            2C  2B  2C  DAD APEX                            APEX                            APEX                            2730                            2730                            2730                            112607                          112607                          112607                          846 846 846 @�X̺g��@�X̺g��@�X̺g��111 @�X�WA@�X�WA@�X�WA@6R� ě�@6R� ě�@6R� ě��dOI�^�dOI�^�dOI�^111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @@  @y��@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�33A���A�  B   B  B  B  B ffB(  B0  B8ffB?��BH  BP  BX  B_��Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dty�Dy��D���D�3�D���D�� D���D�>fD�p�D�� D��D�6D��=D��RD�\D�:�D��D�D��D�-D�
D��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    =���                =���=���    =���>L��        =���    =���        =���                        >L��            =���                    >L��    =���                        >L��>L��>���    =���>L��    =���            =���    =���        =���                    =���>L��>L��    =���        =���=���    =���    =���>L��        >L��=���    =���            =���=���        >L��=���        =���>L��        =���    =���        =���>���=���        >L��=���        =���>���>L��                =���                                =���=���>L��        =���    =���=���    =���=���        =���                    =���>L��        =���=���            =���=���=���        >L��>���>���        >L��>���>���=���    >L��=���        =���=���=���            =���>���=���        >L��=���>���>L��>���        =���=���>L��>���>���>���>L��=���=���=���    =���=���=���=���=���=���>L��>L��=���    =���>���>L��>L��=���=���>���>���>L��>���>���>L��>���=���>���>L��>���>L��>L��>L��>���>���>���>L��>���>���>���>���>���>���>���>L��>L��>L��>���>���>���>L��>���>���=���>���>���>L��>���>���>���>���>L��>���>���>L��>L��>���>���>���>L��?   >���>���>L��>���=���>L��>���>���>���>L��>���>���>L��>L��>L��>L��>L��>L��>L��>���>���>L��>L��>���>���>���=���>���>���>���>L��>���>���>���>���>L��>���>L��>L��>���>���>L��>���>���>���>L��>���>���>���=���>���>���>���>L��>L��>���>L��>L��>���>���=���>L��>���>L��>���>���>���>���>L��>L��>L��>���>L��>L��>L��>L��>���>���>���>���>���>���>L��>L��>���>���>���>L��>���>���>���>���>L��>L��>L��>���>���>���>L��>L��>���>���>L��>L��>���>���>���>���>���>���>L��>L��>L��>���>L��>���>L��>L��>���>���>���>L��>���>���>���>L��>���>���>���>���>L��>���>���=���>L��>���>L��>L��>���>���>���>L��>���>���>���>L��>���>���>L��>L��>���>���>���>���>L��>���>���>L��>���>���>L��>L��>���>���>���>L��>L��>L��>���>L��>���>L��>L��=���>���>L��>���>L��=���>���>���>L��>���>���=���>���>���>���>���>L��>���>L��>���>L��>L��>���>L��>���>���>���>L��=���>���?   >���>L��>���?   ?   ?   ?��?333?L��?fff?fff?fff?�  ?���?���?�ff?�ff?�ff?�33?�  ?�  ?ٙ�?ٙ�?ٙ�?ٙ�?�ff?�33?�33@   @ff@��@ff@ff@��@33@��@��@��@   @   @&ff@&ff@,��@,��@,��@333@9��@@  @Fff@Fff@L��@S33@Y��@`  @`  @l��@s33@s33@y��@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@���@�  @�33@�ff@ə�@�  @�33@�ff@ٙ�@�  @�33@�ff@陚@�  @�33@�ff@���A   A��A33A��AffA  A33A��AffA  A33A��AffA��A33A��AffA!��A#33A$��A&ffA(  A+33A,��A.ffA0  A333A4��A6ffA9��A;33A<��A>ffA@  AA��AD��AFffAH  AI��AL��ANffAP  AQ��AS33AT��AX  AY��AY��A\��A^ffA`  A`  Aa��Ac33Ad��AfffAfffAh  Ai��Ak33Al��Al��AnffAp  Aq��Aq��As33As33At��AvffAvffAx  Ax  Ay��A{33A|��A|��A|��A~ffA�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�  A���A���Ař�A�ffA�33A�33A�  A���Aə�A�ffA�33A�33A�  A���A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  A���Aݙ�DpffDps3Dpy�Dp�fDp��Dp�3Dp� Dp�fDp�3Dp��Dp�fDp��Dp�3Dp� Dp�fDp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�DqFfDqL�DqS3Dq` DqffDqs3Dqy�Dq�fDq��Dq��Dq� Dq�fDq�3Dq��Dq�fDq��DqٚDq� Dq��Dq�3Dr  DrfDr�Dr�Dr  Dr,�Dr33Dr@ DrFfDrS3DrY�DrffDrl�Drs3Dr� Dr�fDr�3Dr��Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3Ds  DsfDs3Ds�Ds  Ds,�Ds9�Ds@ DsL�DsS3Ds` DsffDss3Dsy�Ds�fDs��Ds��Ds� Ds��Ds�3Ds� Ds�fDs�3DsٚDs�fDs��Ds��Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3@9��@@  @Fff@Fff@L��@S33@Y��@`  @`  @l��@s33@s33@y��@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�33@�ff@���@�  @�33@�ff@ə�@�  @�33@�ff@ٙ�@�  @�33@�ff@陚@�  @�33@�ff@���A   A��A33A��AffA  A33A��AffA  A33A��AffA��A33A��AffA!��A#33A$��A&ffA(  A+33A,��A.ffA0  A333A4��A6ffA9��A;33A<��A>ffA@  AA��AD��AFffAH  AI��AL��ANffAP  AQ��AS33AT��AX  AY��AY��A\��A^ffA`  A`  Aa��Ac33Ad��AfffAfffAh  Ai��Ak33Al��Al��AnffAp  Aq��Aq��As33As33At��AvffAvffAx  Ax  Ay��A{33A|��A|��A|��A~ffA�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�  A���A���Ař�A�ffA�33A�33A�  A���Aə�A�ffA�33A�33A�  A���A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  A���Aݙ�DpffDps3Dpy�Dp�fDp��Dp�3Dp� Dp�fDp�3Dp��Dp�fDp��Dp�3Dp� Dp�fDp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�DqFfDqL�DqS3Dq` DqffDqs3Dqy�Dq�fDq��Dq��Dq� Dq�fDq�3Dq��Dq�fDq��DqٚDq� Dq��Dq�3Dr  DrfDr�Dr�Dr  Dr,�Dr33Dr@ DrFfDrS3DrY�DrffDrl�Drs3Dr� Dr�fDr�3Dr��Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3Ds  DsfDs3Ds�Ds  Ds,�Ds9�Ds@ DsL�DsS3Ds` DsffDss3Dsy�Ds�fDs��Ds��Ds� Ds��Ds�3Ds� Ds�fDs�3DsٚDs�fDs��Ds��Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3Dt` DtffDtl�Dty�Dt� Dt�fDt�3Dt��Dt� Dt�fDt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @333@l��@���@���A��A<��A^fgA|��A�ffA�ffA�ffA�ffA͙�A�33A�ffA�ffB33B33B33B��B'33B/33B7��B>��BG33BO33BW33B^��Bg33Bo33Bw��B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI�3CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3D s3D ��D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%y�D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dpl�Dp�3Dqs3Dq�3Drs3Dr��Dss3Ds�3Dtl�Dy�D��4D�-qD�{�D���D��gD�8 D�j>D���D���D�/�D���Dǹ�D��D�4{D�yHD�)D�RD�&�D�x�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�ͽ��;L�;L�;L�;L�ͽ��ͽ��;L�ͽ���1L�;L�;L�ͽ��;L�ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�;L��1L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L��1L�;L�ͽ��;L�;L�;L�;L�;L�;L��1L��1L��=��ξL�ͽ���1L�;L�ͽ��;L�;L�;L�ͽ��;L�ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ���1L��1L�;L�ͽ��;L�;L�ͽ��ͽ��;L�ͽ��;L�ͽ���1L�;L�;L��1L�ͽ��;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�;L��1L�ͽ��;L�;L�ͽ���1L�;L�;L�ͽ��;L�ͽ��;L�;L�ͽ���=��ν��;L�;L��1L�ͽ��;L�;L�ͽ���=���1L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�ͽ��ͽ���1L�;L�;L�ͽ��;L�ͽ��ͽ��;L�ͽ��ͽ��;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ���1L�;L�;L�ͽ��ͽ��;L�;L�;L�ͽ��ͽ��ͽ��;L�;L��1L��=���=��ξL�;L��1L��=���>L�ͽ��;L��1L�ͽ��;L�;L�ͽ��ͽ��ͽ��;L�;L�;L�ͽ���=��ν��;L�;L��1L�ͽ���=���1L��=��ξL�;L�ͽ��ͽ���1L��>L��>L��>L��1L�ͽ��ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���1L��1L�ͽ��;L�ͽ���=���1L��1L�ͽ��ͽ���=���=���1L��=���=���1L��=��ν���=���1L��=���1L��1L��1L��=���=���>L��1L��=���=���>L��=���>L��=���=���1L��1L��1L��=���=���>L��1L��=���=��ν���>L��=���1L��=���=���=���=���1L��=���=���1L��1L��>L��=���=���1L��>���>L��=���1L��=��ν���1L��=���=���=���1L��=���=���1L��1L��1L��1L��1L��1L��1L��>L��=���1L��1L��=���>L��=��ν���>L��=���=���1L��>L��>L��=���=���1L��=���1L��1L��=���=���1L��=���>L��=���1L��=���>L��>L�ͽ���=���=���=���1L��1L��=���1L��1L��=���=��ν���1L��=���1L��=���=���>L��=���1L��1L��1L��=���1L��1L��1L��1L��=���=���=���>L��=���>L��1L��1L��=���=���=���1L��=���=���=���=���1L��1L��1L��=���=���=���1L��1L��=���=���1L��1L��=���=���>L��=���>L��>L��1L��1L��1L��=���1L��=���1L��1L��>L��=���=���1L��>L��=���>L��1L��>L��=���=���=���1L��>L��=��ν���1L��=���1L��1L��=���=���=���1L��=���=���>L��1L��>L��=���1L��1L��=���=���=���=���1L��=���=���1L��>L��>L��1L��1L��=���=���>L��1L��1L��1L��=���1L��=���1L��1L�ͽ���=���1L��=���1L�ͽ���=���=���1L��=���=��ν���=���>L��=���=���1L��=���1L��=���1L��1L��=���1L��>L��>L��=���1L�ͽ���=���>���=���1L��=���>���>���>���>���?   ?��?333?333?333?L��?ffg?�  ?���?���?���?���?�ff?�ff?�  ?�  ?�  ?�  ?���?ٙ�?ٙ�?�ff?�32@   ?�32?�32@   @ff@��@��@��@33@33@��@��@   @   @   @&ff@,��@333@9��@9��@@  @Fff@L��@S33@S33@`  @fff@fff@l��@s33@y��@��@�fg@�fg@���@���@�  @�34@�fg@���@���@�  @�34@�fg@���@�  @�34@���@���@�  @�34@ə�@���@�  @�34@ٙ�@���@�  @�34@陚@���@�  @�34@���@���A   A��A33A��A  A	��A33A��A  A��A33AfgA  A��A33AfgA   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A6fgA8  A9��A;33A<��A>fgAA��AC33AD��AFfgAI��AK33AL��ANfgAP  AQ��AT��AVfgAVfgAY��A[33A\��A\��A^fgA`  Aa��Ac33Ac33Ad��AffgAh  Ai��Ai��Ak33Al��AnfgAnfgAp  Ap  Aq��As33As33At��At��AvfgAx  Ay��Ay��Ay��A{33A|��A|��A~fgA~fgA�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�33A�33A�  A���Ař�Ař�A�ffA�33A�  A���Aə�Aə�A�ffA�33A�  A���A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  DpY�DpffDpl�Dpy�Dp� Dp�fDp�3Dp��Dp�fDp��Dp��Dp� Dp�fDp�3DpٙDp�fDp��Dp��Dq  DqfDq3Dq�Dq&fDq,�Dq9�Dq@ DqFfDqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr33Dr9�DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3DrٙDr�fDr�3Dr��DsfDs�Ds3Ds  Ds,�Ds33Ds@ DsFfDsS3DsY�DsffDsl�Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds�fDs��DsٙDs� Ds��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt�f@,��@333@9��@9��@@  @Fff@L��@S33@S33@`  @fff@fff@l��@s33@y��@��@�fg@�fg@���@���@�  @�34@�fg@���@���@�  @�34@�fg@���@�  @�34@���@���@�  @�34@ə�@���@�  @�34@ٙ�@���@�  @�34@陚@���@�  @�34@���@���A   A��A33A��A  A	��A33A��A  A��A33AfgA  A��A33AfgA   A!��A#33A$��A(  A)��A+33A,��A0  A1��A333A6fgA8  A9��A;33A<��A>fgAA��AC33AD��AFfgAI��AK33AL��ANfgAP  AQ��AT��AVfgAVfgAY��A[33A\��A\��A^fgA`  Aa��Ac33Ac33Ad��AffgAh  Ai��Ai��Ak33Al��AnfgAnfgAp  Ap  Aq��As33As33At��At��AvfgAx  Ay��Ay��Ay��A{33A|��A|��A~fgA~fgA�  A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A���A�ffA�33A�  A�  A���A���A���A�ffA�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A���A�ffA�33A�33A�  A�  A���A���A���A�ffA�33A�33A�  A���A���A���A�ffA�ffA�33A�  A���A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�ffA�33A�  A�  A���A���A�ffA�33A�33A�  A���Ař�Ař�A�ffA�33A�  A���Aə�Aə�A�ffA�33A�  A���A͙�A�ffA�33A�  A���Aљ�A�ffA�33A�  A���Aՙ�A�ffA�  A���Aٙ�A�ffA�33A�  DpY�DpffDpl�Dpy�Dp� Dp�fDp�3Dp��Dp�fDp��Dp��Dp� Dp�fDp�3DpٙDp�fDp��Dp��Dq  DqfDq3Dq�Dq&fDq,�Dq9�Dq@ DqFfDqS3DqY�DqffDql�Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr33Dr9�DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr�3DrٙDr�fDr�3Dr��DsfDs�Ds3Ds  Ds,�Ds33Ds@ DsFfDsS3DsY�DsffDsl�Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds�fDs��DsٙDs� Ds��Ds�3Ds��DtfDt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�AׁA׋DA׉7AׅA׉7A׋DA׍PA׏\A׉7AדuAח�Aן�Aף�Aף�Aף�Aף�Aף�Aץ�Aץ�Aק�Aס�A�x�A�A���A�\)AЬA��`A�5?A���AƅA�(�AŶFA�=qA�`BA��+A��A�K�A�S�A��mA���A�&�A�{A�|�A�?}A���A�A��A�E�A�Q�A���A�1'A��;A�?}A���A�
=A�VA���A�`BA�"�A�VA�A��A���A�p�A��A���A���A�K�A�
=A�M�A���A���A�l�A��;A��DA��^A��7A�^5A���A��+A�A�A�^5A���A��HA�bNA�ƨA��A���A���A�"�A�jA�1'A�ƨA�  A�"�A��yA�  A�+A�oA�33A��/A�33A��A�%AA{�Az �AxA�AuXAo�Ao%Am�Am"�Al�uAl  Ak�Aj�+Ai��AhI�Ae�-Aa�#A^�/A]l�A[��AXZAU|�ASO�AR�AQ/AOO�ANffAK�AHz�AG�AGx�AGVAF�DAF9XAE��AE�7ADM�ACO�AB-AA�TAAC�A?p�A>bNA<��A;�#A;�hA;�A:bNA6�A5;dA4�!A4ZA37LA2��A2I�A0�!A.z�A-�PA-33A,��A,��A,I�A+�mA+dZA)?}A(ZA&�\A%��A%�7A$I�A"�uA!oA �A M�A A\)A�uAdZA�A��A�-A��A��A�uA��AM�A�A�;A��AdZA�9AI�A�-A��A�TA�jA��A�\Az�A1'A  A�A�;A��At�AK�AC�A7LAoA
��A
��A
�A��AjA�A%A(�A�
A��A��A�AS�A�A�7A �`A ~�A ffA   @��@�x�@���@�\)@�ff@���@��\@��^@�(�@�M�@�7L@���@�^5@�G�@�@�+@�9@�\)@�
=@��H@��@�ff@�@�b@�K�@�R@�5?@��@��/@�I�@���@�^5@�X@ץ�@�E�@��T@ԓu@��@�l�@�E�@�1'@�K�@̣�@�bN@�1'@�|�@�n�@�J@��@ǍP@Ƨ�@�=q@ř�@�V@�S�@�M�@���@���@�j@��@�|�@�~�@�@��@��@�1'@�1@��w@��+@��^@��7@�O�@���@���@�7L@���@���@�t�@���@��@���@�@��@���@���@�(�@� �@�bN@���@���@�I�@��w@�~�@��#@���@���@�v�@�{@�O�@���@��^@�/@�r�@��P@�+@�
=@��@���@��D@��@��@���@��
@��m@���@�C�@��@�@���@�5?@�@��7@�Ĝ@��F@�33@��H@�M�@�x�@��`@�r�@�(�@��@��P@�S�@���@��R@�5?@��-@���@�&�@���@��@�1@�t�@�33@���@�~�@�-@��@��7@�&�@�Ĝ@��@�A�@��@�C�@�"�@�
=@�~�@�n�@�ff@�^5@�ff@�V@�5?@�E�@�E�@�=q@�E�@�E�@�v�@�ȴ@���@��!@���@��H@���@��y@�ȴ@���@��R@��\@�v�@�n�@�n�@�-@�@�x�@�G�@�%@�?}@�b@�1@�9X@� �@���@���@�t�@�\)@�"�@�
=@��@���@�ȴ@�ȴ@��\@��@���@�p�@��D@�K�@�
=@��!@�^5@�=q@�-@��#@��-@�x�@�7L@�%@��`@��u@�A�@��@�\)@�+@���@��R@��+@�=q@��@�hs@�%@��@�j@�9X@� �@�b@���@��
@�;d@�
=@��H@���@�-@��#@�@�7L@�%@��D@�9X@�A�@� �@��;@���@�=q@{�4@q�@g��@XbN@Ru%@L]d@D��@B��@9@@4G@/\)@+��@&�@#�Q@=q@#:@��@�@�8@
ںG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��#A��uA�VA�hsAˮA�9XA���A�=qA�+A���A�~�A��yA�%A�A��#A�VA�A�(�A�XA���A��TA�  A�hsAѼjA�x�A���A��;A�ȴA���A���AhA�oA��A���AՁA�;dAÓuA�"�A�bA�|�A�K�A���A�XA�G�A�M�A�~�A���A�E�A�1'A���A�ȴAĥ�A���A�~�AǗ�A���A�ZA�33AȅA�VA�A��A��Aŧ�A���A�=qA�G�A��Aҟ�A�v�A��/A���AռjA�&�A�jA˧�A�7LA�O�A��A���A�G�A�33A���A�^5A�A�A�ȴA�p�AӸRA���A�bNA�VA�Q�A�bA��RAÉ7A�G�A�O�A�
=Aϴ9A�-A�|�A�r�A���A��A�33A�O�A���AȬA�Q�A֙�A�7LA�9XA��A�ZA�33A�C�AѮA��#A�p�A��A�G�A��wA�9XA�l�A��A�  A��A�A��A��HA��#A�I�A�5?Aˡ�A�33A�~�A΍PA�Ać+A�A���A�7LA��Aԛ�A��/A��+A�ƨA�^5A��A԰!A�A�A�oA\A���A�bNA�jAϡ�A�VA��;A�9XA՟�A�VA̧�A��A�1'A�33Aղ-A��TA�v�A�"�A�(�A�{A��A���A�/Aѥ�A��Aէ�A�Q�Aҕ�A�%AļjA���A�dZA�-A�&�AƉ7Aȝ�A��A�7LA�7LA�(�A�1'AʋDA���A�A�A���A�33A�1'A�;dA�;dA�+A�O�A�+A�p�A�`BA� �A��`A�-A�1'A�"�A��A�/A�5?A�(�A�-A�O�Aև+A� �A�-A��HA��A�5?A�=qA�;dA�?}A�E�A�=qA�;dA�=qA�9XA�E�A�I�A�G�A�;dA�A�A�5?A�I�A�K�A�K�A�E�A�K�A�M�A�K�A�I�A�M�A�I�A�
=A�E�A�E�A�C�A�M�A�O�A�M�A�E�A�E�A�=qA�E�A�M�A�E�A�A�A�I�A�K�A�G�A�E�A�E�A�C�A�E�A�bA�M�A�C�A�?}A�C�A�A�A�I�A�C�A�9XA�5?A�?}A�?}A�;dA�;dA�K�A�7LA�7LA�I�A�C�A�/A�E�A�?}A��A�7LA�C�A�K�A�M�A�A�A�E�A�I�A�Q�A�O�A�"�A�I�A�I�A�S�A�Q�A�O�A�7LA�M�A�Q�A�C�A�=qA�9XA�E�A�O�A�K�A�I�A�A�A�C�A�E�A�E�A�VA�VA�XA�A�A�{A�+A�^5A�\)A�^5A�XA�\)A�ZA�ZA�\)A�ZA�ZA�`BA�O�A�VA�`BA�bNA�dZA�\)A�Q�A�S�A�S�A�K�A�M�A�bNA�\)A�^5A�S�A�M�A�VA�ffA�jA�dZA�bNA�VA�\)A�`BA�`BA�^5A�hsA�p�A�jA�n�A�ffA�ffA�bNA�bNA�VA�\)A�dZA�VA�Q�A�I�A�S�A�O�A�M�A�VA�ZA�XA�`BA�XA�O�A�S�A�VA�XA�\)A�\)A�XA�XA�S�A�Q�A�S�A�S�A�G�A�I�A�E�A�I�A�M�A�XA�\)A�O�A�VA�Q�A�S�AӓuA�`BA�dZA�{A�O�A�ffA�dZA�bNA�ZA�VA�dZA�bNA�jA�hsA�bNA�ffA�ffA�hsA�hsA�^5A�ZA�VA�bNA�^5A�jA�n�A�ZA�jA�ffA�jA�l�A�l�A�`BA�^5A�bNA�l�A�jA�t�A�jA�XA�bNA�x�A�r�A�dZA�^5A�hsA�r�A�z�A�r�A�x�A�n�A�hsA�t�A�x�A�r�A�r�A�ffA�p�A�x�A�l�A�jA�t�A�t�A�n�A�|�A�t�A�hsA�ĜA�bA�v�A�t�A�v�A�t�A�t�A�t�A�x�A�r�A�r�A�n�A�p�A�|�A�p�A�n�A�jA�p�A�v�A�l�A�n�A�n�A�n�A�z�A�l�A�jA�l�A�l�A�l�A�n�A�p�A�l�A�n�A�l�A�l�A�l�A�n�A�n�A�r�A�p�A�r�A�t�A�r�A�t�A�v�A�v�A�t�A�r�A�t�A�v�A�|�A�|�A�~�A�z�A�z�A�x�A�z�A�z�A�|�A�~�A�~�A�|�AׅAׁAׁA�|�A�|�AׁA�z�A�~�AׁA׃A׉7A׉7A׉7A׃AׅAׅA׉7AׅA׉7Aׇ+Aׇ+AׅAׅA׃A׉7Aׇ+AׅAׇ+AׅAׅAׅAׇ+AׅAׇ+A׉7Aׇ+Aׇ+Aׇ+Aׇ+Aׇ+A׉7AׅA׃AׁAׁAׁAׁAׁAׁA׃A׃A׃A׃A׃AׅA׃AׅAׅA׋DA׍PA׉7Aׇ+AׅA�~�A׃A׃Aׇ+AׁAׇ+Aׇ+Aׇ+AׅA׋DA׏\A׍PAבhAבhA׏\A׍PA׋DA׋DAבhAדuA׏\A׋DA׉7AׅAׅAׇ+A׉7Aׇ+Aׇ+Aׇ+AׅAׅAׅAׇ+Aׇ+AׅA׉7Aׇ+A׃AׅAׅA׃A׃A׃A׏\A׋DA׉7A׉7A׉7A׋DA׏\A׏\A׍PA׉7AׅAׇ+A׉7AׅAׇ+Aׇ+A׋DAבhA׏\AבhA׍PA׏\AבhA׏\AדuAדuAבhAבhA׍PA׋DA׍PA׏\A׍PAבhA׏\A׉7A׉7Aׇ+A׉7A׉7A׉7A׉7A׋DA׉7A׉7A׋DA׍PA׍PA׏\AבhA׍PA׍PAׇ+A׉7A׉7AׅAׇ+A׃A׉7A׍PA׋DAׁAׅAׅA׃A�~�A�~�AׁAׅA׉7A׃A׉7Aׇ+A׃A׃A�~�A�~�Aׇ+AׁA׉7A׏\A׏\A׏\A׋DA׉7A׉7A׉7A׋DAדuAבhAבhAדuAדuAבhAׇ+A׃Aׇ+Aׇ+A׉7A׍PA׍PA׏\AדuAדuAו�Aו�Aו�Aו�Aו�AדuAדuAבhAבhAבhAדuAדuAבhAדuAדuAו�AדuAו�AדuAו�AבhAדuAי�Aח�Aי�Aו�AדuAדuAדuAח�Aו�AדuAדuAו�AדuAדuAו�Aו�Aי�Aו�Aו�Aח�Aי�Aכ�Aי�Aי�Aח�Aח�Aכ�Aם�Aס�Aן�Aף�Aס�Aס�Aן�Aן�Aם�Aס�Aס�Aן�Aם�Aכ�Aן�Aם�Aם�Aן�Aס�@�@�@�@��^@��^@�@���@��7@��@�x�@�p�@�`B@�O�@�O�@�/@�&�@��@�&�@��@��@��@��@�V@��@��@��@��@��@��@��@�V@�V@�V@�%@���@��j@��9@��9@���@���@���@���@���@���@���@��@��u@��D@�Z@�A�@�9X@�I�@�1'@�1'@�9X@�1'@�9X@�9X@�9X@�9X@�1'@�1'@�(�@�(�@�1'@�1'@�1'@�9X@�9X@�1'@�1'@�9X@�9X@�9X@�A�@�9X@�1'@�9X@�9X@�9X@�9X@�9X@�9X@���@��@��m@�b@��@��m@�b@�b@��
@��
@���@�1@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@���@���@���@���@���@���@���@���@��
@���@���@���@���@���@���A�~�A�~�A�~�A�|�A�|�A�x�A�~�A�|�A�|�A׃A׃AׅAׅAׇ+A׋DAׁA�|�A�|�A�~�A׃AׁAׅA׏\A׍PA׏\A׋DAׇ+A׋DA׋DA׉7A׉7A׍PA׋DA׉7Aׇ+A׉7A׉7A׋DA׉7A׉7A׉7Aׇ+A׉7A׉7A׉7A׉7A׉7Aׇ+A׉7A׉7A׉7Aׇ+A׉7A׉7A׉7A׃A׃A׃A׃A׃A׃AׅAׅAׇ+Aׇ+Aׇ+AׅAׅAׇ+Aׇ+A׉7A׋DA׍PA׋DAׇ+A׃A׃A׃A׃AׅA׉7A׍PA׉7A׋DA׍PA׏\A׏\A׋DAדuAדuA׏\A׍PA׏\Aו�AבhAו�AדuA׏\A׋DA׉7Aׇ+A׉7A׉7Aׇ+A׉7A׉7Aׇ+Aׇ+Aׇ+Aׇ+A׋DA׋DA׍PA׍PA׉7Aׇ+AׅAׇ+AׅA׋DA׏\A׏\A׉7AׅAׇ+AדuAבhA׍PA׏\A׍PAׇ+A׉7A׉7A׉7A׉7A׍PAבhAו�AבhAבhAבhAדuAבhAדuAו�AדuAדuAבhA׏\AבhAבhAדuAדuAבhA׍PA׍PA׍PA׍PA׋DA׍PA׍PA׏\A׍PA׍PA׏\A׏\A׍PA׍PAדuAדuA׍PAבhA׋DA׋DA׏\AבhA׉7A׉7A׋DA׉7AׅA׉7AׅAׅA׃A׃A׃Aׇ+Aׇ+AׅAׇ+Aׇ+Aׇ+A׃AׁAׁAׇ+A׉7A׋DA׏\AבhAדuA׏\A׍PA׍PA׍PA׍PA׏\A׍PAבhAדuAו�A׏\Aׇ+A׉7A׉7A׋DA׏\A׏\AבhAדuAדuAח�Aו�Aח�Aח�Aח�Aו�Aו�Aו�Aח�Aו�Aו�Aו�AדuAו�Aח�Aו�Aו�Aו�Aו�Aו�Aח�AבhAדuAי�Aכ�Aם�Aח�Aי�Aו�Aו�Aח�Aח�Aי�Aח�Aח�Aו�Aו�Aח�Aח�Aי�Aי�Aח�Aכ�Aי�Aי�Aכ�Aי�Aי�Aח�Aכ�Aס�Aס�Aץ�Aץ�Aץ�Aץ�Aץ�Aץ�Aס�Aס�Aס�Aס�Aם�Aם�Aס�Aס�Aס�Aף�Aף�@���@���@���@���@�@�@��-@���@��7@��@�x�@�hs@�X@�X@�G�@�/@�/@�/@�&�@�&�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�%@���@�Ĝ@��@��j@��j@��9@���@���@���@���@��@���@��u@�A�@�A�@�A�@�A�@�I�@�A�@�I�@�A�@�9X@�9X@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�9X@�9X@�9X@�A�@�9X@�9X@�A�@�A�@�A�@�I�@�A�@�A�@�A�@�A�@�9X@�A�@�A�@�A�@��@�1@�  @�b@�1@�b@�b@�b@��m@��;@��@�1@��;@��;@��;@��;@��;@��;@��
@��;@��;@��;@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�~�AׁA׋DA׉7AׅA׉7A׋DG�O�G�O�G�O�G�O�G�O�G�O�G�O�Aף�G�O�G�O�Aף�Aץ�Aץ�Aק�Aס�A�x�G�O�A���A�\)G�O�G�O�A�5?A���AƅA�(�AŶFA�=qA�`BA��+A��A�K�A�S�A��mA���A�&�A�{A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�K�A�
=A�M�A���A���A�l�A��;A��DA��^A��7A�^5A���A��+A�A�A�^5A���A��HA�bNA�ƨA��A���A���A�"�A�jA�1'A�ƨA�  A�"�A��yA�  A�+A�oA�33A��/A�33A��A�%AA{�Az �AxA�AuXAo�Ao%Am�Am"�Al�uAl  Ak�Aj�+Ai��AhI�Ae�-Aa�#A^�/A]l�A[��AXZAU|�ASO�AR�AQ/AOO�ANffAK�AHz�AG�AGx�AGVAF�DAF9XAE��AE�7ADM�ACO�AB-AA�TAAC�A?p�A>bNA<��A;�#A;�hA;�A:bNA6�A5;dA4�!A4ZA37LA2��A2I�A0�!A.z�A-�PA-33A,��A,��A,I�A+�mA+dZA)?}A(ZA&�\A%��A%�7A$I�A"�uA!oA �A M�A A\)A�uAdZA�A��A�-A��A��A�uA��AM�A�A�;A��AdZA�9AI�A�-A��A�TA�jA��A�\Az�A1'A  A�A�;A��At�AK�AC�A7LAoA
��A
��A
�A��AjA�A%A(�A�
A��A��A�AS�A�A�7A �`A ~�A ffA   @��@�x�@���@�\)@�ff@���@��\@��^@�(�@�M�@�7L@���@�^5@�G�@�@�+@�9@�\)@�
=@��H@��@�ff@�@�b@�K�@�R@�5?@��@��/@�I�@���@�^5@�X@ץ�@�E�@��T@ԓu@��@�l�@�E�@�1'@�K�@̣�@�bN@�1'@�|�@�n�@�J@��@ǍP@Ƨ�@�=q@ř�@�V@�S�@�M�@���@���@�j@��@�|�@�~�@�@��@��@�1'@�1@��w@��+@��^@��7@�O�@���@���@�7L@���@���@�t�@���@��@���@�@��@���@���@�(�@� �@�bN@���@���@�I�@��w@�~�@��#@���@���@�v�@�{@�O�@���@��^@�/@�r�@��P@�+@�
=@��@���@��D@��@��@���@��
@��m@���@�C�@��@�@���@�5?@�@��7@�Ĝ@��F@�33@��H@�M�@�x�@��`@�r�@�(�@��@��P@�S�@���@��R@�5?@��-@���@�&�@���@��@�1@�t�@�33@���@�~�@�-@��@��7@�&�@�Ĝ@��@�A�@��@�C�@�"�@�
=@�~�@�n�@�ff@�^5@�ff@�V@�5?@�E�@�E�@�=q@�E�@�E�@�v�@�ȴ@���@��!@���@��H@���@��y@�ȴ@���@��R@��\G�O�@�n�@�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@���@�t�@�\)@�"�@�
=@��@���@�ȴ@�ȴ@��\@��@���@�p�@��D@�K�@�
=@��!@�^5@�=q@�-@��#@��-@�x�@�7L@�%@��`@��u@�A�@��@�\)@�+@���@��R@��+@�=q@��@�hs@�%@��@�j@�9X@� �@�b@���@��
@�;d@�
=@��H@���@�-@��#@�@�7L@�%@��D@�9X@�A�@� �@��;G�O�@�=q@{�4@q�@g��@XbN@Ru%@L]d@D��@B��@9@@4G@/\)@+��@&�@#�Q@=q@#:@��@�@�8@
ںG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��#A��uA�VA�hsAˮA�9XA���A�=qA�+A���A�~�A��yA�%A�A��#A�VA�A�(�A�XA���A��TA�  A�hsAѼjA�x�A���A��;A�ȴA���A���AhA�oA��A���AՁA�;dAÓuA�"�A�bA�|�A�K�A���A�XA�G�A�M�A�~�A���A�E�A�1'A���A�ȴAĥ�A���A�~�AǗ�A���A�ZA�33AȅA�VA�A��A��Aŧ�A���A�=qA�G�A��Aҟ�A�v�A��/A���AռjA�&�A�jA˧�A�7LA�O�A��A���A�G�A�33A���A�^5A�A�A�ȴA�p�AӸRA���A�bNA�VA�Q�A�bA��RAÉ7A�G�A�O�A�
=Aϴ9A�-A�|�A�r�A���A��A�33A�O�A���AȬA�Q�A֙�A�7LA�9XA��A�ZA�33A�C�AѮA��#A�p�A��A�G�A��wA�9XA�l�A��A�  A��A�A��A��HA��#A�I�A�5?Aˡ�A�33A�~�A΍PA�Ać+A�A���A�7LA��Aԛ�A��/A��+A�ƨA�^5A��A԰!A�A�A�oA\A���A�bNA�jAϡ�A�VA��;A�9XA՟�A�VA̧�A��A�1'A�33Aղ-A��TA�v�A�"�A�(�A�{A��A���A�/Aѥ�A��Aէ�A�Q�Aҕ�A�%AļjA���A�dZA�-A�&�AƉ7Aȝ�A��A�7LA�7LA�(�A�1'AʋDA���A�A�A���A�33A�1'A�;dA�;dA�+A�O�A�+A�p�A�`BA� �A��`A�-A�1'A�"�A��A�/A�5?A�(�A�-A�O�Aև+A� �A�-A��HA��A�5?A�=qA�;dA�?}A�E�A�=qA�;dA�=qA�9XA�E�A�I�A�G�A�;dA�A�A�5?A�I�A�K�A�K�A�E�A�K�A�M�A�K�A�I�A�M�A�I�A�
=A�E�A�E�A�C�A�M�A�O�A�M�A�E�A�E�A�=qA�E�A�M�A�E�A�A�A�I�A�K�A�G�A�E�A�E�A�C�A�E�A�bA�M�A�C�A�?}A�C�A�A�A�I�A�C�A�9XA�5?A�?}A�?}A�;dA�;dA�K�A�7LA�7LA�I�A�C�A�/A�E�A�?}A��A�7LA�C�A�K�A�M�A�A�A�E�A�I�A�Q�A�O�A�"�A�I�A�I�A�S�A�Q�A�O�A�7LA�M�A�Q�A�C�A�=qA�9XA�E�A�O�A�K�A�I�A�A�A�C�A�E�A�E�A�VA�VA�XA�A�A�{A�+A�^5A�\)A�^5A�XA�\)A�ZA�ZA�\)A�ZA�ZA�`BA�O�A�VA�`BA�bNA�dZA�\)A�Q�A�S�A�S�A�K�A�M�A�bNA�\)A�^5A�S�A�M�A�VA�ffA�jA�dZA�bNA�VA�\)A�`BA�`BA�^5A�hsA�p�A�jA�n�A�ffA�ffA�bNA�bNA�VA�\)A�dZA�VA�Q�A�I�A�S�A�O�A�M�A�VA�ZA�XA�`BA�XA�O�A�S�A�VA�XA�\)A�\)A�XA�XA�S�A�Q�A�S�A�S�A�G�A�I�A�E�A�I�A�M�A�XA�\)A�O�A�VA�Q�A�S�AӓuA�`BA�dZA�{A�O�A�ffA�dZA�bNA�ZA�VA�dZA�bNA�jA�hsA�bNA�ffA�ffA�hsA�hsA�^5A�ZA�VA�bNA�^5A�jA�n�A�ZA�jA�ffA�jA�l�A�l�A�`BA�^5A�bNA�l�A�jA�t�A�jA�XA�bNA�x�A�r�A�dZA�^5A�hsA�r�A�z�A�r�A�x�A�n�A�hsA�t�A�x�A�r�A�r�A�ffA�p�A�x�A�l�A�jA�t�A�t�A�n�A�|�A�t�A�hsA�ĜA�bA�v�A�t�A�v�A�t�A�t�A�t�A�x�A�r�A�r�A�n�A�p�A�|�A�p�A�n�A�jA�p�A�v�A�l�A�n�A�n�A�n�A�z�A�l�A�jA�l�A�l�A�l�A�n�A�p�A�l�A�n�A�l�A�l�A�l�A�n�A�n�A�r�A�p�A�r�A�t�A�r�A�t�A�v�A�v�A�t�A�r�A�t�A�v�A�~�A�~�A�~�A�|�A�|�A�x�A�~�A�|�A�|�A׃A׃AׅAׅAׇ+A׋DAׁA�|�A�|�A�~�A׃AׁAׅA׏\A׍PA׏\A׋DAׇ+A׋DA׋DA׉7A׉7A׍PA׋DA׉7Aׇ+A׉7A׉7A׋DA׉7A׉7A׉7Aׇ+A׉7A׉7A׉7A׉7A׉7Aׇ+A׉7A׉7A׉7Aׇ+A׉7A׉7A׉7A׃A׃A׃A׃A׃A׃AׅAׅAׇ+Aׇ+Aׇ+AׅAׅAׇ+Aׇ+A׉7A׋DA׍PA׋DAׇ+A׃A׃A׃A׃AׅA׉7A׍PA׉7A׋DA׍PA׏\A׏\A׋DAדuAדuA׏\A׍PA׏\Aו�AבhAו�AדuA׏\A׋DA׉7Aׇ+A׉7A׉7Aׇ+A׉7A׉7Aׇ+Aׇ+Aׇ+Aׇ+A׋DA׋DA׍PA׍PA׉7Aׇ+AׅAׇ+AׅA׋DA׏\A׏\A׉7AׅAׇ+AדuAבhA׍PA׏\A׍PAׇ+A׉7A׉7A׉7A׉7A׍PAבhAו�AבhAבhAבhAדuAבhAדuAו�AדuAדuAבhA׏\AבhAבhAדuAדuAבhA׍PA׍PA׍PA׍PA׋DA׍PA׍PA׏\A׍PA׍PA׏\A׏\A׍PA׍PAדuAדuA׍PAבhA׋DA׋DA׏\AבhA׉7A׉7A׋DA׉7AׅA׉7AׅAׅA׃A׃A׃Aׇ+Aׇ+AׅAׇ+Aׇ+Aׇ+A׃AׁAׁAׇ+A׉7A׋DA׏\AבhAדuA׏\A׍PA׍PA׍PA׍PA׏\A׍PAבhAדuAו�A׏\Aׇ+A׉7A׉7A׋DA׏\A׏\AבhAדuAדuAח�Aו�Aח�Aח�Aח�Aו�Aו�Aו�Aח�Aו�Aו�Aו�AדuAו�Aח�Aו�Aו�Aו�Aו�Aו�Aח�AבhAדuAי�Aכ�Aם�Aח�Aי�Aו�Aו�Aח�Aח�Aי�Aח�Aח�Aו�Aו�Aח�Aח�Aי�Aי�Aח�Aכ�Aי�Aי�Aכ�Aי�Aי�Aח�Aכ�Aס�Aס�Aץ�Aץ�Aץ�Aץ�Aץ�Aץ�Aס�Aס�Aס�Aס�Aם�Aם�Aס�Aס�Aס�Aף�Aף�@���@���@���@���@�@�@��-@���@��7@��@�x�@�hs@�X@�X@�G�@�/@�/@�/@�&�@�&�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�%@���@�Ĝ@��@��j@��j@��9@���@���@���@���@��@���@��u@�A�@�A�@�A�@�A�@�I�@�A�@�I�@�A�@�9X@�9X@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�9X@�9X@�9X@�A�@�9X@�9X@�A�@�A�@�A�@�I�@�A�@�A�@�A�@�A�@�9X@�A�@�A�@�A�@��@�1@�  @�b@�1@�b@�b@�b@��m@��;@��@�1@��;@��;@��;@��;@��;@��;@��
@��;@��;@��;@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��PA�~�A�~�A�~�A�|�A�|�A�x�A�~�A�|�A�|�A׃A׃AׅAׅAׇ+A׋DAׁA�|�A�|�A�~�A׃AׁAׅA׏\A׍PA׏\A׋DAׇ+A׋DA׋DA׉7A׉7A׍PA׋DA׉7Aׇ+A׉7A׉7A׋DA׉7A׉7A׉7Aׇ+A׉7A׉7A׉7A׉7A׉7Aׇ+A׉7A׉7A׉7Aׇ+A׉7A׉7A׉7A׃A׃A׃A׃A׃A׃AׅAׅAׇ+Aׇ+Aׇ+AׅAׅAׇ+Aׇ+A׉7A׋DA׍PA׋DAׇ+A׃A׃A׃A׃AׅA׉7A׍PA׉7A׋DA׍PA׏\A׏\A׋DAדuAדuA׏\A׍PA׏\Aו�AבhAו�AדuA׏\A׋DA׉7Aׇ+A׉7A׉7Aׇ+A׉7A׉7Aׇ+Aׇ+Aׇ+Aׇ+A׋DA׋DA׍PA׍PA׉7Aׇ+AׅAׇ+AׅA׋DA׏\A׏\A׉7AׅAׇ+AדuAבhA׍PA׏\A׍PAׇ+A׉7A׉7A׉7A׉7A׍PAבhAו�AבhAבhAבhAדuAבhAדuAו�AדuAדuAבhA׏\AבhAבhAדuAדuAבhA׍PA׍PA׍PA׍PA׋DA׍PA׍PA׏\A׍PA׍PA׏\A׏\A׍PA׍PAדuAדuA׍PAבhA׋DA׋DA׏\AבhA׉7A׉7A׋DA׉7AׅA׉7AׅAׅA׃A׃A׃Aׇ+Aׇ+AׅAׇ+Aׇ+Aׇ+A׃AׁAׁAׇ+A׉7A׋DA׏\AבhAדuA׏\A׍PA׍PA׍PA׍PA׏\A׍PAבhAדuAו�A׏\Aׇ+A׉7A׉7A׋DA׏\A׏\AבhAדuAדuAח�Aו�Aח�Aח�Aח�Aו�Aו�Aו�Aח�Aו�Aו�Aו�AדuAו�Aח�Aו�Aו�Aו�Aו�Aו�Aח�AבhAדuAי�Aכ�Aם�Aח�Aי�Aו�Aו�Aח�Aח�Aי�Aח�Aח�Aו�Aו�Aח�Aח�Aי�Aי�Aח�Aכ�Aי�Aי�Aכ�Aי�Aי�Aח�Aכ�Aס�Aס�Aץ�Aץ�Aץ�Aץ�Aץ�Aץ�Aס�Aס�Aס�Aס�Aם�Aם�Aס�Aס�Aס�Aף�Aף�@���@���@���@���@�@�@��-@���@��7@��@�x�@�hs@�X@�X@�G�@�/@�/@�/@�&�@�&�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�%@���@�Ĝ@��@��j@��j@��9@���@���@���@���@��@���@��u@�A�@�A�@�A�@�A�@�I�@�A�@�I�@�A�@�9X@�9X@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�9X@�9X@�9X@�A�@�9X@�9X@�A�@�A�@�A�@�I�@�A�@�A�@�A�@�A�@�9X@�A�@�A�@�A�@��@�1@�  @�b@�1@�b@�b@�b@��m@��;@��@�1@��;@��;@��;@��;@��;@��;@��
@��;@��;@��;@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��
@��PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�;oG�O�G�O�;o;o;o;o;o;oG�O�;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��b?��=�0�=���=��>Q�V=�_1=��z>#��@�xW@���>��?���>��>}�H@��k=�A�?V�>~�@V=�<6>��?��=�3	>���@�tT> >>�ؙ@���=�m?:cs?`��=�>'�>�G�@��s>s��>�z�> Ĝ?��=�W�=�1�>*+>�6@���@���@�j>FԀ@���@���>э@��1=���@g>G�??
<!>��@QY=��> u�@7C�=��>��?��=��S>E!W@��P@���?K�x>s�}?��=���>��@���>z�r>E�3?��>@���@��$>Y�@���@��>��i>qP�@��k=�ҳ=�a�>[��@���@_ί>F�@��@��>=��=�>��@�p�@���>��@�p�=�,|?d�>@�(�=�h�>�i@��=@��F=ل�>�L@�@���>��?� >8@���@���@I��=�Ov=ɋ/>�k@ ��>���=��=�b�>aR@g&l=�i�?��>.�@�#�>g�@���@��>
|[@X?>>Ѧ>,2#@��D>h�M?]�@�1=���=�#�?�?)=}�A=��.>.�>�D?I��?Nߤ@��)@��w=ݧ�?ѓ�@���?$��>��?���>	'�@���@���=�͟>"�8@�1�@���@��u@��->�i?�ݘ@��Y@���@���>3�#?�@���@���>	�4?{�>GG�@rn@2��=��=�ֶ@5g#@���@��P?�t=���?�F�@���@��F@���@��!@	�t=���?�`�@�G0@���@���@��y@���@���>3�@���@���@"y@ZdE@o��@��@M:?@��5?#��@��c@��5@���?�~�@o��@��d@���@���@��R@��x@���@��x@���@��Z@���@��Z@��x@���@��@��4@���@���@��@��@���@���@���@��@���@��#@��@��w@��Q@���@��Q@��@���@���@���@��w@���@���@��Z@��Z@��Z@���@���@���@���@���@���@��Z@���@��Z@��@���@���@��w@���@���@���@���@���@��4@���@���@��4@��@���@���@���@��Q@���@��Q@���@���@���@���@��4@��Q@��#@���@��+@��w@��w@��@��@���@���@���@��n@��n@���@���@���@��+@���@���@���@���@��@���@��#@��n@��Q@���@��@���@���@���@��@���@��@��@��@��@��Q@��H@���@��@���@��@��@���@���@���@���@��e@���@���@���@���@���@���@��@���@���@���@���@��+@��4@���@��.@���@��6@���@���@���@���@���@���@���@���@��\@���@���@��6@���@���@���@��@��Q@��+@��n@��@���@���@��+@���@��@���@��H@���@��Q@���@��"@���@���@��Q@��Q@���@���@��+@��n@���@���@���@���@��I@��H@���@��?@���@���@���@���@hE�@��6@��\@���@���@���@���@���@���@��@���@��\@���@���@���@���@���@��.@���@���@��?@��;@��.@��y@��@��@���@��X@��y@��y@��O@���@���@���@���@��@���@��y@���@���@���@���@���@���@���@���@��p@���@���@���@��@��-@���@���@��@��K@��@��K@��K@��S@���@��@���@��@���@���@��@��q@��|@��W@��h@��h@��N@���@��$@���@��@���@��u@��p@���@���@��@��S@��@��K@��y@��O@��y@��@���@���@��@���@��K@���@���@��K@��K@���@��y@��@��p@��@��B@��B@���@���@���@���@���@���@���@��h@��h@���@���@��A@���@���@��@���@���@���@��|@��@��|@��|@��9@��9@���@��|@���@��A@��|@��|@���@���@���@��E@���@���@���@���@���@���@��@��V@��@��@��@���@���@��'@��j@���@��@���@��@��j@���@��'@���@���@���@��'@���@��j@���@���@��'@��j@��E@��E@���@���@���@���@��j@��@��@��j@���@��<@���@���@��b@���@���@���@���@���@��@��<@���@��@���@��w@��4@���@���@��@��P@��P@��P@�¹@��@���@��Y@��+@��P@��P@��P@��+@���@���@��4@���@���@���@���@��@���@��@���@��Y@��Y@���@���@��@��@��@���@���@���@���@��P@��P@���@��+@���@���@���@��v@�¹@��P@��+@��+@���@��e@���@��e@���@��m@�ı@�ı@��@�ı@�ł@��@�ł@���@�ł@�ı@�Ë@���@�ı@�ı@��@��H@���@��"@��v@�¹@��"@��"@�Ë@���@�Ë@�Ë@�Ë@���@���@��]@�ı@���@��]@���@���@��7@���@���@���@���@���@�Ë@�Ë@���@���@���@��@@��@@���@���@���@���@���@�Ë@��7@���@��@@���@��e@���@���@�ł@�ƨ@��@��@���@�ł@�ł@�ł@��T@�ȟ@��K@�ȟ@��@��@��T@��]@���@���@��.@��@��K@�ȴ@�ȴ@��.@���@���@��.@�ʂ@�ʗ@�ʂ@��@�ʂ@��@�ʂ@�ʂ@��.@��.@���@��h@���@���@��h@���@��h@���@�ʗ@��%@��`@���@���@��h@��%@��%@�̎@�̎@�̎@��%@���@���@��y@��%@���@��K@��1@�ͳ@�΅@���@��W@�ϫ@���@���@��W@��}@�Ѣ@��t@��t@���@�ә@�ә@��1@���@��1@��t@�ә@��1@���@��t@���@��o@��1@���@��k@��(@S*@S)�@S(�@S(9@S'=@S%F@S!W@S�@S:@S�@S�@S&@S+@S�@S<@S�@S@@S�@So@S�@S�@SI@S�@S�@S�@S�@SM@SM@SM@S�@S�@S�@S,@S�@S
�@S6@S�@S i@R��@R��@R��@R��@R�@R�r@R��@R�z@R�@R�I@R�|@R�,@R�@R��@R��@R�@R��@R�Z@R�@R�@R�V@R�@R�Z@R��@R�@R�Z@R�V@R�V@R�@R�@R�@R�V@R�R@R�@R��@R��@R�M@R�R@R�@R�@R�@R�@R�@R�V@R�@R�S@R�)@R�F@R�@R�K@R��@R�@R�)@R�@R�@R�@R�@R�e@R�e@R�@R�@R��@R�@R�@R�@R��@R�@R�@R�@R��@R�@R�@R�@R��@R�@R�@R�@R�@R�@R��@R�@R�i@�+�@�-8@�-@�-#@�,�@�+�@�.
@�,�@�-w@�0@�/E@�.�@�/�@�1�@�2#@�.�@�-�@�-�@�-b@�/0@�/@�/o@�3r@�3�@�4�@�3�@�1{@�2M@�2�@�2�@�2#@�3�@�3�@�2v@�1�@�1�@�3r@�3r@�2�@�33@�2�@�2�@�28@�3]@�2�@�33@�2�@�2�@�3�@�3]@�3�@�3]@�3�@�3�@�4�@�2v@�1{@�1�@�1�@�1f@�1{@�1�@�2v@�3�@�3@�3	@�3r@�3@�3�@�3H@�3r@�5i@�5�@�5i@�3�@�2�@�2�@�2�@�2�@�3�@�4@�5�@�5@�5?@�7"@�77@�7a@�7"@�8�@�9@�8�@�6z@�6�@�9�@�8	@�9�@�9X@�7�@�7�@�5?@�4�@�5i@�5~@�5i@�5�@�6;@�5�@�4�@�4�@�5�@�5�@�6e@�8G@�7�@�6e@�5?@�5@�5@�5@�7@�8\@�9@�6;@�5+@�4�@�8�@�:*@�8G@�8	@�7�@�5~@�5�@�6�@�6z@�6�@�8�@�9�@�;d@�9�@�9�@�:?@�:T@�:�@�;@�;:@�;O@�;:@�9C@�8�@�9.@�: @�:i@�:*@�:i@�9C@�8�@�8�@�8�@�8\@�8G@�8�@�8�@�8�@�8�@�9C@�9@�8�@�9C@�:�@�;y@�9@�: @�8�@�7�@�9�@�:~@�8�@�7�@�7�@�8	@�5�@�6�@�6�@�5�@�5+@�5i@�5�@�7L@�6P@�6P@�7a@�6�@�7a@�5+@�5~@�5T@�7L@�8	@�8�@�: @�:�@�:�@�;�@�9�@�9�@�9�@�9X@�;%@�:@�;�@�;O@�=G@�;�@�8	@�7�@�82@�9@�:�@�;d@�;�@�<K@�<�@�>@�>-@�>B@�>�@�>l@�=�@�>@�=�@�=�@�=�@�=�@�=�@�=�@�>-@�>l@�>l@�>W@�>�@�>�@�>�@�?>@�=�@�=�@�?}@�A @�A�@�>�@�>�@�>�@�>�@�?>@�?�@�?�@�?}@�>�@�?}@�>�@�>�@�?)@�?�@�AJ@�@:@�A_@�A�@�A@�AJ@�@�@�@�@�@d@�@�@�D(@�E9@�E�@�E�@�E�@�F@�E�@�E�@�D�@�D�@�Ex@�E$@�D(@�C�@�D=@�D�@�D�@�E$@�E$@QdZ@Qc�@Qc�@Qc@Qc@Qb�@Qa�@Q[�@QY�@QX@QW?@QU�@QS&@QRT@QP�@QL�@QLD@QK�@QKI@QK@QJ�@QI�@QI�@QJM@QI�@QJ#@QJM@QJ#@QJM@QI�@QI{@QI{@QIR@QI(@QGZ@QG�@Q=2@Q8�@Q<@Q;�@Q;d@Q7�@Q7"@Q7�@Q7�@Q7�@Q7L@Q6�@Q)�@Q)@Q(c@Q(�@Q*0@Q)_@Q*@Q)�@Q'�@Q(�@Q(�@Q(�@Q(9@Q'�@Q'=@Q'=@Q'g@Q(c@Q(�@Q)_@Q)�@Q)_@Q)_@Q*0@Q*@Q*@Q,R@Q+V@Q*�@Q+,@Q+,@Q*�@Q*�@Q+�@Q+�@Q)�@Q �@Q"�@Q#�@Q �@Q#�@Q#�@Q$�@Q�@Q�@Q�@Q$t@QC@Q@Q@QC@Q@Qm@Q�@Q�@Q�@Q�@Qm@QC@QC@Qm@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         44444444433444434443444443443444444344444444334433434444444444444433444443444433433443444334334443343443443344434443344444444434443433434434434444444443344344443344333344333443344434444334443333444333333433433343433343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�xX@���G�O�G�O�G�O�G�O�@��kG�O�G�O�G�O�@VG�O�G�O�G�O�G�O�G�O�@�tTG�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�@��pG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@���G�O�G�O�@���@���G�O�@��0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��R@���G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@���@��$G�O�@���@�� G�O�G�O�@��hG�O�G�O�G�O�@���@_εG�O�@��@��=G�O�G�O�G�O�@�p�@���G�O�@�p�G�O�G�O�@�(�G�O�G�O�@��@@��LG�O�G�O�G�O�@���G�O�G�O�G�O�@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@g&sG�O�G�O�G�O�@�#�G�O�@���@��G�O�@X?@G�O�G�O�@��FG�O�G�O�@�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��+@��{G�O�G�O�@���G�O�G�O�G�O�G�O�@���@���G�O�G�O�@�1�@���@��|@��+G�O�G�O�@��V@���@���G�O�G�O�@���@���G�O�G�O�G�O�@rnG�O�G�O�G�O�G�O�@���@��NG�O�G�O�G�O�@���@��E@���@��G�O�G�O�G�O�@�G2@���@���@��x@���@���G�O�@���@���G�O�@ZdH@o��@��G�O�@��7G�O�@��e@��5@���G�O�@o��@��d@���@���@��R@��{@���@��{@���@��Y@���@��W@��v@���@��@��2@���@���@��@��@���@���@���@��@���@��&@��@��w@��V@���@��V@��@���@���@���@��u@���@���@��W@��]@��W@���@���@���@���@���@���@��^@���@��W@��
@���@���@��u@���@���@���@���@���@��5@���@���@��6@��
@���@���@���@��W@���@��V@���@���@��~@���@��1@��Q@��#@���@��-@��z@��w@��@��@���@���@���@��p@��p@���@���@���@��-@���@���@���@���@��@���@��&@��n@��N@���@��@���@���@���@��~@���@��@��@��@��@��P@��E@��@��@���@��@��@���@���@���@���@��f@���@���@���@���@���@���@��@���@���@���@���@��.@��6@���@��*@���@��6@���@���@���@���@���@���@���@���@��_@���@���@��3@���@���@���@��@��Q@��-@��o@��@���@���@��.@���@��@���@��N@���@��N@���@��&@���@���@��Q@��N@���@���@��-@��p@���@���@���@���@��K@��G@���@��>@���@���@���@���@hE�@��6@��_@���@���@���@���@���@���@��@���@��a@���@���@���@���@���@��3@���@���@��@@��9@��4@��{@��
@��@���@��Z@��z@��}@��N@���@���@���@���@��@���@��z@���@���@���@���@���@���@���@���@��t@���@���@���@��
@��.@���@��@��@��L@��@��O@��N@��V@���@��@���@��@���@���@��@��k@��~@��Z@��h@��m@��L@���@��%@���@��@���@��u@��r@���@���@��@��V@��@��M@��z@��W@��}@��@���@���@��@���@��N@���@���@��O@��N@���@��w@��
@��r@��@��G@��@@��@���@��@���@���@���@���@��k@��f@���@���@�+�@�-:@�-@�-'@�,�@�+�@�.@�,�@�-z@�0@�/F@�.�@�/�@�1�@�2&@�.�@�-�@�-�@�-`@�/*@�/@�/n@�3u@�3�@�4�@�3�@�1x@�2N@�2�@�2�@�2"@�3�@�3�@�2v@�1�@�1�@�3r@�3r@�2�@�35@�2�@�2�@�2:@�3^@�2�@�35@�2�@�2�@�3�@�3Z@�3�@�3]@�3�@�3�@�4�@�2z@�1~@�1�@�1�@�1h@�1~@�1�@�2z@�3�@�3@�3@�3x@�3@�3�@�3F@�3o@�5l@�5�@�5e@�3�@�2�@�2�@�2�@�2�@�3�@�4@�5�@�5@�5@@�7%@�75@�7^@�7"@�8�@�9@�8�@�6z@�6�@�9�@�8@�9�@�9Y@�7�@�7�@�5A@�4�@�5h@�5|@�5h@�5�@�6>@�5�@�4�@�4�@�5�@�5�@�6c@�8G@�7�@�6h@�5D@�5@�5@�5@�7@�8Z@�9@�6:@�5/@�4�@�8�@�:)@�8G@�8@�7�@�5�@�5�@�6�@�6{@�6�@�8�@�9�@�;f@�9�@�9�@�:B@�:T@�:�@�;@�;>@�;V@�;=@�9E@�8�@�9-@�9�@�:h@�:,@�:h@�9H@�8�@�8�@�8�@�8]@�8F@�8�@�8�@�8�@�8�@�9B@�9@�8�@�9E@�:�@�;{@�9@�9�@�8�@�7�@�9�@�:@�8�@�7�@�7�@�8@�5�@�6�@�6�@�5�@�5)@�5j@�5�@�7K@�6R@�6Q@�7^@�6�@�7e@�5,@�5|@�5X@�7N@�8
@�8�@�:@�:�@�:�@�;�@�9�@�9�@�9~@�9V@�;'@�:@�;�@�;M@�=J@�;�@�8@�7�@�83@�9@�:�@�;b@�;�@�<K@�<�@�>@�>-@�>A@�>�@�>n@�=�@�>@�=�@�=�@�=�@�=�@�=�@�=�@�>3@�>l@�>l@�>V@�>�@�>�@�>�@�?>@�=�@�=�@�?~@�A&@�A�@�>~@�>�@�?@�>�@�?B@�?�@�?�@�?~@�>�@�?{@�>�@�? @�?)@�?�@�AJ@�@:@�Ab@�A�@�A@�AH@�@�@�@�@�@f@�@�@�D-@�E;@�E�@�E�@�E�@�F@�E�@�E�@�D�@�D�@�Ez@�E(@�D*@�C�@�D8@�D�@�D�@�E"@�E%@Qd]@Qc�@Qc�@Qc
@Qc@Qb�@Qa�@Q[�@QY�@QX@QW>@QU�@QS#@QRV@QP�@QL�@QLE@QK�@QKM@QK@QJ�@QI�@QI�@QJP@QI�@QJ#@QJN@QJ"@QJM@QI�@QI{@QIx@QIN@QI&@QGZ@QG�@Q=.@Q8�@Q<@Q;�@Q;f@Q7�@Q7 @Q7�@Q7�@Q7�@Q7M@Q6�@Q)�@Q)@Q(e@Q(�@Q*2@Q)`@Q*@Q)�@Q'�@Q(�@Q(�@Q(�@Q(=@Q'�@Q'@@Q'=@Q'j@Q(e@Q(�@Q)`@Q)�@Q)`@Q)`@Q*.@Q*@Q*@Q,P@Q+V@Q*�@Q+-@Q+*@Q*�@Q*�@Q+�@Q+�@Q)�@Q �@Q"�@Q#�@Q �@Q#�@Q#�@Q$�@Q�@Q�@Q�@Q$x@Q@@Q@Q@QC@Q@Qj@Q�@Q�@Q�@Q�@Qm@QH@QH@Qn@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@�+�@�-:@�-@�-'@�,�@�+�@�.@�,�@�-z@�0@�/F@�.�@�/�@�1�@�2&@�.�@�-�@�-�@�-`@�/*@�/@�/n@�3u@�3�@�4�@�3�@�1x@�2N@�2�@�2�@�2"@�3�@�3�@�2v@�1�@�1�@�3r@�3r@�2�@�35@�2�@�2�@�2:@�3^@�2�@�35@�2�@�2�@�3�@�3Z@�3�@�3]@�3�@�3�@�4�@�2z@�1~@�1�@�1�@�1h@�1~@�1�@�2z@�3�@�3@�3@�3x@�3@�3�@�3F@�3o@�5l@�5�@�5e@�3�@�2�@�2�@�2�@�2�@�3�@�4@�5�@�5@�5@@�7%@�75@�7^@�7"@�8�@�9@�8�@�6z@�6�@�9�@�8@�9�@�9Y@�7�@�7�@�5A@�4�@�5h@�5|@�5h@�5�@�6>@�5�@�4�@�4�@�5�@�5�@�6c@�8G@�7�@�6h@�5D@�5@�5@�5@�7@�8Z@�9@�6:@�5/@�4�@�8�@�:)@�8G@�8@�7�@�5�@�5�@�6�@�6{@�6�@�8�@�9�@�;f@�9�@�9�@�:B@�:T@�:�@�;@�;>@�;V@�;=@�9E@�8�@�9-@�9�@�:h@�:,@�:h@�9H@�8�@�8�@�8�@�8]@�8F@�8�@�8�@�8�@�8�@�9B@�9@�8�@�9E@�:�@�;{@�9@�9�@�8�@�7�@�9�@�:@�8�@�7�@�7�@�8@�5�@�6�@�6�@�5�@�5)@�5j@�5�@�7K@�6R@�6Q@�7^@�6�@�7e@�5,@�5|@�5X@�7N@�8
@�8�@�:@�:�@�:�@�;�@�9�@�9�@�9~@�9V@�;'@�:@�;�@�;M@�=J@�;�@�8@�7�@�83@�9@�:�@�;b@�;�@�<K@�<�@�>@�>-@�>A@�>�@�>n@�=�@�>@�=�@�=�@�=�@�=�@�=�@�=�@�>3@�>l@�>l@�>V@�>�@�>�@�>�@�?>@�=�@�=�@�?~@�A&@�A�@�>~@�>�@�?@�>�@�?B@�?�@�?�@�?~@�>�@�?{@�>�@�? @�?)@�?�@�AJ@�@:@�Ab@�A�@�A@�AH@�@�@�@�@�@f@�@�@�D-@�E;@�E�@�E�@�E�@�F@�E�@�E�@�D�@�D�@�Ez@�E(@�D*@�C�@�D8@�D�@�D�@�E"@�E%@Qd]@Qc�@Qc�@Qc
@Qc@Qb�@Qa�@Q[�@QY�@QX@QW>@QU�@QS#@QRV@QP�@QL�@QLE@QK�@QKM@QK@QJ�@QI�@QI�@QJP@QI�@QJ#@QJN@QJ"@QJM@QI�@QI{@QIx@QIN@QI&@QGZ@QG�@Q=.@Q8�@Q<@Q;�@Q;f@Q7�@Q7 @Q7�@Q7�@Q7�@Q7M@Q6�@Q)�@Q)@Q(e@Q(�@Q*2@Q)`@Q*@Q)�@Q'�@Q(�@Q(�@Q(�@Q(=@Q'�@Q'@@Q'=@Q'j@Q(e@Q(�@Q)`@Q)�@Q)`@Q)`@Q*.@Q*@Q*@Q,P@Q+V@Q*�@Q+-@Q+*@Q*�@Q*�@Q+�@Q+�@Q)�@Q �@Q"�@Q#�@Q �@Q#�@Q#�@Q$�@Q�@Q�@Q�@Q$x@Q@@Q@Q@QC@Q@Qj@Q�@Q�@Q�@Q�@Qm@QH@QH@Qn@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         44444444433444434443444443443444444344444444334433434444444444444433444443444433433443444334334443343443443344434443344444444434443433434434434444444443344344443344333344333443344434444334443333444333333433433343433343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9���9���9��r9���9��@9��9���9��99��9���9���9��(9���9��9���9���9��i9���9���9��w9��C9���9���9���9���9���9���9���9���9���9���9��H9���9��*9��z9��>9���9���9���9��;9���9���9���9��u9���9��;9���9���9��9��o9��9��t9��9��J9��89��09���9��9��Z9���9���9��Z9��09���9��9���9���9��9��M9��S9���9��c9���9��Y9��*9��K9��f9��Q9���9���9���9���9���9��%9���9���9��)9���9��M9���9��9���9��E9���9��9��79���9���9���9��&9��9��^9��z9��^9���9���9��9���9���9���9��9���9��v9���9���9��*9���9���9���9���9���9���9���9��9��P9��.9��%9��v9��9���9���9���9��@9���9��#9��j9���9���9���9���9��I9��b9���9��p9���9���9���9���9���9���9���9��9��)9��9���9��(9��(9��b9���9��t9��I9��
9��)9��M9���9���9��$9���9���9��9���9���9���9���9���9���9���9��l9���9��$9���9��[9��$9��9��9��`9���9��9���9���9��)9��[9��39��9��z9��G9��9��9���9���9���9��U9��(9��t9���9��19���9���9��9��C9���9���9���9�� 9��h9��Y9���9���9���9��|9��09��9���9���9���9��y9��<9��n9���9���9��f9��-9��9��n9��9���9��99��99��9��X9���9���9��d9���9��s9���9��9��9��S9���9��9���9��j9��R9��o9���9��9�»9���9��9��F9��9��P9���9��r9�ů9���9��M9�Ļ9���9��
9�Ě9��m9���9��z9�˟9�˷9��9���9���9��9�ʓ9��H9���9��i9��z9��}9��y9��y9���9���9�L�9�L69�L39�K�9�K�9�K�9�J�9�F]9�E/9�C�9�CR9�Bc9�@e9�?�9�>�9�;�9�;9�;C9�:�9�:�9�:S9�9�9�9�9�:9�9�9�9�9�:9�9�9�:9�9�9�9�9�9�9�9b9�9F9�7�9�8;9�0�9�-�9�/�9�/�9�/x9�,�9�,l9�,�9�,�9�-9�,�9�,19�"�9�"_9�!�9�"%9�#49�"�9�#9�"�9�!U9�"&9�"%9�"C9�!�9�!�9�!9�!9�!89�!�9�"C9�"�9�"�9�"�9�"�9�#19�#9�#9�$�9�$9�#�9�#�9�#�9�#p9�#�9�$"9�$$9�"�9�l9��9��9�l9��9��9�u9��9��9��9�9�B9�)9�)9�D9�&9�`9��9��9��9��9�b9�H9�H9�c9��9��9��9��9��9��9��9��9��9��9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÖB��B�By�Bu�BgmB;dB)�BoBPBPBoB1BVBoBoBB��BBB��B��B��B��BBB��B�NB��B��BÖBǮB�B��BÖB��B��B��B�%BjB`BBZBVBO�BYB�oB��B��B�hB�Br�BdZBH�BJBB�B�)B��BǮB��B��B��B��B��B��B�PB|�BiyBO�B6FB#�B�BPB  B
�yB
��B
�3B
��B
�{B
x�B
p�B
]/B
T�B
P�B
<jB
.B
 �B
\B	��B	�B	�mB	�NB	�;B	�#B	�B	��B	��B	ÖB	�9B	��B	�VB	�B	y�B	ffB	T�B	K�B	J�B	D�B	<jB	C�B	<jB	1'B	1'B	/B	.B	-B	)�B	(�B	'�B	!�B	�B	�B	�B	�B	PB	B�B�B�B�B�B�5B��B��B�B�B�B�B��BB��B�qB�jB�^B�LB�9B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�JB�=B�1B�+B�%B�B�B}�B{�By�Bw�Bw�Bv�Bt�Bs�Br�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bm�Bl�Bk�Bk�BjBiyBk�BiyBiyBiyBhsBhsBgmBffBffBe`Be`Be`BdZBcTBaHB_;B`BBbNBbNBbNBbNBe`BffBcTB^5B_;B`BBbNB_;BaHBaHBcTBe`Be`Be`Be`BffBiyBjBk�Bl�Bk�Bk�BgmBe`Bl�Bm�Bo�Bs�Bv�Bv�Bu�Bv�B{�B�B�1B�+B�+B�1B�7B�1B�1B�7B�DB�DB�DB�DB�bB�uB�{B�uB��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�FB�RB�XB�dB��B��B��BĜBŢBǮB��B��B��B��B��B�B�B�5B�fB�sB�B�yB�`B�B�B�B�B��B��B��B��B��B��B��B��B	B��B��B	B	B	+B	1B	PB	bB	oB	uB	�B	�B	�B	�B	$�B	+B	-B	.B	1'B	7LB	;dB	=qB	>wB	B�B	C�B	E�B	I�B	J�B	L�B	L�B	L�B	O�B	P�B	S�B	W
B	ZB	\)B	]/B	`BB	cTB	dZB	ffB	hsB	jB	l�B	m�B	q�B	t�B	u�B	u�B	z�B	{�B	}�B	}�B	~�B	�B	�B	�1B	�PB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�?B	�^B	��B	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��Bo�B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�B	�B	�#B	�/B	�5B	�5B	�HB	�`B	�`B	�fB	�mB	�mB	�mB	�yB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
-B
BB
B
)B
)DB
1vB
6B
<�B
>B
A;B
F?B
J=B
TaB
[#B
^OB
c�B
gB
m�B
p�B
s3B
w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��{A�>��>�"�?%�?�&�>�{)?��?I;:B	�B?�?E��AQ]?<�?�|B1u?��@JP�?�QiA��>��	?�Wd@���?wy?�ËB;�?!�	?��A��>��%@��X@��X?8;?U�?�G�B�Z?�=�?��r?"��A��>�޵?(�?W�s@?B
�BXAO��?y	�B
B�?>��Bu�?��A[�j?vc�@:XG?�(�A]��>�?��A�ʎ?��?<�XA4��?0f?wwB�B�@��?�@���>�@ԢB��?�K?ve|A'��?+�-B$B�W?5=CB��B?��?��]B��>�ݾ?��?��BF�A��:?zp�BQ�B.�>��?u�?FB��B.#?-` B� ?��@�-B�9?��?ַ�B�B4?'�?2.AQCB$�?@P�@�V�?c��B�B�A��c>ġ>�\d?(��Ak/�?���? ">�,?+��A���?f�@Y�?;��A���?BēB�B?+,�A�h�@
�?UJ�B%�?�Y�@��zB��>�l?g�A�<>�x?	�?AX?#|�@��p@�m�B�B��?$A YBt�@a&W?1�h@�ˈ?'$1B�B��?�?J[�B�&B	BoB��?D��A.`�B�B`B L?b�A�B&BD�?)M�@��?t؈A���A��l>�d�?R�A�˙B!hB@�"�?f�A4<&B�B%B&�BpAL��?3�As�BQ�BkBB�B�B# ?�
�B!uBg�Ap��A��A�'�B$�A�A�B)�@ZC�B$B"lB%AH-A�˝B`B*�B$+BEmB@�B'NB#B$�B$B"�B$�B#�B$B$WB �B!0B"<B%IB"�B(MB!(B!fB!�B#B!�B �B"B!�B �B!�B�?B"�B#�B"�B!GB �B �B!�B!�B$�B!B�B"�B$TB!sB �B �B!�B!�B"B �B7wB!OB&2B#QB"�B#�B!cB!YB"�B&�B"�B#�B$�B'�B!SB(�B(@B!�B!B'!B"�B$`B1�B(�B$�B$&B#�B%�B$rB$FB!B�B.OB!%B$�B �B!�B!�B-�B#�B B%�B(�B'�B%�B#�B!�B$�B$�B#�B%�B&yB �B!�B 
B$CB>�B)�B �B!�B|B"�B"B"�B".B!�B"�B"6BB%B#�B"8B"�B#iB"eB"�B"B!�B#uB(�B!B$0B!YB!XBuB#�B"UBSB!ZB!B$�B"eB!�B!�B$B zBB B�B DB �BB�B!�B!�B!B �B!xB$>B!�B"�B#�B!LB"|B"3BfB"�B#�B"�B"[B!�B�B<B �B �B!�B"!B �B�B"�B#6B#�B#�B&vB#�B"�B%B!�B'PB"�A���B"�B"jB=�B)�B!CB!B�B!'B$B"B#+B!|B!�B!�B �B"B!�B!B#_B#�B&B#�B'�B#eB �B$�B �B$sB"�B!�B �B!�B%B$BB!�B#B�B"%B&�B'B!�B�B#qB'�B#/B!yB]B"�B &B"�B&B"B |B!B rB$B!;B B!
B#�B ZB9B �B�B!�B#DB�dA��7B �B"cB!�B#KB!�B#BB!B"hB"	B"B�B �B �B!�B �B�B!�B �B �B �B�B!B"
B!�B"[B!�B!�B �B"KB!B!B"�B#BB"B#-B!�B"�B"5B!lB"�B" B!OB!OB!�B"zB �B �B!�B!�B oB!�B!=B!�B"�B#�B"}B"B!�B#lB @B!B!B!=B!xB! B#SB"�B!�B"8B 4B wB�B"B �B!,B!MB".B �B!MB!EB"B!�B"XB![B!rB"~B �B!_B!�B"
B!}B"�B")B �B!TB!�B!WB �B!GB!B"CB"ZB"B"B"GB"?B"IB"9B"B!�B!�B!�B"GB!�B"7B""B"�B!`B �B"B! B!�B#,B"4B"�B!NB#BB!�B"=B!�B#KB"�B!�B"tB �B!0B �B �B!QB!�B �B�B!ZB!�B!8B"�B"dB"<B! B!�B!�B"7B"�B"�B"�B"jB"bB"yB"�B"B#�B"�B"aB#"B#"B%;B �B"[B"rB"B"�B#�B"B!�B!�B"�B#�B"�B"BB$�B#`B#�B#oB"qB"�B!�B#�B"�B"lB"�B!�B!�B"TB!�B"B#.B#B"HB#oB!B!�B#.B#qB#�B#B#B#sB#�B"�B#cB#cB"�B"(B"}B" B"6B"uB"B"�B"�B"�B#_B"�B#�B"qB!�B"JB$rB#7B#/B#NB$�B%�B%lB#�B#HB$�B"�B#XB#iB#B$�B%�B#�B&;B$uB#!B#wB#oB#�B$\B$TB$TB$?B#"B#�B#�B#xB#pB!�B#�B%�B$;B$�B%yB$�B%WB$�B$?B#�B$B#fB#�B#�B#�B$B$aB$�B%B%B#�B#�B%sB%B$�B#�B$�B#�B$�B#�B$�B%�B$8B$�B"�B#�B%`B%`B%�B$B$�B%HB%�B%+B%{B%0B%B%^B$�B%�B&gB%�B%�B$�B%B%B&2B'1B&�B&�B$�B&B%B%�B%yB%�B&1B&IB%�B%QB%�B&)B'?B&)B&�B'@B&�B&�BIlBIBH/BH�BH$BE�BF�BH�BH�BH�BH�BIBJzBH�BK�BLXBMBK�BL@BK�BL�BLGBMBK�BK�BK�BK1BKBK	BJsBJ�BJ�BJ<BJ>BI�BK�BL|BK�BM(BL�BL(BL�BL�BL�BL3BI(BJ�BGIBL\BN�BO.BM�BO�BP=BN�BO�BN�BN�BO
BN�BO<BN�BO�BPBO�BO�BO�BN�BNBOHBO�BN>BNnBOBN)BNvBN�BM�BM�BM�BM�BM9BK|BOBO�BS6BN�BK�BQQBM�BK/BPBPDBN�BI�BO�BOyBO}BOpBO�BO�BO�BOMBOQBO%BP+BO�BP"BO�BO�BPBO�BO�BN�BO�BORBODBN�BN�BN8B�+B�=B�B��B��B�B��B��B�B�B�DB�2B��B��B��B�zB�B�<B��B��B��B�RB� B��B�/B��B�9B�_B��B�kB��B��B�SB�B�YB�]B��B��B�KB�tB�4B��B�{B�qB�B�<B��B��B��B�9B��B��B�B��B�#B��B��B��B��B�kB�nB��B�xB��B�/B�B�1B��B��B�%B�|B�oB��B�WB�xB��B��B��B��B��B��B��B�{B��B��B��B�B�YB��B��B�B��B�>B��B��B�OB��B��B��B�B�&B�&B�0B��B�NB��B�=B�AB�TB��B��B� B��B�pB��B�wB��B�7B��B�vB�B��B�iB�B��B��B��B��B��B�;B�gB��B��B�rB��B�B�-B�B�8B�BB��B��B��B�rB��B��B��B��B��B�jB�B��B�tB�sB��B�yB�yB��B��B��B�|B��B�YB�lB��B��B�IB��B��B�XB�rB��B��B�"B�iB�B�nB��B�	B��B�nB��B�,B��B��B��B�B��B�B��B��B�cB��B�iB�{B�NB��B��B�8B��B��B�?B�PB�_B��B�2B�B��B��B�tB�bB�VB�vB�B��B�[B�PB�2B��B�jB�B��B� B��B�(B�kB�FB��B��B��B��B�DB�1B�aB��B��B�B��B��B��B�B��B��B�lB��B�B��B�{B��B�/B�B��B�iB��B�CB��B�B�HB��B�	B�'B��B�@B�
B�{B�`B��B�QB��B��B� B��B�mB�YB�!B�,B�7B�gB�:B�2B��B��B�IB��B��B��B�B��B��B��B��B	��B	�3B	�%B	��B	��B	�IB	�xB	�+B	��B	��B	�B	�*B	�3B	�B	�`B	�vB	�,B	��B	�\B	�0B	��B	�4B	��B	�XB	� B	�B	�#B	��B	��B	��B	�KB	�0B	�B	��B	��B	��B	�wB	�^B	��B	�>B	��B	�HB	��B	� B	�%B	�&B	��B	�XB
 tB	��B	�7B	�ZB	�QB	��B	�B	��B	�7B
 B	��B	��B	�cB	�B	��B	��B	��B	�%B	�gB	��B	��B	��B	�uB	��B	��B	��B	�.B	�yB	��B	�%B	�B	��B	��B	�B	�B
�B	�eB
 �B	�uB	�#B	�/B	�B	��B
 �B
 7B	��B
 cB	��B	�mB	�SB	�dB	�8B	�[B
 ~B	�`B	��B	�WB
 B	��B	��B	��B	��B	��B
 B	��B	��B	��B	��B	��B	��B	�zB
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444433444434443444443443444444344444444334433434444444444444433444443444433433443444334334443343443443344434443344444444434443433434434434444444443344344443344333344333443344434444334443333444333333433433343433343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B��B��B��B��B��BãG�O�B�By�G�O�G�O�B;vB*B�BbBcBBCBjB}B�B#B��BBB��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�xB�*Br�BdoBH�B^BB��B�<B��B��B�	B��B�B��B��B��B�dB} Bi�BO�B6YB#�B�BcB B
�B
��B
�FB
��B
��B
x�B
p�B
]BB
UB
P�B
<B
.'B
 �B
oB	��B	��B	�B	�bB	�OB	�6B	�+B	�B	��B	ëB	�MB	��B	�kB	�4B	y�B	f|B	UB	K�B	J�B	D�B	<�B	C�B	<~B	1;B	1>B	/2B	.*B	-$B	*B	)B	(B	!�B	�B	�B	�B	�B	eB	$B��B�B�B�B�B�KB� B��B�B�B�(B�B��B¦B��B��B��B�uB�cB�NB�2B�B�"B��B��B��B��B��B��B��B��B��B��B��B��B�sB�cB�TB�IB�EB�=B�1B�B~
B{�By�Bw�Bw�Bv�Bt�Bs�Br�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bm�Bl�Bk�Bk�Bj�Bi�Bk�Bi�Bi�Bi�Bh�Bh�Bg�Bf~Bf~BezBexBeyBdrBckBabB_TB`YBbhBbiBbgBbgBeyBfBckB^PB_RB`[BbgB_TBaaBacBcpBezBewBezBeyBf~Bi�Bj�Bk�Bl�Bk�Bk�Bg�BezBl�Bm�Bo�Bs�Bv�Bv�Bu�Bv�B| B�6B�JB�EB�DB�KB�RB�GB�KB�PB�^B�^B�]B�]B�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�4B�4B�;B�`B�nB�qB�|B��B��B��BĶBżB��B��B��B��B��B�
B�B�5B�MB�B�B�B�B�~B�B�B�B�B��B��B��B�B�B�	B�B�B	#B�B�B	+B	7B	CB	LB	iB	~B	�B	�B	�B	�B	�B	�B	$�B	+B	-'B	.,B	1@B	7gB	;~B	=�B	>�B	B�B	C�B	E�B	I�B	J�B	L�B	L�B	L�B	O�B	P�B	TB	W#B	Z7B	\BB	]HB	`\B	cpB	dwB	f�B	h�B	j�B	l�B	m�B	q�B	t�B	u�B	u�B	z�B	|B	~B	~B	B	� B	�:B	�KB	�lB	�|B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�.B	�<B	�XB	�wB	��G�O�B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�$B	�,B	�1B	�9B	�5B	�?B	�8B	�6B	�=B	�HB	�OB	�PB	�bB	�zB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B
 B
 G�O�B
GB
]B
8B
DB
)^B
1�B
6-B
<�B
>'B
AWB
FYB
JZB
TyB
[;B
^iB
c�B
gB
m�B
p�B
sOB
w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B?�G�O�G�O�G�O�G�O�B1�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�B;�G�O�G�O�AӀG�O�G�O�G�O�G�O�G�O�G�O�B�hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�BfG�O�G�O�BB�G�O�Bu�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�B5B�fG�O�B�B(G�O�G�O�B��G�O�G�O�G�O�BF�A��PG�O�BQ�B.�G�O�G�O�G�O�B��B.2G�O�B�1G�O�G�O�B�HG�O�G�O�B
B4#G�O�G�O�G�O�B$�G�O�G�O�G�O�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�A���G�O�B�B&G�O�A�i	G�O�G�O�B&G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�G�O�G�O�Bt�G�O�G�O�G�O�G�O�B�B��G�O�G�O�B�6BB�B��G�O�G�O�BBrB _G�O�G�O�B&+BD�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�B!zB-G�O�G�O�G�O�B�B5B&�B~G�O�G�O�G�O�BRB|B B�B�B#1G�O�B!�BhG�O�A��A�'�B$�G�O�B)�G�O�B$#B"|B%G�O�A�˳B`B*�B$9BE}BAB'_B#'B$�B$B"�B$�B#�B$,B$iB �B!BB"KB%ZB#B(]B!;B!wB!�B#B!�B �B"B!�B �B!�B�MB"�B#�B"�B!VB �B �B!�B!�B$�B!B�B"�B$gB!�B �B �B"B!�B"0B!B7�B!]B&BB#aB"�B#�B!uB!jB"�B&�B#B#�B$�B'�B!aB(�B(MB!�B!B'1B"�B$qB1�B(�B$�B$6B#�B&B$B$UB!)B�B._B!5B$�B �B!�B!�B-�B#�B $B%�B(�B'�B%�B#�B!�B$�B%B#�B%�B&�B!B"B B$PB>�B)�B!B!�B�B"�B"&B"�B"=B!�B"�B"HB'B%B#�B"JB"�B#{B"vB"�B",B!�B#�B(�B!)B$>B!hB!jB�B#�B"cBcB!kB!)B$�B"vB!�B!�B$B �B%B  B�B UB �BBB"B!�B1B �B!�B$NB!�B"�B#�B!XB"�B"BBxB"�B$	B"�B"kB!�B�BKB �B �B!�B"1B �B B#B#FB#�B#�B&�B$B"�B%B"B'bB"�A���B#B"|B=�B)�B!QB!B�B!5B$&B"B#>B!�B!�B!�B �B"B!�B!B#oB#�B&B$B'�B#uB �B$�B!	B$�B"�B!�B �B!�B%B$QB!�B#B�B"4B&�B'$B!�B�B#�B'�B#@B!�BpB"�B 6B"�B&/B"%B �B!*B �B$%B!LB2B!B#�B hBHB!B�B!�B#TB�rA��RB �B"tB!�B#ZB!�B#B�B!B"|B"B")BB �B �B!�B �B�B!�B �B!B �B�B!B"B"B"lB"B!�B �B"\B!,B!�B"�B#RB"(B#@B!�B#B"CB!}B"�B"0B!`B!`B!�B"�B!B �B�=B�OB�B��B��B�%B��B��B�%B�B�VB�CB��B��B��B��B�B�LB��B��B��B�aB�B�B�@B��B�GB�pB��B�~B��B��B�cB�B�gB�nB��B�B�YB��B�CB�B��B��B�B�OB��B��B��B�GB��B��B��B��B�4B��B��B��B��B�~B��B�B��B��B�=B�#B�CB��B��B�4B��B��B�B�eB��B��B��B��B��B��B��B��B��B��B��B�B�B�iB��B��B�+B��B�QB��B��B�^B��B��B��B�!B�6B�6B�@B��B�]B��B�OB�OB�cB�B��B�B��B��B��B��B�B�GB�B��B�B��B�xB�B��B��B��B��B��B�JB�xB��B��B��B��B�B�;B�(B�GB�QB��B��B��B��B��B��B��B��B��B�zB�,B��B��B��B�B��B��B��B��B�	B��B��B�gB�~B�B��B�VB��B��B�gB��B��B��B�2B�|B�/B�~B��B�B� B��B��B�>B��B��B��B�B��B�B��B��B�qB��B�|B��B�`B��B��B�GB�B��B�QB�aB�nB��B�@B�B��B��B��B�pB�gB��B�B��B�lB�`B�@B��B�xB�B��B�B��B�9B�~B�YB��B��B��B��B�TB�@B�qB��B��B�B��B��B��B�B� B��B�|B��B�B��B��B��B�?B�B��B�|B�B�QB��B�(B�WB��B�B�6B��B�PB�B��B�sB��B�`B��B��B�B��B��B�iB�/B�;B�EB�xB�GB�CB��B��B�[B�B��B�B�B��B��B�B� B	��B	�MB	�>B	��B	��B	�cB	��B	�FB	�B	��B	�'B	�CB	�KB	��B	�yB	��B	�FB	��B	�wB	�KB	��B	�MB	�B	�sB	�B	�*B	�>B	�B	�B	��B	�fB	�HB	�B	��B	��B	��B	��B	�xB	��B	�WB	�B	�aB	��B	�;B	�?B	�?B	��B	�sB
 �B	��B	�RB	�uB	�lB	��B	�%B	��B	�RB
 B	��B	�B	��B	�4B	��B	��B	��B	�AB	��B	��B	��B	��B	��B	��B	��B	��B	�HB	��B	��B	�@B	�2B	��B	��B	�-B	�"B
�B	�B
 �B	��B	�>B	�KB	�.B
 B
 �B
 PB	�B
 B	��B	��B	�nB	�B	�QB	�tB
 �B	�zB	��B	�oB
 9B
  B	��B
 B
 	B	��B
 )B	��B	��B	��B	��B	��B	��B	��B
	B�=B�OB�B��B��B�%B��B��B�%B�B�VB�CB��B��B��B��B�B�LB��B��B��B�aB�B�B�@B��B�GB�pB��B�~B��B��B�cB�B�gB�nB��B�B�YB��B�CB�B��B��B�B�OB��B��B��B�GB��B��B��B��B�4B��B��B��B��B�~B��B�B��B��B�=B�#B�CB��B��B�4B��B��B�B�eB��B��B��B��B��B��B��B��B��B��B��B�B�B�iB��B��B�+B��B�QB��B��B�^B��B��B��B�!B�6B�6B�@B��B�]B��B�OB�OB�cB�B��B�B��B��B��B��B�B�GB�B��B�B��B�xB�B��B��B��B��B��B�JB�xB��B��B��B��B�B�;B�(B�GB�QB��B��B��B��B��B��B��B��B��B�zB�,B��B��B��B�B��B��B��B��B�	B��B��B�gB�~B�B��B�VB��B��B�gB��B��B��B�2B�|B�/B�~B��B�B� B��B��B�>B��B��B��B�B��B�B��B��B�qB��B�|B��B�`B��B��B�GB�B��B�QB�aB�nB��B�@B�B��B��B��B�pB�gB��B�B��B�lB�`B�@B��B�xB�B��B�B��B�9B�~B�YB��B��B��B��B�TB�@B�qB��B��B�B��B��B��B�B� B��B�|B��B�B��B��B��B�?B�B��B�|B�B�QB��B�(B�WB��B�B�6B��B�PB�B��B�sB��B�`B��B��B�B��B��B�iB�/B�;B�EB�xB�GB�CB��B��B�[B�B��B�B�B��B��B�B� B	��B	�MB	�>B	��B	��B	�cB	��B	�FB	�B	��B	�'B	�CB	�KB	��B	�yB	��B	�FB	��B	�wB	�KB	��B	�MB	�B	�sB	�B	�*B	�>B	�B	�B	��B	�fB	�HB	�B	��B	��B	��B	��B	�xB	��B	�WB	�B	�aB	��B	�;B	�?B	�?B	��B	�sB
 �B	��B	�RB	�uB	�lB	��B	�%B	��B	�RB
 B	��B	�B	��B	�4B	��B	��B	��B	�AB	��B	��B	��B	��B	��B	��B	��B	��B	�HB	��B	��B	�@B	�2B	��B	��B	�-B	�"B
�B	�B
 �B	��B	�>B	�KB	�.B
 B
 �B
 PB	�B
 B	��B	��B	�nB	�B	�QB	�tB
 �B	�zB	��B	�oB
 9B
  B	��B
 B
 	B	��B
 )B	��B	��B	��B	��B	��B	��B	��B
	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111144444441441111114114411111111111111114444444444444444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141144444444441111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444433444434443444443443444444344444444334433434444444444444433444443444433433443444334334443343443443344434443344444444434443433434434434444444443344344443344333344333443344434444334443333444333333433433343433343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
G�O�G�O�<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202009011538152020090115381520200901153815202009011538152020090115381520200901153815202009011538152020090115381520200901153815202009011538152020090115381520200901153815AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201811202122032018112021220320181120212203    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122032018112021220320181120212203  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201811202122032018112021220320181120212203  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202009011538152020090115381520200901153815  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                