CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:37Z creation      
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
resolution        =���   axis      Z        *�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  o�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     *�  zT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     *�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *�  �4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *� x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� :   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *� D�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *� oX   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �8   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *� ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *� |   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� /   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *� 9�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� d\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *� o   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173037  20200828145504  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               ,   ,   ,AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @ש� ��.@ש� ��.@ש� ��.111 @ש�����@ש�����@ש�����@5��+@5��+@5��+�cv��+�cv��+�cv��+111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    ,   ,   ,ADA BDA  DA BDA @@  @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�33A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�\D���D��=D�ФD��D�P�D���D��=D� D�F�D���D�ȤD�	�D�Z=Dړ3D�
D���D�
D�n�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�            =���        >L��>���>���    =���=���                    =���    =���=���    =���=���>���        =���    =���=���                >L��>���    =���        =���                        =���        =���            =���=���    >���>���=���    =���>���>L��            =���        =���>���=���    =���    =���    =���>L��>���        >L��=���                >L��>���=���        =���>L��    >���>���>L��    >L��>L��>L��        =���=���        >L��=���=���        =���=���        =���=���    >���        =���=���>L��=���        =���        >L��                =���                =���=���            =���=���                =���=���>L��            >L��>���?   >L��        =���>L��>L��    >L��>L��    =���>L��>L��        =���=���        =���=���    =���>L��=���=���    >L��    =���    >L��    =���>L��>���=���    =���>L��>L��=���    >L��>L��>L��        >L��>���>L��>���>���=���    >L��>���>���>���>���>���    >L��>L��>���>���>���>���>L��=���>���>���>���>L��>L��>���>���>���>L��>L��>L��>L��>���>L��>L��>L��>L��>���>���>L��>L��>���>���>���>L��>L��=���>���>���>���>���>���>���>L��>L��>L��>���>���>���>���>���=���>���>���>���>L��>���>���=���>���>���>���>���>���>L��>L��>���>���=���>���>���>L��>���>���>���>���>L��>L��>L��>���>���>L��>L��>���>���>L��>L��=���>���>���>���>���>���>���>���>���>���?   >���>���>L��=���>���>L��>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>L��?   >���>���>���>L��>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���?   >���>���>���>���?   ?   >���>���>���>���>���>���>���>���>L��=���>���>L��>L��>L��>���>���>���>���>���>���>���>���>���>L��>���>���>L��>L��>���>���>L��>L��>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>L��?   >���>���>���>���>L��>L��>L��>���?   >���>���>���>���>���>���?   >���>���?   ?   >L��>L��>L��>���>���>L��>���>���>L��>���>L��>���>L��>���>L��>L��>L��>L��>���>���>���>���>���>���>���>���>���>L��=���>L��=���>L��>L��>���>���>���>���?   ?��?L��?333?L��?fff?fff?�  ?���?���?�33?�33?�33?�33?�  ?ٙ�?�ff?�ff?�33?�33@ff@ff@��@33@��@��@   @   @,��@,��@333@9��@@  @Fff@S33@Y��@`  @fff@l��@s33@y��@�  @�33@���@���@�  @�33@�ff@���@���@�33@�33@�ff@���@���@�33@�ff@�ff@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@陚@�  @�33@�33@�ff@���@���A   A��A33A��AffA  A	��A33A��AffA  A33A��A��AffA  A33A��AffAffA   A#33A$��A$��A&ffA(  A)��A+33A,��A.ffA.ffA0  A1��A333A4��A6ffA8  A8  A9��A;33A<��A>ffA>ffA@  AA��AC33AD��AD��AFffAH  AI��AI��AK33AL��ANffANffAQ��AQ��AS33AT��AVffAVffAX  AY��A[33A\��A\��A`  Aa��Aa��Ac33Ad��Ad��AfffAh  Ai��Ai��Ak33Al��AnffAp  Ap  Aq��As33AvffAvffAx  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�33A�  A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�33A�  A���A͙�A�ffA�  A���Aљ�A�ffA�33A�  Aՙ�A�ffA�33A���Aٙ�A�ffA�  A���A�ffA�33DqFfDqS3DqY�Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq�3Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr�fDr�3Dr��Dr� Dr�fDr�3Dr��Dr� Dr�fDr��DrٚDr� Dr�fDr��Dr��Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds33Ds9�DsFfDsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt3Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDtl�Dty�Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt� Dt�f@@  @Fff@S33@Y��@`  @fff@l��@s33@y��@�  @�33@���@���@�  @�33@�ff@���@���@�33@�33@�ff@���@���@�33@�ff@�ff@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@���@�  @�33@�ff@陚@陚@�  @�33@�33@�ff@���@���A   A��A33A��AffA  A	��A33A��AffA  A33A��A��AffA  A33A��AffAffA   A#33A$��A$��A&ffA(  A)��A+33A,��A.ffA.ffA0  A1��A333A4��A6ffA8  A8  A9��A;33A<��A>ffA>ffA@  AA��AC33AD��AD��AFffAH  AI��AI��AK33AL��ANffANffAQ��AQ��AS33AT��AVffAVffAX  AY��A[33A\��A\��A`  Aa��Aa��Ac33Ad��Ad��AfffAh  Ai��Ai��Ak33Al��AnffAp  Ap  Aq��As33AvffAvffAx  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�  A���A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�33A�  A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���Ař�A�ffA�33A�  A���Aə�A�ffA�33A�33A�  A���A͙�A�ffA�  A���Aљ�A�ffA�33A�  Aՙ�A�ffA�33A���Aٙ�A�ffA�  A���A�ffA�33DqFfDqS3DqY�Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq��Dq�fDq��Dq�3Dq��Dq� Dq�fDq�3DqٚDq� Dq�fDq�3Dq��Dr  DrfDr�Dr�Dr  Dr&fDr,�Dr33Dr9�DrFfDrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr�fDr�3Dr��Dr� Dr�fDr�3Dr��Dr� Dr�fDr��DrٚDr� Dr�fDr��Dr��Ds  DsfDs�Ds3Ds  Ds&fDs,�Ds33Ds9�DsFfDsL�DsS3DsY�DsffDsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds� Ds�fDs�3Ds��Ds� Ds�fDs�3DsٚDs� Ds�fDs�3Ds��Dt  DtfDt3Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtY�Dt` DtffDtl�Dty�Dt� Dt�fDt��Dt��Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt� Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @9��@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�ffA�  A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC  C	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC  C�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D*  D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dy�qD�)D��D�
D��qD��D�MqD���D��
D��D�C�D��RD��qD�gD�W
Dڐ D��D��D��D�k�D��{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����ͽ��ͽ���0��ͽ��ͽ���=���>���>L�ν���0���0��ͽ��ͽ��ͽ��ͽ��ͽ���0��ͽ���0���0��ͽ���0���0���>L�ν��ͽ���0��ͽ���0���0��ͽ��ͽ��ͽ��ͽ���=���>�������0��ͽ��ͽ���0��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���0��ͽ��ͽ���0��ͽ��ͽ��ͽ���0���0��ͽ���>L��>L��0��ͽ���0���>L��=��ͽ��ͽ��ͽ���0��ͽ��ͽ���0���>L��0��ͽ���0��ͽ���0��ͽ���0���=���>L�ν��ͽ���=���0��ͽ��ͽ��ͽ��ͽ���=���>L��0��ͽ��ͽ���0���=��ͽ���>L��>���=��ͽ���=���=���=��ͽ��ͽ���0���0��ͽ��ͽ���=���0���0��ͽ��ͽ���0���0��ͽ��ͽ���0���0��ͽ���>L�ν��ͽ���0���0���=���0��ͽ��ͽ���0��ͽ��ͽ���=��ͽ��ͽ��ͽ��ͽ���0��ͽ��ͽ��ͽ��ͽ���0���0��ͽ��ͽ��ͽ���0���0��ͽ��ͽ��ͽ��ͽ���0���0���=��ͽ��ͽ��ͽ���=���>L��>���=��ͽ��ͽ���0���=���=��ͽ���=���=��ͽ���0���=���=��ͽ��ͽ���0���0��ͽ��ͽ���0���0��ͽ���0���=���0���0��ͽ���=��ͽ���0��ͽ���=��ͽ���0���=���>���0��ͽ���0���=���=���0��ͽ���=���=���=��ͽ��ͽ���=���>L��=���>L��>���0��ͽ���=���>L��>L��>���>���>L�ν���=���=���>L��>L��>L��>���=���0���>���>L��>���=���=���>L��>L��>L��=���=���=���=���>L��=���=���=���=���>L��>L��=���=���>L��>L��>L��=���=���0���>L��>L��>L��>L��>���>L��=���=���=���>L��>L��>L��>���>L��0���>L��>L��>L��=���>L��>L��0���>L��>L��>L��>L��>L��=���=���>L��>L��0���>L��>L��=���>L��>L��>L��>���=���=���=���>L��>L��=���=���>L��>L��=���=���0���>L��>L��>L��>���>L��>L��>���>L��>L��>���>���>L��=���0���>L��=���>L��>L��>L��>L��>���>L��>���>L��=���>L��>���>���>���>L��=���>���>���>L��>L��=���>���>L��>L��>L��>L��>L��=���>L��>L��>L��>���>���>L��>L��>���>���>���>���>���>L��>���>���>L��>���>���>���>���>���>���>���>���>���>L��=���0���>���=���=���=���>L��>L��>L��>���>���>L��>���>���>L��=���>���>L��=���=���>���>���=���=���=���>���>L��>L��>���>L��>���>���>���>���>���>���>L��>L��>L��>���>���>L��>���>L��=���>L��>L��>���>���>���>L��>���>���>���>L��=���>���>���>L��>L��>L��=���=���=���>L��>���>���>L��>���>���>���>���>���>���>���>���>���=���=���=���>L��>L��=���>���>L��=���>L��=���>L��=���>L��=���=���=���=���>L��>���>L��>L��>L��>L��>L��>���>L��=���0���=���0���=���=���>L��>L��>���>���>���?   ?333?��?333?L��?L��?fff?�  ?���?�ff?�ff?�ff?�ff?�33?���?ٙ�?ٙ�?�ff?�ff?���?���@fg@��@34@34@��@��@&fg@&fg@,��@334@9��@@  @L��@S34@Y��@`  @ffg@l��@s34@y��@�  @�fg@���@���@�  @�33@�fg@���@�  @�  @�33@�fg@���@�  @�33@�33@���@���@�  @�33@�fg@ə�@���@�  @�33@�fg@ٙ�@���@�  @�33@�fg@�fg@���@�  @�  @�33@�fg@���@���A   A��A33A��AffA  A	��A33A��AffA��A33A33A��AffA��A33A��A��AffA!��A#33A#33A$��A&ffA(  A)��A+33A,��A,��A.ffA0  A1��A333A4��A6ffA6ffA8  A9��A;33A<��A<��A>ffA@  AA��AC33AC33AD��AFffAH  AH  AI��AK33AL��AL��AP  AP  AQ��AS33AT��AT��AVffAX  AY��A[33A[33A^ffA`  A`  Aa��Ac33Ac33Ad��AfffAh  Ah  Ai��Ak33Al��AnffAnffAp  Aq��At��At��AvffAx  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�ffA�33A���A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A�33A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�ffA�33A�  A���A͙�A�33A�  A���Aљ�A�ffA�33A���Aՙ�A�ffA�  A���Aٙ�A�33A�  Aݙ�A�ffDq@ DqL�DqS4DqY�Dq` DqfgDql�Dqy�Dq� Dq�gDq��Dq�4Dq� Dq�gDq��Dq�4Dq��Dq� Dq��Dq�4DqٚDq� Dq��Dq�4Dq��Dr  DrgDr4Dr�Dr  Dr&gDr,�Dr34Dr@ DrFgDrL�DrS4DrY�DrfgDrl�Drs4Dry�Dr� Dr��Dr�4Dr��Dr� Dr��Dr�4Dr��Dr� Dr�gDr�4DrٚDr� Dr�gDr�4Dr��Ds  DsgDs�Ds�Ds  Ds&gDs,�Ds34Ds@ DsFgDsL�DsS4Ds` DsfgDsl�Dss4Dsy�Ds�gDs��Ds�4Ds��Ds� Ds��Ds�4Ds��Ds� Ds��Ds�4DsٚDs� Ds��Ds�4Ds��Dt  Dt�Dt4Dt�Dt  Dt&gDt34Dt9�Dt@ DtFgDtS4DtY�Dt` DtfgDts4Dty�Dt� Dt�gDt�4Dt��Dt� Dt�gDt��Dt��Dt� Dt�gDt��DtٚDt� @9��@@  @L��@S34@Y��@`  @ffg@l��@s34@y��@�  @�fg@���@���@�  @�33@�fg@���@�  @�  @�33@�fg@���@�  @�33@�33@���@���@�  @�33@�fg@ə�@���@�  @�33@�fg@ٙ�@���@�  @�33@�fg@�fg@���@�  @�  @�33@�fg@���@���A   A��A33A��AffA  A	��A33A��AffA��A33A33A��AffA��A33A��A��AffA!��A#33A#33A$��A&ffA(  A)��A+33A,��A,��A.ffA0  A1��A333A4��A6ffA6ffA8  A9��A;33A<��A<��A>ffA@  AA��AC33AC33AD��AFffAH  AH  AI��AK33AL��AL��AP  AP  AQ��AS33AT��AT��AVffAX  AY��A[33A[33A^ffA`  A`  Aa��Ac33Ac33Ad��AfffAh  Ah  Ai��Ak33Al��AnffAnffAp  Aq��At��At��AvffAx  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�ffA�ffA�33A���A���A���A�ffA�33A�  A���A���A���A�ffA�33A�  A���A���A�ffA�33A�33A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A�  A���A���A�ffA�33A�  A�  A���Ař�A�ffA�33A�  A���Aə�A�ffA�ffA�33A�  A���A͙�A�33A�  A���Aљ�A�ffA�33A���Aՙ�A�ffA�  A���Aٙ�A�33A�  Aݙ�A�ffDq@ DqL�DqS4DqY�Dq` DqfgDql�Dqy�Dq� Dq�gDq��Dq�4Dq� Dq�gDq��Dq�4Dq��Dq� Dq��Dq�4DqٚDq� Dq��Dq�4Dq��Dr  DrgDr4Dr�Dr  Dr&gDr,�Dr34Dr@ DrFgDrL�DrS4DrY�DrfgDrl�Drs4Dry�Dr� Dr��Dr�4Dr��Dr� Dr��Dr�4Dr��Dr� Dr�gDr�4DrٚDr� Dr�gDr�4Dr��Ds  DsgDs�Ds�Ds  Ds&gDs,�Ds34Ds@ DsFgDsL�DsS4Ds` DsfgDsl�Dss4Dsy�Ds�gDs��Ds�4Ds��Ds� Ds��Ds�4Ds��Ds� Ds��Ds�4DsٚDs� Ds��Ds�4Ds��Dt  Dt�Dt4Dt�Dt  Dt&gDt34Dt9�Dt@ DtFgDtS4DtY�Dt` DtfgDts4Dty�Dt� Dt�gDt�4Dt��Dt� Dt�gDt��Dt��Dt� Dt�gDt��DtٚDt� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�Q�A�Q�A�S�A�VA�VA�VA�\)A�ZA�ZA�ZA�ZA�ZA�XA�/AȲ-A�z�A�1AǶFA�K�A�v�A��A���Aţ�A�|�A�{AĸRAĉ7A�\)A�?}A�&�A��A��/A���AøRAß�A�7LA�bA��`AA�\)A�1'A��RA���A���A��9A���A��\A�jA�I�A�"�A�{A���A���A�jA��FA�x�A�ffA�Q�A�A�A��A��#A�ȴA���A�"�A���A�%A�~�A�{A�$�A��PA�l�A�n�A��yA�$�A�&�A���A���A�E�A�jA��A��A��A��mA��PA�&�A���A�/A�I�A���A��/A��^A��mA�n�A�A�A���A�p�A��A��;A�ZA�ĜA�|�A� �A���A�=qA�K�A���A�9XA��`A�ZA�XA��
A�\)A�ĜA�bNA�G�A��hA���A�VA�/A�r�A�bNA�r�A��;A�oA~v�A}��A|�jA{Ay��Av�AuO�Ar��An�Alv�Ak;dAj�uAjbAg�Ac|�A_%A]VAZ�RAYVAX�`AW�FAV�jAU;dAT�ASoARbAPJAN~�AM\)ALz�AJA�AG"�AEVAA��A@bNA?�#A;�A97LA6n�A5?}A4  A25?A0��A/ƨA.=qA-�mA-�^A,��A+�A+?}A*JA(�`A({A'�wA'��A&�A&E�A%�A%�wA%K�A#�A!`BA!�A �A ffA�AI�A�A��A�^A%A�\AQ�A�#A�^A�A�AXA�HA^5AO�AĜA-AoA
1A��A��A�
A�uAĜAjAE�A�A�HAZAbA�;A�PAK�A (�@�X@�"�@�bN@�^5@�v�@�7L@��@���@�S�@�O�@��;@���@�x�@��@��@��@��m@���@�E�@��@ݡ�@�O�@���@ۥ�@�z�@Լj@�I�@ѡ�@�o@�&�@�33@ȓu@�Q�@� �@��;@Ǿw@Ǖ�@�\)@Ɵ�@�b@�v�@��@�`B@�j@��w@�\)@���@��T@��@�  @�C�@��!@�n�@�-@�$�@���@�bN@�@���@�O�@���@���@���@��#@���@���@���@�7L@�1@��@��h@� �@��y@��@�G�@�Q�@��@���@�S�@�"�@�"�@��@�@���@�5?@��T@���@�r�@�b@��@� �@��@�\)@��@�~�@�=q@��@���@��@��m@�dZ@��R@�^5@�=q@�{@��`@��P@�K�@�33@��@��y@���@��+@�E�@�$�@�{@���@��@��@���@�`B@�hs@���@��7@���@��@��`@���@��/@���@���@�  @��w@���@�t�@�+@��@�"�@���@�b@� �@�
=@���@��H@�o@�\)@���@�v�@�E�@�-@�E�@�E�@�p�@�p�@�p�@��@�`B@���@��j@�I�@� �@�9X@� �@�(�@�(�@�1'@��@��;@�ƨ@��F@�;d@�
=@�@�@���@��@���@�V@���@��-@��h@�hs@�X@�?}@��@���@���@�Z@�A�@�(�@��@��
@�t�@�C�@��H@���@�^5@��@��T@��-@���@��7@�7L@��@��@�V@���@��`@��`@��u@�1'@�  @��@�
=@��R@�~�@�~�@�v�@�n�@�ff@�ff@�^5@�J@��^@�hs@�G�@�?}@�7L@�&�@��@��@��@��`@��/@���@���@�Z@�1@���@��P@�dZ@�33@�@��H@��!@�ff@�M�@�$�@��T@���@�`B@�?}@�V@��`@���@�r�@�bN@�9X@�4n@y��@q�@g�
@_�@X��@Og�@HXy@B�@=�@7@O@2Ta@.�B@)\�@$ѷ@��@/�@��@�X@H�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�7LA��/A�K�A��yA�5?A� �A���A�/A��A���A�%A���A���A��PA�C�A�~�A���A���A�;dA��DA��A���A��AǾwA�  A���A���A��7A�r�A��uA�O�A�r�A�~�A�XA���Aǲ-A��wA���A�ƨA�-A���A�1'A��A��`A�Q�A�C�A��jA��A��/A�%A�VA�O�A�I�A���A�hsA�ĜA�1A��A�bA��uA���A�v�A���A��`A��RA�^5A���A���A�O�A�l�A���A��A�$�A�G�A�VA��FA��\A���A��TA�VA���A��wAǉ7A��A���A��A�hsA��DA�G�A�JA��HA��A�VA�jA��A�VA�A� �A��A�33A��wAȼjA���A�=qA�JA��hA���A�A�=qA��/A�^5A��A�VA�33A��A�E�A��A���A�A�A�v�A��TAĬAȧ�A��hA��
A�(�A�x�A�VA�O�A�"�A��A��A�C�A���A��A��A�M�A��A�?}A�ĜA��A�z�A��RA��wA���A��TA���A���A�(�A���A�ƨA���A��A�~�A��mA�{A���A���A�\)A��A�;dA�{A� �A�bA�A�bA�A�x�A�-A�oA�l�A�1A�{A���A�{A� �A��
A�hsA�1A�oA�JA��A��#A���A���A��+A�ƨA���A���A��#A��PA���A���A�E�A��A�JA�hsA�%A�VAȺ^A���A�(�AƏ\A��A���A�(�A�{A��
A�bA��A�oA�M�A��A��A�dZA��A���A�x�A´9AȼjA�&�A�&�A�(�A�"�A�A�A�;dA�oA� �A�"�A�M�A�$�A��A���A�A�$�A�$�A�$�A���A�\)A�"�A� �AȋDA��A�{A��A�ĜA� �A��A���A�`BA�l�A�"�A�VAƝ�A�+A��A�(�A�r�A� �Aȝ�A���A� �A� �A�"�A�$�A��/A���A�VA��A���A� �A�$�A�|�A��A��!A��DA��A��A��A�bA��Aȝ�A�-A� �A�$�A�C�A�$�A�{A�t�A�M�A��A�I�A�\)A�$�AǑhA��TA�+A�&�A�$�A���AĬA��A��yA�(�A��A�&�A���A�&�A�"�AĴ9A��`A�&�A�$�A�&�A�+A�+A�"�A�&�A�+A�&�A�"�A�$�A��A�jA���A�&�A�{A��A��A��A��A� �A� �A�&�A� �AǕ�A�{A�&�A�(�A�+A�-A�ƨA�&�A�/A���A�(�A��AǶFA�&�A�VA�1'A��A�$�A�(�A��yA�1'A�-A�/A� �A��A��A��A��A�1'A� �A�$�A�$�A��A� �A�&�A�-A�5?A�+A���A�5?A�$�A�33A�-A�-A� �A�"�A���A�A�$�A�bA��mA�-A�&�A�+A�(�A�+A�&�A�+A�(�A�-A�"�A�-A�/A��mAǡ�A��A�+A�"�A�l�A���A�-A�(�A�-A�1'A�-A�-A�-A�+A�1'A�-A�  A�-A���A�-A��A�-A�+A�+A�(�A��A��/A�-A�+A�"�A�(�A�&�A�(�A�(�A�$�A�$�AƇ+A�&�A�(�A�$�AȺ^A�ȴA�1A�I�A�"�A�$�A�-A�1'A�/A�-A�1'A�+A�"�A�9XA�;dA�1'A�5?A�5?A�VAǓuA�G�A�%A�5?A�33A�5?A�7LA�$�A��A�
=A�&�A��A���A�/A� �A��A��A���A�&�A�9XA�7LA�-A��A�/A�5?A�33A�(�A�n�A��A��wA�t�A�M�A�;dA�7LA�E�A�C�A�A�A�A�A�=qA�?}A�;dA�?}A�E�A�G�A�E�A�G�A�G�A�I�A�G�A�I�A�G�A�I�A�I�A�K�A�I�A�K�A�I�A�G�A�G�A�G�A�G�A�G�A�E�A�G�A�G�A�E�A�E�A�E�A�G�A�E�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�I�A�K�A�K�A�I�A�K�A�K�A�I�A�I�A�K�A�I�A�I�A�K�A�M�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�M�A�K�A�K�A�K�A�M�A�K�A�M�A�I�A�I�A�I�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�G�A�K�A�I�A�C�A�?}A�C�A�I�A�C�A�E�A�G�A�G�A�?}A�G�A�G�A�C�A�G�A�I�A�C�A�K�A�K�A�I�A�G�A�G�A�G�A�C�A�I�A�G�A�G�A�G�A�E�A�G�A�G�A�E�A�G�A�G�A�E�A�G�A�I�A�K�A�M�A�M�A�M�A�K�A�G�A�E�A�E�A�C�A�K�A�G�A�E�A�G�A�E�A�G�A�I�A�G�A�K�A�I�A�I�A�K�A�K�A�G�A�I�A�G�A�E�A�G�A�I�A�G�A�I�A�I�A�I�A�G�A�I�A�G�A�I�A�I�A�K�A�K�A�I�A�I�A�M�A�M�A�M�A�K�A�K�A�K�A�K�A�M�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�VA�S�A�Q�A�O�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�O�A�O�A�S�A�Q�A�Q�A�Q�A�S�A�VA�VA�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�S�A�VA�S�A�S�A�S�A�S�A�S�A�S�A�VA�S�A�S�A�Q�A�S�A�VA�S�A�S�A�S�A�VA�S�A�VA�S�A�VA�VA�VA�VA�S�A�VA�VA�XA�S�A�VA�VA�VA�S�A�VA�S�A�S�A�Q�A�S�A�VA�VA�VA�S�A�S�A�VA�S�A�VA�S�A�VA�S�A�VA�VA�S�A�VA�VA�S�A�VA�VA�VA�Q�A�VA�XA�XA�XA�VA�VA�VA�VA�VA�XA�XA�VA�VA�ZA�ZA�\)A�\)A�\)A�ZA�\)@�G�@�G�@�G�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�/@�/@�/@�/@�/@�&�@��@��@��@��@��@��@�V@�V@��@��@�V@�%@�%@�%@�%@���@���@���@���@���@���@��@��@��@��@��`@��`@��/@��/@��/@��/@���@���@���@���@�Ĝ@�Ĝ@��j@��9@��@��@���@���@��D@��D@�z�@�z�@�z�@�z�@�z�@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�j@�j@�j@�r�@�j@�r�@�j@�j@�j@�j@�j@�bN@�j@�j@�bN@�j@�bN@�bN@�bN@�Z@�bN@�Z@�Q�@�I�@�I�@�I�@�I�@�I�@�A�@�I�@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�1'@�(�@�1'@�1'@�(�@�(�@� �@�(�@� �A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�K�A�I�A�I�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�I�A�M�A�M�A�K�A�M�A�K�A�M�A�M�A�M�A�K�A�I�A�M�A�K�A�M�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�K�A�I�A�K�A�K�A�K�A�I�A�K�A�K�A�E�A�I�A�K�A�I�A�G�A�E�A�G�A�E�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�K�A�M�A�I�A�I�A�I�A�I�A�G�A�I�A�G�A�E�A�E�A�G�A�G�A�G�A�E�A�G�A�G�A�E�A�I�A�K�A�M�A�M�A�M�A�K�A�K�A�G�A�G�A�G�A�I�A�K�A�G�A�I�A�G�A�E�A�G�A�I�A�M�A�K�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�G�A�I�A�G�A�K�A�K�A�K�A�K�A�I�A�K�A�M�A�Q�A�M�A�M�A�K�A�K�A�K�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�S�A�VA�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�VA�VA�S�A�VA�VA�VA�S�A�S�A�VA�S�A�S�A�Q�A�S�A�VA�S�A�VA�S�A�S�A�S�A�VA�VA�VA�VA�S�A�S�A�S�A�S�A�VA�VA�VA�S�A�VA�VA�VA�XA�VA�XA�VA�XA�VA�VA�VA�VA�VA�XA�VA�S�A�S�A�VA�VA�XA�VA�VA�VA�VA�S�A�VA�VA�VA�S�A�VA�S�A�VA�S�A�XA�XA�VA�VA�VA�VA�VA�VA�VA�XA�VA�VA�VA�XA�XA�XA�XA�VA�XA�XA�ZA�\)A�\)A�ZA�\)A�\)A�Z@�O�@�O�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�/@�&�@�&�@��@��@��@��@��@��@��@��@�V@�V@�V@�V@�%@�%@���@���@���@���@���@���@��@��@��@��@��@��`@��`@��/@��/@��/@��/@���@���@���@�Ĝ@�Ĝ@��j@��j@��9@��@��@���@��u@��D@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�bN@�j@�bN@�Z@�Z@�Z@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�@�(�@�(�@� �@�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�Q�A�Q�A�S�A�VA�VA�VA�\)A�ZA�ZA�ZA�ZA�ZA�XA�/AȲ-A�z�A�1AǶFA�K�A�v�A��A���Aţ�A�|�A�{AĸRAĉ7A�\)A�?}A�&�A��A��/A���AøRAß�A�7LA�bA��`AA�\)A�1'A��RA���A���A��9A���A��\A�jA�I�A�"�A�{A���A���A�jA��FA�x�A�ffA�Q�A�A�A��A��#A�ȴA���A�"�A���A�%A�~�A�{A�$�A��PA�l�A�n�A��yA�$�A�&�A���A���A�E�A�jA��A��A��A��mA��PA�&�A���A�/A�I�A���A��/A��^A��mA�n�A�A�A���A�p�A��A��;A�ZA�ĜA�|�A� �A���A�=qA�K�A���A�9XA��`A�ZA�XA��
A�\)A�ĜA�bNA�G�A��hA���A�VA�/A�r�A�bNA�r�A��;A�oA~v�A}��A|�jA{Ay��Av�AuO�Ar��An�Alv�Ak;dAj�uAjbAg�Ac|�A_%A]VAZ�RAYVAX�`AW�FAV�jAU;dAT�ASoARbAPJAN~�AM\)ALz�AJA�AG"�AEVAA��A@bNA?�#A;�A97LA6n�A5?}A4  A25?A0��A/ƨA.=qA-�mA-�^A,��A+�A+?}A*JA(�`A({A'�wA'��A&�A&E�A%�A%�wA%K�A#�A!`BA!�A �A ffA�AI�A�A��A�^A%A�\AQ�A�#A�^A�A�AXA�HA^5AO�AĜA-AoA
1A��A��A�
A�uAĜAjAE�A�A�HAZAbA�;A�PAK�A (�@�X@�"�@�bN@�^5@�v�@�7L@��@���@�S�@�O�@��;@���@�x�@��@��@��@��m@���@�E�@��@ݡ�@�O�@���@ۥ�@�z�@Լj@�I�@ѡ�@�o@�&�@�33@ȓu@�Q�@� �@��;@Ǿw@Ǖ�@�\)@Ɵ�@�b@�v�@��@�`B@�j@��w@�\)@���@��T@��@�  @�C�@��!@�n�@�-@�$�@���@�bN@�@���@�O�@���@���@���@��#@���@���@���@�7L@�1@��@��h@� �@��y@��@�G�@�Q�@��@���@�S�@�"�@�"�@��@�@���@�5?@��T@���@�r�@�b@��@� �@��@�\)@��@�~�@�=q@��@���@��@��m@�dZ@��R@�^5@�=q@�{@��`@��P@�K�@�33@��@��y@���@��+@�E�@�$�@�{@���@��@��@���@�`B@�hs@���@��7@���@��@��`@���@��/@���@���@�  @��w@���@�t�@�+@��@�"�@���@�b@� �@�
=@���@��H@�o@�\)@���@�v�@�E�@�-@�E�@�E�@�p�@�p�@�p�@��@�`B@���@��j@�I�@� �@�9X@� �@�(�@�(�@�1'@��@��;@�ƨ@��F@�;d@�
=@�@�@���@��@���@�V@���@��-@��h@�hs@�X@�?}@��@���@���@�Z@�A�@�(�@��@��
@�t�@�C�@��H@���@�^5@��@��T@��-@���@��7@�7L@��@��@�V@���@��`@��`@��u@�1'@�  @��@�
=@��R@�~�@�~�@�v�@�n�@�ff@�ff@�^5@�J@��^@�hs@�G�@�?}@�7L@�&�@��@��@��@��`@��/@���@���@�Z@�1@���@��P@�dZ@�33@�@��H@��!@�ff@�M�@�$�@��T@���@�`B@�?}@�V@��`@���@�r�@�bNG�O�@�4n@y��@q�@g�
@_�@X��@Og�@HXy@B�@=�@7@O@2Ta@.�B@)\�@$ѷ@��@/�@��@�X@H�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�7LA��/A�K�A��yA�5?A� �A���A�/A��A���A�%A���A���A��PA�C�A�~�A���A���A�;dA��DA��A���A��AǾwA�  A���A���A��7A�r�A��uA�O�A�r�A�~�A�XA���Aǲ-A��wA���A�ƨA�-A���A�1'A��A��`A�Q�A�C�A��jA��A��/A�%A�VA�O�A�I�A���A�hsA�ĜA�1A��A�bA��uA���A�v�A���A��`A��RA�^5A���A���A�O�A�l�A���A��A�$�A�G�A�VA��FA��\A���A��TA�VA���A��wAǉ7A��A���A��A�hsA��DA�G�A�JA��HA��A�VA�jA��A�VA�A� �A��A�33A��wAȼjA���A�=qA�JA��hA���A�A�=qA��/A�^5A��A�VA�33A��A�E�A��A���A�A�A�v�A��TAĬAȧ�A��hA��
A�(�A�x�A�VA�O�A�"�A��A��A�C�A���A��A��A�M�A��A�?}A�ĜA��A�z�A��RA��wA���A��TA���A���A�(�A���A�ƨA���A��A�~�A��mA�{A���A���A�\)A��A�;dA�{A� �A�bA�A�bA�A�x�A�-A�oA�l�A�1A�{A���A�{A� �A��
A�hsA�1A�oA�JA��A��#A���A���A��+A�ƨA���A���A��#A��PA���A���A�E�A��A�JA�hsA�%A�VAȺ^A���A�(�AƏ\A��A���A�(�A�{A��
A�bA��A�oA�M�A��A��A�dZA��A���A�x�A´9AȼjA�&�A�&�A�(�A�"�A�A�A�;dA�oA� �A�"�A�M�A�$�A��A���A�A�$�A�$�A�$�A���A�\)A�"�A� �AȋDA��A�{A��A�ĜA� �A��A���A�`BA�l�A�"�A�VAƝ�A�+A��A�(�A�r�A� �Aȝ�A���A� �A� �A�"�A�$�A��/A���A�VA��A���A� �A�$�A�|�A��A��!A��DA��A��A��A�bA��Aȝ�A�-A� �A�$�A�C�A�$�A�{A�t�A�M�A��A�I�A�\)A�$�AǑhA��TA�+A�&�A�$�A���AĬA��A��yA�(�A��A�&�A���A�&�A�"�AĴ9A��`A�&�A�$�A�&�A�+A�+A�"�A�&�A�+A�&�A�"�A�$�A��A�jA���A�&�A�{A��A��A��A��A� �A� �A�&�A� �AǕ�A�{A�&�A�(�A�+A�-A�ƨA�&�A�/A���A�(�A��AǶFA�&�A�VA�1'A��A�$�A�(�A��yA�1'A�-A�/A� �A��A��A��A��A�1'A� �A�$�A�$�A��A� �A�&�A�-A�5?A�+A���A�5?A�$�A�33A�-A�-A� �A�"�A���A�A�$�A�bA��mA�-A�&�A�+A�(�A�+A�&�A�+A�(�A�-A�"�A�-A�/A��mAǡ�A��A�+A�"�A�l�A���A�-A�(�A�-A�1'A�-A�-A�-A�+A�1'A�-A�  A�-A���A�-A��A�-A�+A�+A�(�A��A��/A�-A�+A�"�A�(�A�&�A�(�A�(�A�$�A�$�AƇ+A�&�A�(�A�$�AȺ^A�ȴA�1A�I�A�"�A�$�A�-A�1'A�/A�-A�1'A�+A�"�A�9XA�;dA�1'A�5?A�5?A�VAǓuA�G�A�%A�5?A�33A�5?A�7LA�$�A��A�
=A�&�A��A���A�/A� �A��A��A���A�&�A�9XA�7LA�-A��A�/A�5?A�33A�(�A�n�A��A��wA�t�A�M�A�;dA�7LA�E�A�C�A�A�A�A�A�=qA�?}A�;dA�?}A�E�A�G�A�E�A�G�A�G�A�I�A�G�A�I�A�G�A�I�A�I�A�K�A�I�A�K�A�I�A�G�A�G�A�G�A�G�A�G�A�E�A�G�A�G�A�E�A�E�A�E�A�G�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�K�A�I�A�I�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�I�A�M�A�M�A�K�A�M�A�K�A�M�A�M�A�M�A�K�A�I�A�M�A�K�A�M�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�K�A�I�A�K�A�K�A�K�A�I�A�K�A�K�A�E�A�I�A�K�A�I�A�G�A�E�A�G�A�E�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�K�A�M�A�I�A�I�A�I�A�I�A�G�A�I�A�G�A�E�A�E�A�G�A�G�A�G�A�E�A�G�A�G�A�E�A�I�A�K�A�M�A�M�A�M�A�K�A�K�A�G�A�G�A�G�A�I�A�K�A�G�A�I�A�G�A�E�A�G�A�I�A�M�A�K�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�G�A�I�A�G�A�K�A�K�A�K�A�K�A�I�A�K�A�M�A�Q�A�M�A�M�A�K�A�K�A�K�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�S�A�VA�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�VA�VA�S�A�VA�VA�VA�S�A�S�A�VA�S�A�S�A�Q�A�S�A�VA�S�A�VA�S�A�S�A�S�A�VA�VA�VA�VA�S�A�S�A�S�A�S�A�VA�VA�VA�S�A�VA�VA�VA�XA�VA�XA�VA�XA�VA�VA�VA�VA�VA�XA�VA�S�A�S�A�VA�VA�XA�VA�VA�VA�VA�S�A�VA�VA�VA�S�A�VA�S�A�VA�S�A�XA�XA�VA�VA�VA�VA�VA�VA�VA�XA�VA�VA�VA�XA�XA�XA�XA�VA�XA�XA�ZA�\)A�\)A�ZA�\)A�\)A�Z@�O�@�O�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�/@�&�@�&�@��@��@��@��@��@��@��@��@�V@�V@�V@�V@�%@�%@���@���@���@���@���@���@��@��@��@��@��@��`@��`@��/@��/@��/@��/@���@���@���@�Ĝ@�Ĝ@��j@��j@��9@��@��@���@��u@��D@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�bN@�j@�bN@�Z@�Z@�Z@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�@�(�@�(�@� �@�(�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�K�A�I�A�I�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�I�A�M�A�M�A�K�A�M�A�K�A�M�A�M�A�M�A�K�A�I�A�M�A�K�A�M�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�K�A�I�A�K�A�K�A�K�A�I�A�K�A�K�A�E�A�I�A�K�A�I�A�G�A�E�A�G�A�E�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�K�A�M�A�I�A�I�A�I�A�I�A�G�A�I�A�G�A�E�A�E�A�G�A�G�A�G�A�E�A�G�A�G�A�E�A�I�A�K�A�M�A�M�A�M�A�K�A�K�A�G�A�G�A�G�A�I�A�K�A�G�A�I�A�G�A�E�A�G�A�I�A�M�A�K�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�G�A�I�A�G�A�K�A�K�A�K�A�K�A�I�A�K�A�M�A�Q�A�M�A�M�A�K�A�K�A�K�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�S�A�VA�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�VA�VA�S�A�VA�VA�VA�S�A�S�A�VA�S�A�S�A�Q�A�S�A�VA�S�A�VA�S�A�S�A�S�A�VA�VA�VA�VA�S�A�S�A�S�A�S�A�VA�VA�VA�S�A�VA�VA�VA�XA�VA�XA�VA�XA�VA�VA�VA�VA�VA�XA�VA�S�A�S�A�VA�VA�XA�VA�VA�VA�VA�S�A�VA�VA�VA�S�A�VA�S�A�VA�S�A�XA�XA�VA�VA�VA�VA�VA�VA�VA�XA�VA�VA�VA�XA�XA�XA�XA�VA�XA�XA�ZA�\)A�\)A�ZA�\)A�\)A�Z@�O�@�O�@�G�@�G�@�G�@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�/@�&�@�&�@��@��@��@��@��@��@��@��@�V@�V@�V@�V@�%@�%@���@���@���@���@���@���@��@��@��@��@��@��`@��`@��/@��/@��/@��/@���@���@���@�Ĝ@�Ĝ@��j@��j@��9@��@��@���@��u@��D@��@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�z�@�r�@�r�@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�j@�bN@�j@�bN@�Z@�Z@�Z@�Z@�Z@�Q�@�I�@�I�@�I�@�I�@�A�@�A�@�A�@�A�@�A�@�9X@�9X@�1'@�1'@�1'@�1'@�1'@�1'@�(�@�(�@�(�@�(�@� �@�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=w�]=�Cl=��M=��L>(�F>� @��K@��:?�c>q�?�]�=w��=�`W=�b$=�_1=��?'�*=��a=�]?�c=���=���=�>\��@���=ߊ	>e��=UGZ=f'|=��=�y=�n�=�خ=�	>�z@�G�@��;>>N;�=;�=dO�=u�=k�=�*=�!�=���=���=�z@�c^=a��=�Z�=�3=��=�o�=���?BXO=�>�>t�@��h@�ޔ=��>�1@X�x@�ʬ=Y=u�=s#y=}�A=��=�|�>NV@v<u@��)=ގa>_?{yS=��=�/�=��@>�@^C�@��=���?�p�=��X=}�=�/0=��Q=�x>!i�@���@��=��
=���=޸R>���>
�>�[@�ی@�(�>UW>�#d@��s@���@p�`>��>.s?�zN=��>S;?ݠ�@�NQ=�-w=���>Dd�>��?t]d=p�L=[�=k&�=���=� T>��
@�خ=rQ�=��=��>-5�@�ײ=do=ch�=��=��=�U�>T�@*�=]�U=���=<�=O�;=g�u=un�=��=��a=�<�>�F?�@�=�=O�Y=g�`=V��=n��=y�*=�o�=���=��o=��D>l�X=�z�=�c�=�-#>Eu:@��1@��!@��=z��=���=���>��W?�B�>F��>FF�@��T=��>�s@��t?���>V�)>T7@=aR*=th�=���=���=��4@�Se>��@���@���=��X=�5�>11f?Je=��U=��a>#��@�n>ީ@���@�
�@��=���>�q@�@�@��@��~=ϛ>C��?�Q�@���>o��=��?$D@���@��>���@���?�`=�6>�z�@���@���@���@���@���@���>S�7@��b@��@��@0�@��U@���@$\>�+�@��I@��@��
@��g@f��@��@��@��=@_��@���?E5?�!@��
@��=@��b@C��@~�@���@��I@���@}��@��@��b@��@�u�@���?na@��@��@��b@��b@��A@���>l��@��b@��@��M@���@��@���@I@�>���@��-@��5@��@��@���@��?ߤ@��I@���@h�/@��@��?,U�?��@��I@���@ ?@��Q@��M?l�}@���@���@��s@���>��l>�@��@��U@���@
��@�)J@��s@��>�@J�@��@��Q@���@��@���@��o?� ~@���@���@��@���@��M@�|@� �?>�E@���@���@��A@��@���@��o@��M@���@��@`^@��@��o@���@��@��{@��+@��^@���@�I�@��
@��8@i0j@��
@t��@��@���@���@��E@��V@��@��o@��@��I@��@��@��R@���@��@��s@��s@���@��x@��+@��
@���@���@��@��<@��@@��{@���@��@��'@��@��{?��@���@���@��R@���@���@���@��@��@��@���@��Q@���@��@���@��Q@��@���@F�@��'@��{@��@�\}@���@���@��Q@���@���@��{@��{@��@���@��{@���@��{@���@��@��'@��{@��{@��Q@���@��?���@�@��o@��@���@���@���@��o@���@��
@���@y��@���@��@���@��w@*�@��b>s-�@���@��{@���@��D@���@���@���@��3@��@@��"@��j@��D@���@��P@��j@ �@�T?���@���@��Y@���@��P@���>�]�@mK�@��I@���@�_@���@���@���@���@Yu@���@���@��L@��3?ů:@���@��@@��@@���@at>(Q@3*�>�]�@AD�@���@� �@�y@� @���@� ~@���@���@� @� *@��@��@�2@�2@��@�2@��@��@��@��@�.@��@��@��@��@��@�C@��@��@�2@��@�2@�G@��@��@��@�C@��@��@��@��@��@��@�>@�>@��@��@��@�>@��@��@�S@��@�S@��@��@��@��@��@��@��@��@��@�O@�d@�O@�O@�d@��@�O@�d@�@��@�S@�@��@��@�d@�@�d@��@��@��@��@�d@�d@�@�d@��@�W@�S@�@�@�S@�@�@�@��@�d@�d@�t@��@��@�@�_@�t@�t@�@�@�@�@��@�!@��@��@�t@��@�!@�!@�t@��@��@��@�	l@�	�@�	�@��@�t@�!@�!@�t@��@�p@��@�!@�@�t@�@��@��@�	�@�	-@�	-@�	�@�	-@��@�	-@�	-@�	�@�	�@��@��@�	�@�	�@�	�@�	-@�	�@�	�@�	�@�	�@�
=@�	�@�	�@�	�@�
�@��@�
�@�
�@�
�@�9@��@��@��@�@��@��@�@�@�E@�Z@�Z@��@��@�Z@�E@�Z@��@�E@�Z@�Z@�E@�Z@�@��@��@��@��@��@�@�V@��@�k@��@��@�@��@��@�#@��@��@�#@��@��@�g@�@�#@��@��@�#@��@��@�#@�w@�@�#@�#@�b@�w@�w@�b@��@�w@�b@��@�@�
@��@�4@�s@��@�4@�s@��@�4@�s@��@��@��@��@��@�s@��@��@��@��@��@��@��@�E@�@�E@��@�@�E@�E@�@�E@��@�@�E@��@��@��@�@@�+@�@@�@@�@��@��@�<@��@�Q@��@��@��@��@�@�@�8@�b@��@��@�7@��@��@��@��@Q5+@Q4�@Q4�@Q4�@Q4/@Q3�@Q2�@Q2�@Q28@Q28@Q1�@Q1�@Q1<@Q0�@Q0�@Q/�@Q/�@Q.I@Q.I@Q.I@Q.I@Q.�@Q.I@Q.@Q-�@Q-�@Q-w@Q-#@Q,|@Q,|@Q,(@Q+�@Q+,@Q*�@Q*�@Q*0@Q*0@Q)�@Q)5@Q)5@Q(�@Q(9@Q'�@Q'�@Q&�@Q&�@Q&�@Q&B@Q%�@Q%F@Q$J@Q#O@Q#%@Q!�@Q!-@Q �@Q 2@Q�@Q�@Q�@QC@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@QL@Q�@QL@QL@QL@QL@Q�@QL@QL@QL@Q"@Q�@Q"@Q�@Q"@Q"@Q"@Q�@Q�@Q�@Q�@Q&@Q~@Q�@Q�@Q3@Q�@Q�@Q�@Q3@Q�@Q�@Q�@Q8@Q�@Q<@Q<@Q<@Q�@Q�@Q<@Q�@Q�@Q@@Q@@Q�@Q�@��@�@�2@�G@��@�G@��@�q@�G@��@��@��@��@��@�@��@�@�@�@�@�@�@��@�@�@�@��@��@��@��@��@��@�C@��@�S@�)@�C@��@�@�@�@�W@�C@��@�W@��@�@��@�@��@��@��@��@��@��@�C@�G@�@��@��@��@��@�C@��@�l@�C@�O@�O@��@��@��@�>@��@�)@��@��@�l@��@��@��@��@��@��@��@��@�@�@��@��@�t@��@�S@�}@��@�%@��@�%@�O@��@��@��@�O@�!@��@��@��@��@�!@�d@�5@��@�@��@�@��@��@��@�@��@��@��@�@�[@��@��@�J@�!@��@�	�@�	-@��@�F@��@��@�	@�
g@�
g@�
|@�
g@�
�@�
�@�
�@�
�@�@�N@�N@�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�$@�N@�N@�N@��@�@�@�J@�t@��@�Z@�E@�o@��@�Z@�Z@�E@�@�@�@�Z@��@��@�@�E@�@�Z@��@�E@�Z@��@��@��@��@��@��@��@��@��@�@�V@�A@�A@�A@�k@��@�V@�V@��@��@��@��@��@��@��@��@��@��@�V@�A@��@��@��@��@��@��@��@��@��@��@��@�@��@�@�@�@�g@��@��@�|@�g@��@�R@��@�#@�M@�@�M@��@�b@�w@�M@�M@�b@��@�4@��@�b@��@��@�H@��@��@Q*�@Q*�@Q*�@Q*�@Q)�@Q)5@Q(�@Q(�@Q(�@Q'�@Q'g@Q'=@Q'@Q&�@Q&B@Q%�@Q%@Q$ @Q#�@Q#�@Q#�@Q#�@Q#�@Q#�@Q#y@Q#O@Q"�@Q"}@Q")@Q!�@Q!�@Q!@Q �@Q 2@Q @Q�@Q�@Q�@Q�@Q�@Qd@Q@Q�@Qi@Q@Qm@QC@Q@Q�@QH@Q"@Q&@Q�@QU@QY@Q�@Q	@Q�@Q�@Q�@Qs@Q#@Q#@Q�@Q#@Qw@QM@Qw@QM@Q#@Q#@Q�@Q�@Q�@Q�@Q�@Q(@Q(@Q(@QR@Q(@Q(@Q(@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q,@Q�@Q�@Q�@Q�@Q�@Q5@Q9@Q@Q@Q@Q@Q
�@Q
�@Q
�@Q
g@Q	�@Q	B@Q�@Q�@Q�@Q�@Q�@Q�@QF@Q�@Q�@Q�@QJ@QtG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             444444334444444444444444344444444443344444444444344444444433443344444443344444443344444444334444443344333444444344444444444344443444444444444444444444444444444444333444444434434444444443433444444444333443334443444334344433333343334334433333333334433343333333333433333343333334433333343333344334334333344333433344333333433333334333333333333333333333333333333333333333333333333333333343333333333333333343333333333333333333333334433333333333333434333333333333333444333334333333334333343333344443333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��J@��:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�G�@��=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�c_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��h@�ޓG�O�G�O�@X�}@�ʫG�O�G�O�G�O�G�O�G�O�G�O�G�O�@v<x@��*G�O�G�O�G�O�G�O�G�O�G�O�G�O�@^C{@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@��G�O�G�O�G�O�G�O�G�O�G�O�@�ۋ@�(�G�O�G�O�@��q@���@p�fG�O�G�O�G�O�G�O�G�O�G�O�@�NMG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�خG�O�G�O�G�O�G�O�@�ײG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��/@��"@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��SG�O�G�O�@��vG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�ShG�O�@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@�
�@��G�O�G�O�@�@�@��@�ƂG�O�G�O�G�O�@���G�O�G�O�G�O�@���@��G�O�@���G�O�G�O�G�O�@���@���@���@���@���@���G�O�@��b@��@��G�O�@��V@���G�O�G�O�@��H@��@��@��l@f��@��@��@��>@_��@���G�O�G�O�@��
@��=@��bG�O�@~�@���@��H@���@}�@��@��d@��@�u�@���G�O�@��@�� @��_@��e@��B@���G�O�@��b@��@��O@���@��@���G�O�G�O�@��,@��7@��@��@���@��G�O�@��N@���@h�.@��@��G�O�G�O�@��J@���G�O�@��S@��QG�O�@���@���@��t@���G�O�G�O�@��@��T@���G�O�@�)L@��v@��G�O�G�O�@��@��S@���@���@���@��oG�O�@���@���@��@���@��N@�z@� �G�O�@���@���@��>@��@���@��r@��L@���@��@`^@���@��n@���@��@��z@��*@��_@���@�I�@��
@��8@i0n@��
@t��@��@���@���@��E@��Q@��@��n@��@��M@��@��@��V@���@��@��s@��z@���@��w@��.@��
@���@���@��@��=@��A@��~@���@��@��%@��!@��|G�O�@���@���@��P@���@���@���@���@��@���@���@��P@���@��@���@��Q@���@���G�O�@��*@��{@��@�\|@��@���@��Q@���@���@��z@��z@��@��@��|@���@��|@���@��@��(@��z@��z@��S@���@��G�O�G�O�@��n@��@���@���@���@��t@���@��@���@y��@���@��@���@��wG�O�@��bG�O�@���@��z@���@��G@���@���@���@��2@��B@�� @��k@��F@���@��O@��hG�O�G�O�G�O�@���@��X@���@��P@���G�O�@mK�@��F@���@�_@���@���@��@���G�O�@���@���@��M@��/G�O�@���@��>@��>@���@avG�O�G�O�G�O�G�O�@���@� �@�t@� @���@� ~@���@���@� @� ,@��@��@�1@�6@��@�2@��@��@��@��@�1@��@��@��@��@��@�C@��@��@�.@��@�2@�J@��@��@��@�C@��@�@�5@�F@��@�G@��@�s@�L@��@��@��@��@��@�@��@�@�@�@�@�@�@��@�@�@�@��@��@��@��@��@��@�F@��@�R@�)@�B@��@�@�@�@�Y@�@@��@�Z@��@�@��@�@�@�@��@��@� @��@�F@�F@�@��@��@��@��@�J@��@�l@�F@�Q@�N@��@��@��@�@@��@�)@��@��@�k@��@�@��@��@��@�@��@��@�@�@��@��@�y@��@�U@��@��@�"@��@�'@�M@��@��@��@�K@�"@��@��@��@��@�"@�a@�4@��@�@��@�@��@��@��@�@��@��@��@�@�\@��@��@�J@�&@��@�	�@�	.@��@�D@��@��@�	@�
i@�
i@�
z@�
i@�
�@�
�@�
�@�
�@�@�M@�M@�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�'@�P@�K@�K@��@�@�@�I@�v@��@�\@�H@�n@��@�^@�\@�I@�@�	@�@�Z@��@��@�@�G@�@�Y@��@�E@�V@��@��@��@��@��@��@��@��@��@�@�W@�@@�B@�@@�n@��@�V@�T@��@��@��@��@��@��@��@�@��@��@�Z@�D@��@��@��@��@��@��@��@��@��@��@��@�@��@�@�@�@�i@��@��@�|@�k@��@�V@��@�%@�L@�@�N@��@�d@�z@�O@�N@�c@��@�8@��@�g@��@��@�G@��@��@Q*�@Q*�@Q*�@Q*�@Q)�@Q)3@Q(�@Q(�@Q(�@Q'�@Q'j@Q'@@Q'@Q&�@Q&E@Q%�@Q%@Q$"@Q#�@Q#�@Q#�@Q#�@Q#�@Q#�@Q#z@Q#R@Q"�@Q"{@Q"&@Q!�@Q!}@Q!@Q �@Q 2@Q @Q�@Q�@Q�@Q�@Q�@Qc@Q@Q�@Qj@Q@Qm@Q@@Q@Q�@QH@Q&@Q%@Q @QS@QZ@Q�@Q@Q�@Q�@Q�@Qs@Q%@Q&@Q�@Q%@Qv@QN@Qx@QN@Q(@Q"@Q�@Q�@Q�@Q�@Q�@Q*@Q&@Q&@QS@Q*@Q-@Q&@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q*@Q�@Q�@Q�@Q�@Q�@Q3@Q;@Q@Q@Q@Q@Q
�@Q
�@Q
�@Q
h@Q	�@Q	B@Q�@Q�@Q�@Q�@Q�@Q�@QH@Q�@Q�@Q�@QJ@Qv@��@�@�5@�F@��@�G@��@�s@�L@��@��@��@��@��@�@��@�@�@�@�@�@�@��@�@�@�@��@��@��@��@��@��@�F@��@�R@�)@�B@��@�@�@�@�Y@�@@��@�Z@��@�@��@�@�@�@��@��@� @��@�F@�F@�@��@��@��@��@�J@��@�l@�F@�Q@�N@��@��@��@�@@��@�)@��@��@�k@��@�@��@��@��@�@��@��@�@�@��@��@�y@��@�U@��@��@�"@��@�'@�M@��@��@��@�K@�"@��@��@��@��@�"@�a@�4@��@�@��@�@��@��@��@�@��@��@��@�@�\@��@��@�J@�&@��@�	�@�	.@��@�D@��@��@�	@�
i@�
i@�
z@�
i@�
�@�
�@�
�@�
�@�@�M@�M@�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�
�@�'@�P@�K@�K@��@�@�@�I@�v@��@�\@�H@�n@��@�^@�\@�I@�@�	@�@�Z@��@��@�@�G@�@�Y@��@�E@�V@��@��@��@��@��@��@��@��@��@�@�W@�@@�B@�@@�n@��@�V@�T@��@��@��@��@��@��@��@�@��@��@�Z@�D@��@��@��@��@��@��@��@��@��@��@��@�@��@�@�@�@�i@��@��@�|@�k@��@�V@��@�%@�L@�@�N@��@�d@�z@�O@�N@�c@��@�8@��@�g@��@��@�G@��@��@Q*�@Q*�@Q*�@Q*�@Q)�@Q)3@Q(�@Q(�@Q(�@Q'�@Q'j@Q'@@Q'@Q&�@Q&E@Q%�@Q%@Q$"@Q#�@Q#�@Q#�@Q#�@Q#�@Q#�@Q#z@Q#R@Q"�@Q"{@Q"&@Q!�@Q!}@Q!@Q �@Q 2@Q @Q�@Q�@Q�@Q�@Q�@Qc@Q@Q�@Qj@Q@Qm@Q@@Q@Q�@QH@Q&@Q%@Q @QS@QZ@Q�@Q@Q�@Q�@Q�@Qs@Q%@Q&@Q�@Q%@Qv@QN@Qx@QN@Q(@Q"@Q�@Q�@Q�@Q�@Q�@Q*@Q&@Q&@QS@Q*@Q-@Q&@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q*@Q�@Q�@Q�@Q�@Q�@Q3@Q;@Q@Q@Q@Q@Q
�@Q
�@Q
�@Q
h@Q	�@Q	B@Q�@Q�@Q�@Q�@Q�@Q�@QH@Q�@Q�@Q�@QJ@QvG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             444444334444444444444444344444444443344444444444344444444433443344444443344444443344444444334444443344333444444344444444444344443444444444444444444444444444444444333444444434434444444443433444444444333443334443444334344433333343334334433333333334433343333333333433333343333334433333343333344334334333344333433344333333433333334333333333333333333333333333333333333333333333333333333343333333333333333343333333333333333333333334433333333333333434333333333333333444333334333333334333343333344443333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�~9�~f9�~�9�~�9�~�9�~�9�~�9�~�9�~�9�~�9�~N9�*9�+9� 9�o9�A9�p9��9��9��9�m9��9�?9��9�q9��9��9��9��
9��K9��u9��!9��9��9���9���9��9�Y9��9��9�o9��9��9��9��9��69���9��u9���9��z9��z9�?9�%9��w9��K9��9�~�9�p9�9��99��9��59��9��!9��9��9���9���9��m9��9��(9���9��9���9��29��59��9��29��z9��u9��9��u9��{9��`9��S9���9���9��P9��Z9��9��]9���9���9��L9���9��s9���9���9��<9��c9��D9���9���9��9��19��q9��*9���9���9���9��_9���9��_9���9��19��s9���9���9��19��_9��59���9��9���9��<9���9���9��m9���9���9���9���9��t9��]9���9��.9��.9��@9��.9���9��p9��p9���9���9��9��9���9���9���9���9���9���9���9���9���9���9���9���9���9��"9��9��9��f9���9���9��)9��Y9���9��L9��79��_9���9��N9��L9��89��	9���9��9��J9���9��v9��9��69��9��I9���9��49��F9���9��t9���9��w9���9���9���9���9���9��9��U9��=9��?9��=9��m9���9��T9��R9���9���9���9���9���9���9���9��9���9���9��X9��A9���9���9���9���9���9���9���9���9���9���9���9��9���9��9��9��9��w9���9���9���9��y9���9��b9��9��=9��f9��&9��h9���9���9���9��j9��h9��9���9��`9��9���9���9���9���9��9��)9\��9\��9\��9\�q9\��9\�9\��9\��9\�_9\��9\�+9\��9\��9\��9\��9\�A9\�9\�9\�]9\�]9\�Z9\�W9\�W9\�.9\�9\��9\��9\��9\�9\�F9\��9\�m9\�9\�9\�]9\�9\��9\��9\�$9\��9\�9\�H9\��9\�9\�H9\�9\�`9\�89\��9\�Z9\�(9\�9\��9\�:9\�39\�P9\��9\�u9\�b9\�Q9\��9\ܖ9\ܗ9\�g9\ܖ9\��9\��9\��9\��9\ܙ9\ܓ9\�@9\�9\�i9\��9\�19\ۍ9\ۉ9\ۉ9\۸9\ۍ9\ې9\ۉ9\�9\�	9\�[9\�39\�09\�_9\�39\�9\��9\��9\��9\��9\��9\�k9\�e9\�59\�59\�:9\�59\�9\ֳ9\��9\ֆ9\��9\�P9\��9\ԡ9\ԟ9\��9\��9\��9\�H9\Ӗ9\Ӕ9\Ӕ9\�;9\�jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�`B
VB
bNB
� B
��B
�wB
��B
��B\B�B+B9XBF�BS�BcTBgmBl�Bo�Bw�B~�B|�Bz�Bz�B{�B�1B��B��B�B�B�3B��B��B5?B;dB=qBE�BL�BXB]/BcTBk�By�B�PB�uB�{B��B��B��B�-B�3B�LB��B�)B�ZB�fB�sB�B�B�B�yB�mB�fB�ZB�5B�B��BȴB�^B�-B��B��B��B�\Bw�B[#BH�BB�B8RB+B!�B�B�BoBJB1BB��B�B�;B��B��B�bB�BiyB]/BW
BM�B=qB$�BPB%B
�B
�)B
��B
�wB
�3B
��B
�B
t�B
dZB
\)B
Q�B
F�B
>wB
7LB
.B
�B
B	��B	�ZB	�XB	��B	��B	�JB	�B	_;B	49B	uB	B��B�B�B�B�sB�ZB�BB�#B�B��B��BƨBB�RB�B��B��B��B��B��B�uB�hB�\B�VB�bB�VB�7B�%B�B�B�B}�B{�Bw�Bw�Bw�Bw�Bw�Bv�Bu�Bx�Bw�Bu�Bp�BffBffBe`BdZBbNB_;B]/BZBXBW
BVBT�BR�BQ�BP�BP�BO�BN�BM�BJ�BH�BQ�BR�B[#B`BBffBgmBhsBffBiyBo�Bv�Bu�Bs�Br�Br�Bq�Bo�BhsB]/BT�BJ�BD�B?}B<jB;dB;dB9XB:^B;dBC�BF�BJ�BL�BN�BS�BT�BW
BXBXBXBW
BS�BI�B=qB9XB6FB5?B8RB;dB@�B@�B@�B@�B@�B?}B>wB<jB9XB:^B<jB>wB@�BC�BD�BE�BF�BG�BJ�BL�BN�BN�BN�BN�BN�BO�BP�BS�BT�BXBZBZB^5B_;B_;B_;B`BB`BB^5B_;B`BBdZBhsBk�Bo�Bq�Bt�Bv�Bw�Bw�Bw�Bw�Bz�B|�B|�B}�B�B�1B�7B�=B�JB�PB�VB�bB�oB��B��B��B��B��B�B�B�!B�9B�^B��B��BĜBĜBǮBȴB��B��B�
B�
B�#B�5B�HB�`B�mB�B�B�B��B��B��B	B	B	+B	1B	bB	�B	�B	 �B	&�B	,B	1'B	8RB	?}B	@�B	A�B	D�B	J�B	M�B	O�B	P�B	P�B	S�B	T�B	VB	XB	_;B	aHB	cTB	gmB	m�B	p�B	s�B	r�B	u�B	y�B	{�B	|�B	}�B	�B	�JB	�PB	�VB	�VB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�?B	�FB	�FB	�LB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�wB	�}B	��B	ĜB	ŢB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�BB	�BB	�HB	�HB	�TB	�ZB	�ZB	�`B	�B	�B
)B
�B
!B
&�B
/OB
6�B
>(B
A�B
J	B
O�B
R�B
Y1B
]dB
a�B
f2B
j�B
n�B
tB
w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�u>���>�`�>�G�?^1/@'��B	��B	��@T�?PXG@�>��7>��u>�P�>͉H?�U@jm�>�4�?~�A8F�>�F�>���>��	?�*�B
"�?�o?��W>�S�>�.�>��	>�3>�4U>�p�? ة?;#6A�B
M�?Ga�?�:>m�>��>�X�>�V>�ײ>Ɣ�>�G�>�@?��B�U>��@>�%�>�w�?�1>��>���@��"?e?�:�B	�8B	Ǜ>�"S?B��A�u�B
/�>���>��>�;>�݃>�^�?�?��NA��B��?4?EP@���>�{�>�ZK>��?2TA�iYB
+�?!�@���>�v>�y�>��i>��>�"O?Q�hB	�FB5B>�3{>ɷ�?ˣ@p�?2�g?�]�B	��B	?�p�?ԤB	�B	�A�">?-��?+iA�r>���?/ A#�B	�*>�F�>�u�?�v�?7�@�ۃ>��u>��}>��>�^?��?Ɛ�B	��>��>���>�ׄ?]��B
#�>��x>�� >�E>�AW?�[?��kAx��>��>�j�>o�6>�`g>��u>� �>�2>�[�>�^p?6�[@ְI>�m">�KU>���>�s>��>��f>���>�Y\>��_?Yu?��>�b�?aJ?J�?�(B	ѸB	�B	��>�\�>�+�>� �@�K@�(�?���?��B	�$?G$?C�B	�@�*G?��~?5F,AmWZ>�mU>�z�?��>���>�A҃*?D2fA�GRA�..>��? �?c�7@��>�r>�?R��A^bt?HgB g�A��nB	�V>�%S?5~B��B	�"B	��?
?~%@롥B	ז?�0?��@a�B	ҼB	�?���B	�-@>��?Z�?� .A�nB	��B	ӄB	ӝB	�A�)�?�hMB	�B	��B	�0A�Y�B	ջB	��A�D�?��B	��B	�hB	ӕB	��A�=�B	�}B	�DB
}A���B	��@��@�_-B	�-B	�qA��$A�:�A�*NB	�:B	�kB
{A�MB	��B	�tB
�B	`�B
z@��GB	��B	�aB	��B	�B	�OB	�w?�d9B	؀B
��B	�B	�%B
�B	՗A���?��ZB	�BB	ԝB	��B	�@B	ԃB
@8��B	�gB	ѺA�E�B	�AB	ש@q�A*`�B	�.A�dXAP��B	֣B
m
@��B	��B	�lB	��B	��@��?C�IA���B	�,B	؆AU��BыB	�7B	�	@#o�AJ	BB	��B	֣B	�AB	ӋB	��B	ծA#��B	��B	ԀB	�B	�zB	�>A�PA���@��B	��B	��B	��B	�B	֢B	�vB	�vB	ԀB	�A���B	�B	�B	ԿB	��B	ӤB	�xB��B	�`B�B	�B	��A�;0B	��A�R�B	��B	�KB	�VB	�1B	��B	�5B	��B	�WB	�^B	�"B	�6B	�^B	��B	ҜB	ՅB	��B	�7B	�'B	�B	��B	ՐB	��B	��B	��B	� B	��B	�IB	�+B	�VB	� B	ג@�DA�ϠB	�DB	��A�z�B	�
B	ӫB	ӋB	��B	ӃB	�9B	�MB	ԘB	�[B
��B	ӎB	��B
s[A�C@B	�B	�kB	�BݶB	�NB	�B	�B	��B	�B	ӤB	ӬB	�+B	��B	�B	��B	��B	��B	�AB	�_B	�B	ӤB	�EB	��B	��@���ArM�B	��B	�+B	�B	ԿB	�dB	�WB	ӝB	ӕB	�A�E}B	�IB	��B	�DB	��A��=B	��?���B	�&B	��B	ՐB	ԟB	�B	�8B	��B	�B	�B	�.B	��B	ԧB	�eB	��B	��Ab�zAek�@�SmB	�dB	��B	էB	�4B	��?�vdA��=B	�B	�<A�	1B	רB	دB	�DB	��Af��B	��B	֙B	�$B	�BA�%B	��B	�B	��B	�A�@?V�(A�v�@�$A���B	��B	�QB	ӊB	�B	ӎB	�!B	��B	�!B	�B	ԂB	ԢB	��B	��B	�B	�\B	�7B	ԚB	��B	ԚB	��B	�B	ӘB	�_B	��B	өB	�B	��B	�B	�cB	ӭB	�'B	ӥB	ӸB	ԼB	ԩB	��B	�B	��B	��B	��B	�*B	�!B	��B	�^B	�VB	�B	ԦB	��B	�fB	ԆB	ӶB	�aB	�mB	�B	ӕB	ԚB	ԚB	��B	ҭB	�4B	�#B	�B	�B	��B	��B	��B	��B	��B	�hB	ӭB	��B	�KB	�.B	�lB	�B	өB	�hB	�FB	��B	�6B	�{B	�{B	�sB	ӣB	�NB	��B	��B	��B	՝B	��B	�?B	ӎB	��B	�_B	�=B	�5B	�CB	ԷB	�jB	��B	�UB	��B	�B	�@B	ӒB	�eB	�B	ԳB	ԳB	�:B	��B	�MB	ԥB	�<B	��B	��B	�,B	�LB	�|B	��B	��B	��B	�B	��B	�.B	�&B	ӃB	�B	�;B	��B	�?B	�GB	��B	ԵB	��B	��B	�B	��B	��B	ՇB	��B	��B	��B	�{B	�%B	�^B	��B	դB	ְB	��B	�vB	�>B	�B	��B	��B	�jB	��B	կB	��B	�8B	ԶB	�iB	�(B	� B	�qB	�B	�iB	��B	� B	�]B	սB	�;B	ԌB	ԗB	�AB	�sB	�B	�vB	ԨB	ԳB	��B	�B	�YB	��B	�wB	�JB	��B	�_B	�1B	�)B	�FB	�B	��B	�{B	�MB	�=B	�oB	�eB	ԲB	շB	�B	�+B	�$B	�/B	�uB	�zB	ԄB	��B	�'B	�2B	�xB	�B	�B	ԱB	�CB	�OB	��B	�B	�6B	�B	��B	�B	�cB	��B	�B	�B	�oB	�:B	�:B	��B	�wB	�ZB	�B	�^B	իB	��B	�NB	��B	��B	�iB	��B	��B	�B	�}B	��B	��B	�#B	��B	��B	��B	ԿB	ՒB	��B	ջB	�yB	�8B	իB	ԡB	�GB	�B	��B	ՕB	�B	��B	�B	շB	�B	�B	��B	��B	�
B	�aB	�B	֌B	թB	մB	լB	�uB	��B	ՂB	մB	�rB	�vB	֨B	�B	֫B	֐B	�4B	�$B	�
B	�(B	׺B	��B	�&B	�cB	٨B	��B	�*B	�nB	�B	��B	�B	�tB	��B	�tB	�ZB	��B	��B	�B	�PB	��B	�B	�cB	�B	�B	�B	�B	�B	�B	�B	�rB	�FB	��B	��B	�B	�\B	��B	��B	�|B	�B	�B	�rB	�'B	��B	��B	�B	� B	��B	�kB	�0B	��B	�B	�B	�	B	�B	�vB	��B	�B	��B	�9B	� B	�B	�B	�B	��B	�NB	��B	�;B	��B	�|B	�oB	�bB	�UB	�HB	�B	� B	�B	�B	�pB	�cB	�VB	��B	�B	��B	�B	�B	�B	�B	�xB	�kB	�^B	�2B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�YB	�)B	��B	�B	��B	�B	�B	�eB	�
B	�B	�B	�{B	��B	��B	�B	�B	�GB	�JB	�jB	�B	��B	�B	�B	�(B	�,B	�@B	҅B	��B	��B	�B	��B	�B	��B	�~B	��B	� B	��B	�B	��B	�+B	��B	�B	�&B	�B	�B	��B	�=B	ҊB	�%B	�
B	��B	сB	�@B	�pB	ѢB	��B	�2B	ґB	�4B	ҴB	ѾB	�pB	�B	�9B	�1B	�B	ћB	�?B	ѩB	�JB	��B	� B	�B	��B	��B	��B	�,B	сB	��B	�lB	ҤB	�{B	�ZB	��B	�(B	�B	�B	�kB	��B	�yB	�JB	ҖB	��B	��B	җB	ҢB	�BB	�fB	�B	�|B	�<B	��B	�dB	ҞB	ҖB	�B	҅B	�}B	�)B	�gB	��B	��B	҄B	ѧB	�B	�gB	ґB	ҰB	�B	�sB	�YB	�2B	҉B	��B	��B	ҹB	�pB	ћB	�B	ҥB	�B	ҪB	�BB	�SB	�B	ҫB	��B	қB	��B	�lB	�]B	ҡB	ҴB	�B	�jB	�
B	��B	�B	҅B	�KB	ҽB	��B	ђB	��B	��B	ҐB	ҵB	�B	�B	��B	�4B	�,B	�7B	�B	�aB	�EB	�B	сB	��B	��B	��B	�uB	�B	�>B	�6B	�.B	�%B	�B	��B	�B	��B	�B	��B	��B	�=B	�4B	�4B	�^B	��B	��B	��B	�JB	ңB	�B	��B	�JB	ҐB	��B	�B	�B	��B	ҀB	ҋB	��B	��B	��B	�2B	҉B	ѓB	ҌB	�0B	�hB	�sB	ҮB	��B	��B	ѺB	��B	ҿB	ҶB	ҮB	ҦB	�B	�JB	�/B	��B	�B	�=B	ҖB	�RB	�B	ѫB	�CB	�sB	�FB	�QB	�B	�-B	�B	�KB	�0B	ҏB	�sB	��B	��B	�5B	��B	��B	�
B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	сB	џB	�_B	�CB	�(B	�FB	�B	ҟB	ҽB	�B	ҙB	��B	��B	��B	�
B	��B	��B	ҝB	�B	҇B	�B	ԹB	��B	ՏB	�mB	֒B	�}B	��B	ߕB	��B	�lB	��B	�lB	�!B	��B	��B	�DB	��B	�B	��B	�ZB	��B	�ZB	�B	�LB	��B	��B	��B	��B	�B	��B	�kB	�?B	��B	�}B	�CB	��B	�B	�UB	�
B	�B	�xB	�-B	�B	�B	�oB	�CB	��B	�B	�sB	�,B	��B	�YB	�-B	�B	�B	�aB	�B	��B	�B	�B	�aB	��B	�TB	�B	�B	�NB	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�B	�8B	�B	�MB	�;B	�^B	�B	�B	�tB	�B	�MB	�@B	�2B	��B	�B	��B	�B	�B	�B	�~B	�B	�B	�B	�B	�yB	�MB	�ZB	�B	�kB	�^B	�QB	�GB	�B	��B	��B	�B	�B	�B	�PB	�B	�
B	�B	�B	�B	�B	�B	�B	��B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444444334444444444444444344444444443344444444444344444444433443344444443344444443344444444334444443344333444444344444444444344443444444444444444444444444444444444333444444434434444444443433444444444333443334443444334344433333343334334433333333334433343333333333433333343333334433333343333344334334333344333433344333333433333334333333333333333333333333333333333333333333333333333333343333333333333333343333333333333333333333334433333333333333434333333333333333444333334333333334333343333344443333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�B	�B	�kB
`B
bVB
�B
�B
�B
� B
��BdB�B+
B9aBF�BTBc]BgtBl�Bo�Bw�BB|�Bz�Bz�B{�B�9B��B��B�B�B�;B�B��B5JB;mB=zBE�BL�BXB]7Bc^Bk�By�B�[B�~B��B��B��B��B�4B�<B�SB��B�0B�bB�lB�~B�B�B�B�B�xB�pB�bB�=B�B��B��B�iB�9B��B��B��B�fBw�B[,BH�BB�B8ZB+	B!�B�B�BvBUB7B"B��B�B�BB��B��B�kB�Bi�B]7BWBM�B=yB$�BYB0B
�B
�4B
��B
��B
�?B
��B
�B
t�B
dfB
\1B
Q�B
F�B
>�B
7UB
.B
�B
'B	��B	�dB	�aB	��B	��B	�UB	�B	_DB	4DB	~B	(B��B��B�B�B�~B�cB�MB�-B�B��B��BƱBB�^B�B��B��B��B��B��B��B�uB�eB�`B�nB�bB�BB�2B�,B�$B�B}�B{�Bw�Bw�Bw�Bw�Bw�Bv�Bu�Bx�Bw�Bu�Bp�BftBfpBelBdfBb[B_FB]=BZ'BXBWBVBU
BR�BQ�BP�BP�BO�BN�BM�BJ�BH�BQ�BS B[/B`OBfrBgyBh~BfuBi�Bo�Bv�Bu�Bs�Br�Br�Bq�Bo�Bh~B]<BU
BJ�BD�B?�B<wB;qB;oB9dB:iB;pBC�BF�BJ�BL�BN�BTBU
BWBXBXBXBWBTBI�B=|B9eB6TB5KB8_B;pB@�B@�B@�B@�B@�B?�B>�B<vB9fB:kB<wB>�B@�BC�BD�BE�BF�BG�BJ�BL�BN�BN�BN�BN�BN�BO�BP�BTBUBXBZ)BZ*B^@B_HB_HB_HB`OB`OB^AB_FB`OBdeBh�Bk�Bo�Bq�Bt�Bv�Bw�Bw�Bw�Bw�Bz�B|�B|�B~ B�'B�>B�EB�LB�WB�^B�cB�oB�|B��B��B��B��B��B�B�B�.B�EB�jB��B��BĨBĩBǺB��B��B�B�B�B�0B�EB�UB�mB�zB�B�B�B��B��B��B	B	+B	9B	=B	oB	�B	�B	 �B	&�B	,B	14B	8_B	?�B	@�B	A�B	D�B	J�B	M�B	O�B	P�B	P�B	TB	UB	VB	XB	_HB	aUB	cbB	gzB	m�B	p�B	s�B	r�B	u�B	y�B	{�B	|�B	~B	�+B	�VB	�_B	�bB	�cB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�6B	�8B	�@B	�EB	�NB	�QB	�UB	�WB	�dB	�eB	�eB	�gB	�kB	�mB	�lB	�rB	��B	��B	��B	ĩB	ŮB	ǽB	ǻB	ǻB	ǺB	ǻB	ǻB	ǼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�&B	�*B	�.B	�6B	�7B	�?B	�AB	�LB	�RB	�WB	�UB	�aB	�eB	�fG�O�B	�B	�"B
7B
�B
!!B
&�B
/\B
6�B
>6B
A�B
JB
O�B
R�B
Y>B
]oB
a�B
f>B
j�B
n�B
t)B
w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��B
M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�AB	ǡG�O�G�O�A�u�B
/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�iaB
,G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�MB5KG�O�G�O�G�O�G�O�G�O�G�O�B	��B	G�O�G�O�B	�B	�
A�"LG�O�G�O�G�O�G�O�G�O�G�O�B	�0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��G�O�G�O�G�O�G�O�B
#�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	ѿB	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�*G�O�G�O�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A҃:G�O�A�GcA�.<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B g�A��yB	�^G�O�G�O�B��B	�*B	��G�O�G�O�G�O�B	מG�O�G�O�G�O�B	��B	�G�O�B	�6G�O�G�O�G�O�A�uB	��B	ӌB	ӤB	�A�)�G�O�B	�B	��B	�8G�O�B	��B	��G�O�G�O�B	��B	�qB	ӟB	�A�=�B	ӄB	�MB
�A���B	�G�O�G�O�B	�4B	�xA��0G�O�A�*]B	�BB	�rB
{A�[B	��B	�|B
�B	`�B
�G�O�B	��B	�hB	��B	�B	�WB	�}G�O�B	؇B
��B	ԆB	�.B
�B	՞G�O�G�O�B	�IB	ԧB	��B	�IB	ԊB
G�O�B	�qB	��A�E�B	�IB	װG�O�G�O�B	�7A�dbG�O�B	֭B
mG�O�B	��B	�tB	�B	��G�O�G�O�A��B	�2B	؎G�O�BѕB	�AB	�G�O�G�O�B	��B	֭B	�IB	ӓB	��B	նG�O�B	��B	ԊB	�"B	�B	�GA�_A���G�O�B	��B	��B	��B	�B	֫B	րB	�|B	ԊB	�A���B	�B	�%B	��B	��B	ӬB	�B��B	�hB�B	�B	��A�;=B	��A�R�B	��B	�RB	�]B	�:B	��B	�;B	��B	�^B	�hB	�,B	�>B	�hB	�B	ҦB	ՍB	�B	�@B	�.B	�&B	��B	ՖB	��B	��B	��B	�	B	��B	�SB	�2B	�\B	�+B	ךG�O�A�ϩB	�MB	��A�{B	�B	ӲB	ӓB	��B	ӋB	�AB	�VB	ԡB	�cB
��B	ӔB	��B
sbG�O�B	�B	�sB	�BݾB	�WB	� B	�B	��B	�
B	ӬB	ӳB	�2B	�B	�B	��B	��B	��B	�JB	�gB	�B	ӬB	�PB	�B	��G�O�G�O�B	��B	�2B	�B	��B	�kB	�aB	ӤB	ӟB	�"A�E�B	�RB	��B	�MB	��G�O�B	��G�O�B	�,B	��B	ՖB	ԨB	�B	�>B	�B	�	B	�B	�4B	��B	԰B	�nB	��B	��G�O�G�O�G�O�B	�jB	�B	լB	�;B	��G�O�A��JB	� B	�DA�	@B	ׯB	ضB	�NB	��G�O�B	��B	֡B	�-B	�GG�O�B	��B	�B	��B	�A�@+G�O�G�O�G�O�G�O�B	��B	�WB	ӑB	�
B	ӖB	�)B	�B	�)B	�B	ԊB	ԭB	��B	��B	�!B	�cB	�@B	ԣB	��B	ԣB	��B	�B	ӠB	�gB	��B	ӰB	�B	��B	�B	�lB	ӱB	�1B	ӯB	��B	��B	ԲB	��B	ԈB	�KB	ҏB	��B	��B	�B	��B	�	B	��B	҈B	��B	�(B	��B	�	B	��B	�3B	��B	�$B	�+B	�B	�B	�B	�DB	ґB	�/B	�B	��B	чB	�KB	�xB	ѪB	��B	�9B	ҚB	�:B	һB	��B	�vB	�$B	�AB	�9B	�B	ѣB	�FB	ѱB	�TB	��B	�(B	�B	��B	��B	��B	�2B	цB	��B	�tB	ҮB	ҁB	�dB	��B	�1B	�B	�'B	�vB	��B	ҀB	�SB	ҟB	��B	��B	ҝB	ҫB	�KB	�mB	�$B	҂B	�EB	��B	�kB	ҧB	ҝB	�B	ҌB	҈B	�0B	�pB	��B	��B	ҋB	ѭB	�
B	�pB	қB	ҸB	�B	�yB	�`B	�;B	ґB	��B	��B	��B	�vB	ѣB	�B	ҭB	�B	ҴB	�JB	�YB	�B	ҴB	��B	ңB	��B	�tB	�dB	ҫB	һB	�B	�rB	�B	��B	�
B	ҏB	�UB	��B	��B	ѝB	��B	��B	ҘB	һB	�!B	�B	��B	�<B	�6B	�=B	�$B	�iB	�MB	�B	чB	�B	��B	��B	�}B	�)B	�GB	�@B	�4B	�.B	�$B	��B	�B	��B	�B	��B	��B	�GB	�9B	�9B	�iB	��B	��B	��B	�SB	ҭB	�B	��B	�RB	ҘB	��B	�)B	�B	��B	҈B	ҖB	��B	��B	��B	�9B	ҒB	ќB	ҒB	�9B	�pB	�yB	ҸB	��B	��B	��B	��B	��B	һB	ҶB	ҮB	�"B	�SB	�8B	��B	�'B	�GB	ҟB	�[B	�B	ѴB	�KB	�zB	�MB	�[B	�B	�4B	�B	�VB	�7B	ҘB	�{B	��B	��B	�;B	��B	�	B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	ъB	ѩB	�jB	�IB	�1B	�OB	�B	ҧB	��B	�B	ҡB	��B	� B	�	B	�B	��B	��B	ҦB	�!B	ғB	�'B	��B	��B	ՕB	�tB	֛B	ֆB	��B	ߤB	�B	�xB	��B	�xB	�0B	�B	��B	�RB	��B	��B	��B	�fB	��B	�gB	�B	�ZB	�B	��B	��B	��B	�B	��B	�xB	�MB	�B	��B	�PB	�B	��B	�dB	�B	�B	�B	�<B	� B	�B	�{B	�RB	�B	��B	�B	�9B	��B	�eB	�:B	�B	��B	�nB	�B	��B	�B	�#B	�oB	��B	�_B	�&B	��B	�[B	�"B	�"B	�B	��B	��B	�)B	��B	�B	��B	�B	�B	�GB	�+B	�[B	�HB	�kB	�B	�B	�B	�B	�[B	�PB	�>B	��B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�]B	�fB	�B	�vB	�kB	�_B	�SB	�*B	��B	��B	�B	�B	�B	�\B	�$B	�B	�(B	�B	�B	�B	� B	�B	�B	�B	�B	�KB	ҏB	��B	��B	�B	��B	�	B	��B	҈B	��B	�(B	��B	�	B	��B	�3B	��B	�$B	�+B	�B	�B	�B	�DB	ґB	�/B	�B	��B	чB	�KB	�xB	ѪB	��B	�9B	ҚB	�:B	һB	��B	�vB	�$B	�AB	�9B	�B	ѣB	�FB	ѱB	�TB	��B	�(B	�B	��B	��B	��B	�2B	цB	��B	�tB	ҮB	ҁB	�dB	��B	�1B	�B	�'B	�vB	��B	ҀB	�SB	ҟB	��B	��B	ҝB	ҫB	�KB	�mB	�$B	҂B	�EB	��B	�kB	ҧB	ҝB	�B	ҌB	҈B	�0B	�pB	��B	��B	ҋB	ѭB	�
B	�pB	қB	ҸB	�B	�yB	�`B	�;B	ґB	��B	��B	��B	�vB	ѣB	�B	ҭB	�B	ҴB	�JB	�YB	�B	ҴB	��B	ңB	��B	�tB	�dB	ҫB	һB	�B	�rB	�B	��B	�
B	ҏB	�UB	��B	��B	ѝB	��B	��B	ҘB	һB	�!B	�B	��B	�<B	�6B	�=B	�$B	�iB	�MB	�B	чB	�B	��B	��B	�}B	�)B	�GB	�@B	�4B	�.B	�$B	��B	�B	��B	�B	��B	��B	�GB	�9B	�9B	�iB	��B	��B	��B	�SB	ҭB	�B	��B	�RB	ҘB	��B	�)B	�B	��B	҈B	ҖB	��B	��B	��B	�9B	ҒB	ќB	ҒB	�9B	�pB	�yB	ҸB	��B	��B	��B	��B	��B	һB	ҶB	ҮB	�"B	�SB	�8B	��B	�'B	�GB	ҟB	�[B	�B	ѴB	�KB	�zB	�MB	�[B	�B	�4B	�B	�VB	�7B	ҘB	�{B	��B	��B	�;B	��B	�	B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	ъB	ѩB	�jB	�IB	�1B	�OB	�B	ҧB	��B	�B	ҡB	��B	� B	�	B	�B	��B	��B	ҦB	�!B	ғB	�'B	��B	��B	ՕB	�tB	֛B	ֆB	��B	ߤB	�B	�xB	��B	�xB	�0B	�B	��B	�RB	��B	��B	��B	�fB	��B	�gB	�B	�ZB	�B	��B	��B	��B	�B	��B	�xB	�MB	�B	��B	�PB	�B	��B	�dB	�B	�B	�B	�<B	� B	�B	�{B	�RB	�B	��B	�B	�9B	��B	�eB	�:B	�B	��B	�nB	�B	��B	�B	�#B	�oB	��B	�_B	�&B	��B	�[B	�"B	�"B	�B	��B	��B	�)B	��B	�B	��B	�B	�B	�GB	�+B	�[B	�HB	�kB	�B	�B	�B	�B	�[B	�PB	�>B	��B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�]B	�fB	�B	�vB	�kB	�_B	�SB	�*B	��B	��B	�B	�B	�B	�\B	�$B	�B	�(B	�B	�B	�B	� B	�B	�B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999444444334444444444444444344444444443344444444444344444444433443344444443344444443344444444334444443344333444444344444444444344443444444444444444444444444444444444333444444434434444444443433444444444333443334443444334344433333343334334433333333334433343333333333433333343333334433333343333344334334333344333433344333333433333334333333333333333333333333333333333333333333333333333333343333333333333333343333333333333333333333334433333333333333434333333333333333444333334333333334333343333344443333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455052020082814550520200828145505202008281455052020082814550520200828145505202008281455052020082814550520200828145505202008281455052020082814550520200828145505AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730372019021417303720190214173037    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730372019021417303720190214173037  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730372019021417303720190214173037  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455052020082814550520200828145505  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                